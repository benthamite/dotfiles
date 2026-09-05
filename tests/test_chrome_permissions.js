'use strict';
// All stores are fake LevelDB objects; every filesystem target is disposable.
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const helper = require('../codex/skills/chrome-permission-audit/chrome_permissions.js');
const SENTINEL = 'fixture-private-error-value-not-a-real-token';
function grant(id = 'a', changes = {}) {
  return { id, scope: { type: 'netloc', netloc: 'admin.example.test' }, action: 'allow',
    duration: 'always', createdAt: 1700000000000, surface: 'fixture', ...changes };
}
function fixture(t, profileNames = ['Default']) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'chrome-permission-tests-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const chromeRoot = path.join(root, 'chrome'), stateDir = path.join(root, 'state');
  const backupRoot = path.join(root, 'backups'), rulesFile = path.join(root, 'rules.json');
  fs.mkdirSync(chromeRoot, { mode: 0o700 });
  const info = Object.fromEntries(profileNames.map(dir => [dir, { name: dir, user_name: dir + '@example.invalid' }]));
  const localState = path.join(chromeRoot, 'Local State');
  const saveInventory = () => fs.writeFileSync(localState, JSON.stringify({ profile: { info_cache: info } }));
  saveInventory();
  const rules = { version: 1, rules: [
    { pattern: '^admin\\.', tier: 1, reason: 'fixture sensitive host' },
    { pattern: '^public\\.example\\.test$', tier: 3, reason: 'fixture public host' },
  ] };
  fs.writeFileSync(rulesFile, JSON.stringify(rules));
  const stores = new Map(), handles = [], hooks = {}, active = new Set();
  const storePath = dir => path.join(chromeRoot, dir, 'Local Extension Settings', helper.EXT_ID);
  for (const dir of profileNames) {
    fs.mkdirSync(storePath(dir), { recursive: true, mode: 0o700 });
    stores.set(storePath(dir), JSON.stringify({ permissions: [grant()] }));
  }
  class FakeLevel {
    constructor(store, options) {
      assert.ok(store.startsWith(root + path.sep));
      assert.equal(options.createIfMissing, false);
      this.store = store; this.puts = []; this.gets = []; this.closed = 0;
      handles.push(this);
    }
    async open() {
      if (hooks.open) await hooks.open(this);
      if (active.has(this.store)) throw Object.assign(new Error(SENTINEL), { code: 'LEVEL_LOCKED' });
      active.add(this.store); this.held = true;
    }
    async get(key) {
      assert.equal(key, 'permissionStorage', 'No OAuth or unrelated key may be read');
      this.gets.push(key);
      if (hooks.get) return hooks.get(this);
      return stores.get(this.store);
    }
    async put(key, value, options) {
      assert.equal(key, 'permissionStorage');
      assert.equal(options.sync, true);
      this.puts.push(value);
      if (hooks.put) return hooks.put(this, value);
      stores.set(this.store, value);
    }
    async close() {
      this.closed++;
      if (this.held) active.delete(this.store);
      if (hooks.close) await hooks.close(this);
    }
  }
  let closed = true;
  const messages = [], processes = [];
  const configuration = { home: root, chromeRoot, stateDir, backupRoot, rulesFile,
    driveRoot: path.join(root, 'Drive'), ClassicLevel: FakeLevel,
    assertClosed: () => closed,
    execFileSync: (...args) => { processes.push(args); throw new Error('fixture subprocess forbidden'); },
    print: value => messages.push(value), now: () => '2026-09-05T00:00:00.000Z' };
  const app = helper.createApp(configuration);
  const stateFile = path.join(stateDir, 'state.json');
  const planFile = path.join(root, 'plans', 'review.json');
  const plan = async (args = []) => app.run(['revoke', '--tier', '1', '--dry-run', '--plan', planFile, ...args]);
  const apply = () => app.run(['revoke', '--plan', planFile]);
  const putCount = () => handles.reduce((n, handle) => n + handle.puts.length, 0);
  const set = (records, dir = 'Default', extra = {}) => stores.set(storePath(dir), JSON.stringify({ ...extra, permissions: records }));
  const get = (dir = 'Default') => JSON.parse(stores.get(storePath(dir)));
  return { root, chromeRoot, stateDir, stateFile, backupRoot, rulesFile, info, saveInventory,
    stores, handles, hooks, active, messages, processes, configuration, app, planFile,
    plan, apply, putCount, set, get, storePath, closed: value => { closed = value; } };
}
const rejectsCode = (operation, code) => assert.rejects(operation, error => error.code === code);

test('paired helpers are identical and importing has no workflow side effects', () => {
  assert.equal(typeof helper.createApp, 'function');
  assert.deepEqual(fs.readFileSync(path.join(__dirname, '../codex/skills/chrome-permission-audit/chrome_permissions.js')),
    fs.readFileSync(path.join(__dirname, '../claude/skills/chrome-permission-audit/chrome_permissions.js')));
});

test('audit is non-recording by default and counts only standing site allows', async t => {
  const f = fixture(t);
  f.set([grant(), grant('deny', { action: 'deny' }), grant('once', { duration: 'once', toolUseId: 'tool' }),
    grant('transition', { scope: { type: 'domain_transition', fromDomain: 'one.test', toDomain: 'two.test' } })]);
  const result = await f.app.run(['audit']);
  assert.equal(result.standingAllows, 1); assert.equal(result.otherRecords, 3);
  assert.equal(result.violations, 1); assert.equal(result.recorded, false);
  assert.equal(fs.existsSync(f.stateDir), false); assert.equal(f.putCount(), 0);
  assert.equal(result.profiles[0].account, 'Default@example.invalid');
  assert.equal(result.profiles[0].rows[0].surface, 'fixture');
  assert.match(result.profiles[0].rows[0].created, /^2023-/);
  assert.ok(f.handles.every(handle => handle.closed === 1));
  assert.deepEqual(f.processes, []);
});

test('exact absence is empty, but read/schema failures are inconclusive and always close', async t => {
  const f = fixture(t);
  f.stores.set(f.storePath('Default'), undefined);
  assert.equal((await f.app.run(['audit'])).standingAllows, 0);
  f.hooks.get = () => { throw Object.assign(new Error(SENTINEL), { code: 'LEVEL_NOT_FOUND' }); };
  assert.equal((await f.app.run(['audit'])).standingAllows, 0);
  for (const raw of ['invalid ' + SENTINEL, '{}', '[]', JSON.stringify({ permissions: null }),
    JSON.stringify({ permissions: [grant('x', { action: 'new-action' })] }),
    JSON.stringify({ permissions: [grant('x', { scope: { type: 'new-scope' } })] }),
    JSON.stringify({ permissions: [grant('x', { duration: 'session' })] }),
    JSON.stringify({ permissions: [grant('x', { lastUsed: 'bad-time' })] })]) {
    f.hooks.get = () => raw;
    assert.equal(await f.app.main(['audit', '--record']), 2);
    assert.equal(fs.existsSync(f.stateFile), false);
  }
  f.hooks.get = () => { throw Object.assign(new Error(SENTINEL), { code: 'EIO' }); };
  assert.equal(await f.app.main(['audit']), 2);
  assert.ok(!f.messages.join('\n').includes(SENTINEL));
  assert.ok(f.handles.every(handle => handle.closed === 1));
});

test('Tier 2 standing grants remain explicitly review-required', async t => {
  const f = fixture(t);
  const rules = JSON.parse(fs.readFileSync(f.rulesFile));
  rules.rules[0].tier = 2;
  fs.writeFileSync(f.rulesFile, JSON.stringify(rules));
  f.set([grant(), grant('deny', { action: 'deny' })]);
  const report = await f.app.run(['audit']);
  assert.equal(report.reviewRequired, 1);
  assert.equal(report.violations, 0);
  assert.equal(report.unclassified, 0);
  assert.equal(report.policy, 'review-required');
});

test('open and close errors are not converted to empty grants or assumed lock failures', async t => {
  const f = fixture(t);
  f.hooks.open = () => { throw Object.assign(new Error(SENTINEL), { code: 'EIO' }); };
  await rejectsCode(() => f.app.run(['audit']), 'DB_OPEN');
  f.hooks.open = () => { throw Object.assign(new Error(SENTINEL), { code: 'LEVEL_LOCKED' }); };
  await rejectsCode(() => f.app.run(['audit']), 'DB_LOCKED');
  delete f.hooks.open;
  f.hooks.close = () => { throw new Error(SENTINEL); };
  await rejectsCode(() => f.app.run(['audit', '--record']), 'DB_CLOSE');
  assert.equal(fs.existsSync(f.stateFile), false);
  assert.ok(f.handles.every(handle => handle.closed === 1));
});

test('closed-source evidence is mandatory and a failed probe is never assumed closed', async t => {
  const f = fixture(t);
  f.closed(false);
  await rejectsCode(() => f.app.run(['audit']), 'CHROME_OPEN');
  assert.equal(f.handles.length, 0);
  const app = helper.createApp({ ...f.configuration, assertClosed: undefined,
    execFileSync: () => { throw Object.assign(new Error(SENTINEL), { status: 2 }); } });
  await rejectsCode(() => app.run(['audit']), 'CLOSED_UNKNOWN');
  assert.equal(f.handles.length, 0);
});

test('missing dependencies do not install themselves and setup is explicitly pinned', async t => {
  const f = fixture(t);
  const app = helper.createApp({ ...f.configuration, ClassicLevel: undefined });
  await rejectsCode(() => app.run(['audit']), 'DEPENDENCY_MISSING');
  assert.deepEqual(f.processes, []);
  const calls = [];
  const setup = helper.createApp({ ...f.configuration, execFileSync: (...args) => calls.push(args) });
  assert.equal((await setup.run(['setup'])).installedVersion, '3.0.0');
  assert.equal(calls.length, 1); assert.equal(calls[0][0], 'npm');
  assert.ok(calls[0][1].includes('classic-level@3.0.0'));
  assert.ok(calls[0][1].includes('--ignore-scripts'));
});

test('private paths resolving into Drive are rejected without migrating state', async t => {
  const f = fixture(t);
  const drive = path.join(f.root, 'Drive'); fs.mkdirSync(drive);
  const link = path.join(f.root, 'linked-state'); fs.symlinkSync(drive, link);
  const app = helper.createApp({ ...f.configuration, stateDir: link });
  await rejectsCode(() => app.run(['accept', '--site', 'public.example.test', '--tier', '3']), 'UNSAFE_PATH');
  assert.deepEqual(fs.readdirSync(drive), []);
});

test('invalid CLI options fail before state or database access', async t => {
  const f = fixture(t);
  for (const args of [['audit', '--unknown'], ['audit', '--record', '--no-record'],
    ['revoke', '--sites', 'one.test', '--tier', '1', '--dry-run'], ['revoke', '--tier', '1'],
    ['revoke', '--sites', '', '--dry-run'], ['revoke', '--sites', 'one.test,', '--dry-run'],
    ['accept', '--site', 'one.test', '--tier', 'NaN'], ['accept', '--site', 'https://one.test', '--tier', '3'],
    ['accept', '--site', 'one.test:99999', '--tier', '3'], ['accept', '--site', 'one.test'],
    ['state', '--site', 'one.test'], ['setup', '--yes']]) {
    await assert.rejects(() => f.app.run(args));
  }
  assert.equal(fs.existsSync(f.stateDir), false); assert.equal(f.handles.length, 0);
});

test('profile selectors prefer exact directories and reject ambiguous labels', async t => {
  const f = fixture(t, ['Default', 'Profile 1']);
  f.info.Default.name = 'Profile 1'; f.saveInventory();
  const result = await f.app.run(['revoke', '--tier', '1', '--profile', 'Profile 1', '--dry-run']);
  assert.deepEqual(result.plan.profiles.map(p => p.dir), ['Profile 1']);
  f.info.Default.name = 'Work'; f.info['Profile 1'].name = 'Work'; f.saveInventory();
  await rejectsCode(() => f.app.run(['revoke', '--tier', '1', '--profile', 'Work', '--dry-run']), 'PROFILE_AMBIGUOUS');
});

test('orphan profiles and traversal/symlink inventory cannot silently reduce coverage', async t => {
  const f = fixture(t);
  const orphan = f.storePath('Profile 7'); fs.mkdirSync(orphan, { recursive: true });
  await rejectsCode(() => f.app.run(['audit']), 'PROFILE_COVERAGE');
  f.info['Profile 7'] = { name: 'Recovered' }; f.saveInventory();
  f.stores.set(orphan, undefined);
  assert.equal((await f.app.run(['audit'])).profiles.length, 2);
  f.info['../outside'] = {}; f.saveInventory();
  await rejectsCode(() => f.app.run(['audit']), 'PROFILE_INVALID');
});

test('drift compares last recorded inventory including regrants and action/duration changes', async t => {
  const f = fixture(t);
  await f.app.run(['audit', '--record']);
  f.set([]); assert.equal((await f.app.run(['audit', '--record'])).drift[0].removed, 1);
  f.set([grant()]); assert.equal((await f.app.run(['audit', '--record'])).drift[0].added, 1);
  f.set([grant('a', { duration: 'once' })]);
  assert.deepEqual((await f.app.run(['audit', '--record'])).drift, [{ profile: 'Default', added: 1, removed: 1 }]);
  f.set([grant('a', { duration: 'once', lastUsed: 1700000000100 })]);
  assert.deepEqual((await f.app.run(['audit'])).drift, []);
});

test('legacy baseline retains overrides but needs an explicit new recording', async t => {
  const f = fixture(t); fs.mkdirSync(f.stateDir, { mode: 0o700 });
  const original = JSON.stringify({ lastRun: 'old', seen: { 'Default::admin.example.test': {} },
    overrides: { 'admin.example.test': { tier: 2, note: 'fixture decision' } } });
  fs.writeFileSync(f.stateFile, original);
  const result = await f.app.run(['audit']);
  assert.equal(result.baseline, true); assert.equal(result.violations, 0);
  assert.equal(fs.readFileSync(f.stateFile, 'utf8'), original);
  await f.app.run(['audit', '--record']);
  assert.equal(f.app.loadState().state.version, 2);
  assert.equal(f.app.loadState().state.overrides['admin.example.test'].tier, 2);
});

test('prototype-like site names are own-key overrides only', async t => {
  const f = fixture(t);
  assert.equal(helper.classify('constructor', [], {}).tier, 0);
  await f.app.run(['accept', '--site', '__proto__', '--tier', '2']);
  const state = f.app.loadState().state;
  assert.equal(Object.prototype.tier, undefined);
  assert.equal(helper.classify('__proto__', [], state.overrides).tier, 2);
});

test('dry run writes no state and an explicit plan is private and never overwritten', async t => {
  const f = fixture(t);
  const plain = await f.app.run(['revoke', '--tier', '1', '--dry-run']);
  assert.equal(plain.savedPlan, null); assert.equal(fs.existsSync(f.stateDir), false);
  const result = await f.plan();
  assert.equal(fs.statSync(f.planFile).mode & 0o777, 0o600);
  assert.equal(result.plan.profiles[0].targets[0].surface, 'fixture');
  await assert.rejects(() => f.plan(), error => error.code === 'EEXIST');
  assert.equal(f.putCount(), 0);
});

test('revocation removes only reviewed standing allows and retains exact key backup', async t => {
  const f = fixture(t);
  f.set([grant(), grant('deny', { action: 'deny' }), grant('once', { duration: 'once' }),
    grant('transition', { scope: { type: 'domain_transition', fromDomain: 'a.test', toDomain: 'b.test' } }),
    grant('other', { scope: { type: 'netloc', netloc: 'public.example.test' } })], 'Default', { metadata: { preserve: true } });
  const original = f.stores.get(f.storePath('Default'));
  await f.plan(); const result = await f.apply();
  assert.equal(result.exitCode, 0); assert.equal(result.results[0].removed, 1);
  assert.deepEqual(f.get().permissions.map(r => r.id), ['deny', 'once', 'transition', 'other']);
  assert.deepEqual(f.get().metadata, { preserve: true });
  const backup = result.results[0].backup;
  assert.equal(fs.statSync(backup).mode & 0o777, 0o600);
  assert.equal(JSON.parse(fs.readFileSync(backup)).raw, original);
  assert.equal(f.handles.flatMap(h => h.gets).every(key => key === 'permissionStorage'), true);
  assert.equal(fs.existsSync(f.stateFile), false);
});

test('new/recreated grants or added tier matches invalidate reviewed storage', async t => {
  const f = fixture(t); await f.plan();
  for (const records of [[grant('new')], [grant('a', { duration: 'once' })],
    [grant('a', { action: 'deny' })], [grant(), grant('new')]]) {
    f.set(records); const before = f.stores.get(f.storePath('Default'));
    const result = await f.apply();
    assert.equal(result.exitCode, 2); assert.equal(result.results[0].status, 'unchanged');
    assert.equal(f.stores.get(f.storePath('Default')), before);
  }
  assert.equal(f.putCount(), 0); assert.equal(fs.existsSync(f.backupRoot), false);
});

test('rules and override changes invalidate plans before writes', async t => {
  const f = fixture(t); await f.plan();
  await f.app.run(['accept', '--site', 'admin.example.test', '--tier', '2']);
  await rejectsCode(() => f.apply(), 'PLAN_STALE');
  assert.equal(f.putCount(), 0);
});

test('policy drift during application prevents later profile writes', async t => {
  const f = fixture(t, ['Default', 'Profile 1']); await f.plan();
  f.hooks.put = (db, value) => {
    f.stores.set(db.store, value);
    fs.appendFileSync(f.rulesFile, '\n');
  };
  const result = await f.apply();
  assert.equal(result.results[0].status, 'verified-key-write');
  assert.equal(result.results[1].status, 'unchanged');
  assert.equal(result.results[1].error.code, 'PLAN_STALE');
  assert.equal(result.exitCode, 2); assert.equal(f.putCount(), 1);
});

test('all planned profile presence is checked before any mutation', async t => {
  const f = fixture(t, ['Default', 'Profile 1']); await f.plan();
  delete f.info['Profile 1']; f.saveInventory();
  // Remove the fixture store so discovery sees absence, not an orphan metadata gap.
  fs.rmSync(path.join(f.chromeRoot, 'Profile 1'), { recursive: true });
  await rejectsCode(() => f.apply(), 'PLAN_STALE');
  assert.equal(f.putCount(), 0);
});

test('reviewed account or profile-label changes invalidate all profiles before writes', async t => {
  for (const field of ['user_name', 'name']) {
    const f = fixture(t, ['Default', 'Profile 1']); await f.plan();
    f.info['Profile 1'][field] = 'changed-account@example.invalid'; f.saveInventory();
    await rejectsCode(() => f.apply(), 'PLAN_STALE');
    assert.equal(f.putCount(), 0); assert.equal(fs.existsSync(f.backupRoot), false);
  }
});

test('replacement store with identical permission bytes invalidates a reviewed plan', async t => {
  const f = fixture(t); const planned = await f.plan();
  const store = f.storePath('Default');
  const original = f.stores.get(store);
  fs.renameSync(store, path.join(f.root, 'original-store'));
  fs.mkdirSync(store, { mode: 0o700 });
  assert.notEqual(fs.statSync(store, { bigint: true }).ino.toString(), planned.plan.profiles[0].identity.store.inode);
  await rejectsCode(() => f.apply(), 'PLAN_STALE');
  assert.equal(f.putCount(), 0); assert.equal(f.stores.get(store), original);
});

test('profile identity is revalidated inside the exclusive database operation', async t => {
  for (const change of ['account', 'store']) {
    const f = fixture(t); await f.plan();
    f.hooks.open = db => {
      if (change === 'account') {
        f.info.Default.user_name = 'changed-account@example.invalid'; f.saveInventory();
      } else {
        fs.renameSync(db.store, path.join(f.root, 'original-store'));
        fs.mkdirSync(db.store, { mode: 0o700 });
      }
    };
    const result = await f.apply();
    assert.equal(result.exitCode, 2); assert.equal(result.results[0].status, 'unchanged');
    assert.equal(result.results[0].error.code, 'PLAN_STALE');
    assert.equal(f.putCount(), 0); assert.equal(fs.existsSync(f.backupRoot), false);
    assert.ok(f.handles.every(handle => handle.closed === 1));
  }
});

test('backup failure precedes any permission write', async t => {
  const f = fixture(t); await f.plan();
  const realOpen = fs.openSync;
  fs.openSync = function (target, ...args) {
    if (typeof target === 'string' && target.startsWith(f.backupRoot + path.sep)) throw new Error(SENTINEL);
    return realOpen.call(this, target, ...args);
  };
  try {
    const result = await f.apply();
    assert.equal(result.exitCode, 2); assert.equal(result.results[0].status, 'unchanged');
    assert.equal(f.putCount(), 0);
    assert.ok(!JSON.stringify(result).includes(SENTINEL));
  } finally { fs.openSync = realOpen; }
});

test('partial multi-profile failures report successes and distinguish real lock errors', async t => {
  const f = fixture(t, ['Default', 'Profile 1', 'Profile 2']); await f.plan();
  f.hooks.open = db => {
    if (db.store === f.storePath('Profile 1')) throw Object.assign(new Error(SENTINEL), { code: 'LEVEL_LOCKED' });
    if (db.store === f.storePath('Profile 2')) throw Object.assign(new Error(SENTINEL), { code: 'EIO' });
  };
  const result = await f.apply();
  assert.equal(result.exitCode, 2);
  assert.deepEqual(result.results.map(r => r.status), ['verified-key-write', 'unchanged', 'unchanged']);
  assert.deepEqual(result.results.slice(1).map(r => r.error.code), ['DB_LOCKED', 'DB_OPEN']);
  assert.ok(result.results[0].backup); assert.equal(f.putCount(), 1);
});

test('put-after-effect, readback and close failures retain backup and report uncertain outcome', async t => {
  for (const failure of ['put', 'get', 'close']) {
    const f = fixture(t); await f.plan();
    let written = false;
    f.hooks.put = (db, value) => {
      f.stores.set(db.store, value); written = true;
      if (failure === 'put') throw new Error(SENTINEL);
    };
    f.hooks.get = db => {
      if (written && failure === 'get') throw new Error(SENTINEL);
      return f.stores.get(db.store);
    };
    f.hooks.close = () => { if (written && failure === 'close') throw new Error(SENTINEL); };
    const result = await f.apply();
    assert.equal(result.exitCode, 2); assert.equal(result.results[0].status, 'uncertain');
    assert.equal(f.get().permissions.length, 0);
    assert.equal(JSON.parse(fs.readFileSync(result.results[0].backup)).key, 'permissionStorage');
    assert.ok(!JSON.stringify(result).includes(SENTINEL));
  }
});

test('same-time backups remain unique across profiles', async t => {
  const f = fixture(t, ['Default', 'Profile 1']); await f.plan();
  const result = await f.apply();
  const backups = result.results.map(r => r.backup);
  assert.equal(new Set(backups).size, 2);
  assert.ok(backups.every(file => fs.existsSync(file)));
});

test('malformed state is never reset, and state commits preserve valid old/new JSON on IO faults', async t => {
  const f = fixture(t); await f.app.run(['accept', '--site', 'one.test', '--tier', '2']);
  const original = fs.readFileSync(f.stateFile, 'utf8');
  for (const afterEffect of [false, true]) {
    fs.writeFileSync(f.stateFile, original);
    const realRename = fs.renameSync;
    fs.renameSync = (source, target) => {
      if (afterEffect) realRename(source, target);
      throw new Error(SENTINEL);
    };
    try {
      assert.equal(await f.app.main(['accept', '--site', 'two.test', '--tier', '3']), 2);
    } finally { fs.renameSync = realRename; }
    const stored = JSON.parse(fs.readFileSync(f.stateFile));
    assert.equal(stored.overrides['one.test'].tier, 2);
    assert.equal(Boolean(stored.overrides['two.test']), afterEffect);
    assert.equal(fs.readdirSync(f.stateDir).some(name => name.endsWith('.tmp')), false);
  }
  fs.writeFileSync(f.stateFile, 'invalid ' + SENTINEL);
  await rejectsCode(() => f.app.run(['accept', '--site', 'three.test', '--tier', '1']), 'STATE_INVALID');
  assert.equal(fs.readFileSync(f.stateFile, 'utf8'), 'invalid ' + SENTINEL);
  assert.ok(!f.messages.join('\n').includes(SENTINEL));
});

test('state fsync failure preserves earlier overrides and removes only staging', async t => {
  const f = fixture(t); await f.app.run(['accept', '--site', 'one.test', '--tier', '2']);
  const original = fs.readFileSync(f.stateFile);
  const realSync = fs.fsyncSync; fs.fsyncSync = () => { throw new Error(SENTINEL); };
  try { await assert.rejects(() => f.app.run(['accept', '--site', 'two.test', '--tier', '3'])); }
  finally { fs.fsyncSync = realSync; }
  assert.deepEqual(fs.readFileSync(f.stateFile), original);
  assert.deepEqual(fs.readdirSync(f.stateDir), ['state.json']);
});

test('failed state publication preserves a foreign replacement at the staging path', async t => {
  const f = fixture(t); await f.app.run(['accept', '--site', 'one.test', '--tier', '2']);
  const original = fs.readFileSync(f.stateFile);
  const replacement = path.join(f.stateDir, 'foreign-file');
  fs.writeFileSync(replacement, 'independent replacement');
  const realRename = fs.renameSync;
  let staging;
  fs.renameSync = (source, target) => {
    staging = source;
    realRename(replacement, source);
    throw new Error(SENTINEL);
  };
  try { await assert.rejects(() => f.app.run(['accept', '--site', 'two.test', '--tier', '3'])); }
  finally { fs.renameSync = realRename; }
  assert.deepEqual(fs.readFileSync(f.stateFile), original);
  assert.equal(fs.readFileSync(staging, 'utf8'), 'independent replacement');
});

test('state contention rejects rather than losing successful overrides', async t => {
  const f = fixture(t);
  const first = f.app.run(['accept', '--site', 'one.test', '--tier', '2']);
  const second = f.app.run(['accept', '--site', 'two.test', '--tier', '3']);
  await first; await rejectsCode(() => second, 'STATE_BUSY');
  await f.app.run(['accept', '--site', 'two.test', '--tier', '3']);
  assert.deepEqual(Object.keys(f.app.loadState().state.overrides).sort(), ['one.test', 'two.test']);
});

test('recorded audit and accept cannot overwrite each other', async t => {
  const f = fixture(t);
  let release, entered;
  const gate = new Promise(resolve => { release = resolve; });
  const ready = new Promise(resolve => { entered = resolve; });
  f.hooks.get = async db => { entered(); await gate; return f.stores.get(db.store); };
  const recording = f.app.run(['audit', '--record']); await ready;
  await rejectsCode(() => f.app.run(['accept', '--site', 'one.test', '--tier', '2']), 'STATE_BUSY');
  release(); await recording;
  await f.app.run(['accept', '--site', 'one.test', '--tier', '2']);
  const state = f.app.loadState().state;
  assert.equal(state.current.Default.length, 1); assert.equal(state.overrides['one.test'].tier, 2);
});

test('noncooperating state changes are detected before replacement', async t => {
  const f = fixture(t);
  f.hooks.get = db => {
    fs.writeFileSync(f.stateFile, JSON.stringify({ version: 2, lastRun: null, current: {},
      overrides: { 'other.test': { tier: 3 } } }));
    return f.stores.get(db.store);
  };
  await rejectsCode(() => f.app.run(['audit', '--record']), 'STATE_CHANGED');
  assert.equal(f.app.loadState().state.overrides['other.test'].tier, 3);
});
