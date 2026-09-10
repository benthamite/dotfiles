#!/usr/bin/env node
// Audit stored standing site allows, not the extension's effective runtime access.
// Direct LevelDB opens may change engine housekeeping files. No live DB copies.
'use strict';

const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const { execFileSync } = require('child_process');
const EXT_ID = 'fcoeoabgfenejglbffodgkkbkcdhcgfn';
const LEVEL_VERSION = '3.0.0';
const object = x => x !== null && typeof x === 'object' && !Array.isArray(x);
const own = (x, key) => Object.prototype.hasOwnProperty.call(x, key);
const hash = x => crypto.createHash('sha256').update(x).digest('hex');
function stable(x) {
  if (Array.isArray(x)) return '[' + x.map(stable).join(',') + ']';
  if (object(x)) return '{' + Object.keys(x).sort().map(k => JSON.stringify(k) + ':' + stable(x[k])).join(',') + '}';
  return JSON.stringify(x);
}
class AuditError extends Error {
  constructor(code, message) { super(message); this.code = code; }
}
function fail(code, message) { throw new AuditError(code, message); }
function safeError(error) {
  return error instanceof AuditError ? { code: error.code, message: error.message }
    : { code: 'IO_ERROR', message: 'Operation failed; private error details were not logged.' };
}
function validSite(site) {
  if (typeof site !== 'string' || !site || site.length > 253 ||
      !/^(?:file|(?:\*\.)?[a-zA-Z0-9_-]+(?:\.[a-zA-Z0-9_-]+)*\.?(?::[0-9]{1,5})?|\[[0-9a-fA-F:]+\](?::[0-9]{1,5})?)$/.test(site)) {
    fail('SITE_INVALID', 'Site must be an exact stored host identity, optionally with its port or wildcard.');
  }
  const port = site.match(/:(\d+)$/);
  if (port && Number(port[1]) > 65535) fail('SITE_INVALID', 'Site port is outside the supported range.');
}
function validDir(dir) {
  if (typeof dir !== 'string' || !dir || dir === '.' || dir === '..' || /[\/\\\x00-\x1f]/.test(dir)) {
    fail('PROFILE_INVALID', 'Unsupported Chrome profile directory identity.');
  }
}
function parseStorage(raw) {
  if (raw === undefined) return { parsed: { permissions: [] }, raw: null };
  if (Buffer.isBuffer(raw)) raw = raw.toString('utf8');
  if (typeof raw !== 'string') fail('SCHEMA_UNKNOWN', 'Permission storage has an unsupported value type.');
  let parsed;
  try { parsed = JSON.parse(raw); } catch { fail('SCHEMA_UNKNOWN', 'Permission storage is not valid JSON.'); }
  if (!object(parsed) || !Array.isArray(parsed.permissions)) fail('SCHEMA_UNKNOWN', 'Permission storage schema is unsupported.');
  const ids = new Set();
  for (const record of parsed.permissions) {
    if (!object(record) || typeof record.id !== 'string' || !record.id || ids.has(record.id) ||
        !object(record.scope) || !['allow', 'deny'].includes(record.action) ||
        !['once', 'always'].includes(record.duration) ||
        !Number.isFinite(record.createdAt) || record.createdAt < 0 || record.createdAt > 8640000000000000) {
      fail('SCHEMA_UNKNOWN', 'Permission record schema or identity is unsupported.');
    }
    ids.add(record.id);
    if (record.scope.type === 'netloc') validSite(record.scope.netloc);
    else if (record.scope.type === 'domain_transition') {
      validSite(record.scope.fromDomain); validSite(record.scope.toDomain);
    } else fail('SCHEMA_UNKNOWN', 'Permission scope is unsupported; audit is inconclusive.');
    for (const key of ['toolUseId', 'origin', 'surface']) {
      if (own(record, key) && typeof record[key] !== 'string') fail('SCHEMA_UNKNOWN', 'Optional permission metadata has an unsupported type.');
    }
    if (own(record, 'lastUsed') && (!Number.isFinite(record.lastUsed) || record.lastUsed < 0 ||
        record.lastUsed > 8640000000000000)) fail('SCHEMA_UNKNOWN', 'Permission usage time is unsupported.');
  }
  return { parsed, raw };
}
const standing = r => r.action === 'allow' && r.duration === 'always' && r.scope.type === 'netloc';
const recordHash = r => hash(stable(r));
function driftHash(r) {
  const copy = { ...r };
  delete copy.lastUsed;
  return hash(stable(copy));
}
function classify(netloc, rules, overrides) {
  if (own(overrides, netloc)) return { ...overrides[netloc], source: 'override' };
  // A wildcard is not a verified public host. Never strip it into a keep rule.
  const host = netloc.toLowerCase().replace(/:\d+$/, '').replace(/\.$/, '');
  for (const rule of rules) {
    if (rule.re.test(host) && !(host.startsWith('*.') && rule.tier === 3)) {
      return { tier: rule.tier, note: rule.reason, source: 'rule' };
    }
  }
  return { tier: 0, note: 'No verified matching policy rule.', source: 'none' };
}
function validateOverrides(overrides) {
  if (!object(overrides)) fail('STATE_INVALID', 'Stored classification overrides are malformed.');
  for (const [site, value] of Object.entries(overrides)) {
    validSite(site);
    if (!object(value) || ![1, 2, 3].includes(value.tier) ||
        (own(value, 'note') && typeof value.note !== 'string')) fail('STATE_INVALID', 'Stored classification override is malformed.');
  }
}
function parseArgs(argv) {
  const [command = 'help', ...args] = argv;
  const allowed = {
    audit: { '--json': false, '--record': false, '--no-record': false },
    revoke: { '--sites': true, '--tier': true, '--profile': true, '--dry-run': false, '--plan': true },
    accept: { '--site': true, '--tier': true, '--note': true },
    setup: {}, state: {}, help: {},
  };
  if (!own(allowed, command)) fail('USAGE', 'Unknown command.');
  const options = Object.create(null);
  for (let i = 0; i < args.length; i++) {
    const flag = args[i];
    if (!own(allowed[command], flag) || own(options, flag)) fail('USAGE', 'Unknown, repeated or positional option.');
    if (allowed[command][flag]) {
      if (i + 1 >= args.length || args[i + 1].startsWith('--')) fail('USAGE', 'Option requires a value.');
      options[flag] = args[++i];
    } else options[flag] = true;
  }
  if (options['--record'] && options['--no-record']) fail('USAGE', 'Choose record or no-record, not both.');
  if (own(options, '--tier') && !/^[123]$/.test(options['--tier'])) fail('USAGE', 'Tier must be 1, 2 or 3.');
  if (command === 'accept') {
    if (!options['--site'] || !options['--tier']) fail('USAGE', 'accept requires --site and --tier.');
    validSite(options['--site']);
  }
  if (command === 'revoke') {
    const selectors = Number(own(options, '--sites')) + Number(own(options, '--tier'));
    if (options['--dry-run']) {
      if (selectors !== 1) fail('USAGE', 'A dry run requires exactly one sites or tier selector.');
    } else if (!options['--plan'] || selectors || options['--profile']) {
      fail('USAGE', 'Actual revocation requires only --plan FILE from a reviewed dry run.');
    }
    if (own(options, '--sites')) {
      const sites = options['--sites'].split(',').map(s => s.trim());
      for (const site of sites) validSite(site);
      if (new Set(sites).size !== sites.length) fail('USAGE', 'Repeated sites are ambiguous.');
      options.sites = sites;
    }
    if (own(options, '--profile') && !options['--profile']) fail('USAGE', 'Profile must not be empty.');
  }
  return { command, options };
}

function createApp(injected = {}) {
  const home = injected.home || os.homedir();
  const chromeRoot = injected.chromeRoot || path.join(home, 'Library/Application Support/Google/Chrome');
  const stateDir = injected.stateDir || path.join(home, '.claude/chrome-permission-audit');
  const stateFile = path.join(stateDir, 'state.json');
  const backupRoot = injected.backupRoot || path.join(home, 'Library/Application Support/claude-chrome-permission-backup');
  const rulesFile = injected.rulesFile || path.join(__dirname, 'rules.json');
  const privateRulesFile = path.join(stateDir, 'private-rules.json');
  const driveRoot = injected.driveRoot || path.join(home, 'My Drive');
  const execute = injected.execFileSync || execFileSync;
  const print = injected.print || (text => process.stdout.write(text + '\n'));
  const now = injected.now || (() => new Date().toISOString());
  function resolved(target) {
    if (fs.existsSync(target)) return fs.realpathSync(target);
    const parent = path.dirname(target);
    return parent === target ? target : path.join(resolved(parent), path.basename(target));
  }
  function outsideDrive(target) {
    const relative = path.relative(resolved(driveRoot), resolved(target));
    if (!relative || (!relative.startsWith('..' + path.sep) && relative !== '..' && !path.isAbsolute(relative))) {
      fail('UNSAFE_PATH', 'Private state, plans, backups and dependencies must remain outside Drive.');
    }
  }
  function privateDir(target) {
    outsideDrive(target);
    fs.mkdirSync(target, { recursive: true, mode: 0o700 });
    if (!fs.statSync(target).isDirectory() || (fs.statSync(target).mode & 0o077)) {
      fail('UNSAFE_PATH', 'Private data directory must have owner-only permissions.');
    }
  }
  function writePrivate(target, bytes) {
    outsideDrive(target);
    privateDir(path.dirname(target));
    const fd = fs.openSync(target, 'wx', 0o600);
    const identity = fs.fstatSync(fd);
    try { fs.writeFileSync(fd, bytes); fs.fsyncSync(fd); }
    catch (error) {
      if (fs.existsSync(target) && fs.lstatSync(target).ino === identity.ino) fs.unlinkSync(target);
      throw error;
    } finally { fs.closeSync(fd); }
    return identity;
  }
  function fileDigest(target) {
    try { return hash(fs.readFileSync(target)); }
    catch (error) { if (error.code === 'ENOENT') return null; throw error; }
  }
  function atomicState(state, expected) {
    privateDir(stateDir);
    const temporary = path.join(stateDir, '.state-' + crypto.randomUUID() + '.tmp');
    let identity;
    try {
      identity = writePrivate(temporary, JSON.stringify(state, null, 2));
      if (fileDigest(stateFile) !== expected) fail('STATE_CHANGED', 'State changed during update; no stale replacement was made.');
      fs.renameSync(temporary, stateFile);
    } finally {
      if (identity && fs.existsSync(temporary)) {
        const current = fs.lstatSync(temporary);
        if (current.dev === identity.dev && current.ino === identity.ino) fs.unlinkSync(temporary);
      }
    }
  }
  async function stateLock(operation) {
    privateDir(stateDir);
    const lock = path.join(stateDir, 'state.lock');
    let fd;
    try { fd = fs.openSync(lock, 'wx', 0o600); }
    catch (error) {
      if (error.code === 'EEXIST') fail('STATE_BUSY', 'Another state operation or an unreconciled stale lock exists.');
      throw error;
    }
    const identity = fs.fstatSync(fd);
    try { return await operation(); }
    finally {
      fs.closeSync(fd);
      if (fs.existsSync(lock) && fs.lstatSync(lock).ino === identity.ino) fs.unlinkSync(lock);
    }
  }
  function loadState() {
    outsideDrive(stateDir);
    const digest = fileDigest(stateFile);
    if (digest === null) return { state: { version: 2, lastRun: null, current: {}, overrides: {} }, digest };
    let state;
    try { state = JSON.parse(fs.readFileSync(stateFile, 'utf8')); }
    catch { fail('STATE_INVALID', 'Stored state is unreadable or invalid JSON; it was not reset.'); }
    if (!object(state)) fail('STATE_INVALID', 'Stored state is malformed.');
    validateOverrides(state.overrides);
    if (!own(state, 'version') && object(state.seen)) {
      // Historical ever-seen state cannot establish last-audit drift.
      state = { version: 2, lastRun: null, current: {}, overrides: state.overrides,
        baselineReason: 'Legacy ever-seen baseline requires a new explicit recording.' };
    }
    if (state.version !== 2 || !(state.lastRun === null || typeof state.lastRun === 'string') ||
        !object(state.current) || Object.values(state.current).some(x => !Array.isArray(x) ||
          x.some(y => typeof y !== 'string'))) fail('STATE_INVALID', 'Stored baseline schema is unsupported.');
    return { state, digest };
  }
  function loadRules() {
    let spec;
    let raw = fs.readFileSync(rulesFile, 'utf8');
    try { spec = JSON.parse(raw); } catch { fail('RULES_INVALID', 'Classification rules are invalid JSON.'); }
    if (!object(spec) || spec.version !== 1 || !Array.isArray(spec.rules)) fail('RULES_INVALID', 'Classification rules schema is unsupported.');
    // Owner-specific policy remains outside the distributable skill. A malformed
    // configured policy is an error, never permission to omit its concerns.
    outsideDrive(privateRulesFile);
    try {
      const privateRaw = fs.readFileSync(privateRulesFile, 'utf8');
      let privateSpec;
      try { privateSpec = JSON.parse(privateRaw); }
      catch { fail('RULES_INVALID', 'Private classification rules are invalid JSON.'); }
      if (!object(privateSpec) || privateSpec.version !== 1 || !Array.isArray(privateSpec.rules)) {
        fail('RULES_INVALID', 'Private classification rules schema is unsupported.');
      }
      spec.rules = [...privateSpec.rules, ...spec.rules];
      raw += '\nprivate-rules:\n' + privateRaw;
    } catch (error) {
      if (error.code !== 'ENOENT') throw error;
    }
    const rules = spec.rules.map(rule => {
      if (!object(rule) || ![1, 2, 3].includes(rule.tier) || typeof rule.pattern !== 'string' ||
          typeof rule.reason !== 'string') fail('RULES_INVALID', 'Classification rule is malformed.');
      try { return { ...rule, re: new RegExp(rule.pattern) }; }
      catch { fail('RULES_INVALID', 'Classification rule pattern is invalid.'); }
    });
    return { rules, raw };
  }
  function policy(state, raw) { return hash(raw + stable(state.overrides)); }
  function loadClassic() {
    outsideDrive(stateDir);
    if (injected.ClassicLevel) return injected.ClassicLevel;
    const dependency = path.join(stateDir, 'node_modules/classic-level');
    let metadata;
    try { metadata = JSON.parse(fs.readFileSync(path.join(dependency, 'package.json'), 'utf8')); }
    catch { fail('DEPENDENCY_MISSING', 'classic-level is unavailable; explicit reviewed setup is required.'); }
    if (metadata.version !== LEVEL_VERSION) fail('DEPENDENCY_VERSION', 'Installed classic-level version is not the reviewed version.');
    try {
      const Constructor = require(dependency).ClassicLevel;
      if (typeof Constructor !== 'function') throw new Error();
      return Constructor;
    } catch { fail('DEPENDENCY_BROKEN', 'classic-level could not load; diagnose it without automatic installation.'); }
  }
  function assertClosed() {
    if (injected.assertClosed) {
      if (injected.assertClosed() !== true) fail('CHROME_OPEN', 'Chrome is not confirmed closed; audit is inconclusive.');
      return;
    }
    try {
      execute('pgrep', ['-f', 'Google Chrome[.]app/Contents/'], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
      fail('CHROME_OPEN', 'Chrome is running; no permission access was attempted. Do not automatically quit it.');
    } catch (error) {
      if (error instanceof AuditError) throw error;
      if (error.status === 1 && !error.stdout && !error.stderr) return;
      fail('CLOSED_UNKNOWN', 'Could not establish that Chrome is closed; audit is inconclusive.');
    }
  }
  function discoverProfiles() {
    let info;
    try {
      const local = JSON.parse(fs.readFileSync(path.join(chromeRoot, 'Local State'), 'utf8'));
      info = local.profile.info_cache;
    } catch { fail('PROFILES_UNKNOWN', 'Chrome profile inventory could not be read reliably.'); }
    if (!object(info)) fail('PROFILES_UNKNOWN', 'Chrome profile inventory schema is unsupported.');
    for (const dir of fs.readdirSync(chromeRoot)) {
      if ((dir === 'Default' || /^Profile \d+$/.test(dir)) && !own(info, dir)) {
        try {
          fs.statSync(path.join(chromeRoot, dir, 'Local Extension Settings', EXT_ID));
          fail('PROFILE_COVERAGE', 'An extension store exists outside the profile metadata inventory; coverage is inconclusive.');
        } catch (error) { if (error.code !== 'ENOENT') throw error; }
      }
    }
    const profiles = [];
    for (const [dir, meta] of Object.entries(info)) {
      validDir(dir);
      if (!object(meta)) fail('PROFILES_UNKNOWN', 'Chrome profile metadata is unsupported.');
      const store = path.join(chromeRoot, dir, 'Local Extension Settings', EXT_ID);
      let exists;
      try { exists = fs.statSync(store).isDirectory(); }
      catch (error) { if (error.code === 'ENOENT') continue; throw error; }
      if (!exists || resolved(store) !== path.join(resolved(chromeRoot), dir, 'Local Extension Settings', EXT_ID)) {
        fail('PROFILE_INVALID', 'Extension store is not an ordinary directory in the selected Chrome root.');
      }
      const label = typeof meta.name === 'string' ? meta.name : dir;
      const account = typeof meta.user_name === 'string' ? meta.user_name : null;
      const identity = { label, account,
        profile: directoryIdentity(path.join(chromeRoot, dir)), store: directoryIdentity(store) };
      profiles.push({ dir, label, account, store, identity });
    }
    if (!profiles.length) fail('PROFILES_UNKNOWN', 'No installed extension store was found in the profile inventory.');
    return profiles.sort((a, b) => a.dir.localeCompare(b.dir));
  }
  function directoryIdentity(directory) {
    const stat = fs.statSync(directory, { bigint: true });
    if (!stat.isDirectory()) fail('PROFILE_INVALID', 'Reviewed profile path is not a directory.');
    return { path: fs.realpathSync(directory), device: stat.dev.toString(), inode: stat.ino.toString() };
  }
  function assertProfileIdentity(target, inventory = discoverProfiles()) {
    const current = inventory.find(profile => profile.dir === target.dir);
    if (!current || stable(current.identity) !== stable(target.identity)) {
      fail('PLAN_STALE', 'Reviewed profile account or directory identity changed; review a new plan.');
    }
    return current;
  }
  async function withDB(store, operation) {
    assertClosed();
    const ClassicLevel = loadClassic();
    const db = new ClassicLevel(store, { createIfMissing: false, valueEncoding: 'utf8' });
    let result, error;
    try {
      try { await db.open(); }
      catch (cause) {
        fail(cause.code === 'LEVEL_LOCKED' ? 'DB_LOCKED' : 'DB_OPEN',
          cause.code === 'LEVEL_LOCKED' ? 'Extension store is locked; no write was attempted.' : 'Extension store could not be opened; cause is not assumed to be a Chrome lock.');
      }
      result = await operation(db);
    } catch (cause) { error = cause; }
    finally {
      try { await db.close(); }
      catch { if (!error) error = new AuditError('DB_CLOSE', 'Extension store close failed; result is inconclusive.'); }
    }
    if (error) throw error;
    return result;
  }
  async function readKey(db) {
    let raw;
    try { raw = await db.get('permissionStorage'); }
    catch (error) {
      if (error.code === 'LEVEL_NOT_FOUND') raw = undefined;
      else fail('DB_READ', 'Permission key could not be read; audit is inconclusive.');
    }
    return parseStorage(raw);
  }
  async function readPermissions(store) { return withDB(store, readKey); }
  async function collect() {
    const { state, digest } = loadState();
    const { rules, raw } = loadRules();
    const profiles = [];
    for (const p of discoverProfiles()) {
      const storage = await readPermissions(p.store);
      const rows = storage.parsed.permissions.map(record => ({
        id: record.id, fingerprint: recordHash(record), drift: driftHash(record),
        netloc: record.scope.type === 'netloc' ? record.scope.netloc : null,
        scopeType: record.scope.type, action: record.action, duration: record.duration,
        surface: record.surface === undefined ? null : record.surface,
        created: new Date(record.createdAt).toISOString(),
        lastUsed: record.lastUsed === undefined ? null : new Date(record.lastUsed).toISOString(),
        transition: record.scope.type === 'domain_transition'
          ? { fromDomain: record.scope.fromDomain, toDomain: record.scope.toDomain } : null,
        standing: standing(record), ...(record.scope.type === 'netloc'
          ? classify(record.scope.netloc, rules, state.overrides) : { tier: 0, note: 'Domain-transition record; outside site-scope policy.', source: 'none' }),
      }));
      profiles.push({ ...p, rows, storageDigest: hash(storage.raw === null ? 'ABSENT' : storage.raw) });
    }
    const current = Object.fromEntries(profiles.map(p => [p.dir, p.rows.map(r => r.drift).sort()]));
    const differences = [];
    for (const dir of new Set([...Object.keys(state.current), ...Object.keys(current)])) {
      const previous = [...(own(state.current, dir) ? state.current[dir] : [])];
      const next = [...(own(current, dir) ? current[dir] : [])];
      const added = [...next], removed = [];
      for (const item of previous) {
        const index = added.indexOf(item);
        if (index < 0) removed.push(item); else added.splice(index, 1);
      }
      if (added.length || removed.length) differences.push({ profile: dir, added: added.length, removed: removed.length });
    }
    return { state, stateDigest: digest, current, profiles, policyDigest: policy(state, raw),
      baseline: state.lastRun === null, drift: state.lastRun === null ? [] : differences };
  }
  function auditReport(data) {
    const records = data.profiles.flatMap(p => p.rows);
    const allows = records.filter(r => r.standing);
    const violations = allows.filter(r => r.tier === 1).length;
    const unclassified = allows.filter(r => r.tier === 0).length;
    const reviewRequired = allows.filter(r => r.tier === 2).length;
    return { baseline: data.baseline, lastRun: data.state.lastRun, drift: data.drift,
      standingAllows: allows.length, otherRecords: records.length - allows.length,
      violations, unclassified, reviewRequired,
      policy: violations ? 'violations' : unclassified ? 'inconclusive-unclassified'
        : reviewRequired ? 'review-required' : 'no-classified-standing-violations',
      coverage: 'Stored allow/always/netloc records only; not effective access or all permission scopes.',
      profiles: data.profiles.map(({ store, storageDigest, ...profile }) => profile) };
  }
  async function audit(options) {
    const operation = async () => {
      const data = await collect();
      if (options['--record']) {
        atomicState({ ...data.state, lastRun: now(), current: data.current }, data.stateDigest);
      }
      return { ...auditReport(data), recorded: Boolean(options['--record']) };
    };
    return options['--record'] ? stateLock(operation) : operation();
  }
  function selectProfiles(profiles, selector) {
    if (!selector || selector === 'all') return profiles;
    const exact = profiles.find(p => p.dir === selector);
    if (exact) return [exact];
    const matches = profiles.filter(p => p.label.toLowerCase() === selector.toLowerCase());
    if (matches.length !== 1) fail('PROFILE_AMBIGUOUS', 'Profile selector is unknown or ambiguous; use its exact directory identity.');
    return matches;
  }
  async function planRevoke(options) {
    const data = await collect();
    const profiles = selectProfiles(data.profiles, options['--profile']).map(p => ({
      dir: p.dir, identity: p.identity, storageDigest: p.storageDigest,
      targets: p.rows.filter(r => r.standing && (options.sites ? options.sites.includes(r.netloc) : r.tier === Number(options['--tier'])))
        .map(({ id, fingerprint, netloc, surface, created, lastUsed }) => ({ id, fingerprint, netloc, surface, created, lastUsed })),
    })).filter(p => p.targets.length);
    const plan = { version: 2, chromeRoot: resolved(chromeRoot), policyDigest: data.policyDigest, createdAt: now(), profiles };
    plan.id = hash(stable(plan));
    if (options['--plan']) writePrivate(path.resolve(options['--plan']), JSON.stringify(plan, null, 2));
    return { dryRun: true, plan, savedPlan: options['--plan'] ? path.resolve(options['--plan']) : null };
  }
  function validatePlan(plan) {
    if (!object(plan) || plan.version !== 2 || !Array.isArray(plan.profiles) || plan.chromeRoot !== resolved(chromeRoot)) {
      fail('PLAN_INVALID', 'Reviewed plan schema or Chrome root does not match; create and review a new plan.');
    }
    const { id, ...body } = plan;
    if (id !== hash(stable(body))) fail('PLAN_INVALID', 'Reviewed plan was modified.');
    const directories = new Set();
    for (const profile of plan.profiles) {
      validDir(profile.dir);
      const identity = profile.identity;
      if (!object(identity) || typeof identity.label !== 'string' ||
          !(identity.account === null || typeof identity.account === 'string') ||
          [identity.profile, identity.store].some(directory => !object(directory) ||
            typeof directory.path !== 'string' || !path.isAbsolute(directory.path) ||
            typeof directory.device !== 'string' || !/^\d+$/.test(directory.device) ||
            typeof directory.inode !== 'string' || !/^\d+$/.test(directory.inode))) {
        fail('PLAN_INVALID', 'Reviewed profile context is missing or malformed; review a new plan.');
      }
      if (directories.has(profile.dir) || !Array.isArray(profile.targets) || !profile.targets.length ||
          !/^[0-9a-f]{64}$/.test(profile.storageDigest)) fail('PLAN_INVALID', 'Reviewed profile targets are malformed.');
      directories.add(profile.dir);
      const ids = new Set();
      for (const target of profile.targets) {
        validSite(target.netloc);
        if (typeof target.id !== 'string' || !target.id || ids.has(target.id) ||
            !/^[0-9a-f]{64}$/.test(target.fingerprint)) fail('PLAN_INVALID', 'Reviewed record identity is malformed.');
        ids.add(target.id);
      }
    }
  }
  function backupKey(profile, storage, plan) {
    privateDir(backupRoot);
    const target = path.join(backupRoot, crypto.randomUUID() + '.permission-key.json');
    writePrivate(target, JSON.stringify({ version: 1, profile: profile.dir, planId: plan.id,
      key: 'permissionStorage', raw: storage.raw }, null, 2));
    const saved = JSON.parse(fs.readFileSync(target, 'utf8'));
    if (saved.raw !== storage.raw || saved.planId !== plan.id) fail('BACKUP_FAILED', 'Permission-key backup verification failed; no write was attempted.');
    return target;
  }
  async function applyPlan(options) {
    const file = path.resolve(options['--plan']);
    outsideDrive(file);
    let plan;
    try { plan = JSON.parse(fs.readFileSync(file, 'utf8')); }
    catch { fail('PLAN_INVALID', 'Reviewed plan could not be read.'); }
    validatePlan(plan);
    const { state } = loadState(), { raw } = loadRules();
    if (plan.policyDigest !== policy(state, raw)) fail('PLAN_STALE', 'Rules or classification overrides changed; review a new plan.');
    const inventory = discoverProfiles();
    for (const target of plan.profiles) assertProfileIdentity(target, inventory);
    const results = [];
    for (const target of plan.profiles) {
      const profile = inventory.find(p => p.dir === target.dir);
      let backup = null, attempted = false;
      try {
        const removed = await withDB(profile.store, async db => {
          const storage = await readKey(db);
          if (hash(storage.raw === null ? 'ABSENT' : storage.raw) !== target.storageDigest) fail('PLAN_STALE', 'Permission key changed since review; profile was left untouched.');
          const wanted = new Map(target.targets.map(t => [t.id, t]));
          const hits = storage.parsed.permissions.filter(r => wanted.has(r.id));
          if (hits.length !== wanted.size || hits.some(r => !standing(r)
              || r.scope.netloc !== wanted.get(r.id).netloc || recordHash(r) !== wanted.get(r.id).fingerprint)) {
            fail('PLAN_STALE', 'Reviewed permission records changed; profile was left untouched.');
          }
          const currentState = loadState().state;
          if (plan.policyDigest !== policy(currentState, loadRules().raw)) fail('PLAN_STALE', 'Classification policy changed; profile was left untouched.');
          assertProfileIdentity(target);
          const next = { ...storage.parsed, permissions: storage.parsed.permissions.filter(r => !wanted.has(r.id)) };
          backup = backupKey(profile, storage, plan);
          const encoded = JSON.stringify(next);
          attempted = true;
          try { await db.put('permissionStorage', encoded, { sync: true }); }
          catch { fail('WRITE_UNCERTAIN', 'Permission write reported failure; retain the recovery backup and verify independently.'); }
          const verify = await readKey(db);
          if (verify.raw !== encoded) fail('WRITE_UNCERTAIN', 'Permission write verification failed; retain the recovery backup.');
          return hits.length;
        });
        results.push({ profile: profile.dir, status: 'verified-key-write', removed, backup });
      } catch (error) {
        results.push({ profile: profile.dir, status: attempted ? 'uncertain' : 'unchanged', backup, error: safeError(error) });
      }
    }
    return { results, exitCode: results.some(r => r.status !== 'verified-key-write') ? 2 : 0,
      verification: 'Key readback only. Re-audit closed stores independently; live extension behavior is not verified.' };
  }
  async function accept(options) {
    return stateLock(async () => {
      const { state, digest } = loadState();
      state.overrides = { ...state.overrides, [options['--site']]: {
        tier: Number(options['--tier']), note: options['--note'] || 'Explicit classification decision.',
      } };
      atomicState(state, digest);
      return { site: options['--site'], tier: Number(options['--tier']), recorded: true };
    });
  }
  async function setup() {
    privateDir(stateDir);
    try {
      execute('npm', ['install', '--prefix', stateDir, '--save-exact', '--ignore-scripts',
        '--no-audit', '--no-fund', 'classic-level@' + LEVEL_VERSION],
      { stdio: ['ignore', 'pipe', 'pipe'] });
    } catch { fail('SETUP_FAILED', 'Explicit dependency setup failed; no raw package-manager output was logged.'); }
    loadClassic();
    return { installedVersion: LEVEL_VERSION, directory: stateDir,
      note: 'Lifecycle scripts were disabled; unsupported native builds require a separate reviewed setup.' };
  }
  async function run(argv) {
    const { command, options } = parseArgs(argv);
    if (command === 'audit') return audit(options);
    if (command === 'revoke') return options['--dry-run'] ? planRevoke(options) : stateLock(() => applyPlan(options));
    if (command === 'accept') return accept(options);
    if (command === 'state') {
      const { state } = loadState();
      return { stateFile, lastRun: state.lastRun, baselineKnown: state.lastRun !== null,
        overrides: state.overrides, profileCount: Object.keys(state.current).length };
    }
    if (command === 'setup') return setup();
    return { usage: 'audit [--json] [--record|--no-record]; revoke (--sites A,B|--tier N) [--profile P] --dry-run [--plan FILE]; revoke --plan FILE; accept --site X --tier N [--note TEXT]; state; setup' };
  }
  async function main(argv) {
    try {
      const result = await run(argv);
      print(JSON.stringify(result, null, 2));
      return result.exitCode || 0;
    } catch (error) {
      print(JSON.stringify({ error: safeError(error), inconclusive: true }));
      return 2;
    }
  }
  return { run, main, loadState, collect, readPermissions, assertClosed, discoverProfiles, atomicState, stateLock };
}

module.exports = { createApp, parseArgs, parseStorage, classify, validSite, standing, recordHash, driftHash, AuditError, EXT_ID };
if (require.main === module) createApp().main(process.argv.slice(2)).then(code => { process.exitCode = code; });
