#!/usr/bin/env node
// Audit and revoke Claude-in-Chrome site permissions across all Chrome profiles.
//
// Permissions live in the extension's own LevelDB store, one per Chrome profile:
//   <Chrome>/<Profile>/Local Extension Settings/<EXT_ID>/  key "permissionStorage"
//
// Reads go through a throwaway copy so a running Chrome cannot block them.
// Writes go direct, so LevelDB's lock fails them loudly when the profile is open.

'use strict';

const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');

const EXT_ID = 'fcoeoabgfenejglbffodgkkbkcdhcgfn';
const CHROME_ROOT = path.join(os.homedir(), 'Library/Application Support/Google/Chrome');
const STATE_DIR = path.join(os.homedir(), '.claude/chrome-permission-audit');
const STATE_FILE = path.join(STATE_DIR, 'state.json');
const BACKUP_ROOT = path.join(os.homedir(), 'Library/Application Support/claude-chrome-permission-backup');
const RULES_FILE = path.join(__dirname, 'rules.json');

const TIER_LABEL = {
  1: 'Tier 1 — revoke',
  2: 'Tier 2 — review',
  3: 'Tier 3 — keep',
  0: 'UNCLASSIFIED — needs review',
};

// ---------------------------------------------------------------- dependency

// classic-level is required for correctness, not convenience: the raw .log files
// retain superseded history (this store held 1784 stale netloc records against 51
// live ones), so any string-scraping reader reports revoked grants as active.
// Installed under STATE_DIR, not the skill directory: npm would otherwise drop
// package.json/package-lock.json into one tool's tree and break Claude/Codex parity.
function levelUp() {
  const dep = path.join(STATE_DIR, 'node_modules', 'classic-level');
  try {
    return require(dep).ClassicLevel;
  } catch {
    process.stderr.write(`[setup] installing classic-level into ${STATE_DIR} (one time)\n`);
    fs.mkdirSync(STATE_DIR, { recursive: true, mode: 0o700 });
    execFileSync('npm', ['install', '--silent', '--no-audit', '--no-fund',
      '--prefix', STATE_DIR, 'classic-level'],
      { stdio: ['ignore', 'ignore', 'inherit'] });
    return require(dep).ClassicLevel;
  }
}

// ------------------------------------------------------------------ profiles

function discoverProfiles() {
  const localState = path.join(CHROME_ROOT, 'Local State');
  if (!fs.existsSync(localState)) die(`Chrome Local State not found at ${localState}`);
  const info = JSON.parse(fs.readFileSync(localState, 'utf8')).profile?.info_cache ?? {};
  const out = [];
  for (const [dir, meta] of Object.entries(info)) {
    const store = path.join(CHROME_ROOT, dir, 'Local Extension Settings', EXT_ID);
    if (!fs.existsSync(store)) continue;          // extension not installed here
    out.push({
      dir,
      label: meta.name || dir,
      account: meta.user_name || '(no account)',
      store,
    });
  }
  return out.sort((a, b) => a.dir.localeCompare(b.dir));
}

// -------------------------------------------------------------- store access

// Copy first: Chrome holds an exclusive lock on any profile it has open, and we
// still want to audit those. The copy is deleted before this returns.
async function readPermissions(store) {
  const ClassicLevel = levelUp();
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'chromeperm-'));
  const copy = path.join(tmp, 'db');
  try {
    fs.cpSync(store, copy, { recursive: true });
    const db = new ClassicLevel(copy, { createIfMissing: false });
    await db.open();
    let raw;
    try {
      raw = await db.get('permissionStorage');
    } catch {
      raw = null;                                  // key absent = no grants yet
    }
    await db.close();
    // Only permissionStorage is ever read. The same store holds accessToken and
    // refreshToken; never pull those into a report, a log, or a temp file.
    return raw ? (JSON.parse(raw).permissions ?? []) : [];
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
}

// Direct, unlocked-only. If Chrome has the profile open, open() throws and we
// surface that rather than risking a half-written store.
async function writePermissions(store, mutate) {
  const ClassicLevel = levelUp();
  const db = new ClassicLevel(store, { createIfMissing: false });
  try {
    await db.open();
  } catch (e) {
    throw new Error(
      `profile is open in Chrome (LevelDB lock held) — close that profile's windows, or quit Chrome, and retry.\n  ${e.message}`);
  }
  const parsed = JSON.parse(await db.get('permissionStorage'));
  const before = parsed.permissions.length;
  parsed.permissions = mutate(parsed.permissions);
  await db.put('permissionStorage', JSON.stringify(parsed));
  await db.close();
  return { before, after: parsed.permissions.length };
}

function backup(profiles, tag) {
  const dir = path.join(BACKUP_ROOT, tag);
  fs.mkdirSync(dir, { recursive: true, mode: 0o700 });
  for (const p of profiles) {
    fs.cpSync(p.store, path.join(dir, p.dir.replace(/ /g, '_')), { recursive: true });
  }
  execFileSync('chmod', ['-R', '700', dir]);
  return dir;
}

// ------------------------------------------------------------ classification

function loadRules() {
  const spec = JSON.parse(fs.readFileSync(RULES_FILE, 'utf8'));
  return spec.rules.map(r => ({ ...r, re: new RegExp(r.pattern) }));
}

function classify(netloc, rules, overrides) {
  if (overrides[netloc]) {
    return { tier: overrides[netloc].tier, reason: overrides[netloc].note || 'your decision', source: 'override' };
  }
  for (const r of rules) {
    if (r.re.test(netloc)) return { tier: r.tier, reason: r.reason, source: 'rule' };
  }
  return { tier: 0, reason: 'no rule matched — classify it, then record with `accept`', source: 'none' };
}

// ------------------------------------------------------------------ state

function loadState() {
  if (!fs.existsSync(STATE_FILE)) return { lastRun: null, seen: {}, overrides: {} };
  return JSON.parse(fs.readFileSync(STATE_FILE, 'utf8'));
}

function saveState(state) {
  fs.mkdirSync(STATE_DIR, { recursive: true, mode: 0o700 });
  fs.writeFileSync(STATE_FILE, JSON.stringify(state, null, 1), { mode: 0o600 });
}

const seenKey = (profileDir, netloc) => `${profileDir}::${netloc}`;

// ------------------------------------------------------------------ commands

async function collect() {
  const rules = loadRules();
  const state = loadState();
  const profiles = discoverProfiles();
  if (!profiles.length) die(`Claude extension (${EXT_ID}) not found in any Chrome profile.`);

  const result = [];
  for (const p of profiles) {
    const perms = await readPermissions(p.store);
    const rows = perms.map(x => {
      const netloc = x.scope?.netloc ?? '(malformed scope)';
      const c = classify(netloc, rules, state.overrides);
      return {
        netloc,
        tier: c.tier,
        reason: c.reason,
        duration: x.duration ?? '(none)',
        action: x.action,
        created: x.createdAt ? new Date(x.createdAt).toISOString().slice(0, 10) : null,
        lastUsed: x.lastUsed ? new Date(x.lastUsed).toISOString().slice(0, 10) : null,
        isNew: !state.seen[seenKey(p.dir, netloc)],
      };
    });
    result.push({ ...p, rows });
  }
  return { profiles: result, state, baseline: state.lastRun === null };
}

async function cmdAudit(args) {
  const { profiles, state, baseline } = await collect();
  const json = args.includes('--json');

  if (json) {
    print(JSON.stringify({ baseline, lastRun: state.lastRun, profiles }, null, 1));
  } else {
    printReport(profiles, state, baseline);
  }

  if (!args.includes('--no-record')) {
    for (const p of profiles) {
      for (const r of p.rows) {
        state.seen[seenKey(p.dir, r.netloc)] = { tier: r.tier, firstSeen: state.seen[seenKey(p.dir, r.netloc)]?.firstSeen ?? today() };
      }
    }
    state.lastRun = new Date().toISOString();
    saveState(state);
  }
}

function printReport(profiles, state, baseline) {
  const all = profiles.flatMap(p => p.rows.map(r => ({ ...r, profile: p.label })));
  const total = all.length;
  const t = n => all.filter(r => r.tier === n).length;

  print(`# Claude in Chrome — site permission audit\n`);
  print(`Generated ${new Date().toISOString().slice(0, 16).replace('T', ' ')}`);
  print(baseline
    ? `\n**First run — establishing baseline.** Everything below is recorded as known; future runs report only what is new.\n`
    : `\nLast run: ${state.lastRun?.slice(0, 16).replace('T', ' ') ?? 'never'}\n`);

  print(`**${total} standing grants** across ${profiles.length} profiles — `
    + `Tier 1: ${t(1)} · Tier 2: ${t(2)} · Tier 3: ${t(3)} · unclassified: ${t(0)}\n`);

  print('| Profile | Account | Grants | T1 | T2 | T3 | ? |');
  print('|---|---|---|---|---|---|---|');
  for (const p of profiles) {
    const c = n => p.rows.filter(r => r.tier === n).length;
    print(`| ${p.label} | ${p.account} | ${p.rows.length} | ${c(1)} | ${c(2)} | ${c(3)} | ${c(0)} |`);
  }

  // Policy violations: Tier 1 present at all, plus anything unclassified.
  const violations = all.filter(r => r.tier === 1);
  const unknown = all.filter(r => r.tier === 0);
  const drift = baseline ? [] : all.filter(r => r.isNew);

  section('Policy violations — Tier 1 (sensitive, permanently allowed)', violations,
    'Nothing sensitive is permanently allowed. Policy holds.');
  section('Unclassified — needs your decision', unknown,
    'Every grant matched a known rule.');
  if (!baseline) {
    section('New since last run', drift, 'No new grants since the last audit.');
  }

  print('\n## Tier 2 — review\n');
  for (const p of profiles) {
    const rows = p.rows.filter(r => r.tier === 2);
    if (rows.length) print(`**${p.label}:** ` + rows.map(r => `\`${r.netloc}\``).sort().join(', '));
  }
  print('\n## Tier 3 — keep\n');
  for (const p of profiles) {
    const rows = p.rows.filter(r => r.tier === 3);
    if (rows.length) print(`**${p.label}:** ` + rows.map(r => `\`${r.netloc}\``).sort().join(', '));
  }
}

function section(title, rows, emptyMsg) {
  print(`\n## ${title}\n`);
  if (!rows.length) { print(`_${emptyMsg}_`); return; }
  print('| Profile | Site | Why | Granted | Last used |');
  print('|---|---|---|---|---|');
  for (const r of rows.sort((a, b) => a.profile.localeCompare(b.profile) || a.netloc.localeCompare(b.netloc))) {
    print(`| ${r.profile} | \`${r.netloc}\` | ${r.reason} | ${r.created ?? '?'} | ${r.lastUsed ?? 'never'} |`);
  }
}

async function cmdRevoke(args) {
  const profileArg = argValue(args, '--profile');
  const sitesArg = argValue(args, '--sites');
  const tierArg = argValue(args, '--tier');
  const dryRun = args.includes('--dry-run');
  if (!sitesArg && !tierArg) die('revoke needs --sites <a,b,c> or --tier <n>');

  const { profiles } = await collect();
  const targets = profiles
    .filter(p => !profileArg || profileArg === 'all' || p.label.toLowerCase() === profileArg.toLowerCase() || p.dir === profileArg)
    .map(p => {
      const want = sitesArg
        ? new Set(sitesArg.split(',').map(s => s.trim()).filter(Boolean))
        : null;
      const hits = p.rows.filter(r => want ? want.has(r.netloc) : String(r.tier) === tierArg);
      return { ...p, hits };
    })
    .filter(p => p.hits.length);

  if (!targets.length) { print('Nothing matched — no changes.'); return; }

  print('Planned revocations:\n');
  for (const p of targets) {
    print(`  ${p.label}: ${p.hits.length}`);
    for (const h of p.hits) print(`    - ${h.netloc}  (tier ${h.tier})`);
  }
  if (dryRun) { print('\n--dry-run: nothing written.'); return; }

  const tag = new Date().toISOString().replace(/[:.]/g, '-').slice(0, 19);
  const dir = backup(targets, tag);
  print(`\nBackup: ${dir}`);
  print('  NOTE: backups contain live OAuth access and refresh tokens. Mode 700. Delete when satisfied; never sync this path.\n');

  const state = loadState();
  for (const p of targets) {
    const kill = new Set(p.hits.map(h => h.netloc));
    try {
      const { before, after } = await writePermissions(p.store, perms => perms.filter(x => !kill.has(x.scope?.netloc)));
      print(`  ${p.label}: ${before} -> ${after} (removed ${before - after})`);
      for (const n of kill) delete state.seen[seenKey(p.dir, n)];
    } catch (e) {
      print(`  ${p.label}: SKIPPED — ${e.message}`);
    }
  }
  saveState(state);
  print('\nRe-run `audit` to verify against the live stores.');
}

function cmdAccept(args) {
  const site = argValue(args, '--site');
  const tier = argValue(args, '--tier');
  const note = argValue(args, '--note');
  if (!site || !tier) die('accept needs --site <netloc> --tier <1|2|3>');
  const state = loadState();
  state.overrides[site] = { tier: Number(tier), note: note || `accepted ${today()}` };
  saveState(state);
  print(`Recorded: ${site} -> tier ${tier}. It will classify this way from now on.`);
}

function cmdState() {
  const s = loadState();
  print(`state file: ${STATE_FILE}`);
  print(`last run:   ${s.lastRun ?? 'never'}`);
  print(`known grants: ${Object.keys(s.seen).length}`);
  print(`overrides:  ${Object.keys(s.overrides).length}`);
  for (const [k, v] of Object.entries(s.overrides)) print(`  ${k} -> tier ${v.tier}  (${v.note})`);
}

// ------------------------------------------------------------------- helpers

const print = m => process.stdout.write(m + '\n');
const today = () => new Date().toISOString().slice(0, 10);
function die(m) { process.stderr.write(`error: ${m}\n`); process.exit(1); }
function argValue(args, flag) {
  const i = args.indexOf(flag);
  return i === -1 ? null : args[i + 1];
}

const USAGE = `chrome_permissions.js <command>

  audit  [--json] [--no-record]     Inventory every profile, classify, report drift
  revoke --sites a,b,c | --tier N
         [--profile <name>|all] [--dry-run]
  accept --site <netloc> --tier N [--note "..."]
  state                             Show recorded baseline and overrides
`;

(async () => {
  const [cmd, ...args] = process.argv.slice(2);
  switch (cmd) {
    case 'audit': return cmdAudit(args);
    case 'revoke': return cmdRevoke(args);
    case 'accept': return cmdAccept(args);
    case 'state': return cmdState();
    default: process.stdout.write(USAGE); process.exit(cmd ? 1 : 0);
  }
})().catch(e => die(e.stack || e.message));
