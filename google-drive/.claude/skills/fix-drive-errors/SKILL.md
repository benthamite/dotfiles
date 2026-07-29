---
name: fix-drive-errors
description: Triage Google Drive sync errors by finding generated dependency/build/cache directories, moving confirmed offenders out of Drive, repairing symlinks, and restarting Drive. For one named directory use nosync.
---

# Fix Google Drive sync errors

Drive every known generated directory under `~/My Drive/` to a single invariant: **the in-Drive path is either absent or a healthy symlink into `~/.drive-nosync/`, AND no copy of it remains in the cloud.** Then restart Google Drive.

Both halves of that invariant are load-bearing. A symlink alone is not enough: Drive skips symlinks, so a surviving cloud copy has no local counterpart and Drive re-downloads it beside the symlink as `name (1)` on the next full re-merge. See "Step 4.5: Remove the cloud copies".

## Hard limits — never do these

Both of these are drawn from a single documented incident (2026-07-26/27), verified against the Drive API. Do not perform them, and do not suggest them to the user, even if the Drive UI or a support article proposes them:

1. **Never disconnect and reconnect the Google Drive account**, and never remove or re-add the sync root. Reconnecting could not adopt the existing non-empty `~/My Drive`. Three attempts, and what was actually observed in each:
   - (a) The folder was registered as a *computer backup*: 1,567 items / 7.9 GB were re-uploaded as brand-new files under a "My MacBook Pro" cloud folder. **Verified** by Drive API — every new item's parent chain terminated at that folder.
   - (b) A mirror was registered against a *new* `My Drive (account@example.com)` folder, which began downloading the cloud into it. **Verified** from the `roots` row and from 8.7 MB of cloud content already present in that folder when it was caught.
   - (c) Selecting the real folder produced a confirm dialog that then failed to register. A `machine_root` row (the computer-backup marker) was **verified present** in `mirror_sqlite.db` afterwards; whether uploads from this attempt actually landed in a backup folder was **not** checked.

   Not verified: whether the disconnect cleared the original error count. The mapping database was certainly wiped (`mirror_item` went to 0 rows), but the user-visible error count was never re-measured afterwards, so do not claim disconnecting fixes anything.
2. **Never infer sync direction from the Drive UI or a network meter.** In the same incident, `nettop` byte columns were misread and the user was told Drive was downloading while it was in fact uploading. Separately, while the UI displayed "Syncing… 27,826 files" with rows marked "Successfully uploaded", a Drive API query showed **zero** items created in the cloud during that window — so the UI's activity list does not reliably indicate current cloud writes. The only authoritative check is the cloud itself — see "Verifying sync direction" below.

## Verifying sync direction

Before telling the user that sync activity is safe, and before claiming anything about what Drive is doing, query the cloud for items created since a timestamp. A correct merge of already-matching content creates **nothing**; a mass of newly created cloud items means Drive is re-uploading from scratch.

Uses the personal-account OAuth token that `gdoc` stores at `~/.config/gdoc/accounts/personal/token.json` (see `context/google-services.md`). Pass an ISO8601 UTC timestamp as `$1`:

```python
import json, urllib.request, urllib.parse, pathlib, sys
tok = json.loads((pathlib.Path.home()/".config/gdoc/accounts/personal/token.json").read_text())
body = urllib.parse.urlencode({"client_id": tok["client_id"], "client_secret": tok["client_secret"],
    "refresh_token": tok["refresh_token"], "grant_type": "refresh_token"}).encode()
at = json.loads(urllib.request.urlopen(urllib.request.Request(tok["token_uri"], data=body)).read())["access_token"]

def get(fid, fields="id,name,parents"):
    u = f"https://www.googleapis.com/drive/v3/files/{fid}?fields={urllib.parse.quote(fields)}"
    return json.loads(urllib.request.urlopen(urllib.request.Request(u, headers={"Authorization": f"Bearer {at}"})).read())

since = sys.argv[1]          # e.g. "2026-07-27T03:20:00"
q = urllib.parse.quote(f"createdTime > '{since}' and trashed = false")
u = f"https://www.googleapis.com/drive/v3/files?q={q}&fields=nextPageToken,files(id,name,createdTime)&pageSize=1000"
j = json.loads(urllib.request.urlopen(urllib.request.Request(u, headers={"Authorization": f"Bearer {at}"})).read())
files = j.get("files", [])
print(f"created in cloud since {since}: {len(files)}{' (more pages)' if j.get('nextPageToken') else ''}")
for f in files[:5]:                                  # show where a sample landed
    chain, cur = [], f["id"]
    for _ in range(10):
        m = get(cur); chain.append(m["name"])
        if not m.get("parents"): break
        cur = m["parents"][0]
    print("  ", " <- ".join(chain))
```

Report the count and the parent chain of the sample. If items are landing anywhere other than their expected My Drive paths (for example under a computer-backup folder), report it and stop Drive activity **only** with the user's explicit permission.

## Recovery runbook: Drive re-registered as a computer backup

This is the recovery for the incident in "Hard limits". Symptoms: files appearing in a cloud folder named after the Mac (`My MacBook Pro`) instead of My Drive; local folders reappearing as `name (1)`; the Drive UI showing endless "Upload will begin shortly"; a wedged client at ~99% CPU.

### Diagnose: read four values, not the UI

```bash
DFS="$HOME/Library/Application Support/Google/DriveFS"
ACC="$DFS/<account-id>"                    # numeric dir
sqlite3 "$DFS/root_preference_sqlite.db" \
  "select root_id,title,root_path,sync_type,destination,is_my_drive,doc_id from roots;"
sqlite3 "$ACC/mirror_sqlite.db" "select count(*) from machine_root;"
sqlite3 "$ACC/mirror_sqlite.db" "select * from root_config;"
```

A healthy My Drive mirror looks like this — all four must hold:

| Value | Healthy | Broken (computer backup) |
|---|---|---|
| `roots.sync_type` / `destination` | `1` / `1` | `1` / `1` (identical — **not** diagnostic) |
| `roots.is_my_drive` | `1` | `0` |
| `roots.doc_id` | the My Drive root id (`0A…`) | empty |
| `machine_root` row count | `0` | `1`, pointing at the backup folder's cloud id |

`machine_root` is the sharpest signal: a row there means a folder-backup registration exists, whatever else looks right. A stale row can coexist with a correct `roots` entry — that state still produces duplicates, and it is what went undetected for 21 hours.

### Fix

1. **Pause syncing** (menu-bar gear) to stop accumulation.
2. **Remove the backup registration**: Preferences → the **"My MacBook Pro"** tab (*Folders from your computer*) → uncheck the folder. Google documents that this stops the backup without deleting cloud copies.
3. **Register the mirror**: Preferences → **"Google Drive"** tab (*Folders from Drive*) → **Mirror files** → location `~/My Drive`. Expect a "This folder has content that will be synced" warning — that is the correct merge prompt. If instead it errors or proposes a new `My Drive (account@…)` folder, a backup registration for that path still exists; go back to step 2.
4. **Trash the backup tree** in the cloud (Drive API `PATCH {"trashed": true}` on the `My MacBook Pro` folder). Verify first that its contents have local originals — walk a sample and check each path exists on disk. It is recoverable from Trash.

### If the client is wedged or the state is corrupt

Signature: `task_manager.cc:LogInfoLevelLongLivedTasks … task name: PushTask … progress descriptor: Prefetching file cloud IDs`, plus mass `revert_flow.cc:RunRevertFlow … failed_status: UNAVAILABLE_RESOURCE` and `CONTENT_UPDATE`/`REVERT` pairs. That means Drive is pushing content updates for cloud ids that no longer resolve, and it will not self-heal.

Reset the local cache — **narrower than disconnecting the account, but be clear-eyed: it still forces a full re-onboarding.** In the observed incident the sync-root registration did *not* survive, despite living in `root_preference_sqlite.db` outside the deleted folder. Plan for signing in and re-choosing the mirror.

```bash
# 1. dump the registration so it can be inspected/restored
sqlite3 "$DFS/root_preference_sqlite.db" ".dump roots" > /tmp/roots-dump.sql
cp "$DFS/root_preference_sqlite.db" /tmp/
# 2. quit Drive, then RENAME the account cache aside (reversible; do not delete)
killall "Google Drive"; while pgrep -x "Google Drive" >/dev/null; do sleep 1; done
mv "$ACC" "$ACC.bak-$(date +%Y%m%d-%H%M%S)"
# 3. relaunch; sign in; choose "Google Drive → Folders from Drive" → Mirror files → ~/My Drive
open -a "Google Drive"
```

At the account-picker step the wrong choice is one click away: **"My MacBook Pro — Folders from your computer" is the trap**; the correct entry is **"Google Drive — Folders from Drive"**.

### Arm a tripwire before re-onboarding

Poll the cloud every 30s while the user clicks through setup. Alert on either signal, since both appear within seconds of a wrong choice:

- any live folder named after the Mac (`name = 'My MacBook Pro' and trashed = false`), or
- a burst of newly created files (`createdTime > <baseline>`), which in a correct merge is ~0 because matching creates nothing.

### Verify with a marker file, not with the absence of activity

Create `~/My Drive/_route-test-<ts>.txt`, wait for it to sync, then resolve its cloud parent chain. Landing under `My Drive` proves routing; landing under the machine name proves it is still broken.

**Do not** treat "no new cloud files" as success on its own. A paused client, a stopped-accounts state, and a correct merge all produce exactly zero creations. Confirm sync is actually running first (`UPLOAD_PAUSED` absent from the log; the index and queue counters moving), or the measurement means nothing.

Also expect the index counter to sit near zero for the first several minutes after setup while Drive fetches cloud metadata; it is not a stall. In the observed recovery it went 6 → 514,642 once that phase finished, roughly 4,000 files/minute.

## Background

Google Drive cannot sync symlinks, and struggles with certain directory contents (node_modules `.bin/` symlinks, Python bytecode, etc.). The **only** reliable fix is to move the problematic directory entirely outside `~/My Drive/` and leave a symlink in its place. The symlink is invisible to Drive (it skips symlinks), and the real data lives outside Drive's sync root.

**Important**: the `.nosync` suffix is an iCloud convention. Google Drive does **not** honor it. A directory named `foo.nosync` inside `~/My Drive/` is synced like any other directory. Do not use the `.nosync` rename trick; it does not work for Google Drive.

### Why this skill is idempotent, not one-shot

Normal operation routinely undoes the symlink, and that is expected, not a failure:

- `npm ci` (and `rm -rf node_modules`) delete `node_modules` wholesale and recreate it as a **real directory**, destroying the symlink. Plain `npm install` follows the symlink and is fine. Builds, `git checkout`, interpreter-version changes, and fresh clones can likewise replace a symlink with a real dir.
- When that happens you end up with a **real dir in Drive** plus a now-stale copy in `~/.drive-nosync/` — a conflict.

There is no way to permanently prevent wholesale-recreation tools from clobbering the symlink. The durable answer is therefore **re-running this skill, which always converges to the invariant** — including automatically resolving the recurring conflict for fully regenerable directories. Re-running when nothing is wrong is a safe no-op. The user has chosen on-demand re-running over any background trigger; do not add hooks or cron jobs unless explicitly asked.

## When this skill is invoked

When triggered, follow the execution steps below. Do not just describe what the skill does.

The convergence logic below already encodes what is safe to do automatically and what needs a human. Proceed without asking for **regenerable** directories (the whitelist below) — including auto-resolving their conflicts. Stop and ask only for **ambiguous** directories that are not proven generated, and never touch git-tracked paths.

Do not use this skill for one explicitly named directory; use `nosync` for that narrower workflow. Do not use it for Google Docs/content sync problems that are not caused by local generated directories.

### Execution steps

#### Step 0: Triage the actual errors before changing anything

**This skill fixes exactly one error class.** Do not assume the reported error count is caused by generated directories, and do not promise the count will drop. Classify first, from Drive's own logs:

```bash
LOGS="$HOME/Library/Application Support/Google/DriveFS/Logs"
# Severity is the letter appended to the timestamp: ...ZI (info), ZW (warning), ZE (error).
grep -hE '^[0-9T:.-]+Z[EW] ' "$LOGS/drive_fs.txt" \
  | sed -E 's/^[^ ]+ \[[^]]+\] //' | sed -E 's/:[0-9]+:/:/' \
  | awk '{print $1, $2}' | sort | uniq -c | sort -rn | head -20
```

Map what you find to the right response:

Signatures seen in the 2026-07-26/27 incident, with counts from one `drive_fs.txt`:

| Log signature | What was established | Handled here? |
|---|---|---|
| `file_stream_posix.cc ... Is a directory` | Drive tried to read a generated dir/symlink as a file. Confirmed: every path named was a generated dir or an externalized symlink | **Yes** — this skill |
| `error_handler.cc:HandleFailedChange [Upload{Modify,Retry,Create}MergeQueueItem: ...]` | All three variants occur; check for each. In the first log examined, `UploadModify…NOT_FOUND` was 753 of ~760 error lines, and resolving the ids confirmed Drive's local database mapped those files to cloud ids that 404, so every upload retried forever. A later log from the same machine showed 37,641 `UploadRetry…` plus 1,944 `UploadCreate…` lines — so counts and dominant variant shift with Drive's state; re-measure, never reuse a previous run's numbers | **No** — see "Dead cloud-ID mappings" below |
| `merger.cc:AddExternalContentLocalProperty` | 671 lines. **Not investigated.** Logged at error severity; no user-visible failure was traced to it | Unknown — investigate before acting on it |
| `PushRoots ... UNIMPLEMENTED`, `syncing_status_controller ... no_user` | Low volume, appeared only at startup. **Not investigated** | Unknown — investigate before acting on it |

Treat the last two rows as unclassified rather than harmless: nobody has verified they are benign. Report the breakdown to the user before acting, so expectations match what this skill can actually fix. Note that error *lines* in the log do not map one-to-one onto the error *count* in the Drive UI; if the user cares about that number, ask them for it rather than inferring it from the log.

#### Dead cloud-ID mappings (a different problem — do not "fix" it with this skill)

When `HandleFailedChange`/`NOT_FOUND` dominates, resolve the affected `local_stable_id` values to paths for diagnosis (read-only):

```bash
ACC="$HOME/Library/Application Support/Google/DriveFS/<account-id>"
# mirror_sqlite.db: mirror_item maps local_stable_id -> stable_id and filename
# metadata_sqlite_db: stable_ids maps stable_id -> cloud file id
```

Then check those cloud IDs against the Drive API. A 404 while a live file of the same name exists at the same path means the mapping is dead and uploads have been failing since it broke.

**Do not assume which side is newer — establish it.** Checksum every affected local file against the live cloud copy at the same path and record the split. In the observed incident that produced: 650 differing, 82 identical, 21 whose cloud folder was gone entirely; no cloud copy had a modification time later than a date ten days before the run, while local files had kept changing, and a spot diff confirmed local content the cloud lacked. That evidence is what justified treating local as authoritative — not an assumption. If your comparison shows a different pattern (for example some cloud copies newer), stop and report rather than overwriting.

Once established, the repair is to upload local content into the *live* cloud file at that path (`files.update`), verifying each upload against the server-reported `md5Checksum` and skipping any file that already matches. Drive keeps prior revisions of a file updated this way, so an overwrite is recoverable through the file's version history in the web UI — confirm this yourself for a sample before relying on it, as it was not verified during the incident. Require exactly one unambiguous cloud target per path; if two same-name files exist, skip and report rather than guessing. Do **not** attempt to repair the mapping database, and do **not** disconnect the account.

#### Step 1: Set roots, classification, and helpers

Paste this block first. It defines the classification model and the `reconcile` convergence engine that every later step uses.

```bash
DRIVE_ROOT="$HOME/My Drive"
NOSYNC_ROOT="$HOME/.drive-nosync"
mkdir -p "$NOSYNC_ROOT"
MOVED_MANIFEST="$(mktemp)"   # NUL-free list of external targets newly created this run

# Fully regenerable / disposable. Conflicts on these are auto-resolved.
REGENERABLE="node_modules .next .nuxt .svelte-kit .parcel-cache .turbo .cache __pycache__ .pytest_cache .mypy_cache .ruff_cache .tox .nox venv .venv"
# May hold source or user data. Movable ONLY when proven generated (gitignored); conflicts need a human.
AMBIGUOUS="dist build out data"

is_regenerable() { case " $REGENERABLE " in *" $1 "*) return 0;; *) return 1;; esac; }
is_ambiguous()   { case " $AMBIGUOUS "   in *" $1 "*) return 0;; *) return 1;; esac; }

# True if path is tracked by git (must NEVER be moved — moving would dirty/break the work tree).
is_git_tracked() {
  local parent base; parent="$(dirname "$1")"; base="$(basename "$1")"
  git -C "$parent" rev-parse --is-inside-work-tree >/dev/null 2>&1 || return 1
  [ -n "$(git -C "$parent" ls-files "$base" 2>/dev/null | head -1)" ]
}

# True if path is gitignored (our proof that an ambiguous dir is generated/disposable).
is_gitignored() {
  local parent base; parent="$(dirname "$1")"; base="$(basename "$1")"
  git -C "$parent" rev-parse --is-inside-work-tree >/dev/null 2>&1 || return 1
  git -C "$parent" check-ignore -q "$base" 2>/dev/null
}

# True if any ancestor within DRIVE_ROOT is a symlink — i.e. this path is *inside* an
# already-externalized tree and must be ignored (prevents the recreation false-positive).
has_symlinked_ancestor() {
  local probe="$1"
  while [ "$probe" != "$DRIVE_ROOT" ] && [ "$probe" != "/" ]; do
    probe="$(dirname "$probe")"
    [ -L "$probe" ] && return 0
  done
  return 1
}

# Move a real dir out and leave a symlink. External target must not already exist.
externalize() {
  local dir="$1" rel ext; rel="${dir#$DRIVE_ROOT/}"; ext="$NOSYNC_ROOT/$rel"
  mkdir -p "$(dirname "$ext")"
  mv "$dir" "$ext" && ln -s "$ext" "$dir"
  if [ -L "$dir" ] && [ -e "$dir" ]; then
    printf '%s\n' "$ext" >> "$MOVED_MANIFEST"
    echo "MOVED    $dir -> $ext ($(du -sh "$ext" | cut -f1))"
  else
    echo "FAIL     $dir"
  fi
}

# Drive ONE candidate path to the invariant. Handles every state.
reconcile() {
  local dir="$1" base rel ext; base="$(basename "$dir")"; rel="${dir#$DRIVE_ROOT/}"; ext="$NOSYNC_ROOT/$rel"

  has_symlinked_ancestor "$dir" && return                    # inside an externalized tree; ignore

  # Symlinks first — a symlink (even a git-tracked one) is ALREADY externalized. Committing the
  # symlink to git is a fine, established pattern; do not relabel it as a tracked dir to skip.
  if [ -L "$dir" ]; then
    case "$(readlink "$dir")" in "$NOSYNC_ROOT"/*|*/.drive-nosync/*) : ;; *) return ;; esac  # not ours
    if [ -e "$dir" ]; then echo "OK       $dir"; return; fi  # healthy — invariant already holds
    if is_git_tracked "$dir"; then
      echo "BROKEN   $dir (tracked symlink, external target missing; resolve manually — do not rm)"
    elif is_regenerable "$base"; then
      rm "$dir"; echo "DROP     dead symlink removed (regenerates on next use): $dir"
    else
      echo "BROKEN   $dir -> $(readlink "$dir") (external target missing; resolve manually)"
    fi
    return
  fi

  # From here the path is a real dir. Never move git-tracked content (would dirty/break the tree).
  if is_git_tracked "$dir"; then echo "SKIP     git-tracked real dir: $dir"; return; fi

  [ -d "$dir" ] || { echo "SKIP     not a directory: $dir"; return; }

  # Ambiguous dirs must be proven generated before any move.
  if is_ambiguous "$base" && ! is_gitignored "$dir"; then
    echo "ASK      ambiguous, not proven generated (skip unless confirmed disposable): $dir"; return
  fi

  if [ ! -e "$ext" ] && [ ! -L "$ext" ]; then externalize "$dir"; return; fi   # clean move

  # CONFLICT: real dir in Drive AND an external copy exists (symlink was clobbered, dir recreated).
  if is_regenerable "$base"; then
    trash "$ext" && echo "trashed stale external: $ext"     # safe: fully regenerable
    externalize "$dir"                                      # promote the fresh Drive copy
  else
    echo "ASK      conflict on ambiguous dir (resolve manually): $dir  vs  $ext"
  fi
}
```

Notes:
- Use `trash` (never `rm -rf`) for the stale external copy. It is safe to discard only because the type is regenerable and the Drive copy is the current one.
- The git-tracked guard is mandatory: a `dist`/`build`/`out` that holds committed files must never be moved, or the working tree breaks.

Before making consequential cleanup or untracking recommendations for generated-looking tracked paths (`__pycache__`, `.venv`, `.pytest_cache`, `.egg-info`, `.browser-data-*`, etc.), check git object mode:

```bash
git ls-files -s "$dir"
```

Interpret mode `120000` as a tracked symlink / intentional externalization, not committed generated junk. Interpret modes `100644` or `100755` as tracked files; inspect the listed children before calling the path a git-tracked real dir. Do this before recommending `git rm --cached`, ignore changes, deletion, or untracking.

#### Step 2: Build the candidate list

Gather every path the engine should reconcile: real generated dirs, our existing nosync symlinks (to verify/heal), and legacy `.nosync` dirs. `find -type d` already skips symlinked directories, so nested dirs inside an externalized tree never appear. Pipe NUL-separated so paths with spaces/backslashes survive.

```bash
CANDIDATES="$(mktemp)"
{
  find "$DRIVE_ROOT" -maxdepth 6 -type d \( \
    -name node_modules -o -name .next -o -name .nuxt -o -name .svelte-kit \
    -o -name .parcel-cache -o -name .turbo -o -name .cache -o -name __pycache__ \
    -o -name .pytest_cache -o -name .mypy_cache -o -name .ruff_cache -o -name .tox -o -name .nox \
    -o -name venv -o -name .venv -o -name dist -o -name build -o -name out \
  \) -print0
  find "$DRIVE_ROOT" -maxdepth 6 -type l -print0          # existing symlinks (reconcile filters to ours)
  find "$DRIVE_ROOT" -maxdepth 6 -type d -name "*.nosync" -print0   # legacy
} > "$CANDIDATES" 2>/dev/null
```

Review the list before acting if anything looks surprising (e.g. an `out`/`build`/`dist` you do not recognize). The engine will skip git-tracked and unproven-ambiguous paths on its own, but a human glance catches misclassified user data.

#### Step 3: Converge every candidate to the invariant

Process largest real dirs first so the biggest sync wins land early, then run the engine over everything. Legacy `.nosync` dirs are handled separately below.

```bash
# Reconcile non-legacy candidates (sorted so big real dirs move first).
while IFS= read -r -d '' dir; do
  case "$dir" in *.nosync) continue;; esac
  reconcile "$dir"
done < <(sort -z "$CANDIDATES")
```

For **legacy `.nosync` directories** (`dir.nosync` with a sibling `dir -> dir.nosync` symlink), migrate to the real layout:
1. Move `dir.nosync` to `~/.drive-nosync/.../dir`.
2. `trash` the old `dir -> dir.nosync` symlink.
3. Create `dir -> ~/.drive-nosync/.../dir`.

Never merge two real directories, and never delete a stale **in-Drive** copy, without explicit confirmation. (Auto-resolution only ever trashes a stale **external** copy of a regenerable type and promotes the fresh Drive copy.)

#### Step 4: Rewrite broken relative symlinks in moved trees

For each tree newly externalized this run, rewrite broken relative symlinks whose original targets still exist, so relative `..` links do not silently break after the move from `~/My Drive/` to `~/.drive-nosync/`.

```bash
while IFS= read -r ext; do
  src="$DRIVE_ROOT/${ext#$NOSYNC_ROOT/}"
  python3 - "$src" "$ext" <<'PY'
import os, sys
from pathlib import Path
src, dst = sys.argv[1], sys.argv[2]
fixed, remaining = [], []
for link in Path(dst).rglob("*"):
    if not link.is_symlink() or link.exists():
        continue
    target = os.readlink(link)
    if os.path.isabs(target):
        remaining.append((str(link), target)); continue
    orig_link = src + str(link)[len(dst):]
    abs_target = os.path.normpath(os.path.join(os.path.dirname(orig_link), target))
    if os.path.exists(abs_target):
        link.unlink(); link.symlink_to(abs_target); fixed.append((str(link), abs_target))
    else:
        remaining.append((str(link), target))
for l, t in fixed:     print(f"rewrote {l} -> {t}")
for l, t in remaining: print(f"still broken {l} -> {t}")
PY
done < "$MOVED_MANIFEST"
```

Report any remaining broken symlinks as unresolved rather than hiding them.

#### Step 4.5: Remove the cloud copies (mandatory — this is the root-cause half)

Externalizing locally is only half the job. Any generated directory that was previously synced **still exists in the cloud**, and Drive skips symlinks, so the cloud copy has no local counterpart. On the next full re-merge Drive downloads it back beside the symlink as `node_modules (1)`, `.venv (1)`, and so on — silently consuming gigabytes and polluting repos. This is not hypothetical: it happened, and it is why this step exists.

For every externalized path (a symlink under `~/My Drive/` pointing into `~/.drive-nosync/`) **whose base name is in the regenerable whitelist**, check whether a folder of that name still exists at the corresponding cloud path, and if so move it to Drive's Trash. (Google documents Trash as recoverable for 30 days; that retention was not tested here.)

Enumerate the externalized set cheaply by walking `~/.drive-nosync/` (small) rather than `~/My Drive/` (potentially terabytes), pruning at each path whose in-Drive counterpart is a symlink:

```bash
python3 - <<'PY'
import os, pathlib
HOME = pathlib.Path.home(); NOSYNC = HOME/".drive-nosync"; DRIVE = HOME/"My Drive"
for root, dirs, files in os.walk(NOSYNC):
    rel_root = os.path.relpath(root, NOSYNC)
    for d in list(dirs):
        rel = d if rel_root == "." else os.path.normpath(os.path.join(rel_root, d))
        if (DRIVE/rel).is_symlink():
            print(rel); dirs.remove(d)
PY
```

Then, per path, resolve the cloud folder by walking parent→child from `root` (name + `'<parent>' in parents` + `trashed = false`) and `PATCH {"trashed": true}`. Report each trashed path with its size, and the total reclaimed.

Safety notes:
- **Regenerable types only.** Do *not* extend this to ambiguous paths (`dist`, `build`, `out`, `data`, `results`, `public`, `*.egg-info`, `pdfs`, and similar), even when they are gitignored. Once such a directory has been externalized it is no longer synced anywhere, so the cloud copy is its **only remaining backup** — trashing it destroys the last copy. Leave those cloud folders in place. They are also not what produces the `(N)` duplicates.
- Before trashing, re-check the local counterpart: it must be a symlink into `~/.drive-nosync/` **that resolves** (`[ -L "$p" ] && [ -e "$p" ]`). Never trash a cloud folder whose local counterpart is a real directory, is git-tracked, or is a dangling symlink.
- Trashing a regenerable cloud copy does not endanger the real data: it lives in `~/.drive-nosync/`, outside Drive's view, reachable through the symlink — and it is regenerable in any case.
- Do this **after** externalizing, never before.

#### Step 4.6: Clean up `(N)` re-download duplicates

If a re-merge already happened, Drive has likely created duplicates. Find them, then prove each one is Drive-created before removing anything:

```bash
find "$HOME/My Drive" -maxdepth 8 \( -name "* (1)" -o -name "* (2)" \) -print -prune
```

Only remove an entry that satisfies **all three**:

- (a) the name with the ` (N)` suffix stripped is in the **regenerable** whitelist;
- (b) the sibling base path is a symlink into `~/.drive-nosync/` that resolves;
- (c) its birth time postdates the re-merge — `stat -f '%SB' <path>`. This test matters: in the observed incident 27 candidates were found, but only about 19 were Drive-created; the rest were long-standing user folders that merely happen to end in `(1)` (music albums, archived website copies), with birth times years earlier. Removing those would have destroyed real data.

Use `trash`, never recursive deletion. List what was removed with sizes, and list what was left alone with the reason. If a candidate fails any test, leave it and report it rather than deciding for the user.

#### Step 5: Update .gitignore files

For each affected git repository, ensure the original directory names are gitignored — and check with `git check-ignore -q <name>` rather than assuming, because **an existing entry is often the wrong shape**. A pattern with a trailing slash (`node_modules/`, `.venv/`) matches only a directory, so it stops matching once the path becomes a symlink, and git then reports every externalized path as a new untracked file. Use the bare name (`node_modules`, `.venv`), which matches both. Fix existing trailing-slash entries for paths this run externalized.

No `.nosync` entries are needed; remove any stale `.nosync` gitignore entries from the old approach.

If you edit a `.gitignore`, commit only that `.gitignore` hunk in the affected repository with a scoped message such as `chore: ignore generated Google Drive nosync directory`. If the repository has unrelated dirty changes and safe partial staging is not practical, report the uncommitted `.gitignore` edit instead of staging unrelated work.

#### Step 6: Restart Google Drive

Restart Google Drive only if this run changed something (any `MOVED`, `DROP`, legacy migration, or `.gitignore` edit):

```bash
if pgrep -x "Google Drive" >/dev/null; then
  killall "Google Drive"
  while pgrep -x "Google Drive" >/dev/null; do sleep 1; done
fi
open -a "Google Drive"
sleep 5
if pgrep -x "Google Drive" >/dev/null; then
  echo "Google Drive restarted successfully"
else
  echo "Google Drive did not appear to start; report this as unresolved"
fi
```

If the run made no changes, do not restart Drive unless the user explicitly asked for a restart.

#### Step 7: Verify and report

**The deliverable is the user-visible error count, not the local invariant.** A clean convergence pass proves only that the local layout is right; it does not prove Drive's error list shrank. Never say the errors are fixed, cleared, or resolved on the strength of local checks alone.

Verify in this order:

1. Convergence check — re-running Step 2 + Step 3 must be a clean no-op (only `OK`/`SKIP` lines, no `MOVED`/`DROP`/`trashed`/`ASK`/`BROKEN`). If a second pass still moves things, investigate before reporting done. Confirm each new symlink resolves with `[ -L "$dir" ] && [ -e "$dir" ]`.
2. Cloud-copy check — re-run the Step 4.5 enumeration; no externalized path should still resolve to a live cloud folder.
3. Error-count check — after the restart and a few minutes of settling, re-run the Step 0 triage and compare the `file_stream_posix ... Is a directory` count against the pre-run number. Then **ask the user what the Drive UI now reports**, since that number is the thing they actually care about and it is not readable from the logs.

If the count did not drop, say so plainly and give the Step 0 breakdown of what the remaining errors actually are. If verification is partial, write `Not verified end-to-end:` and name exactly what was and was not checked.

Clean up the temp files (`rm -f "$CANDIDATES" "$MOVED_MANIFEST"`).

Summarize:
- **Directories moved** outside Drive and total size removed from sync.
- **Conflicts auto-resolved** (stale external trashed, fresh Drive copy promoted), with sizes.
- **Dead symlinks dropped** and **broken symlinks** reported (if any).
- **`.gitignore` files** updated (if any).
- Confirmation that Google Drive was restarted, or that no restart was needed.
- **Unresolved / needs-a-human**: `ASK` (ambiguous or unproven-generated), `BROKEN` (external target gone for a non-regenerable type), git-tracked candidates skipped, or a failed restart.

Remind the user that:
- Wholesale-recreation operations (`npm ci`, `rm -rf node_modules`, some builds, fresh clones, interpreter-version changes) will re-clobber the symlink. This is expected; just re-run `/fix-drive-errors` — it converges and auto-resolves the conflict. Plain `npm install` follows the symlink and does not clobber it.
- The `/nosync` skill can be used to fix individual directories on the spot.
