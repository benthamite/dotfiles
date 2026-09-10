---
name: move-session-log
description: Use when the user asks to relocate or import a session log into the current project, or to make session history and resume follow a renamed project directory in the current tool; not for merely opening or inspecting the current session log.
argument-hint: "<session-id> | --rename <old-project-path> <new-project-path>"
---

# Move Claude session history

Relocate one explicitly identified Claude Code session, or its history after a
project-directory rename. This does not rename the project itself, merge stores,
or authorize changes to Codex sessions. Simple inspection belongs to
`open-session-log`. A review, diagnosis or planning request remains read-only.

Use the bundled [adapter](scripts/move_session_log.py) and its
[safety helper](scripts/migration_safety.py). Do not substitute inline `mv`,
truncating JSON rewrites, or another account's store if the helpers fail.

## Establish identity and scope

- Identify the actual account/configuration directory and target project from
  reliable context. The adapter honors `CLAUDE_CONFIG_DIR`; otherwise it uses
  `~/.claude`. Inspect resolved symlink destinations, not a guessed profile.
  Discovery is limited to that configuration root. Sibling/custom profiles'
  separate histories are not inventoried, even when their projects store is
  shared. Report this boundary and absent history stores as unupdated.
- Import requires the exact UUID and matching transcript metadata. Pass an
  explicit absolute `--project` when the shell directory is not the target.
  With no ID the adapter only lists candidates; select from established context
  or ask one concise question. Do not import the newest candidate automatically.
- Rename takes absolute old and new paths, in that order. It relocates history,
  not the filesystem project. Claude's encoded paths are lossy: different
  projects can share a bucket. Refuse ambiguous/mixed origins, unknown root
  artifacts or destination collisions instead of merging or overwriting.
- Inventory the exact transcript, recursively nested sidecars, selected history
  records and resolved shared files. Only supported `subagents/agent-*.jsonl`
  sidecars contain routing metadata; tool results remain opaque regardless of
  extension. Verify existing destination ownership before importing. A later
  different `cwd` does not by itself establish another originating project.

## Preview both modes

Use this skill's resolved directory for `SKILL_DIR`. Pass literal quoted
arguments, never paths interpolated into Python or shell source.

```bash
python3 "$SKILL_DIR/scripts/move_session_log.py" --dry-run --project "$TARGET_PROJECT" "$SESSION_ID"
python3 "$SKILL_DIR/scripts/move_session_log.py" --dry-run --rename "$OLD_PROJECT" "$NEW_PROJECT"
```

Review every selected identity, store, destination and proposed count. Resolve
unknown ownership, malformed inputs, unexpected symlinks, conflicting metadata,
reversed paths or unexpectedly broad scope before writing. Dry-run is a preview
of observed inputs, not a reservation or proof that a later plan is identical.

## Apply only to offline stores

This is the Claude adapter's requirement. The paired Codex skill can import a
closed session while unrelated Codex sessions remain open, using SQLite
transactions. Do not transfer Claude's full-store offline requirement to that
Codex operation; the transcript layouts and concurrency mechanisms differ.
The Codex workflow also verifies the exact session's Emacs buffer and live
working directory; a migrated history index does not rename an existing buffer.

Establish that affected Claude sessions and other writers of shared history are
stopped. Do not kill sessions, close Emacs, restart applications or switch
accounts merely to satisfy this step. An active session cannot safely replace
its own append-only transcript. If quiescence cannot be established within the
request, preserve the preview and report that boundary.

Apply requires `--offline` and an explicit, new absolute `--backup-dir` outside
Google Drive. The flag asserts established quiescence; it does not stop writers.
The helper also revalidates captured inputs and inspects kernel device/inode
records using the supported Darwin/libproc `lsof` 4.91 format. Unrecognized
formats or diagnostics refuse the operation. This inspection can miss processes
the OS hides and cannot exclude future writers; it does not establish quiescence.

```bash
python3 "$SKILL_DIR/scripts/move_session_log.py" --offline --backup-dir "$NEW_PRIVATE_BACKUP" --project "$TARGET_PROJECT" "$SESSION_ID"
python3 "$SKILL_DIR/scripts/move_session_log.py" --offline --backup-dir "$NEW_PRIVATE_BACKUP" --rename "$OLD_PROJECT" "$NEW_PROJECT"
```

Choose a durable uniquely named location under a private off-Drive state root.
Protected originals and a recovery journal are created before writes. They
contain private history, not disposable scratch: do not publish, commit or
delete them as cleanup. Follow secret-handling rules when settings are in scope.

Only known top-level runtime path metadata and selected history ownership fields
change. Mapping is exact origin-to-target: unrelated and descendant directories
are preserved, not flattened or prefix-replaced. Report remaining descendant
contexts when relevant. Historical tool arguments, results, prose and untouched
JSONL records remain unchanged.

Project settings in `.claude.json` remain untouched by default: they can contain
trust, tool permissions and MCP approvals. Only a separately explicit request
to carry settings to the same verified renamed project permits
`--migrate-project-settings` on both preview and apply. Relocating history alone
does not grant that authority. Existing destination settings are not merged.

## Verify and recover honestly

Check exit status and journal, then independently inspect exact destination
layout, source disposition, metadata and preservation of unrelated records and
symlink identities. A repeated preview should show no remaining intended
metadata changes; explain unsupported contexts or missing stores instead of
calling counts complete coverage.

Per-file replacement is not one transaction across every transcript, move and
history file. On failure, stop and inspect completed operations and retained
originals. Do not blindly retry, roll back over newer state or claim that failure
changed nothing.

Use `end-to-end` for a claim that actual project-filtered history/resume now works.
Offline fixtures or metadata counts do not establish live resume behavior. Report
the relocated identity, meaningful result and material recovery/verification gaps.
