---
name: move-session-log
description: Use when the user asks to relocate or import a session log into the current project, or to make session history and resume follow a renamed project directory in the current tool; not for merely opening or inspecting the current session log.
---

# Relocate Codex session metadata

Import one explicitly identified Codex session into a project, or relocate
metadata after a project-directory rename. Codex stores transcripts globally:
this changes metadata, not their physical project directory. It does not rename
the project, unarchive sessions, merge histories or operate on Claude stores.
Use `open-session-log` for simple inspection. Review, diagnosis and planning
requests remain read-only.

Use the bundled [adapter](scripts/move_session_log.py) and its
[safety helper](scripts/migration_safety.py). Do not invent an inline SQL or
transcript-rewrite fallback when the helpers or preflight fail.

## Establish identity and scope

- Resolve the actual active `CODEX_HOME`, defaulting to `~/.codex` only when
  unset. Inspect resolved store identities, not a guessed account or recent file.
  Import requires the exact UUID and one matching `session_meta.payload.id`;
  a filename substring is not identity evidence.
- Resolve the absolute target project; pass `--project` explicitly when it is
  not the shell directory. With no ID the adapter only lists candidates. Choose
  from reliable context or ask one identity question; do not select the newest.
- Rename takes absolute old and new paths, in that order. Mapping is exact, not
  prefix replacement. Report descendant contexts that also need relocation.
- Inventory active `sessions/`, associated `archived_sessions/`, and each
  discovered profile's history, session index and thread database. Discovery
  covers the active home and sibling `.codex*` homes sharing its resolved session
  store, not arbitrarily located profiles. Deduplicate resolved shared files.
  Report absent artifacts as unupdated; do not create a replacement index or
  claim complete coverage from this bounded discovery.

## Preview both modes

Use this skill's resolved directory for `SKILL_DIR`; pass literal quoted
arguments, not paths or IDs interpolated into SQL, Python or shell source.

```bash
python3 "$SKILL_DIR/scripts/move_session_log.py" --dry-run --project "$TARGET_PROJECT" "$SESSION_ID"
python3 "$SKILL_DIR/scripts/move_session_log.py" --dry-run --rename "$OLD_PROJECT" "$NEW_PROJECT"
```

Validate the whole selected inventory, JSONL identity and SQLite schemas before
any apply phase. Database inspection uses owned temporary snapshots with captured
WAL state, not a connection that can recover/checkpoint originals. Refuse
malformed/truncated targeted files, identity conflicts, unsupported existing
schemas or unstable inputs. Resolve reversed paths or unexpected scope. Zero
matches do not prove the search was complete.

The database adapter supports the reviewed `state_5.sqlite` thread schema and
known triggers that do not fire on `cwd` updates. Unhandled generations or side effects
refuse before writes. Its database scope is thread `cwd`, not independent project
associations: observed `project_id`, project tables or project roots are reported
as unchanged. Do not claim those associations were migrated.

## Apply only to offline stores

Establish that affected sessions and every writer of shared files/databases are
stopped. An active session must not replace its own append-only transcript:
existing writers can keep appending successfully to an unlinked old file.
Do not kill sessions, close Emacs, restart Codex or switch accounts just to make
this possible. If quiescence cannot be established within the request, keep the
preview and report that boundary.

Apply requires `--offline` and a new absolute `--backup-dir` outside Google Drive.
The flag asserts established quiescence; it does not stop writers. The helper
revalidates inputs and inspects kernel device/inode records using the supported
Darwin/libproc `lsof` 4.91 format. Unrecognized formats or diagnostics refuse the
operation. This can miss processes the OS hides and cannot exclude future writers;
it does not independently establish quiescence.

```bash
python3 "$SKILL_DIR/scripts/move_session_log.py" --offline --backup-dir "$NEW_PRIVATE_BACKUP" --project "$TARGET_PROJECT" "$SESSION_ID"
python3 "$SKILL_DIR/scripts/move_session_log.py" --offline --backup-dir "$NEW_PRIVATE_BACKUP" --rename "$OLD_PROJECT" "$NEW_PROJECT"
```

Choose a durable uniquely named location under a private off-Drive state root.
Protected originals and the recovery journal contain private user history, not
scratch or public repository artifacts. Do not publish or delete them as cleanup.

Only consumer-relevant `session_meta`/`turn_context` payload `cwd` and supported
top-level history/index metadata are eligible. Import maps the identified
original project to the target, preserving different contexts. SQLite updates
match exact thread IDs, never `rollout_path LIKE` or a nested arbitrary `id`.
Historical tool arguments, results, prose and unrelated records stay unchanged.
Archives remain archived; shell snapshots are not moved.

## Verify the right result

Inspect exit status and journal; independently read back selected metadata,
exact thread rows and unchanged unrelated records. Compare the actual result
with the preview and explain changed inputs or coverage limits. Repeat a preview
for remaining intended changes: matching counts alone are not proof.

Files and profile-local SQLite transactions are not one atomic transaction. On
failure, stop and inspect completed operations and retained originals. Report
partial application and its recovery path. Do not blindly retry or roll back
over newer state.

The installed app-server interface accepts a `cwd` override for `thread/resume`,
and the Emacs client sends it. That alone does not establish durable relocation
of the original rollout header. Project-filtered listing also uses stored
metadata and can repair it from JSONL; changing only a database row is insufficient
consistency evidence.

Use `end-to-end` when claiming that the actual history/resume consumer now works.
Do not start a live session as part of a dry run or an audit of this skill.
Report the session/project, material outcome and any partial-state, coverage or
live-verification gap briefly.
