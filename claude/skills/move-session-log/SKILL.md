---
name: move-session-log
description: Relocate Codex session logs by rewriting embedded project paths so resume and log lookup associate a session with the current project. Use for move session log, import log, or project rename session cleanup.
---

# Move session log

Import one Codex session into the current project, or rewrite Codex session
metadata after a project directory rename.

Codex stores sessions globally under `~/.codex/sessions/YYYY/MM/DD/` rather
than in per-project directories. Do not use `~/.claude/projects`,
`~/.claude/history.jsonl`, or `~/.claude.json` for this skill. For Codex, the
"move" is a metadata rewrite, not a filesystem move. Current Codex also stores
the resumable thread list in `~/.codex/state_5.sqlite`; the `threads.cwd` row
must be rewritten too, or `/resume` can still offer the old session directory.

Do not use this skill to inspect or open the current conversation log; use
`open-session-log` for that. Do not use it for Claude Code logs, which are
handled by the Claude-side `move-session-log` skill.

Use the bundled script:

```bash
python3 /Users/pablostafforini/My\ Drive/dotfiles/codex/skills/move-session-log/scripts/move_session_log.py <session-id>
```

The script defaults to the current working directory as the target project.
In single-session mode, the target project path is resolved to its canonical
absolute path before writing, so symlinked paths such as `/tmp` may be reported
under their resolved location.

## Modes

- **Single session**: `move-session-log <session-id>` - import one Codex
  session into the current project by setting structured path metadata in its
  session JSONL to `PWD`. This includes direct `cwd`/`project` fields and
  parsed JSON tool-call `workdir` fields that `/resume` can treat as the latest
  session directory. Also update matching `history.jsonl` and
  `session_index.jsonl` rows if those files contain path fields in the future,
  plus the matching `state_5.sqlite` `threads.cwd` row.
- **Whole-project rename**:
  `move-session-log --rename <old-project-path> <new-project-path>` - rewrite
  Codex session metadata from an old absolute project path to a new absolute
  project path across all session JSONL files and Codex index/history files.

## Single-session mode

`$ARGUMENTS` usually contains a session ID, for example
`019df86a-a988-7632-a9b1-603858683498`.

Run:

```bash
python3 /Users/pablostafforini/My\ Drive/dotfiles/codex/skills/move-session-log/scripts/move_session_log.py "$ARGUMENTS"
```

The script:

1. Finds the matching session JSONL under `~/.codex/sessions`. Codex filenames
   usually look like
   `rollout-YYYY-MM-DDTHH-MM-SS-<session-id>.jsonl`; the script also inspects
   `session_meta.payload.id` if needed.
2. Rewrites structured path fields in that session file to the current project
   root. Current Codex logs store direct session context at `payload.cwd` on
   `session_meta` and `turn_context`, and can also store tool-call directories
   as JSON-encoded `payload.arguments.workdir`.
3. Rewrites matching rows in `~/.codex/history.jsonl` and
   `~/.codex/session_index.jsonl` only if those rows contain structured path
   fields. Current Codex history rows may contain only `session_id`, `ts`, and
   `text`; that is normal.
4. Rewrites the matching `~/.codex/state_5.sqlite` `threads.cwd` row. Codex
   `/resume` reads this thread-store row for the "Session directory" prompt.
5. Reports counts and matching shell snapshots under `~/.codex/shell_snapshots`.
   Shell snapshots are session-global, not project-specific, so they are not
   moved.

If no session ID is supplied, list recent sessions from `~/.codex/sessions`
whose `session_meta.payload.cwd` is not the current project and ask the user
which one to import.

If the target project is not the shell's current directory, pass it explicitly:

```bash
python3 /Users/pablostafforini/My\ Drive/dotfiles/codex/skills/move-session-log/scripts/move_session_log.py --project '<target-project-path>' '<session-id>'
```

## Whole-project rename mode

Both paths must be absolute:

```bash
python3 /Users/pablostafforini/My\ Drive/dotfiles/codex/skills/move-session-log/scripts/move_session_log.py --dry-run --rename '<old-project-path>' '<new-project-path>'
python3 /Users/pablostafforini/My\ Drive/dotfiles/codex/skills/move-session-log/scripts/move_session_log.py --rename '<old-project-path>' '<new-project-path>'
```

The script rewrites exact structured path fields only:

- `cwd` fields equal to the old path
- `project` fields equal to the old path
- `workdir` and `working_dir` fields equal to the old path
- those same fields inside valid JSON `arguments` objects

It does not perform raw string replacement in transcript text, command output,
or command strings inside function-call arguments.

The script also rewrites exact `threads.cwd` matches in `state_5.sqlite`.

Always run the dry run first for `--rename`, inspect the reported counts, and
stop if the old/new paths appear reversed or the count is unexpectedly broad.

## Verification

After the script runs, verify the reported counts. For a single-session import,
the session file should have no structured `cwd` values left for the old project
path and should have no parsed JSON tool-call `workdir` values left for the old
project path.
Report:

- the session file path
- the previous `session_meta.payload.cwd`, if found
- the number of session JSONL path fields rewritten
- whether `history.jsonl` or `session_index.jsonl` had path fields to update
- whether shell snapshots were found

For `--rename`, report whether the dry run matched the final run's counts:

- session files scanned and rewritten
- session path fields rewritten
- `history.jsonl` path fields rewritten
- `session_index.jsonl` path fields rewritten
- `state_5.sqlite` `threads.cwd` rows rewritten
