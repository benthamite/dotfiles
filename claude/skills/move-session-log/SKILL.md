---
name: move-session-log
description: Move a Claude Code internal session log (.jsonl and associated directory) from another project's internal logs to the current project's internal logs. Use when the user says "move session log", "move session", "move log", or wants to relocate an internal Claude Code session.
argument-hint: "<session-id>"
tools: Bash, Glob
---

# Move session log

Move a Claude Code internal session log from one project to the current project.

## Arguments

`$ARGUMENTS` must contain a session ID (a UUID like `ea9393a8-9a19-457a-bcb2-d5ca0c5ded55`).

If no argument is provided, list all `.jsonl` files across `~/.claude/projects/` that are NOT in the current project's directory (sorted by modification time, most recent first, showing only the 10 most recent), and ask the user to pick one.

## Steps

1. **Derive the current project's internal logs directory.** The directory name under `~/.claude/projects/` is the absolute path of the project root with every `/` and space replaced by `-`. Use `$CLAUDE_PROJECT_DIR` to get the project root, then derive the directory name:

   ```bash
   current_dir="$HOME/.claude/projects/$(echo "$CLAUDE_PROJECT_DIR" | sed 's|[/ ]|-|g')"
   ```

   Verify this directory exists. If not, report the error and stop.

2. **Find the session log in other project directories.** Search for `<session-id>.jsonl` across all subdirectories of `~/.claude/projects/`, excluding the current project's directory.

   ```bash
   find "$HOME/.claude/projects" -maxdepth 2 -name "<session-id>.jsonl" ! -path "$current_dir/*"
   ```

   If not found, report that no session log with that ID was found.

3. **Move the files.** Move both the `.jsonl` file and the associated directory (same name without extension) if it exists:

   ```bash
   mv "<source>/<session-id>.jsonl" "$current_dir/"
   mv "<source>/<session-id>" "$current_dir/" 2>/dev/null  # directory may not exist
   ```

4. **Confirm.** Report what was moved and from where (show the source project path for clarity).
