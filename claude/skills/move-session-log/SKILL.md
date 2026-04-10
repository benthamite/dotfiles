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

4. **Rewrite history.jsonl.** Update the `project` field in `~/.claude/history.jsonl` for all entries matching this session ID to point to `$CLAUDE_PROJECT_DIR`:

   ```python
   import json

   history = os.path.expanduser("~/.claude/history.jsonl")
   new_project = os.environ["CLAUDE_PROJECT_DIR"]

   lines = []
   with open(history) as f:
       for line in f:
           entry = json.loads(line)
           if entry.get("sessionId") == session_id:
               entry["project"] = new_project
           lines.append(json.dumps(entry, ensure_ascii=False))

   with open(history, "w") as f:
       f.write("\n".join(lines) + "\n")
   ```

5. **Rewrite cwd in the session .jsonl.** Update all `cwd` fields in the moved session file to point to `$CLAUDE_PROJECT_DIR`:

   ```python
   import json

   session_file = os.path.join(current_dir, f"{session_id}.jsonl")
   new_cwd = os.environ["CLAUDE_PROJECT_DIR"]

   lines = []
   with open(session_file) as f:
       for line in f:
           line = line.rstrip("\n")
           if not line.strip():
               lines.append(line)
               continue
           try:
               entry = json.loads(line)
           except json.JSONDecodeError:
               lines.append(line)
               continue
           if "cwd" in entry:
               entry["cwd"] = new_cwd
           lines.append(json.dumps(entry, ensure_ascii=False))

   with open(session_file, "w") as f:
       f.write("\n".join(lines) + "\n")
   ```

6. **Confirm.** Report what was moved and from where (show the source project path for clarity), and note that `history.jsonl` and session `cwd` fields were updated.
