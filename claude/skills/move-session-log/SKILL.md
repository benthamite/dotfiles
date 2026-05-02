---
name: move-session-log
description: Relocate Claude Code internal session logs and rewrite the embedded paths so resume keeps working. Two modes — single session (move one .jsonl from another project to the current project) and whole-project rename (move all sessions when a project directory has been renamed). Use when the user says "move session log", "move session", "move log", "rename project session logs", or wants Claude's session data to follow a project rename.
argument-hint: "<session-id> | --rename <old-project-path> <new-project-path>"
tools: Bash, Glob
---

# Move session log

Relocate one or all Claude Code session logs and rewrite every embedded path (`history.jsonl` `project` field, `.jsonl` `cwd` fields, `~/.claude.json` `projects` map key) so `agent-log-resume-session` and `claude --resume` continue to work.

## Modes

- **Single session**: `move-session-log <session-id>` — move a specific session from another project's logs to the **current** project's logs. Use when one session got recorded under the wrong project (e.g., you `cd`'d during a session, or invoked Claude from the wrong dir).
- **Whole-project rename**: `move-session-log --rename <old-project-path> <new-project-path>` — move all session data when a project's filesystem path changes. Used by the `rename-project` skill. This is also the per-package primitive that `migrate-profile` performs in bulk; that skill stays separate because it adds merge semantics and `~/.claude.json` profile-path rewrites that the simple rename doesn't need.

## Path encoding

Claude Code encodes project paths as directory names under `~/.claude/projects/` by replacing every `/`, `.`, and space with `-`. The leading `/` becomes a leading `-`.

```bash
encode() { echo "$1" | sed 's|[/. ]|-|g'; }
```

Example: `/Users/pablostafforini/My Drive/Epoch/projects/ai-productivity-digest` becomes `-Users-pablostafforini-My-Drive-Epoch-projects-ai-productivity-digest`.

## Single-session mode

`$ARGUMENTS` must contain a session ID (a UUID like `ea9393a8-9a19-457a-bcb2-d5ca0c5ded55`). If empty, list all `.jsonl` files across `~/.claude/projects/` that are NOT in the current project's directory (sorted by mtime, most recent first, top 10), and ask the user to pick one.

### Steps

1. **Derive the current project's encoded directory.**

   ```bash
   current_dir="$HOME/.claude/projects/$(echo "$PWD" | sed 's|[/. ]|-|g')"
   ```

   Verify the directory exists; stop if not.

2. **Find the session file in another project's directory.**

   ```bash
   find "$HOME/.claude/projects" -maxdepth 2 -name "<session-id>.jsonl" ! -path "$current_dir/*"
   ```

   Stop if not found.

3. **Move the `.jsonl` and the matching same-name subdirectory** (the latter holds tool-results / subagents and may be absent):

   ```bash
   mv "<source>/<session-id>.jsonl" "$current_dir/"
   mv "<source>/<session-id>" "$current_dir/" 2>/dev/null
   ```

4. **Rewrite `history.jsonl`** — set the `project` field on entries matching this `sessionId` to the current project root:

   ```python
   import json, os

   history = os.path.expanduser("~/.claude/history.jsonl")
   new_project = os.getcwd()

   lines = []
   with open(history) as f:
       for line in f:
           entry = json.loads(line)
           if entry.get("sessionId") == "<session-id>":
               entry["project"] = new_project
           lines.append(json.dumps(entry, ensure_ascii=False))

   with open(history, "w") as f:
       f.write("\n".join(lines) + "\n")
   ```

5. **Rewrite `cwd` in the moved `.jsonl`** to the current project root:

   ```python
   import json, os

   session_file = os.path.join(current_dir, "<session-id>.jsonl")
   new_cwd = os.getcwd()

   lines = []
   with open(session_file) as f:
       for line in f:
           line = line.rstrip("\n")
           if not line.strip():
               lines.append(line); continue
           try:
               entry = json.loads(line)
           except json.JSONDecodeError:
               lines.append(line); continue
           if "cwd" in entry:
               entry["cwd"] = new_cwd
           lines.append(json.dumps(entry, ensure_ascii=False))

   with open(session_file, "w") as f:
       f.write("\n".join(lines) + "\n")
   ```

6. **Confirm**: report what was moved, from where, and that `history.jsonl` + `cwd` were rewritten.

## Whole-project rename mode

`$ARGUMENTS` is `--rename <old-project-path> <new-project-path>`. Both paths must be absolute. The two paths refer to the project's filesystem location before and after the rename.

### Steps

1. **Encode both paths**:

   ```bash
   src="$HOME/.claude/projects/$(echo '<old-project-path>' | sed 's|[/. ]|-|g')"
   dst="$HOME/.claude/projects/$(echo '<new-project-path>' | sed 's|[/. ]|-|g')"
   ```

2. **Move the session-log directory if it hasn't moved already.** Idempotent — the `rename-project` skill may have already done the `mv` before invoking this skill, in which case the source is gone and only path rewrites remain.

   ```bash
   if [ -d "$src" ] && [ -d "$dst" ]; then
     echo "ERROR: both source and destination exist; refusing to merge. Investigate."; exit 1
   elif [ -d "$src" ] && [ ! -d "$dst" ]; then
     mv "$src" "$dst"
   elif [ ! -d "$src" ] && [ -d "$dst" ]; then
     :  # already moved; proceed to path rewrites
   else
     echo "Neither $src nor $dst exists; nothing to rename."; exit 0
   fi
   ```

3. **Rewrite `~/.claude/history.jsonl`** — every entry whose `project` is exactly the old path becomes the new path:

   ```python
   import json, os

   history = os.path.expanduser("~/.claude/history.jsonl")
   old, new = "<old-project-path>", "<new-project-path>"

   n = 0
   lines = []
   with open(history) as f:
       for line in f:
           line = line.rstrip("\n")
           if not line.strip():
               lines.append(line); continue
           try:
               entry = json.loads(line)
           except json.JSONDecodeError:
               lines.append(line); continue
           if entry.get("project") == old:
               entry["project"] = new
               n += 1
           lines.append(json.dumps(entry, ensure_ascii=False))

   with open(history, "w") as f:
       f.write("\n".join(lines) + "\n")
   print(f"history.jsonl: rewrote {n} entries")
   ```

4. **Rewrite `cwd` in every `.jsonl` under the destination directory.** Iterate top-level files only — the same-name session subdirs may contain tool-result files but those don't have `cwd` fields. (If subagent `.jsonl` files turn up in subdirectories, extend the glob.)

   ```python
   import json, os, glob

   dst = os.path.expanduser('~/.claude/projects/' + '<new-project-path>'.replace('/', '-').replace('.', '-').replace(' ', '-'))
   old, new = "<old-project-path>", "<new-project-path>"

   total = 0
   for jsonl in glob.glob(os.path.join(dst, "*.jsonl")) + glob.glob(os.path.join(dst, "*", "*.jsonl")):
       n = 0
       lines = []
       with open(jsonl) as f:
           for line in f:
               line = line.rstrip("\n")
               if not line.strip():
                   lines.append(line); continue
               try:
                   entry = json.loads(line)
               except json.JSONDecodeError:
                   lines.append(line); continue
               if entry.get("cwd") == old:
                   entry["cwd"] = new
                   n += 1
               lines.append(json.dumps(entry, ensure_ascii=False))
       if n:
           with open(jsonl, "w") as f:
               f.write("\n".join(lines) + "\n")
           total += n
           print(f"{os.path.basename(jsonl)}: rewrote {n} cwd entries")
   print(f"total cwd rewrites: {total}")
   ```

5. **Migrate the `~/.claude.json` projects-map entry** if one exists for the old path. This entry stores per-project trust state, MCP server allowlists, and tool permissions — without it Claude Code re-prompts the trust dialog after the rename.

   ```python
   import json, os

   path = os.path.expanduser("~/.claude.json")
   old, new = "<old-project-path>", "<new-project-path>"
   data = json.load(open(path))
   projects = data.get("projects", {})
   if old in projects and new not in projects:
       projects[new] = projects.pop(old)
       data["projects"] = projects
       json.dump(data, open(path, "w"), indent=2)
       print(f"~/.claude.json: migrated projects['{old}'] → projects['{new}']")
   elif old in projects and new in projects:
       print("~/.claude.json: both old and new entries exist; left as-is. Inspect manually.")
   else:
       print("~/.claude.json: no projects entry for old path; skipped.")
   ```

6. **Confirm**: report counts (history entries rewritten, cwd rewrites across N files, projects-map status).

## When to use which

| Scenario | Mode |
|---|---|
| One session got logged under the wrong project | single |
| A project directory was renamed and you want session resume to keep working | --rename |
| Bulk migration across an elpaca profile change | use the `migrate-profile` skill instead — it handles merge semantics and profile-path rewrites that this skill doesn't |
