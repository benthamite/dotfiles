---
name: rename-project
description: Rename an Epoch project end-to-end — local directory, separate-gitdir, GitHub repo, 1Password item, Slack channel, cross-references, and Claude session-log dir. Use when the user says "rename project", "rename this project", or wants to change a project's slug across all the places it's referenced.
user-invocable: true
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, AskUserQuestion
argument-hint: "<old-slug> [new-slug]"
---

# Rename an Epoch project

Rename a project's slug everywhere it lives: local fs, GitHub, 1Password, Slack, cross-project references, and Claude Code's own session-log/rendered-transcript dirs. Order the rename to keep the deployed automation working at every step.

## Arguments

`$ARGUMENTS` is `<old-slug> [<new-slug>]`. If only one slug is given, ask for the new one. Slugs are kebab-case directory names like `ai-productivity-bot`. If `$ARGUMENTS` is empty and the user is `cd`'d inside a project, infer the old slug from the basename of `projects/<slug>/`.

Distinguish three derived strings up front:
- **slug** (`ai-productivity-bot`) — directory, repo, channel, 1P-item suffix
- **display name** (`AI productivity bot`) — title in CLAUDE.md, project org file, projects index
- **gitdir name** (`epoch-ai-productivity-bot`) — `~/git-dirs/` prefix is `epoch-`

Generic English uses of words inside the slug (e.g. "Slack bot that posts...") should **not** be rewritten — only explicit slug/display-name references.

## Step 1: Inventory everything that references the old slug

Run these in parallel and note every hit:

1. `grep -rln "<old-slug>" --exclude-dir=.git "<Epoch root>"` — Epoch parent repo
2. `grep -rln "<old-slug>" ~/.claude/` — Claude config + rendered transcripts + session logs
3. `ls ~/git-dirs/ | grep <old-slug>` — separate-gitdir
4. `cat <project>/repo/.git` — confirm it's a `gitdir:` pointer; capture target
5. `cd <project>/repo && git remote -v` — capture remote URL
6. `gh repo view epoch-research/<old-slug>` — confirm GitHub repo exists
7. `op item get "Anthropic - <old-slug>" --vault Automations 2>&1` (and any other obvious `<service> - <old-slug>` patterns from the workflow file)
8. `mcp__slack-official-epochai__slack_search_channels query=<old-slug>` — confirm channel state and ID

Categorize each grep hit as:
- **active reference** (rewrite): CLAUDE.md, `.gitignore`, `current-list-of-automation-projects.org`, project's own org file, sibling `automation-watchdog/repo/config.yml`, code/workflow files inside the project repo, etc.
- **historical record** (leave alone): anything under `logs/<date>.md`, `meetings/<person>/<date>.org`, `~/.claude/history.jsonl`, `~/.claude/projects/*/...jsonl` content, `:summary` strings inside `~/.claude/rendered/_index.el`. These describe past events; rewriting them falsifies history.

## Step 2: Decide on external services

Use `AskUserQuestion` for each that exists:

1. **Slack channel**: if `#<old-slug>` exists, ask whether to rename it. If yes, instruct the user to rename it in Slack (the skill itself shouldn't do this — it's visible to all members and Slack's API for channel rename requires admin scope we don't load by default). Confirm the new name before proceeding. The channel ID is stable across rename, so `SLACK_CHANNEL = "<id>"` constants in code stay correct.
2. **1Password item**: if `<service> - <old-slug>` exists in the `Automations` vault (or any other vault flagged by Step 1), ask whether to rename. If yes, the skill will do it via the biometric fallback (see Step 4).

Skip the question if the corresponding artifact doesn't exist.

## Step 3: Present a dry-run plan and confirm

Show the user a structured plan before any destructive action:
- Local fs renames (project dir, gitdir, .git pointer, Claude session-log dir, rendered-transcripts dir, project org file)
- GitHub repo rename + description update
- 1P item rename (if approved)
- File-content rewrites: list each file with the count of matches
- What is being explicitly **left alone** (historical logs, summaries, etc.)
- Any external-service flags for manual followup

Get explicit confirmation. **Do not auto-confirm** even in auto mode — this touches shared systems.

## Step 4: Execute in this exact order

The order is chosen to keep the live automation working with no silent breakage. Don't reorder.

### 4a. Edit project-internal files (don't push yet)

Inside `projects/<old-slug>/repo/`:
- `README.md`: title heading, channel name, 1P item references
- `post.py` / equivalent: docstring, `SLACK_CHANNEL` comment, any literal channel mentions
- `.github/workflows/<file>.yml`: `op://Automations/<service> - <old-slug>/credential` references → new slug
- Any other code/doc files that grep flagged

Stage and commit with message `Rename: <old-slug> → <new-slug>`. **Do not push yet** — the new code references the new 1P item name, which doesn't exist yet.

### 4b. Rename the 1Password item

Service-account token is read-only. Establish a desktop-app session, then edit:

```bash
env -u OP_SERVICE_ACCOUNT_TOKEN op signin --force
env -u OP_SERVICE_ACCOUNT_TOKEN op item edit <item-id> --vault=Automations \
  --title="<service> - <new-slug>"
```

Verify both directions:
```bash
op read "op://Automations/<service> - <new-slug>/credential"   # service-account token reads → success
op read "op://Automations/<service> - <old-slug>/credential"   # → 404 expected
```

If the item lives in any vault other than `Automations`, the service-account token can't read it; verify with `env -u OP_SERVICE_ACCOUNT_TOKEN op read ...` instead. See the `store-secret` skill for vault rules.

### 4c. Push the project repo, then rename the GitHub repo

```bash
cd projects/<old-slug>/repo
git pull --rebase origin master   # likely needed: workflow auto-commits cutoff/state files
git push
gh repo rename <new-slug> --repo epoch-research/<old-slug> --yes
git remote set-url origin git@github.com:epoch-research/<new-slug>.git
git fetch origin
```

GitHub auto-creates a permanent redirect from old → new for both git operations and the API, so any sibling automation that hasn't been updated yet keeps working.

### 4d. Rename local filesystem

```bash
mv ~/git-dirs/epoch-<old-slug> ~/git-dirs/epoch-<new-slug>
# Rewrite the .git pointer (Write tool — single line: "gitdir: <new-path>")
mv "<Epoch>/projects/<old-slug>/<old-slug>.org" "<Epoch>/projects/<old-slug>/<new-slug>.org"
mv "<Epoch>/projects/<old-slug>" "<Epoch>/projects/<new-slug>"
mv ~/.claude/projects/-Users-...-<old-slug> ~/.claude/projects/-Users-...-<new-slug>
mv ~/.claude/rendered/<old-slug> ~/.claude/rendered/<new-slug>     # if the dir exists
```

If `~/.claude/rendered/_index.el` exists, rewrite the path strings inside it too (the file is large — use Python with `replace`, not Edit):

```python
src = '/Users/pablostafforini/.claude/rendered/_index.el'
data = open(src).read()
open(src + '.bak', 'w').write(data)
open(src, 'w').write(data.replace(f'/rendered/<old-slug>/', f'/rendered/<new-slug>/'))
```

`trash` the `.bak` after the next agent-log session loads the index without errors. **Don't** rewrite `:summary` strings inside the index — those are historical descriptions of past sessions.

### 4e. Rewrite cross-references in active files

For each active-reference file from Step 1, do an `Edit` (or `Edit replace_all=true` when the file has many slug occurrences with no false-positive risk):
- `<Epoch>/.gitignore`: `projects/<old-slug>/repo/` → new
- `<Epoch>/projects/current-list-of-automation-projects.org`: section heading, `:ID:` property, prose mentions, watchdog list entry on the watchdog section
- `<Epoch>/projects/<new-slug>/CLAUDE.md`: title, channel name
- `<Epoch>/projects/<new-slug>/<new-slug>.org`: title, all slug references (replace_all is safe here)
- `<Epoch>/projects/automation-watchdog/repo/config.yml`: `repo:` line — only update the slug, leave the `label:` (it's a human-readable label that may or may not contain the slug)
- `<Epoch>/projects/automation-watchdog/automation-watchdog.org`: prose mentions, table row in the "Currently monitored" section

Stage and commit each repo's changes separately (one logical commit per repo):
- Inside `automation-watchdog/repo/`: commit + push (the watchdog config is live code)
- Inside the Epoch parent repo: commit + push

### 4f. Update the GitHub repo description

```bash
gh repo edit epoch-research/<new-slug> --description "<new description with new channel name if applicable>"
```

## Step 5: Verification sweep

1. **Final stale-slug grep** (active files only):
   ```bash
   grep -rln "<old-slug>" --exclude-dir=.git "<Epoch>" 2>/dev/null \
     | grep -vE "/logs/|/meetings/[^/]+/2[0-9]{3}-"
   # should be empty
   ```
2. **GitHub state**: `gh repo view epoch-research/<new-slug> --json name,description,url` and `gh repo view epoch-research/<old-slug>` (latter should resolve to new via redirect).
3. **1P state**: `op read "op://Automations/<service> - <new-slug>/credential"` (using service-account token, not biometric session) — proves GH Actions will see it.
4. **Local repo health**: `cd <Epoch>/projects/<new-slug>/repo && git status && git log -1 --oneline`.
5. **End-to-end (best test)**: if the project has a GH Actions workflow with `workflow_dispatch` and a `dry_run` input, trigger one:
   ```bash
   gh workflow run <workflow>.yml -f dry_run=true
   gh run watch <run-id> --repo epoch-research/<new-slug> --exit-status
   ```
   A successful dry run proves the new repo + new 1P ref + new code wire together.

## Pitfalls observed

- **First push after the local commit may be rejected** because the workflow has been auto-committing state (cutoff timestamps, dedup state) since the last fetch. `git pull --rebase` first.
- **`op item edit --title` fails silently** with the read-only service-account token (returns "Couldn't update the item" with no detail). Always prefix `env -u OP_SERVICE_ACCOUNT_TOKEN` and ensure `op signin --force` succeeded.
- **The `.git` pointer file inside the worktree is a single text line** (`gitdir: <path>`); use `Write`, not `Edit` (Edit needs `Read` first and the format is brittle).
- **The Edit tool requires `Read` on every target file** even if you've already read it via Bash `cat`.
- **`replace_all` on slugs is safe** when the slug is unambiguous (e.g. `ai-productivity-bot` doesn't appear as a substring of any other word). Verify by inspecting the grep output before flipping the switch.
- **Generic English uses** ("Slack bot that posts…") should not be rewritten. Only rewrite explicit slug references and the project display name.
- **`~/.claude/rendered/_index.el` is large** (>500k tokens) and must be edited via Python text-replace, not Read/Edit.

## What this skill deliberately does NOT touch

- Historical session logs and meeting notes (rooted by date)
- `~/.claude/history.jsonl`
- Old session-log `.jsonl` files' content (the dir is renamed, the content is preserved)
- `:summary` strings in `~/.claude/rendered/_index.el`
- Asana project name, Gmail filters, Google Drive folders — flag these for manual followup if they exist

## Reference: this skill was authored from the rename of `ai-productivity-bot` → `ai-productivity-digest` (2026-05-02). End-to-end workflow_dispatch verification: `gh run 25254428602` succeeded.
