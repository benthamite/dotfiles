---
name: fix-drive-errors
description: Diagnose and clear Google Drive sync errors. Distinguishes stale error entries from live offenders, since Drive's list does not clear itself. Use when Drive reports sync errors, when a symlink under ~/My Drive is suspected, or when content under ~/My Drive fails to sync. Never disconnects the account and never edits Drive's internal databases.
---

# Fix Google Drive sync errors

## The one thing to know first

**Drive's error count is not a measure of how much is wrong.** The list does not
clear itself when the offending path disappears, so entries survive long after
the cause is gone. On 2026-08-03 the panel reported 200+ errors across 27 pages
while exactly 8 live offenders remained; two of the six reported names had zero
instances left anywhere under Drive.

So never act on the count. Resolve each reported name against the filesystem
first and treat stale and live entries differently — only live ones need a fix,
and stale ones need a Drive restart.

## What Drive cannot sync

Two classes, and the first is the one that matters:

1. **Symlinks.** Google Drive has no representation for a symbolic link, so
   every symlink under `~/My Drive` is a permanent error for as long as it
   exists. This is the dominant cause.
2. **Churning generated directories** — `node_modules`, `.venv`, `__pycache__`,
   `.pytest_cache`, `.next`, build output. These sometimes sync and sometimes
   error, and they produce constant re-upload traffic either way.

## Current structure (2026-08-03)

No git repository lives under `~/My Drive` any more. 109 personal repositories
are at `~/repos` and 35 Epoch project repositories at `~/repos/epoch/<slug>`;
Epoch *notes* stay in Drive at `~/My Drive/Epoch/projects/<slug>/`, and the
dotfiles repository stays at `~/My Drive/dotfiles`. That migration removed the
bulk of the problem: build state now lands outside the sync root by location
rather than by any mechanism.

What remains under Drive is a small, enumerable set of symlinks. Run
`bin/drive-errors locate` for the current list rather than trusting this one.

## Workflow

1. **Read the panel and classify.**

       ~/My\ Drive/dotfiles/bin/drive-errors

   `list` reads Drive's menu-bar error panel through the accessibility API — the
   only way to get at it, since there is no file or API — and marks each reported
   name `LIVE` or `STALE` by counting instances under Drive. `locate` inventories
   every symlink and generated directory. With no argument it does both.

   If the panel cannot be read, grant Accessibility permission to the terminal,
   or click the Drive menu-bar item by hand and rerun.

2. **Fix the live offenders.** For each, decide by what it is:

   - **Throwaway cache** (`__pycache__`, `.pytest_cache`, `.ruff_cache`): delete
     the symlink and its target. It regenerates.
   - **Dependencies or a build tree needed to run something** (`node_modules`,
     `.venv`): do not delete silently. Either move the whole project out of
     Drive, which is the durable fix, or ask. Replacing the symlink with a real
     directory just moves the churn into Drive.
   - **Vendor content that cannot be regenerated** (for example, bundled
     `bootstrap-3.3.1/dist` trees): materialize it as real content. It is small
     and static, so Drive syncs it fine once it is not a link.

3. **Clear the stale entries by restarting Drive.** Quit and relaunch the app so
   it re-evaluates. A scripted `quit` may return `User canceled (-128)` if Drive
   raises a dialog; if so, quit it from the menu bar by hand. Restarting is not
   the same as disconnecting the account — see the limits below.

4. **Verify by re-reading the panel**, not by assuming the fix worked.

## Hard limits

Do not do these, and do not suggest them, even if the Drive UI or a support
article proposes them:

1. **Never disconnect or reconnect the Google Drive account**, and never remove
   or re-add the sync root. A past reconnection re-registered the folder as a
   computer backup and re-uploaded thousands of files into the wrong cloud
   folder. Quitting and relaunching the app is fine and is step 3.
2. **Never edit Drive's internal databases** — `mirror_sqlite.db`,
   `metadata_sqlite_db`, `root_preference_sqlite.db`, or anything else under
   `~/Library/Application Support/Google/DriveFS`. Reading them to diagnose is
   fine; writing, deleting or renaming them is not.
3. **Never create a symlink under `~/My Drive`.** This is what the retired
   `nosync` scheme did — keep the project in Drive, move its generated
   directories out, leave a link behind — and every link it created became a
   permanent sync error. The scheme was withdrawn on 2026-08-03. Move the
   project out of Drive instead.

## Known remaining cases

- **`dotfiles/enchant/*.dic|exc`** (16 links into `~/repos/<lang>/dict/`). These
  are committed in the dotfiles repository, so they return on any fresh
  checkout. Fixing them means either vendoring the dictionaries into dotfiles or
  moving dotfiles out of Drive — the latter is its own project, since dotfiles is
  the live config root that `~/.claude`, `~/.codex` and `~/.zshrc` link into.
- **`dotfiles/claude/skills/proofread/node_modules`** (57 MB). Real dependencies
  for a Node project that lives inside dotfiles. Needs an owner decision.
- **Two course `venv` directories** and **two static vendor trees**.
