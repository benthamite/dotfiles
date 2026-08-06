---
name: fix-drive-errors
description: Use when Google Drive reports sync errors, a symlink or generated directory exists under ~/My Drive, or a Google Docs pointer fails to upload from the synced personal-account root.
---

# Fix Google Drive sync errors

## Core rule

Treat the current, fully paginated Drive error panel as the source of truth. Its
rows may survive after an offender disappears, but a reported basename is not a
path: finding the same name elsewhere does not prove that instance caused the
row. Restart Drive after repairs, then read the actual panel again.

## Read and inventory

Run:

    ~/My\ Drive/dotfiles/bin/drive-errors

`list` opens the error summary's **View** button, reads every page, preserves row
order and duplicates, and summarizes these exact categories:

- `Can’t upload some Google files`: usually a Google-native pointer such as a
  cross-account `.gdoc` file.
- `Can’t upload some files`: commonly a symlink, but inspect the candidates.

The candidate paths are basename matches only, not `LIVE`/`STALE` proof.
`locate` exhaustively inventories symlinks and generated directories, including
archives. Active repositories belong under `~/repos`, but Drive still contains
the dotfiles repository, notes, archives, and some retained git metadata; never
infer that the whole sync root is repository-free.

## Repair the cause

- **Symlink:** Drive cannot represent it. Materialize small static vendor data;
  move required dependencies/build state outside Drive and update the consumer;
  remove truly disposable cache content. Do not replace it with another Drive
  symlink.
- **Generated directory:** keep dependencies, virtual environments, caches, and
  worktrees outside the sync root. Do not silently delete required state.
- **Cross-account `.gdoc`:** inspect its `doc_id` and `email`. A pointer for an
  Epoch-owned document inside the personal-account sync root can be readable in
  the Epoch account yet rejected by personal Drive. Preserve the document ID,
  create an ordinary `.url` through `epoch-doc-link create DOC_ID PATH`, verify
  the shortcut opens the document in the Epoch Chrome profile, and only then
  trash the rejected `.gdoc`. Do not change sharing permissions as a workaround.

Resolve ambiguous rows using the exact file contents, location, account, and
creation workflow. A zero basename-match count only says the old path is absent.
The panel exposes no full path. If a row survives restart with no candidate,
record its exact name and reason as unresolved; do not mutate an unrelated file
to make the count fall.

## Clear and verify

Wait until Drive reports `Sync completed`, then quit and relaunch it normally so
it re-evaluates repaired paths. Rerun `drive-errors list` and verify the full
current panel, not a cached count or the filesystem inventory alone. If rows
remain, repeat diagnosis on those exact rows.

## Hard limits

1. Never disconnect/reconnect the account or remove/re-add the sync root. A past
   reconnection re-uploaded thousands of files into the wrong cloud folder.
2. Never write, delete, or rename DriveFS internal databases under
   `~/Library/Application Support/Google/DriveFS`. Read-only diagnosis is fine.
3. Never create a symlink, dependency tree, cache, virtual environment, build
   output, or worktree under `~/My Drive`.
