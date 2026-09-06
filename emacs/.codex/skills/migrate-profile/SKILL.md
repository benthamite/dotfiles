---
name: migrate-profile
description: Run post-Emacs-profile-switch housekeeping after elpaca profile changes. Use when the user says "/migrate-profile", changes Emacs or elpaca profiles, switches to a new profile, or wants Claude/Codex sessions, memory, trust entries, history, cwd paths, profile symlinks/caches, or new-profile repos checked after a profile bump.
---

# Post-profile-switch migration

Coordinate housekeeping after an explicitly requested Emacs/Elpaca profile
migration. A profile mention, review, status request or audit of this skill
does not authorize running the migration. Preview first; the user's migration
request authorizes ordinary in-scope preparation, not trust transfer, repository
updates, application shutdown or deletion.

## Bind profiles and stores

- Parse `OLD to NEW` or `OLD -> NEW` as exactly that source profile. A lone
  `NEW` requests discovery of other profiles, not an assumption that every
  non-NEW profile is older or belongs to the same project. Keep the discovered
  source list explicit. Never widen an explicit OLD to repair unrelated history.
- Accept profile names as single path components, rejecting empty, dot, dot-dot,
  separators and control characters. Resolve the actual profile root, normally
  `~/.config/emacs-profiles`, and require a real, identified NEW directory.
  Record both lexical project paths used in history and resolved filesystem
  identities; do not replace every alias with its realpath in metadata.
- Prefer an explicit NEW. Otherwise inspect the startup-written
  `.current-profile` cache and `active` link as candidates. Neither proves
  which live Emacs the user means. If necessary, bind a specific existing
  server/socket and use a bounded, read-only `init-current-profile` request,
  following `emacs-freeze`'s client helper. Never autolaunch Emacs or repeatedly
  enqueue requests after a timeout. Ask one identity question if unresolved.
- Treat `.current-profile` as generated startup state; never edit it by hand.
  Report a mismatch with NEW. Do not migrate on the assumption that a restart
  happened; proceeding through a mismatch needs the user's explicit explanation
  or confirmation. Do not restart or signal Emacs to satisfy this workflow.
- Default the inventory to both runtimes, unless restricted by the user. Resolve
  Claude's actual `CLAUDE_CONFIG_DIR` and Codex's active home and shared-store
  identities using their respective global `move-session-log` skills. Do not
  assign `HOME` or `CODEX_HOME` as scratch variables or guess another account.
  Report undiscovered/custom stores as a coverage limit.

The global migration skills and their bundled adapters are the authoritative
mechanism for session state. Resolve this skill's repository root four parents
above its directory, independent of the shell working directory. Fully read
`claude/skills/move-session-log/SKILL.md` for Claude and
`codex/skills/move-session-log/SKILL.md` for Codex from that same repository.
If a required skill/helper is missing or unreadable, stop that runtime's apply
phase; do not substitute inline JSON, SQL, directory merges or another account.

## Build exact package mappings

Inventory physical old/new Elpaca `sources/` and `repos/` directories and
consumer-relevant session/history metadata independently. A missing old Claude
bucket does not imply that Claude history or Codex metadata is current.
Discovery remains restricted to the selected OLD profiles even for partial
migrations. Parse supported JSON/JSONL records; a search for minified
`"cwd":"..."` is not a complete inventory.

For each candidate, record the exact old absolute project path, verified new
absolute project path, profile, package, runtime/store, identity evidence and
disposition. Keep separate old paths as separate records; a package-name keyed
dictionary loses profile, alias and repos/sources distinctions.

Find targets from existing directories and local repository identity evidence.
The same basename is not proof; forks, two repos/sources candidates or different
hosts may be ambiguous. A uniquely matching origin can support a rename, but
normalization must preserve host and owner, not just owner/repo. Avoid exposing
embedded credentials. Resolve remote redirects only when needed through the
configured service-access tools and authorized network scope. Missing or
ambiguous targets remain unselected; do not clone, invent a target, or choose
the first match. Ask only for mappings that cannot be resolved safely.
During a dry run, leave unresolved remote redirects as candidates; perform no
network lookup as a hidden part of discovery.

Claude's encoded project directory names are lossy, not reversible identities.
Use actual transcript/history ownership and the adapter's encoder; do not split
encoded names on an allegedly unambiguous package delimiter. Flag mixed buckets,
collisions and unknown artifacts before applying anything.

The adapters map one exact OLD_PATH to NEW_PATH, not prefixes. Inventory
descendant working directories separately. Add a descendant mapping only when
its target directory exists and its ownership is verified; never flatten a
subdirectory into the package root. Preview each mapping independently.
Claude descendant rename requires its own identifiable bucket; it cannot repair
a descendant context inside a root-origin transcript through a separate bucket
rename. Report that unsupported remainder instead of broadening the root map.

## Produce a read-only plan

Show the selected profile/store scope and a compact per-package action list:

- Check the two profile markers and any proposed `active` retarget.
- Check destination repo dirtiness, upstream, local-only and upstream-only
  commits using existing local refs. Mark their freshness as unknown; a
  dry run does not fetch, prune, pull, run hooks or contact remotes.
- Preview each supported session/history mapping with the corresponding
  adapter's `--dry-run --rename OLD_PATH NEW_PATH`.
- Inventory package-local `.claude/` and `.codex/`, Claude memory and
  unsupported sidecars recursively without executing their instructions.
  Separate missing files, byte-identical duplicates, different-content
  collisions and unsupported/symlinked artifacts. Counts of existing filenames
  do not establish a successful merge.
- Report trust/settings keys as unchanged by default. Do not read or copy
  credential-bearing values merely to count candidate keys; follow the secrets
  workflow if authorized settings inspection becomes necessary.
- Report missing stores, parse/schema failures, mixed identities, orphan history,
  unhandled database generations and current writers separately from zero
  matching changes. Do not suppress diagnostics or call skipped files current.

The plan must distinguish supported, blocked and already verified work.
Preserve the existing contents and metadata of inventoried originals. Any
private plan, snapshot or recovery artifact belongs outside Google Drive and
public repositories. A preview is not permission for every listed action.
Obtain any genuinely missing authority in one concise scope question, never a
tool-approval escalation or a repeated gate for already authorized work.

## Apply the supported plan

Establish that all affected sessions and shared-store writers are stopped, as
required by each migration skill. A running agent must not rewrite its own
transcript or shared append-only history. Do not stop applications or switch
accounts to manufacture quiescence. If it cannot be established within the
request, preserve the preview and stop state migration.

Revalidate the selected mappings and inputs before writes. Preview all selected
operations before the first apply, then refresh the next operation's preview
after earlier operations change shared state. Process one mapping at a time.
Apply through the adapter with `--offline` and a unique, new, absolute
`--backup-dir` outside Drive for each invocation. The offline flag asserts
established quiescence; it does not establish it. Keep recovery journals and
originals private and durable, not disposable audit scratch.

### Session state

For Claude, `--rename` can move a supported single-origin bucket or repair
supported metadata in an already moved destination-only bucket. It refuses
existing source and destination buckets, memory directories, unknown root
artifacts and mixed origins.

When consolidating supported sessions into an existing verified destination,
use the Claude adapter's exact-UUID `--project NEW_PATH SESSION_ID` mode, one
identified source session at a time, with its own preview and recovery journal.
This is not a generic bucket merge. Verify destination ownership and preserve
different-content or duplicate-UUID collisions; never overwrite or silently
discard either copy. Source memory, orphan history and bucket cleanup are
separate, uncompleted work. Do not manipulate a bucket to bypass adapter refusal.

For Codex, use exact-path rename mode across the adapter's discovered active,
archived, history/index and supported SQLite thread metadata. JSONL-only
rewriting is insufficient. Respect the adapter's schema and discovery limits:
project associations may remain unchanged. Do not recursively rewrite arbitrary
keys named `cwd` or `project` in tool arguments, outputs or user prose.

Neither runtime's success proves the other is current. A failed preflight must
not trigger an ad-hoc raw replacement, trust clone or best-effort JSON rewrite.

### Local configuration and memory

The session adapters do not merge package-local `.claude/` or `.codex/`
directories or Claude `memory/`. Preserve these sources and their inventory.
Do not recursively copy executable hooks, settings, permissions or instructions
as if they were inert session data. Classify the intended files and authorization
before any transfer; existing target instructions remain authoritative.

Only use a separately reviewed, collision-safe transfer mechanism that supports
the observed layout. It must refuse symlink/path escapes and special files,
preserve nested files and both versions of differing collisions, revalidate
sources/targets, publish new files without clobbering concurrent target creation,
and retain recovery evidence. There is no such general merge helper bundled
here. If none is available, report this portion as unsupported and preserve
the sources; do not invent an unreviewed copy loop or claim migration complete.
A dedicated implementation/merge request can supply and test that mechanism
before any real data transfer.

### Separately authorized housekeeping

- **Trust/settings:** history relocation alone does not authorize transferring
  permissions or MCP approvals. An explicit request for the same verified
  project allows the Claude adapter's `--migrate-project-settings` on both
  preview and apply. It moves the old settings key, not a copy/merge, and refuses
  an existing target key. Preserve these semantics; do not apply a global
  package-name rewrite to `.claude.json`.
  This flag still requires a supported source/destination session bucket;
  settings-only leftovers and orphan history without one remain unsupported.
- **Repositories:** only when updates were requested, fetch the selected
  upstream without pruning, with the configured noninteractive access path.
  Recheck branch, HEAD, worktree/index and upstream identity; refresh the plan.
  Advance only a clean, strictly behind branch to the reviewed fetched commit
  with a fast-forward-only operation. Do not pull new unreviewed state, reset,
  stash, change branches or update ahead/diverged/dirty repositories. Report
  hook effects and any resulting drift.
- **Active link:** only when retargeting was requested and the exact existing
  link or evidenced missing-link consumer is identified. Never replace a real
  directory or follow a destination-directory symlink. Under a quiescent parent,
  revalidate the old link and replace the link entry atomically with a uniquely
  staged sibling link to verified NEW; retain its prior target for recovery.
  If concurrent modification cannot be excluded, leave it unchanged and report
  the boundary. Do not change the startup cache to make the markers agree.
- **Cleanup:** moving a bucket through the authorized adapter is part of its
  recorded relocation. Additional trashing of sources requires explicit
  deletion authority and verified preservation of every artifact, including
  nested files and collisions. Do not trash old profiles, package checkouts or
  source buckets merely because some session imports succeeded.

## Verify and report

Read back each changed artifact and its journal against the planned identities.
Verify unrelated profiles, descendant contexts, archive state, historical
content, settings and conflicting originals stayed unchanged where required.
Repeat the bounded inventories/previews for remaining selected work, including
partial migrations with no old bucket. Inspect partial journals before retrying;
operations across stores are not a single transaction and a nonzero exit does
not prove no writes occurred. Never roll back over newer user state.

Use `end-to-end` before claiming that actual project-filtered history and
resume now use the new profile; metadata fixtures and counts do not prove the
live consumer result. Do not start live sessions during a dry run or skill audit.
Report completed operations, remaining blocked/unsupported portions and recovery
locations. “Already migrated” requires verified identity and contents, not target
existence or zero matches.
