# Drive-Compatible Local Links Design

## Goal

Reduce Google Drive for desktop's native error count from 56 to zero without
sharing Epoch documents with the personal account, duplicating canonical
content, or weakening the existing meeting, agent-instruction, Uqbar, and
spell-checking workflows.

The design replaces only file representations that Drive cannot accept. Each
subsystem retains one canonical source, and every migration is verified through
the real user-visible workflow before the next subsystem changes.

## Observed problem

The generated-directory cleanup reduced the native Drive error count from 190
to 56. A clean restart reproduced exactly two remaining classes:

- 36 `INVALID_GOOGLE_DOCUMENT` errors from ignored `.gdoc` pointer files under
  `Epoch/meetings/`. Cross-account pointers can fail when the account owning the local
  mirror lacks access to the document. Verify access through the owning account
  without copying permission records into public documentation.
- 20 `UNSUPPORTED` errors from Git-tracked file symlinks: 16 Enchant dictionary
  links in `dotfiles/enchant`, two `CLAUDE.md -> AGENTS.md` links in
  `rubric-visualizer` and `uqbar`, and the `uqbar` compatibility names
  `build.py -> build` and `launch.py -> launch`.

Drive's internal log also records `PARTIAL_RESULTS` for externalized directory
symlinks, but these entries do not appear in the native error count after Drive
settles. This project targets the 56 user-visible errors. It does not claim that
Drive's log will contain no error-severity lines.

## Policy decisions

The Drive error list should be meaningful rather than permanently noisy, but a
zero count is not sufficient reason to introduce duplicate writable state.
Accordingly:

- cross-account meeting links remain local convenience artifacts and never
  change document permissions;
- `AGENTS.md`, Uqbar's extensionless commands, and each language repository's
  dictionary remain canonical;
- file symlinks may continue to exist outside the Drive mirror;
- no component silently falls back to a personal browser profile, default
  browser, copied implementation, or alternate dictionary;
- migration proceeds one error category at a time and stops when the native
  count does not fall by the exact expected amount; and
- local commits remain separate from publication. No repository is pushed by
  this work without a later explicit request.

## Architecture

### Epoch document links

The 36 Epoch-account `.gdoc` pointers become normal `.url` files. A shortcut
for document `<DOC_ID>` contains exactly:

```ini
[InternetShortcut]
URL=epoch-doc:///document/<DOC_ID>
```

The path form is deliberate. Putting the case-sensitive document ID in a URL
host risks case normalization; a path segment preserves it.

Dotfiles owns a small `epoch-doc-link` command with two operations:

- `create` validates a document ID against `[A-Za-z0-9_-]+` and atomically
  writes the exact `.url` representation; and
- `open` accepts only `epoch-doc:///document/<DOC_ID>`, validates the ID again,
  and launches an argument vector equivalent to:

  ```text
  chrome-profile-open epoch https://docs.google.com/document/d/<DOC_ID>/edit
  ```

Neither operation invokes a shell. A missing or invalid `epoch` profile is a
hard error; there is no default-browser or personal-profile fallback.

LaunchServices needs an application to own the `epoch-doc:` scheme. Tracked
source lives under `dotfiles/macos/epoch-doc-handler/`, with a small Swift
application delegate and its `Info.plist`. The application receives opened
URLs, passes them to `~/bin/epoch-doc-link open` as an argument array, shows a
visible error if validation or profile routing fails, and terminates after the
request. `dotfiles/bin/install-epoch-doc-handler` builds, ad-hoc signs,
installs, and registers the application under `~/Applications/`, outside the
Drive mirror. The installed bundle is generated state; tracked source and tests
remain canonical in dotfiles.

The `meeting-debrief` Step 8 instructions in Epoch's paired Claude and Codex
skills call `epoch-doc-link create` instead of writing JSON. Their verification
and commit guidance refer to the `.url` artifact and still treat it as ignored
local convenience state. Epoch uses a scoped `/meetings/**/*.url` ignore rule;
it does not globally ignore unrelated Internet shortcuts.

Only the 36 pointers whose JSON identifies the owning work account migrate. The eight valid
personal-account `.gdoc` files remain unchanged unless they later produce a
user-visible error.

Direct HTTPS shortcuts with `authuser=<email>` are rejected because account
selection depends on browser session state and has not been reliable enough to
fail closed. Numeric `/u/N/` routing is also session-order-dependent. A
`.command` file would select the profile exactly but would open Terminal and
depend on executable metadata surviving cloud round trips.

### Claude instruction bridges

In `rubric-visualizer` and `uqbar`, `CLAUDE.md` becomes a regular mode-`100644`
file containing exactly:

```markdown
@AGENTS.md
```

Claude Code supports this import form, so `AGENTS.md` remains the only content
source. This is semantic synchronization rather than byte-for-byte mirroring.

The dotfiles parity tools change before either repository changes:

- `ai-config-sync` recognizes the exact import-only bridge as synchronized,
  suppresses false edit reminders, and still reports ordinary content drift;
- `mirror-claude-agents` recognizes the bridge and performs a no-op instead of
  copying the import line over `AGENTS.md`; and
- the paired `update-log` skills recognize the bridge before their ordinary
  byte-for-byte mirror procedure, update canonical session state in
  `AGENTS.md`, and leave the import-only `CLAUDE.md` unchanged; and
- focused tests cover bridge equivalence, normal drift, a missing target,
  reminder behavior, helper safety, and the `update-log` bridge branch.

`agents/README.org` documents import bridges alongside ordinary mirrored pairs.
Historical repository design documents that describe the old symlink remain
unchanged because they accurately record the earlier design.

### Uqbar compatibility commands

`uqbar/build.py` and `uqbar/launch.py` become regular executable Bash forwarding
scripts. Despite their suffixes, the current symlink targets are already Bash
programs and callers execute the compatibility names directly.

Each wrapper enables strict Bash behavior, resolves its own directory without
depending on the caller's working directory, and uses `exec` with quoted
arguments to invoke the canonical extensionless command. For example,
`build.py` has the behavior:

```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$SCRIPT_DIR/build" "$@"
```

`launch.py` differs only in its target. This preserves argument boundaries,
stdout and stderr, exit status, and signal behavior without copying command
implementation.

At investigation time the local Uqbar checkout was 94 commits behind its
already-fetched `origin/main`. Implementation must re-measure this state,
preserve unrelated untracked files, and fast-forward safely before editing. It must then reread the newer `build`,
`launch`, and `AGENTS.md` rather than applying this design to stale content.

Uqbar's existing debugpy configuration for `launch.py` is a separate pre-existing
problem: it has already pointed at Bash through a symlink since April. Repairing
that debugger configuration is not part of this project.

### Enchant runtime configuration

The current write path contains two symlink layers inside or into Drive:

```text
Jinx -> Enchant -> ~/.config/enchant/<lang>.dic
                    -> dotfiles/enchant/<lang>.dic
                    -> repos/<lang>/dict/<lang>.dic
```

Jinx's Personal action calls Enchant's dictionary-add operation, so the
dictionary files are writable state rather than disposable copies. The
canonical files remain:

- `repos/{ar,de,en,es,fr,it,ru,tr}/dict/<lang>.{dic,exc}`;
- `dotfiles/enchant/pl.{dic,exc}` for Polish; and
- `dotfiles/enchant/enchant.ordering` for provider selection.

The new runtime path is:

```text
Jinx -> Enchant -> ~/.config/enchant/<lang>.dic
                    -> canonical repository file
```

`~/.config/enchant` becomes a real local directory outside Drive. It contains
runtime symlinks directly to the canonical files. The 16 forwarding symlinks in
`dotfiles/enchant` are removed from Git; the regular Polish files and ordering
file remain there.

`dotfiles/bin/install-enchant-config` owns the runtime layout. It derives the
dotfiles path, supports explicit config/dotfiles/repos roots for disposable
tests, validates every canonical target before mutation, creates or repairs
only the expected links, and stops on unknown files or type conflicts. The
current `~/.config/enchant -> dotfiles/enchant` link is unmanaged manual state;
no tracked bootstrap mechanism currently recreates it. The new installer and
README documentation therefore become the first canonical bootstrap and
healing path.

Regular mirrored dictionary copies and bidirectional synchronization are
rejected because Enchant writes these files and conflict handling would create
a second source of truth. Moving canonical dictionaries back into dotfiles
would reverse the established repository boundary and create the same drift in
the opposite direction.

## Migration sequence

Each phase records the native Drive count and targeted post-restart log entries,
applies one bounded change, restarts Drive, waits for it to settle, and requires
the exact expected reduction before continuing.

### 1. Install and pilot the document-link handler

1. Build, install, and register the handler outside Drive.
2. Record one pilot document's permissions through the Epoch API.
3. Create its sibling `.url` while retaining the `.gdoc`.
4. Open the `.url` through Finder and prove that Chrome uses the configured
   Epoch profile, the document loads without an account chooser, and malformed
   URLs or an invalid alias fail without another-profile fallback.
5. Re-query permissions and prove the personal account was not added.
6. Move only the pilot `.gdoc` to Trash.
7. Restart Drive and require `56 -> 55`, with no error for the `.url`.
8. Migrate the other 35 only after the pilot passes, then require `55 -> 20`.
9. Update both `meeting-debrief` skill copies and their scoped ignore guidance.

The generated `.url` files are expected to sync into the corresponding personal
My Drive meeting paths. Resolve a sample through the Drive API and verify the
expected parent chain; a pointer outside My Drive or under a machine-backup tree
is a failure.

### 2. Teach parity tools about import bridges

1. Add the failing focused tests in dotfiles.
2. Implement bridge recognition in `ai-config-sync` and
   `mirror-claude-agents`.
3. Update documentation and run the focused suite plus full
   `bin/ai-config-sync audit`.
4. Replace the `rubric-visualizer` symlink and live-verify Claude's imported
   project instructions; require `20 -> 19`.
5. After Uqbar is current, replace its instruction symlink, repeat live
   verification, and require `19 -> 18`.

The tool change precedes repository changes so existing automation can never
overwrite a canonical `AGENTS.md` with the literal bridge.

### 3. Install Uqbar wrappers

1. Protect unrelated untracked files and fast-forward the checkout safely.
2. Revalidate that the compatibility names still point at Bash commands and
   that repository callers still execute them directly.
3. Replace one symlink at a time with a mode-`100755` wrapper.
4. Run the disposable forwarding harness and safe real-command comparisons.
5. Restart Drive and require `18 -> 16`.

### 4. Move Enchant runtime links outside Drive

1. Build the complete proposed runtime directory in a temporary sibling of
   `~/.config/enchant`.
2. Validate every link and exercise dictionary reads and writes against
   disposable dictionary targets through that directory.
3. Move the old directory symlink to Trash and atomically install the validated
   real directory.
4. Confirm the active Jinx/Enchant configuration reads an existing personal
   word through the real runtime path. Do not add a test word to canonical user
   data merely for verification.
5. Remove the 16 tracked forwarding symlinks from dotfiles and add the installer,
   tests, and README documentation.
6. Restart Drive and require `16 -> 0`.

## Failure handling and rollback

- The URL handler rejects unknown schemes, malformed paths, invalid IDs, and a
  missing profile with a visible error. It never opens another profile.
- The meeting migration keeps each `.gdoc` until its replacement opens
  correctly. A failed pilot removes its new `.url` and restores the `.gdoc`
  from Trash.
- Parity-tool tests must pass before an import bridge is introduced. A failed
  bridge conversion is restored from the preceding repository commit.
- Uqbar is never reset, stashed, rebased, or updated over untracked user files.
  Unexpected divergence or changed upstream semantics stops that phase.
- The Enchant installer validates the complete target layout before touching
  the live path. Unknown content and missing targets fail closed. A failed live
  swap restores the old directory symlink from Trash.
- Category-specific commits make every rollback local to one subsystem.
- No cloud permission, GitHub publication, or Drive-account re-registration is
  part of rollback or recovery.

## Implementation surfaces and commit boundaries

Expected logical commits are:

1. **dotfiles handler infrastructure:** `bin/epoch-doc-link`,
   `bin/install-epoch-doc-handler`, `macos/epoch-doc-handler/`, tests, and
   directly required documentation;
2. **Epoch meeting workflow:** the paired `meeting-debrief` skills and scoped
   ignore rule;
3. **dotfiles parity tooling:** `ai-config-sync`, `mirror-claude-agents`, the
   paired `update-log` skills, tests, `agents/README.org`, and the directly
   required Claude/Codex README updates;
4. **rubric-visualizer bridge:** regular import-only `CLAUDE.md`;
5. **Uqbar bridge:** regular import-only `CLAUDE.md` after safe synchronization;
6. **Uqbar wrappers:** regular executable `build.py` and `launch.py` plus
   focused verification;
7. **dotfiles Enchant runtime:** installer, tests, README documentation, and
   removal of the 16 tracked forwarding links.

The ignored local shortcut migration occurs after the handler and workflow
commits, but remains live state outside Git. Commit inspection must preserve
all unrelated dirty and untracked files in every repository.

## Verification

### Automated checks

- `epoch-doc-link`: valid and malformed URL parsing, case-preserving document
  IDs, atomic exact-format creation, shell-free argument passing, missing-profile
  failure, and no fallback.
- handler installation: bundle structure, registered scheme, generated artifact
  location outside Drive, idempotent reinstall, and visible propagation of CLI
  errors.
- parity tooling: bridge equivalence, normal drift, missing target, reminder
  behavior, safe helper no-op, `update-log` bridge handling, focused tests, and
  full `ai-config-sync audit`.
- Uqbar wrappers: `bash -n`, mode `100755`, arguments containing spaces,
  arbitrary caller working directories, stdout and stderr, nonzero status, and
  `exec`-preserved behavior through disposable targets.
- Enchant installer: fresh setup, idempotent rerun, repair of a known stale
  link, refusal to overwrite unknown content, missing-target failure, and
  disposable dictionary read/write behavior.
- paired Claude/Codex meeting skills remain semantically equivalent except for
  tool-specific frontmatter and pass their applicable skill/configuration audit.

### Live checks

- Finder opens an `epoch-doc:` shortcut in the configured Epoch Chrome profile
  and the expected document loads.
- Normalized Epoch document permission records are unchanged before and after
  the pilot; the personal account is absent.
- Claude in both affected repositories loads a distinctive instruction from
  canonical `AGENTS.md` through the regular bridge.
- Safe real Uqbar wrapper invocations match their canonical commands, including
  from outside the repository.
- Active Jinx/Enchant reads canonical dictionaries through the new runtime
  directory.
- The native Drive UI follows `56 -> 20 -> 18 -> 16 -> 0` and remains at zero
  after a final fresh restart and settling period.
- Targeted logs contain no new `INVALID_GOOGLE_DOCUMENT` or `UNSUPPORTED`
  entries for migrated paths. Existing directory-symlink `PARTIAL_RESULTS` are
  reported separately and do not invalidate the UI criterion unless they
  become user-visible.
- The Drive API root still matches the registered personal My Drive root,
  migrated `.url` samples have the intended parent chain, and no item lands in
  a computer-backup or unexpected tree.

### Repository checks

- Each logical commit contains only its intended paths.
- Relevant Git object modes are `100644` for instruction bridges and `100755`
  for command wrappers.
- Dotfiles documentation and tests cover the new handler, bridge convention,
  and Enchant installer.
- Unrelated dirty and untracked state is preserved.
- No repository is pushed.

## Rejected alternatives

### Tolerate or dismiss the 56 errors

This is the lowest-change option but leaves Drive's warning surface permanently
noisy and makes future real failures harder to notice. Dismissing entries would
also treat the symptom rather than the unsupported local representations.

### Delete all `.gdoc` pointers

The pointers are ignored convenience files, so deletion would be recoverable,
but it would remove useful Finder access and leave `meeting-debrief` generating
the same problem again.

### Direct Google account-routing URLs

Email-valued `authuser` parameters and numeric account indices depend on browser
session state. They do not provide the required fail-closed guarantee that the
Epoch profile, rather than the personal profile, opens the document.

### Replace symlinks with tracked content copies

This reaches zero quickly but creates duplicate writable dictionaries,
duplicated agent instructions, or copied command implementations. Drift becomes
silent and the Drive counter improves at the expense of source integrity.

### Bidirectionally synchronize Enchant copies

This introduces conflict detection and resolution for files that Enchant writes
interactively. Keeping runtime symlinks outside Drive preserves immediate
single-source writes with less machinery.

### Move entire repositories outside Drive

This is far broader than the 20 unsupported file links and conflicts with the
established filesystem organization, including dotfiles as the Drive-side
source of truth.

## Non-goals and adjacent findings

- Eliminating every error-severity line in Drive's internal log.
- Changing the eight valid personal-account `.gdoc` files.
- Sharing Epoch documents with the personal account or creating personal Drive
  shortcuts to the underlying documents.
- Repairing Uqbar's pre-existing debugpy configuration.
- Repairing the separate `repos/add-to-repo/dist` tracked-content/symlink
  mismatch.
- Changing directory externalization under `~/.drive-nosync/`.
- Repairing repository integrity for the `ar`, `en`, and `tr` language
  repositories. At investigation time they had no commits/default branch, and
  the untracked English dictionary contained substantial user data. That risk
  warrants a separate repository-integrity task but does not block this runtime
  migration because the canonical files remain regular Drive-synced files.

## Acceptance criteria

- The native Google Drive error count reaches zero and remains zero after a
  clean restart and settling period.
- All 36 Epoch meeting pointers open the intended document through the Epoch
  Chrome profile, and no document permission changes.
- Malformed document links and missing profile configuration fail visibly with
  no fallback.
- Future `meeting-debrief` runs create ignored `.url` pointers through the
  validated helper and no longer create Epoch-account `.gdoc` pointers.
- Both regular `CLAUDE.md` bridges import canonical `AGENTS.md`; parity tooling
  treats only the exact import-only form as synchronized and cannot overwrite
  the target. The `update-log` workflow edits canonical `AGENTS.md` without
  replacing or hand-editing the bridge.
- Uqbar compatibility names remain executable from any working directory and
  preserve canonical command behavior without copied implementation.
- Jinx/Enchant reads and writes each language's canonical dictionary through
  runtime symlinks located outside Drive.
- Re-running the Enchant and URL-handler installers is idempotent, while unknown
  conflicts and missing dependencies fail closed.
- No targeted path produces a new `INVALID_GOOGLE_DOCUMENT` or `UNSUPPORTED`
  entry after the final restart.
- The registered Drive root and cloud routing remain correct, and expected
  `.url` uploads land only under their corresponding My Drive meeting folders.
- Every repository commit is single-purpose, preserves unrelated state, and
  remains local.
