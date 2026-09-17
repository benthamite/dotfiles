# Personal update lanes

The scan job records pending Homebrew updates. The delayed job waits seven days
for each release and checks its version and artifact checksum again inside
Homebrew before installation. Formula revisions, bottle rebuilds, and changed
artifact checksums start a new waiting period, even if a displayed version has
not changed. Existing state without artifact observations starts a fresh period
on its next scan; old version-only timestamps do not authorize installation.

Discovery includes self-updating casks and reads their installed application
bundle versions. Homebrew receipts can lag an application's own updater, and
Homebrew's normal bundle comparison can miss Brave's Chromium-prefixed version
format. The scan compares numeric release and build versions, normalizes that
Brave-specific prefix, and omits bundles already at or ahead of the cask release.
It checks installed bundles even when their receipts claim the current release.
An older bundle with a current receipt is reported as needing a verified
reinstall, because Homebrew's upgrade command would otherwise skip it.
Missing or incomparable bundle versions appear with an explicit check error and
are deferred by the delayed lane; an old receipt is not permission to overwrite
an unverified self-updated application. These checks do not shorten the waiting
period or exempt checksum-free artifacts.

The delayed lane also checks implicit dependencies. Each scan records, per
package, the dependencies Homebrew would install or upgrade with it: runtime
formula dependencies not at their latest version anywhere in the expanded
dependency tree (Homebrew skips a dependency that is already current but still
expands the dependencies beneath it), and for casks, formulae with no linked
installation and casks not installed. A candidate whose transitive
recorded dependencies are not all age-qualified is deferred with a message
naming them, so the job exits cleanly while they age. The recorded set errs
toward listing more: Homebrew may accept an older dependency that satisfies a
bottle's minimum version, and the pre-check defers anyway. The in-Homebrew
guard remains the authoritative check for a dependency that has not completed
its waiting period, and it blocks the installation. Holds and excluded casks
apply to these dependencies too. Metadata changes after candidate selection are
checked against the actual installer object, before a formula install or the
removal of a predecessor cask. Automatic Homebrew refresh and unrelated dependent
upgrades are disabled for the guarded operation.
If an approved cask upgrade fails, Homebrew may restore that exact recorded
predecessor. This exception exists only during its rollback; it cannot authorize
an unrelated older cask or a fresh installation of the predecessor.

The guard supports checksum-identified casks and bottled formulae. It refuses
`latest`/checksum-free artifacts, source builds, and a bottle's fallback to a
source build. A formula whose stable source is a pinned VCS checkout (a git tag
or svn revision) has no source checksum; it is identified by the checksum of
the bottle Homebrew would pour on this host, and it stays unchecked when no
bottle applies. Homebrew's separate build process can reload a changed recipe
after the parent installer check. Refusals are explicit errors or
deferral messages; the updater never retries with an unguarded upgrade. The Ruby
adapter uses Homebrew's installer classes and needs compatibility verification
when those interfaces change.

The age check identifies package binaries and source-download metadata, not
Homebrew recipe code. It still trusts Homebrew, tap recipes, cask installer
actions, and formula post-install code. It is not a sandbox for malicious
Homebrew recipes or a guarantee that their code has aged seven days.

`delayed --dry-run` reports `eligible` and `deferred` entries without updating the
saved state. It checks current metadata; it does not install anything. A newly
observed release or dependency completes its waiting period through subsequent
scans, so retrying does not grant immediate approval.

Scans observe declared runtime dependencies without downloading cask archives.
An additional unpacking helper discovered only during a download is refused if
it would need installation and has no age-qualified observation. The guard does
not silently exempt those helpers.

The fast lane for Claude Code and Codex, additional entries in `fast-brew.txt`,
the macOS update lane, and the existing hold/exclusion files retain their roles.
The delayed lane does not prevent applications from using their own updaters.
