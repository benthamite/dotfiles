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
Homebrew reports bundle versions only for casks that install an app, so
self-updating casks installed by a pkg or script (Karabiner-Elements, macFUSE,
Mullvad VPN, Google Drive, gcloud-cli) have their install location listed in
`INSTALLED_VERSION_SOURCES` in `bin/personal-updates`.
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

Because Homebrew does not upgrade dependents here, a candidate is also deferred
while any pending item whose upgrade would include it, or anything upgraded with
it, is itself waiting: upgrading a library alone can remove the version an
installed dependent links to (2026-11-09: simdjson went ahead while node's
rebuild waited on other dependencies, and node no longer started). The two
then go ahead together. Each `delayed` run also runs `brew linkage --test` and
alerts on any installed formula with missing libraries, however it broke.
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

Claude Code uses its native installer and updater, outside Homebrew.
The fast lane for Codex, additional entries in `fast-brew.txt`,
the macOS update lane, and the existing hold/exclusion files retain their roles.
The delayed lane does not prevent applications from using their own updaters.

Codex updates do not wait for running sessions to exit. Normal shell and Emacs
launches use `shell/shims/codex`, which runs a retained immutable copy of the
installed Homebrew package. Existing sessions keep their signed executable and
resources when Homebrew removes its predecessor; new sessions select the new
release. The Chrome helper's `CODEX_CLI_PATH` uses the same launcher. Direct
launches through `/opt/homebrew/bin/codex` bypass this protection. See
`shell/README.org` for runtime storage and retention details.

Both Homebrew lanes hold back a package whose LuLu allow rule is tied to its
versioned path and has no directory rule (`bin/lulu-directory-rules check`),
because upgrading it would raise a LuLu alert on every connection
(`claude/context/lulu-firewall.md`). The delayed lane alerts about held
packages through `launchd-heartbeat`, and both lanes send the `personal-updates`
heartbeat after a successful run (`claude/context/alerting.md`).

## whatsmeow lane

wa-archive (`~/repos/wa-archive`) is built on whatsmeow (`go.mau.fi/whatsmeow`),
which has no releases, only commits on `main`, and WhatsApp rejects clients
that fall too far behind it. The scan and delayed jobs each run a whatsmeow
lane first, so a failure in either lane does not stop the other.

The scan asks the Go module proxy for the newest commit on `main`
(`go list -m -json go.mau.fi/whatsmeow@main` in a throwaway module under
`/private/tmp`) and records each commit newer than `go.mod`'s with its commit
time and when it was first seen, in
`~/.local/state/personal-updates/whatsmeow.json`. It does not touch wa-archive.

The delayed job takes the newest recorded commit newer than `go.mod`'s whose
commit time and first sighting are both at least seven days old; taking the
newest aged commit, not the newest commit, keeps a busy upstream from
restarting the wait forever. It runs `go get` and `go mod tidy` for that commit
on a temporary copy of wa-archive's `HEAD`, and goes ahead only if every module
version the update adds to `go.mod` or `go.sum` (a version whose code joins a
`go.sum` that held only its `go.mod` counts), and any changed `go` or
`toolchain` line, was published at least seven days ago, by the proxy's time
(`go list -m -json MODULE@VERSION`; Go versions by their
`golang.org/toolchain` module). Otherwise it defers, naming them. All Go
commands run with `GOTOOLCHAIN=local`: a commit that needs a newer Go defers
until Homebrew's delayed lane brings one, rather than downloading an unaged
toolchain.

It also defers while wa-archive is not on `master` or has uncommitted or
untracked files, because `install.sh` would deploy them. Deferrals are logged
and printed, not alerted, like the Homebrew lane's. When the update qualifies,
the job checks that a signed commit can be made, writes the resolved `go.mod`
and `go.sum` into wa-archive, runs `go test ./...` and `./install.sh` (which
rebuilds and restarts the daemon, and itself restores the previous binary
unless the new one connects), and commits both files as "Update whatsmeow to
VERSION". If any step fails it restores `go.mod` and `go.sum` and alerts
through `launchd-heartbeat` (monitor `personal-updates`). A failure after
`install.sh` succeeded (the commit) leaves the new binary running on the old
`go.mod`; the alert says which step failed.

When WhatsApp reports the client outdated, wa-archive alerts with the command
to run, which takes `main` at once:

```sh
bin/personal-updates whatsmeow --waive-age
```

The waiver covers whatsmeow alone: modules it pulls in must still have aged,
and the command names any that have not. `bin/personal-updates whatsmeow`
without the flag runs only the nightly whatsmeow lane. Either command exits
nonzero when it defers or fails.

`delayed --dry-run` and `whatsmeow --dry-run` print the lane's report
(`current`, `candidate`, `changes`, `deferred` or `waiting`) without writing
state or logs, touching wa-archive or alerting. They still resolve the update
on the temporary copy, which downloads modules into the Go module cache.
