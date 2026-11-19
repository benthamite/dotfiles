# Tap-lag alert and one-run Elisp evidence

Two changes Pablo approved on 19 Nov 2026. They are independent and ship as
separate commits.

## 1. Alert when a third-party Homebrew tap lags its upstream

### Problem

`personal-updates` learns of updates only from `brew outdated`, which compares
an install with what its tap advertises. A tap that stops tracking upstream
looks current forever. wuzapi (tap `asternic/wuzapi`) sat at 1.0.5 while
upstream released v1.1.1 (13 Nov 2026); WhatsApp then rejected it as outdated.

Current state, checked against GitHub on 19 Nov 2026:

| Installed from tap | Tap version | Upstream (GitHub latest release) | Lags |
|---|---|---|---|
| `asternic/wuzapi/wuzapi` | 1.0.5 | asternic/wuzapi v1.1.1 (2026-11-13) | yes |
| two other third-party formulae | (omitted) | (omitted) | yes |
| `guumaster/tap/hostctl` | 1.1.4 | guumaster/hostctl v1.1.4 | no |
| `yqrashawn/goku/goku` | 0.8.0 | yqrashawn/GokuRakuJoudo v0.8.0 | no |
| `d12frosted/emacs-plus/emacs-plus-app` (cask) | 31.1-406 | the tap repository publishes its own builds | n/a |
| `benthamite/local` | nothing installed (one cask, `fireflies`, defined but not installed) | private local tap, no remote | n/a |

All taps' clones were fetched today (FETCH_HEAD 19 Nov 10:07), so the lag is
real tap staleness, not a stale local clone.

### Why not `brew livecheck`

- It is a developer command: its first run installs Homebrew's developer gem
  bundle from rubygems.org into `Library/Homebrew/vendor`. Here that install
  timed out (`Failed to open TCP connection to rubygems.org:443`), so livecheck
  produced nothing for any formula. Installing that bundle would also put
  unaged third-party Ruby gems into Homebrew, which the delayed lane's
  seven-day rule exists to avoid.
- For these five formulae its default strategy would be GitHub releases or git
  tags anyway, since every one has a GitHub `stable` URL or homepage.
  wuzapi's stable URL is a branch tarball (`refs/heads/main`); how livecheck
  handles that was not verified, because livecheck could not run here.

So the scan does the GitHub check directly. It cannot check a formula whose
`stable` URL and homepage are not on github.com, or whose repository has no
releases and no version-like tags; those are handled under "Uncheckable" below.
Today none of the installed third-party formulae is in that set.

### Design

New subcommand `personal-updates tap-lag [--dry-run]`, also run at the end of
`scan` (the 02:00 daily job). Detection only: it reads, records and alerts; it
never changes what `delayed` or `fast-brew` installs.

1. List installed formulae and casks with
   `brew info --json=v2 --installed` (no auto-update; `scan` has just run
   `brew update`). Keep those whose `tap` is neither `homebrew/core` nor
   `homebrew/cask`, minus entries in a new policy file
   `macos/personal-updates/tap-lag-exempt.txt` (one `tap` or `tap/name` per
   line, each with a `#` reason). Initial entries:
   - `d12frosted/emacs-plus`: the tap repository publishes the builds itself
     (`cask-stable-N` releases), so there is no separate upstream to lag, and
     `brew outdated` already reports its updates.
   - `benthamite/local`: Pablo's private local tap with no remote; its cask
     points at a vendor bucket, not a repository.
2. Upstream repository: the first of `urls.stable.url`, `urls.head.url`,
   `homepage` (casks: `url`, `homepage`) that is a `github.com/OWNER/REPO`
   URL.
3. Upstream version: `GET https://api.github.com/repos/OWNER/REPO/releases/latest`
   (excludes drafts and prereleases). If the repository has no release (404),
   the highest version-like tag from `/tags`. Requests go through
   `/usr/bin/curl`, unauthenticated (no credential; about six requests a day
   against a 60/hour limit), 20 s timeout.
4. Compare with the tap's version (`versions.stable`, cask `version`) after
   stripping a leading `v`, using the script's existing `numeric_version`.
   Upstream strictly greater means the tap lags.
5. One alert per scan through `launchd-heartbeat alert personal-updates`
   listing every lagging package: name, tap version, upstream version and
   its date, the upstream URL, and the action ("the tap has not caught up;
   upgrade by hand or switch source"). The Worker's noise rules already send a
   repeated text once and log it for 7 days. No lag, no alert.
6. Uncheckable package (no GitHub URL, no release or version-like tag,
   incomparable versions): listed in the same alert under "cannot check",
   with the reason and "add it to tap-lag-exempt.txt with a reason, or fix
   the source". Nothing third-party goes unchecked silently.
7. Fetch failures (network, HTTP 5xx, rate limit): logged. Per the
   "nothing to do, no alert" rule, a transient failure is not alerted; a
   package whose check has failed on 3 consecutive scans is alerted under
   "cannot check". The streak lives in `state.json` under `tap_lag`, together
   with the last result per package; a success resets it.
8. Alert delivery failure makes `scan` exit 1 with the error logged, as other
   alert sends in the script do.
9. `--dry-run` prints the findings as JSON and sends nothing, writes nothing.

### Docs

- `macos/personal-updates/README.md`: a "Tap lag" section (what is checked,
  the exempt file, uncheckable handling, why not livecheck).
- `claude/context/alerting.md`: the `personal-updates` instance line gains
  "and alerts when a third-party tap lags its GitHub upstream".

### Tests (`tests/test_personal_updates.py`)

Pure functions with injected `brew info` JSON and fake HTTP responses:
third-party filtering and exemptions; repository extraction from each URL
field; release, tag fallback, prerelease exclusion; lag/no-lag/incomparable;
failure streak reaching 3 and resetting; alert text; dry run writes nothing.

### Verification

- `personal-updates tap-lag --dry-run` on this Mac must report exactly wuzapi
  and the two other formulae as lagging, hostctl and goku as current, and
  nothing uncheckable.
- One real run of the scan job under launchd (`launchctl kickstart` of
  `com.stafforini.personal-updates.scan`), so curl's network access is
  checked in the job's own context (LuLu), and the alert arrives on Telegram
  with those three packages. This sends Pablo one real alert.

## 2. One project-check run covers every file it names

### Problem

For a `file:` label (non-package Elisp: dotfiles `config.org` and non-extras
`.el`, files of repositories that are not a single package, deleted files),
the pre-commit hook requires one `elisp-check-evidence file:PATH -- CHECK` run
per path. The Protean worktree commit of 19 Nov needed three runs of the same
`emacs/check` on the same contents. Each run records the full Elisp snapshot,
yet counts only for its one label.

The agent.el commit Pablo cites needed only `batch-test.sh agent`: agent.el
is a package repository, so every changed file maps to the label `agent`.
The session also ran `elisp-check-evidence file:… -- bin/check` per file on
its own, which the gate never asked for (hook output at 11:32:45). The extra
~70 s runs there were not required by the gate; see open decision 3 below.

### Design

- `elisp-check-evidence [--staged] file:PATH [file:PATH ...] -- COMMAND [ARG...]`.
  One or more `file:` labels; the existing single-label form is unchanged.
  Package labels stay rejected.
- After a passing run, `elisp-evidence record-test` (now taking several
  labels) checks the snapshot digest once and appends one record per label,
  all carrying the same snapshot, under one lock. The store format, `covers`,
  pre-commit, post-commit debts and the Stop hook are unchanged: each label's
  record still has to match the exact staged (or committed) contents of every
  path it covers.
- The pre-commit message prints one command naming every missing project
  `file:` label, instead of one line per label. `elisp-ert` lines for extras
  test files and `batch-test.sh` lines are unchanged.
- Docs: `elisp-conventions` SKILL.md and `references/testing.md` (Claude and
  Codex copies), `claude/README.org` evidence paragraph,
  `claude/bin/README.org` table row.

### Tests (`tests/test_elisp_evidence.py`)

- One run with two labels lets a commit touching both paths pass.
- A path edited after the run is still refused (guarantee kept), as is a
  label not named on the command line.
- A failing check records nothing for any label; a mid-run source change
  records nothing for any label.
- `--staged` with several labels.
- Hook message lists the labels in one command.

### Verification

End to end through the real global Git hooks: a scratch repository with two
non-package Elisp files and a tracked `check` script, committed under an
agent session id with `AGENT_ELISP_EVIDENCE_CHECK_TEMP=1` and a scratch
`AGENT_ELISP_EVIDENCE_DIR`: the commit is refused with a single suggested
command, passes after one run of it, and is refused again after editing one
file post-run.

## Decisions (fixed)

1. Tap lag is detection only; `delayed` and `fast-brew` are untouched. (Pablo's brief.)
2. The check is a direct GitHub release/tag query, not `brew livecheck`.
   (livecheck needs an unaged gem install from rubygems.org, which failed here,
   and would use the same GitHub data for every installed tap formula.)
3. Scope is every installed package from any non-official tap, with a policy
   file of reasoned exemptions, rather than a hard-coded list of five. (A tap
   added later is covered without anyone remembering to list it.)
4. Exempt `d12frosted/emacs-plus` and `benthamite/local`. (Self-published
   builds already seen by `brew outdated`; private local tap with no upstream.)
5. Runs in the daily scan, alerts via the shared `launchd-heartbeat`
   `personal-updates` monitor. (alerting.md principle; Pablo asked for the scan.)
6. Uncheckable packages are alerted, transient fetch failures only after 3
   consecutive scans. (Nothing silently unchecked; "nothing to do, no alert".)
7. Evidence gate, rule change approved by Pablo on 19 Nov 2026 in the brief:
   - Old rule: one `elisp-check-evidence` run records evidence for exactly one
     `file:` label, so N changed non-package files need N runs of the same check.
   - New rule: one passing run records evidence for every `file:` label named on
     its command line; each label's evidence is still bound to the exact staged
     contents of every path it covers.
   - Why: the same check on the same snapshot proves the same thing N times;
     the repeats cost a full suite each (~70 s for agent.el's `bin/check`).
8. The agent names the files a run covers; the tool does not infer coverage
   from the check command. (Same trust as today, where the agent already picks
   the check for each file; inference would need per-project declarations of
   what a check covers.)
9. Package labels keep their package runners (`batch-test.sh`, `elisp-ert`).
   (Those load against the active Elpaca dependency set, which a project check
   does not guarantee; see open decision 3.)

## Open decisions for Pablo (after review)

1. Run the tap-lag check once live through the launchd scan job, sending one
   real Telegram alert (recommend: do).
2. Upgrade or replace the three lagging formulae (wuzapi and two
   others) (recommend: defer; out of scope, wuzapi is being replaced by
   the wsp work).
3. Let a passing project check also satisfy a package label, so agent.el
   commits need `bin/check` alone, without `batch-test.sh agent` (recommend:
   skip; `batch-test.sh` proves loading against the live profile's
   dependencies, which `make test` does not).
