# Codex GitHub Admin Authorization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the Codex GitHub write guard allow repositories administered by the authenticated `gh` account, preserve the fail-closed allowlist fallback, and then publish the pending YASnippet fix.

**Architecture:** Port the established live `.permissions.admin` check from the Claude guard into the Codex guard. Add behavioral parity tests around the two real hook entry points, update the shared policy documentation, and verify the exact `benthamite/yasnippet` target before publishing.

**Tech Stack:** Bash, GitHub CLI, Python `unittest`, Org/Markdown documentation, Git, Emacs/Elpaca.

---

### Task 1: Add behavioral parity coverage

**Files:**
- Create: `tests/test_github_write_guard.py`
- Test: `tests/test_github_write_guard.py`

- [ ] **Step 1: Write the failing parity test**

Create `tests/test_github_write_guard.py`:

```python
"""Behavioral parity tests for the Claude and Codex GitHub write guards."""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
GUARDS = {
    "claude": ROOT / "claude" / "hooks" / "block-github-write-command.sh",
    "codex": ROOT / "codex" / "hooks" / "block-github-write-command.sh",
}
ALLOWLIST = ROOT / "agents" / "github-write-allowlist.txt"


def payload(tool: str, command: str) -> dict:
    if tool == "claude":
        return {"tool_name": "Bash", "tool_input": {"command": command}}
    return {
        "tool_name": "functions.exec_command",
        "tool_input": {"cmd": command},
    }


def decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"]["permissionDecision"]


class GitHubWriteGuardParityTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.gh_log = Path(self.temp_dir.name) / "gh-args"
        fake_gh = Path(self.temp_dir.name) / "gh"
        fake_gh.write_text(
            """#!/bin/sh
printf '%s\\n' "$@" > "$FAKE_GH_LOG"
case "$FAKE_GH_ADMIN" in
  true) printf 'true\\n' ;;
  false) printf 'false\\n' ;;
  *) exit 1 ;;
esac
"""
        )
        fake_gh.chmod(0o755)

    def tearDown(self):
        self.temp_dir.cleanup()

    def run_guard(self, tool: str, command: str, admin: str):
        self.gh_log.unlink(missing_ok=True)
        env = os.environ.copy()
        env["PATH"] = f"{self.temp_dir.name}:{env['PATH']}"
        env["FAKE_GH_ADMIN"] = admin
        env["FAKE_GH_LOG"] = str(self.gh_log)
        return subprocess.run(
            ["bash", str(GUARDS[tool])],
            input=json.dumps(payload(tool, command)),
            capture_output=True,
            text=True,
            check=True,
            cwd=ROOT,
            env=env,
        )

    def assert_both(self, command: str, admin: str, expected: str):
        for tool in GUARDS:
            with self.subTest(tool=tool, command=command, admin=admin):
                self.assertEqual(
                    decision(self.run_guard(tool, command, admin)),
                    expected,
                )

    def test_administered_repo_is_allowed(self):
        command = (
            "git push https://github.com/benthamite/yasnippet.git "
            "fix/post-command-handler-quit"
        )
        for tool in GUARDS:
            with self.subTest(tool=tool):
                result = self.run_guard(tool, command, "true")
                self.assertEqual(decision(result), "allow")
                self.assertEqual(
                    self.gh_log.read_text().splitlines(),
                    [
                        "api",
                        "repos/benthamite/yasnippet",
                        "--jq",
                        ".permissions.admin",
                    ],
                )

    def test_nonadmin_repo_is_denied(self):
        self.assert_both(
            "git push https://github.com/example/unowned.git topic",
            "false",
            "deny",
        )

    def test_github_query_failure_denies_nonallowlisted_repo(self):
        self.assert_both(
            "git push https://github.com/example/unowned.git topic",
            "error",
            "deny",
        )

    def test_allowlist_remains_fallback_when_query_fails(self):
        entries = [
            line.split("#", 1)[0].strip()
            for line in ALLOWLIST.read_text().splitlines()
            if line.split("#", 1)[0].strip()
        ]
        self.assertTrue(entries, "test requires one configured fallback entry")
        self.assert_both(
            f"git push https://github.com/{entries[0]}.git topic",
            "error",
            "allow",
        )


if __name__ == "__main__":
    unittest.main()
```

- [ ] **Step 2: Run the test to verify the current Codex policy fails**

Run:

```bash
python3 -m unittest tests.test_github_write_guard -v
```

Expected: `test_administered_repo_is_allowed` fails for the Codex subtest because the Codex guard returns `deny`; the Claude subtest and the other policy cases pass.

### Task 2: Port the live admin check to Codex

**Files:**
- Modify: `codex/hooks/block-github-write-command.sh:1-145`
- Test: `tests/test_github_write_guard.py`

- [ ] **Step 1: Update the guard contract and denial text**

Change the header to state that writes are allowed when the authenticated `gh`
user administers the target repository, with the allowlist as a fallback.
Replace the final sentence in `deny()` with:

```text
GitHub writes are auto-allowed for repos you administer on GitHub (checked live via `gh`). Otherwise the target must be listed in `~/My Drive/dotfiles/agents/github-write-allowlist.txt` — add a repo there only if you cannot administer it but still need write access.
```

- [ ] **Step 2: Add the fail-closed live check**

Add this function after `repo_from_local_git`:

```bash
# True when GitHub reports that the authenticated gh user has admin permission
# on the repo. Fails CLOSED: if gh is missing, unauthenticated, offline, or the
# repo is inaccessible, this returns non-zero and the caller falls through to
# the explicit allowlist.
repo_owned_p() {
  local repo
  repo=$(normalize_repo "$1")
  [ -n "$repo" ] || return 1
  command -v gh >/dev/null 2>&1 || return 1
  local is_admin
  is_admin=$(gh api "repos/$repo" --jq '.permissions.admin' 2>/dev/null || true)
  [ "$is_admin" = "true" ]
}
```

Update `require_allowed_repo` so the live check precedes the allowlist:

```bash
require_allowed_repo() {
  local action="$1"
  local repo="$2"
  if [ -z "$repo" ]; then
    deny "$action has no unambiguous repository target" "The guard blocks ambiguous GitHub writes. Make the target repo explicit; use the allowlist only after Pablo explicitly authorizes agent writes to it."
  fi
  if repo_owned_p "$repo"; then
    return 0
  fi
  if ! repo_allowed_p "$repo"; then
    deny "$action targets non-allowlisted repo $repo" "Do not infer write permission from org membership, affected-repo context, maintainer requests, or a general \"proceed\"."
  fi
}
```

The guard source is self-protected. If normal editing is denied, do not bypass
the protection with another writer. Open the file in Emacs, copy the exact
approved changes to the kill ring, and ask Pablo to apply and save them.

- [ ] **Step 3: Run the parity test to verify green behavior**

Run:

```bash
python3 -m unittest tests.test_github_write_guard -v
```

Expected: four tests pass.

- [ ] **Step 4: Exercise the exact live target without pushing**

Run:

```bash
claude_output=$(
  printf '%s' \
    '{"tool_name":"Bash","tool_input":{"command":"git push https://github.com/benthamite/yasnippet.git fix/post-command-handler-quit"}}' |
    claude/hooks/block-github-write-command.sh
)
codex_output=$(
  printf '%s' \
    '{"tool_name":"functions.exec_command","tool_input":{"cmd":"git push https://github.com/benthamite/yasnippet.git fix/post-command-handler-quit"}}' |
    codex/hooks/block-github-write-command.sh
)
test -z "$claude_output"
test -z "$codex_output"
gh api repos/benthamite/yasnippet --jq '.permissions.admin'
```

Expected: both output variables are empty, all commands exit zero, and the
final command prints:

```bash
true
```

### Task 3: Align documentation and configuration inventory

**Files:**
- Modify: `codex/README.org:33`
- Modify: `codex/README.org:682-689`
- Modify: `agents/README.org:74-83`
- Modify: `agents/github-write-allowlist.txt:1-6`
- Modify: `ai-config-sync.json:315-321`
- Modify: `ai-config-sync.json:664-674`

- [ ] **Step 1: Describe the shared authorization rule**

In the overview paragraph at `codex/README.org:33`, replace the existing
`block-github-write-command.sh` sentence with:

```text
=hooks/block-github-write-command.sh= blocks =git push= and write-style =gh=
operations unless the authenticated =gh= user has admin permission on the
target repository or the repository is listed in the shared
=agents/github-write-allowlist.txt= fallback.
```

Replace the dedicated section at `codex/README.org:682-689` with:

```text
=block-github-write-command.sh= blocks agent-initiated GitHub writes unless the
authenticated =gh= user has admin permission on the target repository or the
target repository is listed in =agents/github-write-allowlist.txt=. The live
check uses =gh api repos/<owner>/<repo> --jq .permissions.admin= and fails
closed to the allowlist when =gh= is unavailable, unauthenticated, offline, or
cannot access the repository. It covers =git push=, mutating =gh pr= / =gh
issue= / =gh secret= / =gh workflow= / =gh release= / =gh repo= commands, and
write-style =gh api= calls. Read-only =gh= inspection remains allowed.

=block-github-guard-edit.sh= makes the allowlist, guard files, tracked
=codex/hooks.json=, live =~/.codex/hooks.json=, and live
=~/.claude/settings.json= self-protected so an agent cannot first edit its own
policy and then perform the blocked write.
```

Replace the external-write paragraph at `agents/README.org:74-82` with:

```text
=agents/github-write-allowlist.txt= is the shared fallback for
agent-initiated GitHub writes to repositories where the authenticated =gh= user
does not have admin permission. Claude and Codex first check
=gh api repos/<owner>/<repo> --jq .permissions.admin= and fail closed to this
allowlist when =gh= is unavailable, unauthenticated, offline, or cannot access
the repository. The guards cover =git push= and write-style =gh= operations,
including PRs, issues, secrets, workflow runs, releases, repository mutations,
and write-style =gh api= calls.
```

Leave the existing self-protection paragraph at `agents/README.org:84-89`
unchanged so its protected registration paths remain documented exactly once.

Replace the `block-github-write-command.sh` note in `ai-config-sync.json` with
this JSON string:

```json
"note": "paired GitHub write gate for shell commands; permits repository-scoped writes when the authenticated gh user has admin permission, then fails closed to agents/github-write-allowlist.txt when the live check is unavailable or false. Codex normalizes Bash, exec_command, functions.exec, and functions.exec_command payloads via lib-codex-hook-json.sh; Claude is delegated by pretooluse-bash.sh."
```

Replace the `agents/github-write-allowlist.txt` inventory note in
`ai-config-sync.json` with:

```json
"note": "Shared fallback allowlist for agent-initiated GitHub writes when the authenticated gh user does not have admin permission on the target repository. Both Claude and Codex check GitHub permissions live and fail closed to this file. Entries require Pablo's explicit authorization."
```

Change the comments in `agents/github-write-allowlist.txt` to:

```text
# GitHub repositories where agents are allowed to perform write operations
# despite the authenticated account not having admin permission.
#
# Format: one OWNER/REPO per line. Blank lines and comments are ignored.
#
# Add a repository only when Pablo has explicitly authorized agent writes.
```

The allowlist is self-protected. If its comment edit is denied, leave the
existing entries untouched and ask Pablo to apply only the approved comment
replacement.

- [ ] **Step 2: Run documentation and sync checks**

Run:

```bash
python3 -m unittest tests.test_github_write_guard -v
bin/ai-config-sync audit
git diff --check
```

Expected: all guard tests pass, the configuration audit passes, and
`git diff --check` prints nothing.

- [ ] **Step 3: Run the complete Python test suite**

Run:

```bash
python3 -m unittest discover -s tests -v
```

Expected: all tests pass. Diagnose any failure before changing implementation.

- [ ] **Step 4: Commit only the guard-policy change**

Keep the unrelated `codex/config.toml` modification unstaged. Stage:

```bash
git add \
  tests/test_github_write_guard.py \
  codex/hooks/block-github-write-command.sh \
  codex/README.org \
  agents/README.org \
  agents/github-write-allowlist.txt \
  ai-config-sync.json
git commit -m "codex: allow writes to administered github repos"
```

### Task 4: Publish the YASnippet branch and open the PR

**Files:**
- Read: `/tmp/yasnippet-pr-body.md`
- Resolve repository: `bin/elpaca-package-path yasnippet`

- [ ] **Step 1: Reconfirm publication state**

Run:

```bash
yasnippet_repo=$(bin/elpaca-package-path yasnippet)
git -C "$yasnippet_repo" status -sb
git -C "$yasnippet_repo" log -1 --oneline
rg -n '^## ' /tmp/yasnippet-pr-body.md
```

Expected: branch `fix/post-command-handler-quit` is clean at `fa254b6c`; the PR
body contains only `## Summary` and `## Reproduction`.

- [ ] **Step 2: Push to the administered fork**

Run:

```bash
yasnippet_repo=$(bin/elpaca-package-path yasnippet)
git -C "$yasnippet_repo" \
  push https://github.com/benthamite/yasnippet.git \
  fix/post-command-handler-quit
git -C "$yasnippet_repo" fetch fork fix/post-command-handler-quit
git -C "$yasnippet_repo" \
  branch --set-upstream-to=fork/fix/post-command-handler-quit
```

Expected: `benthamite/yasnippet` receives commit `fa254b6c`.

- [ ] **Step 3: Authorize the exact upstream PR target**

The shell guard authorizes the branch push because `benthamite` administers the
fork, but the PR record is created in `joaotavora/yasnippet`, which
`benthamite` does not administer. The user has explicitly authorized this exact
PR, but the self-protected guard cannot infer that from the conversation.

Open `agents/github-write-allowlist.txt` in Emacs, put
`joaotavora/yasnippet` in the kill ring, and ask Pablo to append and save that
one entry through the documented manual path. Do not add any broader target or
bypass the guard.

- [ ] **Step 4: Open the upstream draft PR**

Run:

```bash
gh pr create \
  --repo joaotavora/yasnippet \
  --base master \
  --head benthamite:fix/post-command-handler-quit \
  --title "Preserve post-command setup after quitting a prompt" \
  --body-file /tmp/yasnippet-pr-body.md \
  --draft
```

Use draft status because the GitHub publishing workflow defaults to a draft
unless the user explicitly requests ready-for-review status.

- [ ] **Step 5: Remove the one-time upstream authorization**

After `gh pr create` returns the PR URL, ask Pablo to remove only the
`joaotavora/yasnippet` line from `agents/github-write-allowlist.txt` in Emacs
and save the file. Verify:

```bash
if rg -n '^joaotavora/yasnippet$' agents/github-write-allowlist.txt
then
  exit 1
fi
```

Expected: the one-time entry is absent; the PR remains open.

- [ ] **Step 6: Verify the published PR**

Run:

```bash
gh pr view \
  --repo joaotavora/yasnippet \
  --json url,state,isDraft,title,body,headRefName,headRepository,baseRefName
```

Expected: an open draft PR into `master`, with the reviewed title, the
`benthamite` head branch, and no `## Testing` section.

- [ ] **Step 7: Close the push-triggered CI loop**

Run:

```bash
ci-after-push \
  --no-push \
  --repo benthamite/yasnippet \
  --commit fa254b6cbd73be203431094323d1e37e90845917
```

Expected: all runs for the pushed commit pass, or a concrete no-workflow result
is recorded.

### Task 5: Pin YASnippet to the open PR branch

**Files:**
- Modify: `emacs/config.org:3769`
- Verify: active profile `init.el`

- [ ] **Step 1: Add the fork recipe to the existing declaration**

Change the single canonical declaration to:

```elisp
(use-package yasnippet
  :ensure (:host github
                 :repo "benthamite/yasnippet"
                 :branch "fix/post-command-handler-quit")
  :custom
```

On the `:branch` line, append `; awaiting PR merge:` followed by the exact URL
printed by `gh pr view --repo joaotavora/yasnippet --json url --jq .url`.
The GitHub-assigned PR number cannot be known before Task 4. Do not add another
`use-package` or `elpaca` declaration.

- [ ] **Step 2: Tangle and verify the single recipe owner**

Run:

```bash
emacsclient -e '(init-build-profile (file-name-directory user-init-file))'
rg -n '\(use-package yasnippet\b|\(elpaca \(yasnippet\b|awaiting PR merge:.*yasnippet' emacs/config.org
profile=$(emacsclient -e init-current-profile | tr -d '"')
rg -n '\(use-package yasnippet\b|\(elpaca \(yasnippet\b' \
  "$HOME/.config/emacs-profiles/$profile/init.el"
```

Expected: one source recipe and one tangled recipe, both pointing to the fork
branch.

- [ ] **Step 3: Run a fresh profile startup warning check**

Run:

```bash
profile=$(emacsclient -e init-current-profile | tr -d '"')
profile_dir="$HOME/.config/emacs-profiles/$profile"
startup_log=$(mktemp /tmp/yasnippet-profile-startup.XXXXXX)
if ! emacs -Q --batch \
  --eval "(setq user-emacs-directory \"$profile_dir/\" user-init-file \"$profile_dir/init.el\")" \
  -l "$profile_dir/early-init.el" \
  -l "$profile_dir/init.el" \
  --eval '(message "yasnippet profile startup check complete")' \
  >"$startup_log" 2>&1
then
  sed -n '1,240p' "$startup_log"
  trash "$startup_log"
  exit 1
fi
if rg -n 'previously queued as dependency|Duplicate item ID queued' "$startup_log"
then
  trash "$startup_log"
  exit 1
fi
trash "$startup_log"
```

Expected: Emacs exits zero and the captured startup output contains neither
duplicate-queue warning.

- [ ] **Step 4: Commit the pin separately**

Run:

```bash
git add emacs/config.org
git commit -m "emacs: pin yasnippet to pr branch"
```

Keep the unrelated pre-existing `codex/config.toml` modification unstaged.

### Task 6: Final verification and cleanup

**Files:**
- Remove: `/tmp/yasnippet-pr-body.md`

- [ ] **Step 1: Inspect both repositories**

Run:

```bash
git -C "/Users/pablostafforini/My Drive/dotfiles" status --short --branch
yasnippet_repo=$(
  "/Users/pablostafforini/My Drive/dotfiles/bin/elpaca-package-path" yasnippet
)
git -C "$yasnippet_repo" status --short --branch
```

Expected: the YASnippet branch is clean and tracking the fork; dotfiles contains
only the pre-existing `codex/config.toml` modification.

- [ ] **Step 2: Remove the temporary PR body**

Run:

```bash
trash /tmp/yasnippet-pr-body.md
```

- [ ] **Step 3: Report the result**

Report the guard-policy commit, YASnippet commit and PR URL, CI outcome, pin
commit, and the preserved unrelated `codex/config.toml` change.
