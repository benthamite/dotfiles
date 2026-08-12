# Bash Guard Prefilters Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reduce Claude Bash guard latency by skipping selected `grep` calls while preserving exact security decisions and command rewrites.

**Architecture:** Add one helper that accepts the original producer, input, unchanged BSD `grep` regex, and its necessary fixed literals at the same call site. Optimize only positive Boolean tests with explicit matching examples. A test-only switch runs the original producer and regex path, which supports exact enabled-versus-disabled comparisons without maintaining a second dispatcher.

**Tech Stack:** Bash 5, BSD `grep`, Python `unittest`, JSON hook payloads.

---

### Task 1: Make the focused suite work in linked worktrees

**Files:**
- Modify: `tests/test_security_hook_hardening.py`

- [ ] **Step 1: Write the failing worktree-path test**

Add a test that asserts `test_value_free_shell_export_classifier_is_allowed`
uses the canonical helper path that the guard intentionally allowlists, even
when the tests run from a linked worktree.

- [ ] **Step 2: Run the test and verify RED**

Run the new test from the linked worktree. Expected: FAIL because `DOTFILES`
currently supplies the linked-worktree helper path.

- [ ] **Step 3: Use the canonical helper path**

Define:

```python
CANONICAL_DOTFILES = Path.home() / "My Drive" / "dotfiles"
```

Build `SENSITIVE_READ_HELPERS` from `CANONICAL_DOTFILES`, while keeping the
guard scripts under the current checkout.

- [ ] **Step 4: Run the focused baseline**

```bash
/opt/homebrew/bin/python3 -m unittest \
  tests.test_claude_bash_pretooluse \
  tests.test_destructive_command_guard \
  tests.test_op_automations \
  tests.test_security_hook_hardening
```

Expected: 45 tests pass with no failures.

- [ ] **Step 5: Commit**

```bash
git add tests/test_security_hook_hardening.py
git commit -m "tests: use canonical shell export helper path"
```

### Task 2: Add differential, structural, and invariant tests

**Files:**
- Modify: `tests/test_claude_bash_pretooluse.py`

- [ ] **Step 1: Extend `run_hook` for environment and tracing**

Import `os`. Add optional `env` and `trace` arguments. Merge `env` with
`os.environ`; when tracing, invoke `bash -x SCRIPT`.

- [ ] **Step 2: Add a failing process-count test**

Run `pwd` normally and with `CLAUDE_BASH_PREFILTERS=0` under `bash -x`. Count
traced external `grep` invocations. Require the enabled path to start at least
20 fewer `grep` processes.

- [ ] **Step 3: Verify RED**

Run only the new process-count test. Expected: FAIL because the current script
does not implement the switch or prefilters.

- [ ] **Step 4: Add an exact differential corpus**

For every command below, compare return code, stdout, and stderr with
prefilters enabled and disabled:

```python
(
    "pwd",
    "git status --short",
    "op item get Example --reveal",
    "cat ~/.zshenv-secrets",
    "cat .env.local",
    "cat ~/.ssh/id_ed25519",
    "cat ~/.gnupg/private-keys-v1.d/key",
    "cat ~/.config/tool/tokens.json",
    "cat ~/.gmail-mcp-epoch/credentials/key.json",
    "cat ~/.config/tool/secret.json",
    "cat credentials.json",
    "git reset --hard",
    "git push --force origin main",
    "git clone https://example.com/example/repo.git",
    "git clean -fd",
    "git checkout -- .",
    "git branch -D topic",
    "gh repo delete owner/repo",
    "dropdb example",
    "bq rm dataset.table",
    "aws s3 rm s3://example --recursive",
    "op item delete Example",
    "printenv PATH",
    "printf '%s\\n' 'git reset --hard'",
    "printf 'first\\nsecond\\n'; cat .env.local",
)
```

- [ ] **Step 5: Add a structural adjacency test**

Require every optimized call to use one of these single-line forms:

```bash
prefilter_grep printf "$VALUE" 'UNCHANGED_REGEX' literal [literal ...]
prefilter_grep echo "$VALUE" 'UNCHANGED_REGEX' literal [literal ...]
```

Reject any `prefilter_grep` production call split across lines. Require the
helper to call `grep -qE -- "$regex"` itself. This keeps each necessary-literal
claim adjacent to its authoritative regex.

- [ ] **Step 6: Add per-site invariant cases**

Create a table with one regex-matching command for every optimized call site.
For each command, generate variants by adding prefixes, suffixes, newlines, and
shell separators. Run the dispatcher normally and with prefilters disabled and
require exact equality for every variant. Also require the site table's regex
to match each base command through `/usr/bin/grep -qE`. This proves that every
declared site has a matching witness and that its prefilter does not reject the
generated matching family.

- [ ] **Step 7: Commit the tests**

```bash
git add tests/test_claude_bash_pretooluse.py
git commit -m "claude: test bash guard prefilter invariants"
```

### Task 3: Add adjacent fixed-literal prefilters

**Files:**
- Modify: `claude/hooks/pretooluse-bash.sh`
- Modify: `claude/hooks/README.org`

- [ ] **Step 1: Add the helper**

```bash
prefilter_grep() {
  local producer="$1" text="$2" regex="$3" literal matched=1
  shift 3
  if [ "${CLAUDE_BASH_PREFILTERS:-1}" != 0 ]; then
    for literal in "$@"; do
      case "$text" in
        *"$literal"*) matched=0; break ;;
      esac
    done
    [ "$matched" -eq 0 ] || return 1
  fi
  case "$producer" in
    printf) printf '%s' "$text" ;;
    echo) echo "$text" ;;
    *) return 2 ;;
  esac | grep -qE -- "$regex"
}
```

The test-only switch only bypasses the substring check. It never bypasses the
original regex or any security rule.

- [ ] **Step 2: Convert selected positive Boolean tests**

Convert only existing `printf '%s' "$VALUE" | grep -qE 'REGEX'` and
`echo "$VALUE" | grep -qE 'REGEX'` Boolean sites represented in the invariant
table. The helper's explicit producer argument preserves the original producer,
quiet extended-regex flags, and text input semantics. Each call must keep the
producer, unchanged regex, and all necessary literals on one line. Include:

- sensitive-path classification tests;
- destructive-command positive tests whose action word is absent from common
  benign commands;
- simple 1Password positive tests with an explicit `op` literal.

Do not convert negative `grep` tests, output-redaction rules, dynamic
secret-pattern arrays, extraction pipelines, delegated-guard output parsing,
non-`-qE` invocations, or checks whose only useful literal would also occur in
`git status --short`.

- [ ] **Step 3: Document the contract**

In `claude/hooks/README.org`, state that selected checks use adjacent
fixed-literal necessary conditions, the original regex remains authoritative,
prefilters may over-match but must not under-match, and
`CLAUDE_BASH_PREFILTERS=0` is test-only and restores the original regex path.

- [ ] **Step 4: Run focused tests and verify GREEN**

```bash
/opt/homebrew/bin/python3 -m unittest \
  tests.test_claude_bash_pretooluse \
  tests.test_destructive_command_guard \
  tests.test_op_automations \
  tests.test_security_hook_hardening
```

Expected: all tests pass, exact enabled/disabled output is equal, and the
common path starts at least 20 fewer `grep` processes.

- [ ] **Step 5: Commit**

```bash
git add claude/hooks/pretooluse-bash.sh claude/hooks/README.org
git commit -m "claude: skip irrelevant bash guard regex checks"
```

### Task 4: Benchmark, integrate, and verify the live path

**Files:**
- No additional tracked changes expected.

- [ ] **Step 1: Audit paired configuration**

Run `bin/ai-config-sync audit`. Expected: PASS with no Claude/Codex drift.

- [ ] **Step 2: Run an interleaved full-hook benchmark**

Run 30 fixed-seed randomized pairs for `pwd`, a realistic `rg` command,
`git reset --hard`, `printenv PATH`, and `git push --dry-run`. Compare normal
execution with `CLAUDE_BASH_PREFILTERS=0`; validate exact outputs. Require at
least a 25% median improvement for both benign commands and no more than a 10%
median regression for any guarded command.

- [ ] **Step 3: Inspect the isolated branch**

Run `git diff --check master...HEAD`, `git log --oneline master..HEAD`, and
`git status --short`. Expected: focused commits and a clean worktree.

- [ ] **Step 4: Integrate the focused commits into the canonical checkout**

Cherry-pick the plan, test, fixture, and implementation commits onto `master`,
preserving the unrelated `codex/config.toml` and `emacs/abbrev/abbrev_defs`
changes. Stop if any overlap or conflict occurs.

- [ ] **Step 5: Re-run the complete focused suite in the canonical checkout**

Run the four-module test command from Task 3. Expected: all tests pass.

- [ ] **Step 6: Verify the live Claude surface**

Acceptance criterion: the installed Claude CLI loads the canonical tracked
dispatcher and preserves one allow and one denial through the real PreToolUse
event. Record canonical `HEAD` and the dispatcher hash. Run disposable,
non-persistent Claude CLI prompts from the canonical repository that request
`pwd` and `git reset --hard`; require the first tool call to proceed and the
second to receive the dispatcher's exact denial reason. Do not execute the
destructive command. Record hook events or debug output that identify the
canonical dispatcher path. Remove only agent-created CLI output files.

- [ ] **Step 7: Clean up the temporary worktree**

After canonical verification succeeds, remove the agent-created linked
worktree through `git worktree remove`, delete the temporary branch, and confirm
`git worktree list` no longer contains it.
