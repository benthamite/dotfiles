# Fail-Closed Elisp Reload Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve short asynchronous Elisp rebuilds while making stale live code visible, durable across sessions, reload-safe, and unable to start a new Agent session.

**Architecture:** A new state utility records reload debt before the first live-Emacs request and accepts only a matching completion receipt returned by the intended daemon. The paired Claude/Codex hooks use that state, the Elpaca callback attaches exact runtime provenance to its finished token, and Agent reconciles mode-owned wiring on hot reload while rejecting sessions whose loaded generation or advice is stale.

**Tech Stack:** Bash hook scripts, Python `unittest` and a small Python state CLI, Emacs Lisp/ERT, Elpaca asynchronous callbacks, JSON/base64 receipts, and isolated named Emacs daemons for end-to-end verification.

---

## File map

### Dotfiles repository

- Create `bin/elisp-reload-state`: own durable JSON reload records under the
  profile-independent state root and perform atomic state transitions.
- Create `tests/test_elisp_reload_state.py`: exercise identity matching,
  persistence, and failure-state behavior without Emacs.
- Modify `codex/hooks/lib-codex-hook-json.sh`: extract recorded nested
  `tools.apply_patch` payloads from `functions.exec` source.
- Modify `codex/hooks/lib-codex-paths.sh`: expose direct and nested patch paths
  through one `codex_changed_paths` interface.
- Modify paired `load-elisp-after-edit.sh`: create debt before delivery, report
  every terminal failure, require a matching receipt, and clear only automatic
  reload debt after success.
- Modify paired verification tracker/guard scripts and Claude's live dispatcher:
  block unresolved durable debt while preserving recovery commands.
- Modify `codex/hooks.json` and the Claude hook deployment configuration:
  register outer `functions.exec` edits and give bounded polling enough time.
- Modify `emacs/extras/elpaca-extras.el`: carry request identity into terminal
  status and create an exact runtime receipt after reload.
- Modify `emacs/extras/test/elpaca-extras-test.el`: cover receipt creation and
  enqueue errors.
- Modify paired hook docs, Elisp workflow docs, Elpaca Extras manual, and
  `ai-config-sync.json`: describe the fail-closed contract and paired artifacts.

### Agent repository

- Modify `agent-codex.el`: separate owned wiring installation/removal, add a
  per-load identity, reconcile an enabled mode after reload, and install a
  negative-depth session-start guard.
- Modify `test/agent-codex-test.el`: cover stale identities, missing advice,
  refresh idempotence, and symmetric disablement.
- Create `test/agent-codex-hot-reload-e2e.el`: disposable-daemon assertions for
  hot reload plus the real two-skill exit state machine.
- Create `test/agent-codex-hot-reload-e2e.sh`: create, drive, stop, and clean the
  isolated daemon.
- Modify `Makefile`, `README.org`, and `agent.texi`: expose the acceptance target
  and document the runtime freshness contract.

## Task 1: Durable reload-state utility

**Files:**
- Create: `bin/elisp-reload-state`
- Create: `tests/test_elisp_reload_state.py`
- Modify: `bin/README.org`

- [ ] **Step 1: Write failing state-transition tests**

Add tests that run the CLI with `ELISP_RELOAD_STATE_DIR` pointing at a temporary
directory:

```python
class ElispReloadStateTests(unittest.TestCase):
    def run_state(self, *args: str, check: bool = False):
        env = os.environ.copy()
        env["ELISP_RELOAD_STATE_DIR"] = str(self.state_dir)
        return subprocess.run(
            [str(DOTFILES / "bin/elisp-reload-state"), *args],
            text=True, capture_output=True, check=check, env=env,
        )

    def test_pending_record_survives_process_and_session_boundaries(self):
        result = self.run_state(
            "pending", "--source", str(self.source),
            "--identity", "source-a", check=True,
        )
        record = Path(result.stdout.strip())
        self.assertEqual(json.loads(record.read_text())["state"], "pending")
        self.assertTrue(record.is_relative_to(self.state_dir))

    def test_matching_loaded_receipt_clears_debt(self):
        record = self.pending("source-a")
        receipt = self.receipt(source_identity="source-a")
        result = self.run_state(
            "loaded", "--record", str(record),
            "--receipt", json.dumps(receipt), check=True,
        )
        self.assertEqual(json.loads(record.read_text())["state"], "loaded")

    def test_mismatched_receipt_cannot_clear_debt(self):
        record = self.pending("source-a")
        result = self.run_state(
            "loaded", "--record", str(record),
            "--receipt", json.dumps(self.receipt(source_identity="source-b")),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(json.loads(record.read_text())["state"], "pending")

    def test_failure_is_durable_and_reported_as_debt(self):
        record = self.pending("source-a")
        self.run_state(
            "failed", "--record", str(record), "--state", "delivery-failed",
            "--message", "server request timed out", check=True,
        )
        debt = self.run_state("debt", "--json", check=True)
        self.assertEqual(json.loads(debt.stdout)[0]["state"], "delivery-failed")
```

- [ ] **Step 2: Run the focused test and verify RED**

Run:

```bash
python3 -m unittest tests.test_elisp_reload_state -v
```

Expected: FAIL because `bin/elisp-reload-state` does not exist.

- [ ] **Step 3: Implement the state CLI**

Implement an executable Python script with these commands and contracts:

```python
def state_root() -> Path:
    override = os.environ.get("ELISP_RELOAD_STATE_DIR")
    if override:
        return Path(override)
    base = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state"))
    return base / "elpaca-reload"

def record_path(source: Path) -> Path:
    canonical = str(source.resolve())
    key = hashlib.sha256(canonical.encode()).hexdigest()
    return state_root() / f"{key}.json"

def atomic_write(path: Path, value: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(dir=path.parent, prefix=f".{path.name}.")
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(value, stream, sort_keys=True)
            stream.write("\n")
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)

def set_loaded(record: Path, receipt: dict[str, object]) -> None:
    current = json.loads(record.read_text())
    if receipt.get("source_identity") != current.get("source_identity"):
        raise SystemExit("receipt source identity does not match pending reload")
    required = ("package", "profile", "artifact", "artifact_hash",
                "emacs_pid", "emacs_started_at")
    missing = [key for key in required if not receipt.get(key)]
    if missing:
        raise SystemExit(f"receipt missing required fields: {', '.join(missing)}")
    atomic_write(record, {**current, **receipt, "state": "loaded",
                          "updated_at": time.time()})
```

`pending` writes schema version, canonical source, source identity, and time;
`failed` accepts only `delivery-failed`, `build-failed`, or `reload-failed`;
`loaded` validates before replacing the record; `debt --json` returns every
record whose state is not `loaded`; and `show --source PATH` returns one record.

- [ ] **Step 4: Run tests and verify GREEN**

Run the focused unittest command again. Expected: all reload-state tests PASS.

- [ ] **Step 5: Document and commit**

Add the command and state-root contract to `bin/README.org`, then run:

```bash
git add bin/elisp-reload-state tests/test_elisp_reload_state.py bin/README.org
git commit -m "hooks: add durable elisp reload state"
```

## Task 2: Make edit delivery fail closed for every tool envelope

**Files:**
- Modify: `codex/hooks/lib-codex-hook-json.sh`
- Modify: `codex/hooks/lib-codex-paths.sh`
- Modify: `codex/hooks/load-elisp-after-edit.sh`
- Modify: `claude/hooks/load-elisp-after-edit.sh`
- Modify: `codex/hooks.json`
- Modify: `tests/test_elisp_reload_hooks.py`

- [ ] **Step 1: Add recorded-envelope and delivery-failure tests**

Extend `ElispReloadHookTests` so fake `emacsclient` responses are scripted from
a file, every payload carries a session id, and the state directory is isolated.
Add paired-hook tests with these exact behaviors:

```python
def test_reload_enqueue_failure_leaves_durable_delivery_debt(self):
    result, records = self.run_reload(
        self.elisp_file, emacsclient_exit=124, emacsclient_output="timeout",
    )
    self.assertNotEqual(result.returncode, 0)
    self.assertEqual(records[0]["state"], "delivery-failed")
    self.assertIn("could not enqueue", result.stdout + result.stderr)

def test_finished_reload_requires_matching_receipt(self):
    result, records = self.run_reload(
        self.elisp_file,
        responses=["example:token-1", "finished:Rebuilt and reloaded: example",
                   self.receipt_response(source_identity=self.source_identity)],
    )
    self.assertEqual(result.returncode, 0, result.stderr)
    self.assertEqual(records[0]["state"], "loaded")

def test_finished_reload_with_mismatched_receipt_stays_pending(self):
    result, records = self.run_reload(
        self.elisp_file,
        responses=["example:token-1", "finished:ok",
                   self.receipt_response(source_identity="other")],
    )
    self.assertNotEqual(result.returncode, 0)
    self.assertNotEqual(records[0]["state"], "loaded")

def test_nested_functions_exec_apply_patch_creates_same_debt(self):
    patch = f"*** Begin Patch\n*** Update File: {self.elisp_file}\n@@\n-old\n+new\n*** End Patch"
    source = f"const patch = {json.dumps(patch)}; await tools.apply_patch(patch);"
    direct = self.run_reload(self.elisp_file, emacsclient_exit=124)
    nested = self.run_outer_reload(source, emacsclient_exit=124)
    self.assertEqual(self.debt_shape(direct), self.debt_shape(nested))
```

Also cover missing package/token, terminal build/reload failure, poll timeout,
mixed multi-file partial success, test-only edits, and Git-operation skips.

- [ ] **Step 2: Run the hook suite and verify RED**

Run:

```bash
python3 -m unittest tests.test_elisp_reload_hooks.ElispReloadHookTests -v
```

Expected failures: initial enqueue still exits zero, no durable records exist,
and the outer `functions.exec` patch path is not extracted.

- [ ] **Step 3: Extend nested patch extraction**

Add `codex_nested_apply_patch_texts` beside `codex_nested_exec_contexts`. Its
lexer must recognize both generated forms without evaluating JavaScript:

```javascript
await tools.apply_patch("*** Begin Patch\\n...")
const patch = "*** Begin Patch\\n...";
await tools.apply_patch(patch);
```

Resolve only literal strings and identifiers bound by `const` to literal
strings. Mark dynamic arguments ambiguous and emit no guessed path. Update
`codex_changed_paths` to run every extracted patch through the existing AWK
path parser. Keep direct Edit/Write/apply_patch behavior unchanged and dedupe
paths with `sort -u`.

- [ ] **Step 4: Make paired reload hooks persist state before delivery**

For each normalized eligible production file:

```bash
source_identity=$(shasum -a 256 "$file_path" | awk '{print $1}')
record=$(ELISP_RELOAD_STATE_DIR="${ELISP_RELOAD_STATE_DIR:-}" \
  "$state_tool" pending --source "$file_path" --identity "$source_identity")

if ! result=$(timeout "$enqueue_timeout" emacsclient -e "$enqueue_form" 2>&1); then
  "$state_tool" failed --record "$record" --state delivery-failed \
    --message "$result"
  failed=$((failed + 1))
  last_status="$file_path: could not enqueue rebuild: $result"
  continue
fi
```

Pass `source_identity` into `elpaca-extras-rebuild-and-reload`. Poll terminal
status with bounded calls. On `failed`, record `build-failed` or
`reload-failed` from the returned message. On `finished`, request
`elpaca-extras-format-build-reload-receipt`, decode it, and call
`elisp-reload-state loaded`; receipt validation failure is terminal and nonzero.

Set `enqueue_timeout` below the outer hook timeout. Count every eligible file;
return success only when every eligible file reaches matching `loaded` state.
Irrelevant/test-only events create no record. A Git-operation skip leaves a
clear pending record instead of claiming freshness.

- [ ] **Step 5: Register the outer envelope and coherent timeout**

Add `functions.exec` to the edit/reload PostToolUse matcher and raise only this
hook's outer timeout above `enqueue_timeout + reload_timeout + one poll`, for
example 150 seconds for a 10-second enqueue and 120-second reload deadline.
Do not lengthen live Emacs requests themselves.

- [ ] **Step 6: Run hook tests and shell checks**

Run:

```bash
python3 -m unittest tests.test_elisp_reload_hooks.ElispReloadHookTests -v
shellcheck codex/hooks/lib-codex-hook-json.sh codex/hooks/lib-codex-paths.sh \
  codex/hooks/load-elisp-after-edit.sh claude/hooks/load-elisp-after-edit.sh
```

Expected: all tests PASS and shellcheck reports no findings.

- [ ] **Step 7: Commit the delivery layer**

```bash
git add codex/hooks/lib-codex-hook-json.sh codex/hooks/lib-codex-paths.sh \
  codex/hooks/load-elisp-after-edit.sh claude/hooks/load-elisp-after-edit.sh \
  codex/hooks.json tests/test_elisp_reload_hooks.py
git commit -m "hooks: fail closed when elisp reload delivery fails"
```

## Task 3: Attach exact runtime receipts to Elpaca completion

**Files:**
- Modify: `emacs/extras/elpaca-extras.el`
- Modify: `emacs/extras/test/elpaca-extras-test.el`
- Modify: `emacs/extras/doc/elpaca-extras.org`
- Regenerate: `emacs/extras/doc/elpaca-extras.texi`

- [ ] **Step 1: Write failing ERT tests**

Add tests proving that enqueue errors become terminal failures and successful
reloads record exact provenance:

```elisp
(ert-deftest elpaca-extras-test-rebuild-enqueue-error-records-failed ()
  "A synchronous enqueue error becomes a terminal token status."
  (cl-letf (((symbol-function 'add-hook) #'ignore)
            ((symbol-function 'remove-hook) #'ignore)
            ((symbol-function 'elpaca-rebuild)
             (lambda (&rest _) (error "cannot enqueue"))))
    (let* ((token (elpaca-extras-rebuild-and-reload
                   'my-pkg '(:source-identity "source-a")))
           (status (elpaca-extras-build-reload-status token)))
      (should (eq (plist-get status :state) 'failed))
      (should (string-match-p "cannot enqueue" (plist-get status :message))))))

(ert-deftest elpaca-extras-test-finished-status-records-runtime-receipt ()
  "Finished means the requested source is loaded in this daemon."
  (let ((before-init-time (seconds-to-time 123))
        (init-current-profile "test-profile"))
    (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'fake))
              ((symbol-function 'elpaca--status) (lambda (_) 'finished))
              ((symbol-function 'elpaca-extras-reload) #'ignore)
              ((symbol-function 'elpaca-extras--package-artifact)
               (lambda (_) "/tmp/example.elc"))
              ((symbol-function 'secure-hash)
               (lambda (&rest _) "artifact-sha"))
              ((symbol-function 'emacs-pid) (lambda () 1234))
              ((symbol-function 'remove-hook) #'ignore)
              ((symbol-function 'message) #'ignore))
      (elpaca-extras--handle-build-complete
       'example #'ignore "Rebuilt" "token"
       '(:source-identity "source-a"))
      (let ((receipt (plist-get
                      (elpaca-extras-build-reload-status "token") :receipt)))
        (should (equal (plist-get receipt :source-identity) "source-a"))
        (should (equal (plist-get receipt :artifact-hash) "artifact-sha"))
        (should (= (plist-get receipt :emacs-pid) 1234))))))
```

- [ ] **Step 2: Run focused ERT and verify RED**

Run:

```bash
emacs/extras/test/run-tests.sh --ci emacs/extras/test/elpaca-extras-test.el
```

Expected: the new receipt and enqueue-error assertions fail.

- [ ] **Step 3: Implement request propagation and receipts**

Change the public functions to accept an optional request plist while
preserving interactive callers:

```elisp
(defun elpaca-extras-rebuild-and-reload (&optional pkg request)
  "Rebuild PKG, reload it, and associate REQUEST with its status token."
  (interactive (list (elpaca--read-queued "Rebuild and reload package: ")))
  (elpaca-extras--build-and-reload pkg #'elpaca-rebuild "Rebuilt" request))
```

Propagate `request` through the callback. Wrap the synchronous build enqueue in
`condition-case`; on error remove the callback and record `failed` with the
concrete error. After `elpaca-extras-reload` succeeds, compute:

```elisp
(list :source-identity (plist-get request :source-identity)
      :package (symbol-name pkg)
      :profile (format "%s" (and (boundp 'init-current-profile)
                                  init-current-profile))
      :artifact artifact
      :artifact-hash (secure-hash 'sha256 artifact)
      :emacs-pid (emacs-pid)
      :emacs-started-at (float-time before-init-time))
```

Store it under `:receipt` only after reload returns normally. Add
`elpaca-extras-format-build-reload-receipt`, which JSON-serializes the receipt
and base64-encodes it without line breaks. Unknown/non-finished tokens return an
empty string.

- [ ] **Step 4: Run ERT, batch load, and documentation generation**

Run:

```bash
emacs/extras/test/run-tests.sh --ci emacs/extras/test/elpaca-extras-test.el
~/My\ Drive/dotfiles/claude/bin/batch-test.sh elpaca-extras
```

Update the Org manual with request/receipt semantics and regenerate its Texinfo
through the repository's sanctioned documentation command. Confirm `git
diff --check` passes.

- [ ] **Step 5: Commit the receipt layer**

```bash
git add emacs/extras/elpaca-extras.el emacs/extras/test/elpaca-extras-test.el \
  emacs/extras/doc/elpaca-extras.org emacs/extras/doc/elpaca-extras.texi
git commit -m "elpaca-extras: record exact reload receipts"
```

## Task 4: Gate unresolved debt without blocking recovery

**Files:**
- Modify: `codex/hooks/track-elisp-verify.sh`
- Modify: `claude/hooks/track-elisp-verify.sh`
- Modify: `codex/hooks/require-elisp-verify-after-commit.sh`
- Modify: `claude/hooks/require-elisp-verify-after-commit.sh`
- Modify: `claude/hooks/pretooluse-bash.sh`
- Modify: `tests/test_elisp_reload_hooks.py`

- [ ] **Step 1: Write failing guard/tracker tests**

Add tests that create durable debt through the state CLI and exercise the real
scripts:

```python
def test_require_hook_blocks_durable_reload_debt(self):
    self.create_debt(state="delivery-failed")
    result = self.run_require("rg -n foo .")
    self.assertIn("permissionDecisionReason", result.stdout)
    self.assertIn("delivery-failed", result.stdout)

def test_require_hook_allows_recovery_commands(self):
    self.create_debt(state="delivery-failed")
    for command in ("emacsclient -e '(elpaca-extras-rebuild-and-reload 'example)'",
                    "batch-test.sh example", "bin/elisp-reload-state debt --json"):
        with self.subTest(command=command):
            self.assertEqual(self.run_require(command).returncode, 0)

def test_status_poll_cannot_clear_post_commit_verification(self):
    result, marker = self.run_direct(
        self.repo,
        "emacsclient -e '(elpaca-extras-format-build-reload-status \"token\")'",
        self.session, exit_code=0, initial_marker=True,
    )
    self.assertTrue(marker.exists())
```

Also prove that arbitrary successful `emacsclient` evaluation no longer clears
durable reload debt, a failed evaluation clears nothing, both Claude and Codex
guards agree, no-debt commands pass, and active-session `/tmp` verification
markers remain independent from package reload records.

- [ ] **Step 2: Run the hook suite and verify RED**

Run:

```bash
python3 -m unittest tests.test_elisp_reload_hooks -v
```

Expected: guards ignore durable records and status polls incorrectly discharge
the old verification marker.

- [ ] **Step 3: Implement debt-aware guards**

Before ordinary shell commands, call:

```bash
debt_json=$("$state_tool" debt --json)
if [ "$debt_json" != "[]" ] && ! recovery_command_p "$COMMAND"; then
  # Return a deny response naming every package/source state and last error.
fi
```

Permit only bounded recovery lanes: state inspection, relevant batch tests,
and explicit Elpaca rebuild/reload/status expressions. Do not treat any of
those commands as proof that debt cleared; only the matching loaded receipt can
do that. Keep the existing post-commit “exercise the changed live path” marker
separate and stop deleting current-session markers merely because they are two
hours old.

- [ ] **Step 4: Run tests and shellcheck**

```bash
python3 -m unittest tests.test_elisp_reload_hooks -v
shellcheck codex/hooks/track-elisp-verify.sh \
  claude/hooks/track-elisp-verify.sh \
  codex/hooks/require-elisp-verify-after-commit.sh \
  claude/hooks/require-elisp-verify-after-commit.sh \
  claude/hooks/pretooluse-bash.sh
```

Expected: all tests PASS and shellcheck is clean.

- [ ] **Step 5: Commit the guard layer**

```bash
git add codex/hooks/track-elisp-verify.sh claude/hooks/track-elisp-verify.sh \
  codex/hooks/require-elisp-verify-after-commit.sh \
  claude/hooks/require-elisp-verify-after-commit.sh \
  claude/hooks/pretooluse-bash.sh tests/test_elisp_reload_hooks.py
git commit -m "hooks: block workflows on unresolved elisp reload debt"
```

## Task 5: Make Agent mode wiring hot-reload safe

**Files:**
- Modify: `agent-codex.el:1157-1231`
- Modify: `test/agent-codex-test.el:1768-1817`
- Modify: `README.org`
- Modify: `agent.texi`

- [ ] **Step 1: Add failing mode-generation tests**

Add these ERT cases:

```elisp
(ert-deftest agent-codex-test-mode-refreshes-enabled-mode-after-load-change ()
  "Reloading an enabled mode reconciles advice and records this load."
  (let ((agent-codex-mode t)
        (agent-codex--wired-load-identity (make-symbol "old")))
    (advice-remove 'codex--app-server-turn-completed
                   #'agent-codex--after-app-server-turn-completed)
    (agent-codex--mode-refresh-after-reload)
    (should (eq agent-codex--wired-load-identity
                agent-codex--load-identity))
    (should (advice-member-p #'agent-codex--after-app-server-turn-completed
                             'codex--app-server-turn-completed))))

(ert-deftest agent-codex-test-session-start-rejects-stale-load-identity ()
  "A child session cannot start through stale Agent wiring."
  (let ((agent-codex-mode t)
        (agent-codex--wired-load-identity (make-symbol "old")))
    (should-error (agent-codex--assert-mode-wiring-current)
                  :type 'user-error)))

(ert-deftest agent-codex-test-session-start-rejects-missing-completion-advice ()
  "Matching generation is insufficient when required advice is absent."
  (let ((agent-codex-mode t)
        (agent-codex--wired-load-identity agent-codex--load-identity))
    (advice-remove 'codex--app-server-turn-completed
                   #'agent-codex--after-app-server-turn-completed)
    (unwind-protect
        (should-error (agent-codex--assert-mode-wiring-current)
                      :type 'user-error)
      (advice-add 'codex--app-server-turn-completed :after
                  #'agent-codex--after-app-server-turn-completed))))
```

Also test a complete positive state, preservation of the originally saved
notification function during refresh, negative-depth start-hook registration,
and complete symmetric removal on disable.

- [ ] **Step 2: Run focused ERT and verify RED**

Run each new test through:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-ert \
  agent test/agent-codex-test.el TEST-NAME
```

Expected: FAIL because generation/reconciliation helpers do not exist.

- [ ] **Step 3: Implement declarative, idempotent wiring**

Add:

```elisp
(defconst agent-codex--load-identity (make-symbol "agent-codex-load")
  "Unique identity created each time this library is evaluated.")

(defvar agent-codex--wired-load-identity nil
  "Load identity whose mode-owned wiring is currently installed.")

(defconst agent-codex--mode-advice-specs
  '((codex--app-server-turn-completed :after
     agent-codex--after-app-server-turn-completed)
    (codex--do-send-command :around agent-codex--intercept-exit)
    (codex--send-command-to-buffer :around
     agent-codex--intercept-exit-to-buffer)))
```

Create `agent-codex--mode-install-wiring`,
`agent-codex--mode-remove-wiring`, `agent-codex--mode-wiring-current-p`,
`agent-codex--assert-mode-wiring-current`, and
`agent-codex--mode-refresh-after-reload`. Install the guard separately:

```elisp
(add-hook 'codex-start-hook #'agent-codex--assert-mode-wiring-current -100)
```

The refresh path removes and reinstalls only Agent-owned advice/hooks, never
overwrites `agent-codex--saved-notification-function`, and records the new wired
identity only after every operation succeeds. Before `provide`, run:

```elisp
(when agent-codex-mode
  (agent-codex--mode-refresh-after-reload))
```

The start guard detects stale state; it must not silently repair it.

- [ ] **Step 4: Run focused and full Agent checks**

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-ert agent test/agent-codex-test.el
~/My\ Drive/dotfiles/claude/bin/batch-test.sh agent
make compile
make test
```

Expected: all Agent tests pass, compilation is warning-free, and the test count
includes the new mode-generation cases.

- [ ] **Step 5: Update docs and commit Agent wiring**

Document that an enabled backend reconciles owned wiring after package reload
and validates the current load before a child session completes initialization.
Then:

```bash
git add agent-codex.el test/agent-codex-test.el README.org agent.texi
git commit -m "agent-codex: reconcile mode wiring after reload"
```

## Task 6: Add the isolated hot-reload acceptance workflow

**Files:**
- Create: `test/agent-codex-hot-reload-e2e.el`
- Create: `test/agent-codex-hot-reload-e2e.sh`
- Modify: `Makefile`

- [ ] **Step 1: Write the failing disposable-daemon driver**

The Elisp driver must assert, in one long-lived process:

```elisp
(agent-codex-mode 1)
(setq original-notification agent-codex--saved-notification-function)
(advice-remove 'codex--app-server-turn-completed
               #'agent-codex--after-app-server-turn-completed)
(setq agent-codex--wired-load-identity (make-symbol "stale"))
(should-error (agent-codex--assert-mode-wiring-current) :type 'user-error)

;; Reload exact current artifact inside this owned disposable daemon.
(load agent-codex-hot-reload-artifact nil 'nomessage)

(should agent-codex-mode)
(should (agent-codex--mode-wiring-current-p))
(should (eq original-notification
            agent-codex--saved-notification-function))
(agent-codex--assert-mode-wiring-current)
```

Then create an owned synthetic Codex app-server buffer/process, configure two
before-exit skills, invoke real `agent-exit`, feed completions through the
actually advised `codex--app-server-turn-completed`, and assert submission order
`$first`, `$second`, `/exit` plus final buffer death. Stub only Codex transport
and its completion body; use Agent's real advice, queue, timer, and teardown.
Write `PASS` plus the load identity and submission sequence to the owned result
file, then call `kill-emacs 0`.

- [ ] **Step 2: Write the shell orchestrator and verify RED**

The shell script must:

```bash
fixture=$(mktemp -d "${TMPDIR:-/tmp}/agent-hot-reload.XXXXXX")
socket="agent-hot-reload-$$"
cleanup() {
  if [ -n "${daemon_pid:-}" ] && kill -0 "$daemon_pid" 2>/dev/null; then
    emacsclient --socket-name "$socket" -e '(kill-emacs 1)' >/dev/null 2>&1 || true
  fi
  trash "$fixture"
}
trap cleanup EXIT
```

Start `/Applications/Emacs.app/Contents/MacOS/Emacs -Q --fg-daemon="$socket"`
with only the active profile's build directories and the Agent checkout on
`load-path`. Target every client call at the unique socket. Wait on conditions,
not fixed sleeps; require the result file, PASS content, and stopped daemon PID.

Run:

```bash
test/agent-codex-hot-reload-e2e.sh
```

Expected before Task 5 is present: FAIL at stale-generation repair or missing
completion advice. After Task 5: PASS and cleanup confirms no daemon/temp path.

- [ ] **Step 3: Add the Make target and rerun**

Add `test-hot-reload` to `.PHONY` and invoke the shell driver. Run both the new
target and the normal full suite.

- [ ] **Step 4: Commit the acceptance workflow**

```bash
git add test/agent-codex-hot-reload-e2e.el \
  test/agent-codex-hot-reload-e2e.sh Makefile
git commit -m "test: cover Agent hot reload and exit end to end"
```

## Task 7: Synchronize documentation, configuration, and paired artifacts

**Files:**
- Modify: `codex/hooks/README.org`
- Modify: `claude/hooks/README.org`
- Modify: `codex/README.org`
- Modify: `claude/README.org`
- Modify: `codex/skills/elisp-conventions/SKILL.md`
- Modify: `claude/skills/elisp-conventions/SKILL.md`
- Modify: `emacs/extras/doc/elisp-development-workflow.org`
- Modify: `ai-config-sync.json`
- Deploy: Claude hook timeout in `~/.claude/settings.json`

- [ ] **Step 1: Document the operational contract**

State plainly in both tool trees:

```text
An Elisp save creates durable reload debt before contacting Emacs. Short,
bounded requests still enqueue and poll asynchronous builds. Delivery, build,
reload, or receipt failure leaves debt visible across sessions and blocks
freshness-dependent work while allowing diagnosis and recovery. Batch checks
and cold-start Emacs processes do not clear active-daemon debt.
```

Document the state tool, recovery lanes, receipt fields, nested edit-envelope
coverage, and the difference between automatic reload debt and the separate
post-commit live-exercise requirement.

- [ ] **Step 2: Update paired manifest and deployed timeout**

Record the new paired utility/hook contracts in `ai-config-sync.json`. Update
the Claude runtime hook registration to match the tracked bounded timeout. Do
not change unrelated user settings.

- [ ] **Step 3: Run parity and documentation checks**

```bash
python3 -m unittest tests.test_ai_config_sync_audit tests.test_docs_audit -v
bin/ai-config-sync audit
git diff --check
```

Expected: relevant checks pass; any unrelated pre-existing audit finding is
reported separately rather than folded into this change.

- [ ] **Step 4: Commit documentation/configuration**

```bash
git add codex/hooks/README.org claude/hooks/README.org \
  codex/README.org claude/README.org \
  codex/skills/elisp-conventions/SKILL.md \
  claude/skills/elisp-conventions/SKILL.md \
  emacs/extras/doc/elisp-development-workflow.org ai-config-sync.json
git commit -m "docs: define fail-closed elisp reload workflow"
```

## Task 8: Full verification and controlled live recovery

**Files:**
- No new production files expected
- Verify both repositories and deployed hook configuration

- [ ] **Step 1: Run every supporting check fresh**

In dotfiles:

```bash
python3 -m unittest tests.test_elisp_reload_state tests.test_elisp_reload_hooks -v
emacs/extras/test/run-tests.sh --ci emacs/extras/test/elpaca-extras-test.el
~/My\ Drive/dotfiles/claude/bin/batch-test.sh elpaca-extras
bin/ai-config-sync audit
git diff --check
git status --short
```

In Agent:

```bash
~/My\ Drive/dotfiles/claude/bin/elisp-ert agent test/agent-codex-test.el
~/My\ Drive/dotfiles/claude/bin/batch-test.sh agent
make compile
make test
make test-hot-reload
git diff --check
git status --short
```

- [ ] **Step 2: Prove deployed hook registration**

Parse the tracked Codex and deployed Claude hook configurations. Assert that
direct edits and `functions.exec` reach the reload hook, the timeout exceeds the
internal bounded workflow, and the state root is outside Drive. Use recorded
payload fixtures to exercise both registrations without editing user files.

- [ ] **Step 3: Recover the active daemon only through a safe surface**

Do not signal, restart, or blindly type into the active Emacs daemon. If its
server queue is responsive, run the sanctioned tokenized rebuild/reload for
Agent, verify the matching receipt, confirm current mode wiring, and then start
a fresh disposable live Agent/Codex session in the active daemon. If the queue
remains wedged, report this exact blocked layer; the new isolated hot-reload
acceptance remains supporting proof and cannot be called verification of the
user's active daemon.

- [ ] **Step 4: Perform the decisive live `agent-exit` workflow**

Against a proven-current active daemon, start one disposable Agent Codex
session with two no-op local skills, invoke real `agent-exit`, and observe:

1. both skill commands in order in the persisted Codex rollout;
2. `/exit` submission after the second completion;
3. the displayed session buffer dying;
4. no lingering owned Codex process, buffer, skill fixture, or transcript.

Never use the user's existing `macos` or other conversation as the fixture.

- [ ] **Step 5: Report exact evidence**

Report commit identities, daemon/build/source receipt identities, supporting
test counts, hot-reload acceptance output, active-daemon result, and cleanup.
Do not say the original user-visible behavior is fixed unless Step 4 passes on
the intended active surface.
