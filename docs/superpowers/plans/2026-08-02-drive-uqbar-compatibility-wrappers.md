# Drive-Compatible Uqbar Compatibility Wrappers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Uqbar's two Drive-rejected command symlinks with regular executable wrappers that preserve the behavior of the canonical extensionless Bash commands.

**Architecture:** `build` and `launch` remain the only implementations. Their `.py` compatibility names become strict Bash wrappers that resolve the repository directory and use `exec`, while a permanent standard-library test harness verifies representation, arguments, output, exit status, working directory, and signal behavior.

**Tech Stack:** Bash, Python 3 standard library, `unittest`, Git, Google Drive for desktop.

---

This is program plan 10 of 12. Start only after the instruction-bridge plan has
safely fast-forwarded Uqbar, committed its `CLAUDE.md` bridge, preserved the
zero-directory-symlink invariant, and cleared both targeted bridge records.
Snapshot the remaining native error records by exact path and category.
Execute in the real Drive-side Uqbar checkout.

## File map

- Create: `tests/test_launcher_wrappers.py` — permanent behavior and representation harness.
- Replace: `build.py` — mode `120000` symlink becomes a mode `100755` Bash wrapper.
- Replace: `launch.py` — mode `120000` symlink becomes a mode `100755` Bash wrapper.
- Create outside Drive: `~/.local/state/drive-workspace-migration/uqbar-wrappers/<timestamp>/` — mode `0700` category manifest plus two path journals.

Do not modify the canonical `build` or `launch`, the two untracked files under `secrets/`, or the known out-of-scope `.vscode/launch.json` debugger configuration.

### Task 1: Revalidate the prerequisite state

**Files:**

- Inspect: `AGENTS.md`, `CLAUDE.md`, `build`, `launch`, `build.py`, `launch.py`

- [ ] **Step 1: Confirm the preceding plan's state**

Run:

~~~bash
git status --porcelain=v2 --branch
git ls-files -s AGENTS.md CLAUDE.md build launch build.py launch.py
head -1 build
head -1 launch
readlink build.py
readlink launch.py
test -f secrets/KNOWN_HOSTS.txt
test -f secrets/SSH_KEY.txt
~~~

Require:

- only the two known secret files are untracked;
- `CLAUDE.md` is a regular `100644` import bridge;
- `build` and `launch` are executable Bash files;
- `build.py → build` and `launch.py → launch` remain mode `120000` symlinks.

Stop if upstream semantics or working-tree state differ.

Create the external category directory and a mode-`0600` fsynced manifest.
Capture `build.py` and `launch.py` separately with `drive-workspace
capture-path --final-type file`, run `record-path-cloud` for each, and record
both journal references, canonical-command hashes, Git modes/status, native
baseline, and for each path either the exact pre-existing cloud object
ID/parent or the authoritative state `preexisting_cloud_object: absent` before
mutation. Do not invent an ID for a rejected symlink; more than one exact-parent
candidate is a blocker.

### Task 2: Add the failing wrapper test harness

**Files:**

- Create: `tests/test_launcher_wrappers.py`

- [ ] **Step 1: Create the complete test file**

~~~python
from __future__ import annotations

import shutil
import signal
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PAIRS = (("build.py", "build"), ("launch.py", "launch"))


class LauncherWrapperTests(unittest.TestCase):
    def assert_regular_executable(self, path: Path) -> None:
        self.assertFalse(path.is_symlink(), f"{path.name} must be a regular file")
        self.assertTrue(path.is_file())
        self.assertEqual(0o755, stat.S_IMODE(path.stat().st_mode))

    def install_wrapper_and_target(
        self,
        directory: Path,
        wrapper_name: str,
        target_name: str,
        target_text: str,
    ) -> Path:
        source = ROOT / wrapper_name
        self.assert_regular_executable(source)
        wrapper = directory / wrapper_name
        shutil.copy2(source, wrapper)

        target = directory / target_name
        target.write_text(target_text, encoding="utf-8")
        target.chmod(0o755)
        return wrapper

    def test_wrappers_are_regular_executable_valid_bash(self):
        for wrapper_name, _target_name in PAIRS:
            with self.subTest(wrapper=wrapper_name):
                wrapper = ROOT / wrapper_name
                self.assert_regular_executable(wrapper)
                subprocess.run(
                    ["bash", "-n", str(wrapper)],
                    check=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                )

    def test_wrappers_preserve_cwd_arguments_output_and_status(self):
        target_text = (
            "#!/usr/bin/env bash\n"
            'printf "cwd=<%s>\\n" "$PWD"\n'
            'for arg in "$@"; do printf "arg=<%s>\\n" "$arg"; done\n'
            'printf "stderr-marker\\n" >&2\n'
            "exit 37\n"
        )

        for wrapper_name, target_name in PAIRS:
            with self.subTest(wrapper=wrapper_name):
                with tempfile.TemporaryDirectory() as temporary:
                    root = Path(temporary)
                    caller = root / "caller with spaces"
                    caller.mkdir()
                    wrapper = self.install_wrapper_and_target(
                        root,
                        wrapper_name,
                        target_name,
                        target_text,
                    )
                    process = subprocess.run(
                        [str(wrapper), "plain", "two words", "*"],
                        cwd=caller,
                        check=False,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                    )

                    self.assertEqual(37, process.returncode)
                    self.assertEqual(
                        (
                            f"cwd=<{caller}>\n"
                            "arg=<plain>\n"
                            "arg=<two words>\n"
                            "arg=<*>\n"
                        ),
                        process.stdout,
                    )
                    self.assertEqual("stderr-marker\n", process.stderr)

    def test_wrappers_exec_the_canonical_process(self):
        target_text = (
            "#!/usr/bin/env bash\n"
            'printf "%s\\n" "$$"\n'
            "exec sleep 30\n"
        )

        for wrapper_name, target_name in PAIRS:
            with self.subTest(wrapper=wrapper_name):
                with tempfile.TemporaryDirectory() as temporary:
                    root = Path(temporary)
                    wrapper = self.install_wrapper_and_target(
                        root,
                        wrapper_name,
                        target_name,
                        target_text,
                    )
                    process = subprocess.Popen(
                        [str(wrapper)],
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                    )
                    try:
                        assert process.stdout is not None
                        canonical_pid = int(process.stdout.readline().strip())
                        self.assertEqual(process.pid, canonical_pid)
                        process.terminate()
                        self.assertEqual(
                            -signal.SIGTERM,
                            process.wait(timeout=5),
                        )
                    finally:
                        if process.poll() is None:
                            process.kill()
                            process.wait(timeout=5)


if __name__ == "__main__":
    unittest.main()
~~~

- [ ] **Step 2: Run the test and verify the representation failure**

Run:

~~~bash
python3 -m unittest discover -s tests -p 'test_launcher_wrappers.py' -v
~~~

Expected: all three tests fail because the two source paths are still symlinks.

### Task 3: Replace the command symlinks

**Files:**

- Replace: `build.py`
- Replace: `launch.py`
- Test: `tests/test_launcher_wrappers.py`

- [ ] **Step 1: Pause, confirm both journals, and replace `build.py` safely**

Pause Drive and run `drive-workspace confirm-path-paused --journal JOURNAL`
for both wrapper journals. Require both confirmations to be fresh. While Drive
remains paused, re-read both journals and require the exact symlink texts,
canonical-command hashes, Git modes/status, and authoritative cloud-preimage
states to match Task 1. Move only the `build.py` symlink to Trash. Create a
regular file through `apply_patch` with:

~~~bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$SCRIPT_DIR/build" "$@"
~~~

Set its mode with:

~~~bash
chmod 755 build.py
~~~

- [ ] **Step 2: Replace `launch.py` and resume only after local verification**

Move only the `launch.py` symlink to Trash. Create a regular file through `apply_patch` with:

~~~bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec "$SCRIPT_DIR/launch" "$@"
~~~

Set its mode with:

~~~bash
chmod 755 launch.py
~~~

While Drive remains paused, require both wrappers to be mode-`0755` regular
files with exact planned bytes, both canonical commands to retain their
journaled hashes, and the Git diff to show only the two intended mode changes.
On any mutation or immediate local-verification failure, run
`rollback-path-local` for each wrapper already mutated while paused, require
any untouched wrapper still matches its journal, then require both exact
symlinks and clean baseline Git state, resume, and stop. On success, resume
Drive before the parity, cloud, or native checks.

- [ ] **Step 3: Run focused verification**

Run:

~~~bash
bash -n build.py launch.py
python3 -m unittest discover -s tests -p 'test_launcher_wrappers.py' -v
git diff --summary -- build.py launch.py
git diff --check
~~~

Expected: three tests pass and both Git changes report mode `120000 → 100755`.

### Task 4: Compare the wrappers with the real commands

**Files:**

- Verify only: `build`, `build.py`, `launch`, `launch.py`

- [ ] **Step 1: Capture safe real-command behavior**

Create a temporary directory with `mktemp -d`. From a working directory outside Uqbar, run each of these four commands with `--help`, recording stdout, stderr, and exit status separately:

~~~text
/Users/pablostafforini/My Drive/repos/uqbar/build --help
/Users/pablostafforini/My Drive/repos/uqbar/build.py --help
/Users/pablostafforini/My Drive/repos/uqbar/launch --help
/Users/pablostafforini/My Drive/repos/uqbar/launch.py --help
~~~

Require the `build`/`build.py` pair and the `launch`/`launch.py` pair to have byte-identical stdout, byte-identical stderr, and identical status. The commands may update their disposable Docker Launcher environment as part of their existing canonical behavior; they must not start a project. Move the capture directory to Trash afterward.

- [ ] **Step 2: Verify repository representation**

Run:

~~~bash
test ! -L build.py
test ! -L launch.py
git ls-files -s build build.py launch launch.py
git status --short
~~~

Expected: canonical commands and wrappers all show mode `100755`; the two untracked secret files remain the only unrelated state.

### Task 5: Verify both native path transitions, then commit

**Files:**

- Create: `tests/test_launcher_wrappers.py`
- Replace: `build.py`
- Replace: `launch.py`

- [ ] **Step 1: Review exactly the uncommitted wrapper change**

Run:

~~~bash
git diff --name-only -- build.py launch.py
git diff --summary -- build.py launch.py
git diff --check -- build.py launch.py
git status --short --untracked-files=all
~~~

Require exactly the two intended tracked wrapper changes, the new untracked
`tests/test_launcher_wrappers.py`, and the two pre-existing untracked secret
files. Nothing from this plan may be staged yet.

- [ ] **Step 2: Keep the wrapper change unstaged and uncommitted**

Confirm `secrets/KNOWN_HOSTS.txt` and `secrets/SSH_KEY.txt` remain present and
untracked. Do not stage, commit, or push any plan path until the local, cloud,
and native transaction closes.

- [ ] **Step 3: Restart Drive and require both wrapper errors to clear**

Record the restart timestamp, gracefully quit and relaunch Google Drive, and
wait for the native UI to settle. Require the prior `uqbar/build.py` and
`uqbar/launch.py` records to clear, with no new record for either regular
wrapper. Search only post-restart log entries for `UNSUPPORTED` plus those
paths; require no new match. Require every other baseline record to remain
unchanged.

Run `verify-path-local` and `verify-path-cloud` for both wrappers, append their
native transitions, fsync the category manifest, and require two closed
transactions. If either local, cloud, parity, or native check fails, pause
Drive and obtain a fresh successful `confirm-path-paused` record for both
journals. For each journal with pre-existing object IDs, run
`restore-path-cloud` for only those exact IDs. For a journal with
`preexisting_cloud_object: absent`, skip cloud restore and require absence at
the exact parent. Run `rollback-path-local` for both paths while paused, move
only the newly created untracked test file to Trash, and require the two
original symlinks, clean baseline Git state, authoritative cloud-preimage
states, and baseline native records before resuming and stopping.

Only after all path-level checks pass, run:

~~~bash
git add build.py
git add launch.py
git add tests/test_launcher_wrappers.py
git diff --cached --name-only
git diff --cached --summary
git diff --cached --check
git commit -m "chore: replace launcher symlinks with forwarding scripts"
~~~

Require the staged paths before commit to be exactly `build.py`, `launch.py`,
and `tests/test_launcher_wrappers.py`; then require one local commit, no push,
and final `verify-path-local` results that record both committed mode-`100755`
wrappers.

Do not begin the Enchant plan unless both wrapper paths pass this path-level
gate.
