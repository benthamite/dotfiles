# Drive-Compatible Uqbar Compatibility Wrappers Implementation Plan

> **Superseded:** Do not execute this plan. Its Drive baseline and sequencing
> rely on the invalid assumption that directory-symlink failures are not
> user-visible. Rewrite it from the revised workspace design before use.

**Goal:** Replace Uqbar's two Drive-rejected command symlinks with regular executable wrappers that preserve the behavior of the canonical extensionless Bash commands.

**Architecture:** `build` and `launch` remain the only implementations. Their `.py` compatibility names become strict Bash wrappers that resolve the repository directory and use `exec`, while a permanent standard-library test harness verifies representation, arguments, output, exit status, working directory, and signal behavior.

**Tech Stack:** Bash, Python 3 standard library, `unittest`, Git, Google Drive for desktop.

---

This is plan 3 of 4. Start only after the instruction-bridge plan has safely fast-forwarded Uqbar, committed its `CLAUDE.md` bridge, and reduced the native Drive count to 18. Execute in the real Drive-side Uqbar checkout.

## File map

- Create: `tests/test_launcher_wrappers.py` — permanent behavior and representation harness.
- Replace: `build.py` — mode `120000` symlink becomes a mode `100755` Bash wrapper.
- Replace: `launch.py` — mode `120000` symlink becomes a mode `100755` Bash wrapper.

Do not modify the canonical `build` or `launch`, unrelated untracked files, or the known out-of-scope `.vscode/launch.json` debugger configuration.

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
git ls-files --others --exclude-standard
~~~

Require:

- unrelated untracked paths match the privately recorded baseline;
- `CLAUDE.md` is a regular `100644` import bridge;
- `build` and `launch` are executable Bash files;
- `build.py → build` and `launch.py → launch` remain mode `120000` symlinks.

Stop if upstream semantics or working-tree state differ.

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

- [ ] **Step 1: Replace `build.py` safely**

Move only the `build.py` symlink to Trash. Create a regular file through `apply_patch` with:

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

- [ ] **Step 2: Replace `launch.py` safely**

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

Expected: canonical commands and wrappers all show mode `100755`; unrelated untracked files match the privately recorded baseline.

### Task 5: Commit and verify the Drive decrement

**Files:**

- Create: `tests/test_launcher_wrappers.py`
- Replace: `build.py`
- Replace: `launch.py`

- [ ] **Step 1: Stage exactly the wrapper change**

Run:

~~~bash
git add build.py
git add launch.py
git add tests/test_launcher_wrappers.py
git diff --cached --name-only
git diff --cached --summary
git diff --cached --check
~~~

Expected staged paths: exactly `build.py`, `launch.py`, and `tests/test_launcher_wrappers.py`.

- [ ] **Step 2: Commit**

Run:

~~~bash
git commit -m "chore: replace launcher symlinks with forwarding scripts"
~~~

Confirm unrelated untracked files match the privately recorded baseline. Do not push.

- [ ] **Step 3: Restart Drive and require the exact gate**

Record the restart timestamp, gracefully quit and relaunch Google Drive, and wait for the native UI to settle. Require `18 → 16`. Search only post-restart log entries for `UNSUPPORTED` plus `uqbar/build.py` or `uqbar/launch.py`; require no new match.

Do not begin the Enchant plan unless the count is exactly 16.
