# Drive-Compatible Enchant Runtime Implementation Plan

> **Superseded:** Do not execute this plan. Its Drive baseline and sequencing
> rely on the invalid assumption that directory-symlink failures are not
> user-visible. Rewrite it from the revised workspace design before use.

**Goal:** Move all writable Enchant runtime symlinks outside Google Drive while preserving each dictionary's existing canonical file and reducing the final 16 Drive errors to zero.

**Architecture:** A tested installer owns a real `~/.config/enchant` directory containing 19 direct absolute symlinks to canonical dictionary and ordering files. The installer validates the whole target and live layout before mutation, migrates the exact legacy directory symlink through Trash, repairs only known link names, and refuses unknown content.

**Tech Stack:** Python 3 standard library, `unittest`, Enchant 2, Emacs/Jinx, Org documentation, Git, Google Drive for desktop.

---

This is plan 4 of 4. Start only after the Uqbar wrapper plan has reduced the native Drive count to 16. Execute in the real dotfiles checkout because both the live Enchant path and the Drive count are acceptance surfaces.

## File map

- Create: `bin/install-enchant-config` — idempotent fail-closed installer, mode `100755`.
- Create: `tests/test_install_enchant_config.py` — disposable layout and Enchant integration tests.
- Modify: `README.org` — canonical files, runtime layout, and repair command.
- Delete: `enchant/{ar,de,en,es,fr,it,ru,tr}.{dic,exc}` — exactly 16 mode-`120000` forwarding links.
- Preserve unchanged: `enchant/pl.dic`, `enchant/pl.exc`, and `enchant/enchant.ordering`.

No Emacs configuration, language repository, ignore rule, or Jinx package source changes belong in this plan.

### Task 1: Add the failing installer tests

**Files:**

- Create: `tests/test_install_enchant_config.py`

- [ ] **Step 1: Create the complete test file**

~~~python
from __future__ import annotations

import os
import shutil
import stat
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "install-enchant-config"
LANGUAGES = ("ar", "de", "en", "es", "fr", "it", "ru", "tr")
EXPECTED_NAMES = {
    *(
        f"{language}.{extension}"
        for language in LANGUAGES
        for extension in ("dic", "exc")
    ),
    "pl.dic",
    "pl.exc",
    "enchant.ordering",
}


class InstallEnchantConfigTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.dotfiles = self.root / "dotfiles"
        self.repos = self.root / "repos"
        self.config = self.root / "runtime" / "enchant"
        self.fake_bin = self.root / "bin"
        self.fake_trash = self.root / "trash"
        self.fake_bin.mkdir()
        self.fake_trash.mkdir()

        for language in LANGUAGES:
            dictionary = self.repos / language / "dict"
            dictionary.mkdir(parents=True)
            (dictionary / f"{language}.dic").write_text(
                "CodexExistingPwlZqv\n" if language == "en" else "",
                encoding="utf-8",
            )
            (dictionary / f"{language}.exc").write_text("", encoding="utf-8")

        tracked = self.dotfiles / "enchant"
        tracked.mkdir(parents=True)
        (tracked / "pl.dic").write_text("", encoding="utf-8")
        (tracked / "pl.exc").write_text("", encoding="utf-8")
        (tracked / "enchant.ordering").write_text("*:aspell\n", encoding="utf-8")

        fake = self.fake_bin / "trash"
        fake.write_text(
            textwrap.dedent(
                """\
                #!/bin/sh
                set -eu
                /bin/mv "$1" "$FAKE_TRASH_DIR/enchant"
                """
            ),
            encoding="utf-8",
        )
        fake.chmod(fake.stat().st_mode | stat.S_IXUSR)

    def run_installer(self):
        environment = dict(os.environ)
        environment["PATH"] = f"{self.fake_bin}:{environment['PATH']}"
        environment["FAKE_TRASH_DIR"] = str(self.fake_trash)
        return subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--config-root",
                str(self.config),
                "--dotfiles-root",
                str(self.dotfiles),
                "--repos-root",
                str(self.repos),
            ],
            env=environment,
            capture_output=True,
            text=True,
        )

    def target_for(self, name):
        if name in {"pl.dic", "pl.exc", "enchant.ordering"}:
            return self.dotfiles / "enchant" / name
        language = name.split(".", 1)[0]
        return self.repos / language / "dict" / name

    def assert_complete_layout(self):
        self.assertTrue(self.config.is_dir())
        self.assertFalse(self.config.is_symlink())
        self.assertEqual(
            {child.name for child in self.config.iterdir()},
            EXPECTED_NAMES,
        )
        for name in EXPECTED_NAMES:
            link = self.config / name
            self.assertTrue(link.is_symlink(), name)
            self.assertEqual(link.resolve(), self.target_for(name).resolve())

    def test_fresh_setup_creates_complete_real_directory(self):
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_complete_layout()
        self.assertIn("Installed Enchant config", result.stdout)

    def test_rerun_is_idempotent(self):
        self.assertEqual(self.run_installer().returncode, 0)
        directory_inode = self.config.stat().st_ino
        link_inodes = {
            name: (self.config / name).lstat().st_ino
            for name in EXPECTED_NAMES
        }
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("already current", result.stdout)
        self.assertEqual(self.config.stat().st_ino, directory_inode)
        self.assertEqual(
            {
                name: (self.config / name).lstat().st_ino
                for name in EXPECTED_NAMES
            },
            link_inodes,
        )
        self.assertEqual(list(self.fake_trash.iterdir()), [])

    def test_repairs_known_stale_link_only(self):
        self.assertEqual(self.run_installer().returncode, 0)
        untouched_inode = (self.config / "pl.dic").lstat().st_ino
        stale = self.root / "stale-en.dic"
        stale.write_text("", encoding="utf-8")
        (self.config / "en.dic").unlink()
        (self.config / "en.dic").symlink_to(stale)
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_complete_layout()
        self.assertEqual(
            (self.config / "pl.dic").lstat().st_ino,
            untouched_inode,
        )
        self.assertIn("1 repaired", result.stdout)

    def test_refuses_unknown_content_without_mutation(self):
        self.config.mkdir(parents=True)
        unknown = self.config / "notes.txt"
        unknown.write_text("keep me", encoding="utf-8")
        result = self.run_installer()
        self.assertEqual(result.returncode, 1)
        self.assertIn("unexpected entries", result.stderr)
        self.assertEqual(unknown.read_text(), "keep me")
        self.assertEqual(
            {child.name for child in self.config.iterdir()},
            {"notes.txt"},
        )

    def test_refuses_non_symlink_at_expected_path(self):
        self.config.mkdir(parents=True)
        conflict = self.config / "en.dic"
        conflict.write_text("keep me", encoding="utf-8")
        result = self.run_installer()
        self.assertEqual(result.returncode, 1)
        self.assertIn("non-symlink content", result.stderr)
        self.assertEqual(conflict.read_text(), "keep me")

    def test_missing_target_prevents_any_repair(self):
        self.assertEqual(self.run_installer().returncode, 0)
        stale = self.root / "stale-en.dic"
        stale.write_text("", encoding="utf-8")
        live = self.config / "en.dic"
        live.unlink()
        live.symlink_to(stale)
        missing = self.repos / "tr" / "dict" / "tr.exc"
        missing.unlink()
        result = self.run_installer()
        self.assertEqual(result.returncode, 1)
        self.assertIn("canonical target is not a regular file", result.stderr)
        self.assertEqual(live.resolve(), stale.resolve())
        self.assertEqual(
            list(self.config.parent.glob(".enchant-stage-*")),
            [],
        )

    def test_migrates_exact_legacy_directory_symlink_via_trash(self):
        self.config.parent.mkdir(parents=True)
        self.config.symlink_to(
            self.dotfiles / "enchant",
            target_is_directory=True,
        )
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_complete_layout()
        trashed = self.fake_trash / "enchant"
        self.assertTrue(trashed.is_symlink())
        self.assertEqual(
            trashed.resolve(),
            (self.dotfiles / "enchant").resolve(),
        )

    def test_refuses_unexpected_top_level_symlink(self):
        other = self.root / "other"
        other.mkdir()
        self.config.parent.mkdir(parents=True)
        self.config.symlink_to(other, target_is_directory=True)
        result = self.run_installer()
        self.assertEqual(result.returncode, 1)
        self.assertIn("unexpected config symlink", result.stderr)
        self.assertTrue(self.config.is_symlink())
        self.assertEqual(self.config.resolve(), other.resolve())
        self.assertEqual(list(self.fake_trash.iterdir()), [])

    @unittest.skipUnless(shutil.which("enchant-2"), "enchant-2 not installed")
    def test_disposable_dictionary_read_and_write_through_runtime_link(self):
        result = self.run_installer()
        self.assertEqual(result.returncode, 0, result.stderr)
        environment = dict(os.environ, ENCHANT_CONFIG_DIR=str(self.config))
        added = "CodexAddedPwlQrx"
        add = subprocess.run(
            ["enchant-2", "-a", "-d", "en"],
            input=f"*{added}\n#\n",
            env=environment,
            capture_output=True,
            text=True,
        )
        if add.returncode:
            self.skipTest(f"English Aspell provider unavailable: {add.stderr}")
        canonical = self.repos / "en" / "dict" / "en.dic"
        self.assertIn(added, canonical.read_text().splitlines())
        self.assertTrue((self.config / "en.dic").is_symlink())
        check = subprocess.run(
            ["enchant-2", "-l", "-d", "en"],
            input=(
                "CodexExistingPwlZqv\n"
                f"{added}\n"
                "DefinitelyNotAWordZqxy\n"
            ),
            env=environment,
            capture_output=True,
            text=True,
            check=True,
        )
        self.assertEqual(
            check.stdout.splitlines(),
            ["DefinitelyNotAWordZqxy"],
        )


if __name__ == "__main__":
    unittest.main()
~~~

- [ ] **Step 2: Run the focused suite and verify failure**

Run:

~~~bash
python3 -m unittest discover -s tests -p 'test_install_enchant_config.py' -v
~~~

Expected: tests fail because `bin/install-enchant-config` does not exist.

### Task 2: Implement the fail-closed installer

**Files:**

- Create: `bin/install-enchant-config`
- Test: `tests/test_install_enchant_config.py`

- [ ] **Step 1: Create the complete installer**

~~~python
#!/usr/bin/env python3
"""Install Enchant runtime links outside the Google Drive mirror."""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
REPOSITORY_LANGUAGES = ("ar", "de", "en", "es", "fr", "it", "ru", "tr")


class InstallError(RuntimeError):
    """Raised when the Enchant layout cannot be changed safely."""


def lexical_absolute(value: str | os.PathLike[str]) -> Path:
    return Path(os.path.abspath(os.path.expanduser(os.fspath(value))))


def lexists(path: Path) -> bool:
    return os.path.lexists(os.fspath(path))


def expected_links(
    dotfiles_root: Path,
    repos_root: Path,
) -> dict[str, Path]:
    links = {
        f"{language}.{extension}":
        repos_root / language / "dict" / f"{language}.{extension}"
        for language in REPOSITORY_LANGUAGES
        for extension in ("dic", "exc")
    }
    links.update(
        {
            "pl.dic": dotfiles_root / "enchant" / "pl.dic",
            "pl.exc": dotfiles_root / "enchant" / "pl.exc",
            "enchant.ordering":
                dotfiles_root / "enchant" / "enchant.ordering",
        }
    )
    return links


def validate_targets(links: dict[str, Path]) -> None:
    problems = []
    for name, target in links.items():
        if target.is_symlink() or not target.is_file():
            problems.append(
                f"{name}: canonical target is not a regular file: {target}"
            )
    if problems:
        raise InstallError("\n".join(problems))


def resolved_link_target(link: Path) -> Path:
    raw = Path(os.readlink(link))
    candidate = raw if raw.is_absolute() else link.parent / raw
    return candidate.resolve(strict=False)


def link_matches(link: Path, target: Path) -> bool:
    return (
        link.is_symlink()
        and resolved_link_target(link) == target.resolve(strict=True)
    )


def inspect_live(
    config_root: Path,
    dotfiles_root: Path,
    links: dict[str, Path],
) -> str:
    if not lexists(config_root):
        return "absent"
    if config_root.is_symlink():
        legacy = (dotfiles_root / "enchant").resolve(strict=True)
        if resolved_link_target(config_root) == legacy:
            return "legacy"
        raise InstallError(
            f"unexpected config symlink: {config_root} -> "
            f"{os.readlink(config_root)}"
        )
    if not config_root.is_dir():
        raise InstallError(f"config root is not a directory: {config_root}")

    unknown = sorted(
        child.name
        for child in config_root.iterdir()
        if child.name not in links
    )
    if unknown:
        raise InstallError(
            "unexpected entries in config directory: " + ", ".join(unknown)
        )

    conflicts = []
    for name in links:
        live = config_root / name
        if lexists(live) and not live.is_symlink():
            conflicts.append(str(live))
    if conflicts:
        raise InstallError(
            "expected link paths have non-symlink content: "
            + ", ".join(conflicts)
        )
    return "directory"


def remove_owned_stage(stage: Path) -> None:
    if not stage.exists():
        return
    for child in stage.iterdir():
        if not child.is_symlink():
            raise InstallError(
                f"refusing to remove unexpected staged content: {child}"
            )
        child.unlink()
    stage.rmdir()


def build_stage(config_root: Path, links: dict[str, Path]) -> Path:
    config_root.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(
        tempfile.mkdtemp(
            prefix=".enchant-stage-",
            dir=config_root.parent,
        )
    )
    try:
        for name, target in links.items():
            (stage / name).symlink_to(target)
        for name, target in links.items():
            if not link_matches(stage / name, target):
                raise InstallError(f"staged link validation failed: {name}")
        return stage
    except Exception:
        remove_owned_stage(stage)
        raise


def move_to_trash(path: Path) -> None:
    trash_command = shutil.which("trash")
    if trash_command is None:
        raise InstallError("trash command not found")
    result = subprocess.run(
        [trash_command, str(path)],
        capture_output=True,
        text=True,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        raise InstallError(
            f"could not move legacy config to Trash: {detail}"
        )


def install(
    config_root: Path,
    dotfiles_root: Path,
    repos_root: Path,
) -> str:
    links = expected_links(dotfiles_root, repos_root)
    validate_targets(links)
    state = inspect_live(config_root, dotfiles_root, links)

    if state == "directory":
        changes = [
            name
            for name, target in links.items()
            if not link_matches(config_root / name, target)
        ]
        if not changes:
            return f"Enchant config already current: {config_root}"
    else:
        changes = list(links)

    stage = build_stage(config_root, links)
    try:
        if state == "absent":
            os.replace(stage, config_root)
            return (
                f"Installed Enchant config: {config_root} "
                f"({len(links)} links)"
            )

        if state == "legacy":
            legacy_target = os.readlink(config_root)
            move_to_trash(config_root)
            try:
                os.replace(stage, config_root)
            except Exception:
                if not lexists(config_root):
                    config_root.symlink_to(
                        legacy_target,
                        target_is_directory=True,
                    )
                raise
            return (
                f"Migrated Enchant config: {config_root} "
                f"({len(links)} links)"
            )

        created = 0
        repaired = 0
        for name in changes:
            live = config_root / name
            if lexists(live):
                repaired += 1
            else:
                created += 1
            os.replace(stage / name, live)
        return (
            f"Repaired Enchant config: {config_root} "
            f"({created} created, {repaired} repaired)"
        )
    finally:
        if stage.exists():
            remove_owned_stage(stage)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Install Enchant runtime links outside Google Drive."
    )
    parser.add_argument(
        "--config-root",
        type=lexical_absolute,
        default=lexical_absolute("~/.config/enchant"),
    )
    parser.add_argument(
        "--dotfiles-root",
        type=lexical_absolute,
        default=ROOT,
    )
    parser.add_argument("--repos-root", type=lexical_absolute)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    repos_root = args.repos_root or args.dotfiles_root.parent / "repos"
    try:
        print(install(args.config_root, args.dotfiles_root, repos_root))
    except (InstallError, OSError) as error:
        print(f"install-enchant-config: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
~~~

- [ ] **Step 2: Make the installer executable**

Run:

~~~bash
chmod 755 bin/install-enchant-config
~~~

- [ ] **Step 3: Run focused tests**

Run:

~~~bash
python3 -m py_compile bin/install-enchant-config tests/test_install_enchant_config.py
python3 -m unittest discover -s tests -p 'test_install_enchant_config.py' -v
~~~

Expected: nine passing tests. The Enchant integration test should run on this machine; a skip is acceptable only if the installed provider is genuinely unavailable and the reason is reported.

### Task 3: Document the runtime contract

**Files:**

- Modify: `README.org`

- [ ] **Step 1: Insert the Enchant section before Skills**

~~~org
* Enchant

Personal Enchant dictionaries remain canonical in their language repositories.
The Polish dictionary and provider ordering remain canonical in
=file:enchant/=.  Google Drive cannot sync the file symlinks that previously
forwarded through this repository, so the live configuration is now a real
directory outside Drive:

#+begin_src text
~/.config/enchant/<language>.dic -> ~/My Drive/repos/<language>/dict/<language>.dic
#+end_src

Create or repair the complete runtime layout with:

#+begin_src sh
~/bin/install-enchant-config
#+end_src

The installer validates every canonical target before changing the live
directory.  It repairs missing or stale expected links, but refuses unknown
files, non-link conflicts, unexpected top-level links, and missing targets.
For disposable tests, =--config-root=, =--dotfiles-root=, and =--repos-root=
override the three layout roots.
~~~

- [ ] **Step 2: Run documentation and full tests**

Run:

~~~bash
git diff --check
python3 -m unittest discover -s tests -v
bin/ai-config-sync audit
~~~

Expected: full tests and the configuration audit pass.

### Task 4: Migrate the live runtime before deleting forwarding links

**Files:**

- Replace live state: `~/.config/enchant`
- Preserve canonical targets under `repos/` and `dotfiles/enchant/`

- [ ] **Step 1: Record exact checksums for all 19 canonical files**

Generate a mode-`0600` temporary checksum file covering:

- `repos/{ar,de,en,es,fr,it,ru,tr}/dict/<lang>.{dic,exc}`;
- `dotfiles/enchant/pl.dic`;
- `dotfiles/enchant/pl.exc`;
- `dotfiles/enchant/enchant.ordering`.

Sort by absolute path before hashing so the post-migration comparison is deterministic. Confirm the current top-level path is the exact legacy symlink:

~~~bash
test -L "$HOME/.config/enchant"
test "$(readlink "$HOME/.config/enchant")" = "/Users/pablostafforini/My Drive/dotfiles/enchant"
~~~

- [ ] **Step 2: Run the installer while all 16 forwarding links still exist**

Run:

~~~bash
~/bin/install-enchant-config
~~~

Expected:

~~~text
Migrated Enchant config: /Users/pablostafforini/.config/enchant (19 links)
~~~

- [ ] **Step 3: Verify the complete live layout**

Run:

~~~bash
test -d "$HOME/.config/enchant"
test ! -L "$HOME/.config/enchant"
python3 - <<'PY'
from pathlib import Path

home = Path.home()
config = home / ".config" / "enchant"
dotfiles = home / "My Drive" / "dotfiles"
repos = home / "My Drive" / "repos"
languages = ("ar", "de", "en", "es", "fr", "it", "ru", "tr")
expected = {
    **{
        f"{language}.{extension}":
        repos / language / "dict" / f"{language}.{extension}"
        for language in languages
        for extension in ("dic", "exc")
    },
    "pl.dic": dotfiles / "enchant" / "pl.dic",
    "pl.exc": dotfiles / "enchant" / "pl.exc",
    "enchant.ordering": dotfiles / "enchant" / "enchant.ordering",
}
assert {child.name for child in config.iterdir()} == set(expected)
for name, target in expected.items():
    link = config / name
    assert link.is_symlink(), name
    assert link.resolve(strict=True) == target.resolve(strict=True), name
print("verified 19 direct runtime links")
PY
~~~

Recompute and byte-compare the canonical checksum list. Require no changed target.

- [ ] **Step 4: Verify real Enchant and Jinx reads**

Run:

~~~bash
printf 'Aaronson\nDefinitelyNotAWordZqxy\n' | enchant-2 -l -d en
~~~

Expected:

~~~text
DefinitelyNotAWordZqxy
~~~

Run:

~~~bash
emacsclient -e \
  '(progn
     (require (quote jinx))
     (jinx--load-module)
     (let ((dict (jinx--mod-dict "en")))
       (and dict (jinx--mod-check dict "Aaronson"))))'
~~~

Expected: `t`. This is read-only and must not add a test word to canonical user data.

### Task 5: Remove the Drive-side forwarding links and commit

**Files:**

- Delete exactly: `enchant/{ar,de,en,es,fr,it,ru,tr}.{dic,exc}`
- Keep: `enchant/pl.dic`, `enchant/pl.exc`, `enchant/enchant.ordering`

- [ ] **Step 1: Move the 16 tracked symlinks to Trash**

Resolve and inspect their Git object modes first. Require mode `120000` for all 16. Then pass the 16 explicit paths to `trash`; do not use a wildcard that could include the three regular canonical dotfiles files.

- [ ] **Step 2: Verify runtime behavior after deletion**

Run:

~~~bash
find enchant -maxdepth 1 -type l -print
printf 'Aaronson\nDefinitelyNotAWordZqxy\n' | enchant-2 -l -d en
~/bin/install-enchant-config
~~~

Expected: `find` prints nothing, Enchant prints only `DefinitelyNotAWordZqxy`, and the installer reports `already current`.

- [ ] **Step 3: Stage only the Enchant category**

Run:

~~~bash
git add bin/install-enchant-config
git add tests/test_install_enchant_config.py
git add README.org
git add -u -- enchant
git diff --cached --name-only
git diff --cached --summary
git diff --cached --check
git ls-files -s bin/install-enchant-config tests/test_install_enchant_config.py README.org enchant
~~~

Require:

- installer mode `100755`;
- test, README, Polish files, and ordering file mode `100644`;
- none of the 16 deleted links remains in the index;
- unrelated concurrent changes and plan files are not staged.

- [ ] **Step 4: Run final repository verification and commit**

Run:

~~~bash
python3 -m py_compile bin/install-enchant-config tests/test_install_enchant_config.py
python3 -m unittest discover -s tests -p 'test_install_enchant_config.py' -v
python3 -m unittest discover -s tests -v
bin/ai-config-sync audit
git diff --cached --check
git commit -m "enchant: move runtime links outside Drive"
~~~

Expected: one local dotfiles commit; no push.

### Task 6: Prove Drive reaches and retains zero

**Files:**

- Verify live Google Drive state and post-restart logs.

- [ ] **Step 1: Require `16 → 0`**

Record a UTC restart timestamp, gracefully quit and relaunch Google Drive, and wait for settling. Inspect the native menu-bar error panel and require exactly zero user-visible errors. Search only log entries after the restart for `UNSUPPORTED` plus any removed `dotfiles/enchant` path; require no new match.

- [ ] **Step 2: Run a second fresh restart**

Restart Drive again, wait for settling, and require the native panel to remain at zero. Internal directory-symlink `PARTIAL_RESULTS` may remain in logs, but report them separately and fail this step if any become user-visible.

- [ ] **Step 3: Recheck routing and all final invariants**

Require:

- the registered Drive root still has `is_my_drive=1`, the expected My Drive document ID, and zero `machine_root` rows;
- no unexpected cloud items were created since the restart baseline;
- all externalized generated-directory links remain healthy;
- all 36 Epoch `.url` links remain normal files;
- both `CLAUDE.md` bridges and both Uqbar wrappers remain regular files with the expected modes;
- `~/.config/enchant` remains a real 19-link directory;
- all commits remain local and no repository was pushed.

Only after these direct checks pass may the original 190-error task be reported as complete.
