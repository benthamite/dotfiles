#!/usr/bin/env python3
"""Offline runtime boundaries; fixture Yarn/tsx do not certify real installs."""

from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CLAUDE_SKILL = ROOT / "claude" / "skills" / "proofread"
CODEX_SKILL = ROOT / "codex" / "skills" / "proofread"
PUBLIC_FILES = (
    "package.json", "package-lock.json", "yarn.lock", "tsconfig.json",
    "scripts/install-runtime.sh", "scripts/run-with-runtime.sh",
    "scripts/resolve-runtime-dir.mjs",
)
NODE = shutil.which("node")
NATIVE_TS_LOADER = """import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';
const [script, ...args] = process.argv.slice(2);
process.argv = [process.execPath, resolve(script), ...args];
await import(pathToFileURL(resolve(script)).href);
"""


def copy_public_files(source: Path, target: Path) -> None:
    """Never enumerate or copy ignored/private files from the real skill."""
    for relative in PUBLIC_FILES:
        destination = target / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source / relative, destination)


@unittest.skipUnless(NODE, "Node is required for the runtime resolver")
class ProofreadRuntimeLocationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory(prefix="proofread-runtime-", dir="/tmp")
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name).resolve()
        self.home = self.root / "home"
        self.drive = self.home / "My Drive"
        self.skill_dir = self.drive / "proofread"
        copy_public_files(CLAUDE_SKILL, self.skill_dir)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.runtime = self.root / "runtime"
        self.record = self.root / "yarn-record.json"
        self.install_fake_tools()

    def install_fake_tools(self) -> None:
        yarn = self.bin / "yarn"
        yarn.write_text(
            f"#!{sys.executable}\n"
            "import json, os, pathlib, sys\n"
            "if '--version' in sys.argv[1:]:\n"
            "    pathlib.Path(os.environ['FIXTURE_RECORD']).with_suffix('.version.json').write_text(json.dumps(sys.argv[1:]))\n"
            "    print(os.environ.get('FIXTURE_YARN_VERSION', '1.22.22'))\n"
            "    sys.exit(0)\n"
            "args = sys.argv[1:]\n"
            "cwd = pathlib.Path.cwd()\n"
            "record = {'cwd': str(cwd), 'args': args, 'tmpdir': os.environ.get('TMPDIR'), 'files': sorted(p.name for p in cwd.iterdir()),\n"
            "          'package': (cwd / 'package.json').read_text(),\n"
            "          'lock': (cwd / 'yarn.lock').read_text()}\n"
            "pathlib.Path(os.environ['FIXTURE_RECORD']).write_text(json.dumps(record))\n"
            "(cwd / 'yarn-error.log').write_text('owned simulated install state')\n"
            "if os.environ.get('FIXTURE_YARN_FAIL'):\n"
            "    sys.exit(23)\n"
            "modules = pathlib.Path(args[args.index('--modules-folder') + 1])\n"
            "cli = modules / 'tsx' / 'dist' / 'cli.mjs'\n"
            "cli.parent.mkdir(parents=True, exist_ok=True)\n"
            f"cli.write_text({NATIVE_TS_LOADER!r})\n"
            "if '--cache-folder' in args:\n"
            "    cache = pathlib.Path(args[args.index('--cache-folder') + 1])\n"
            "    cache.mkdir(parents=True, exist_ok=True)\n"
            "    (cache / 'fixture-cache').write_text('owned')\n",
            encoding="utf-8",
        )
        yarn.chmod(0o700)
        trash = self.bin / "trash"
        trash.write_text(
            f"#!{sys.executable}\n"
            "import pathlib, shutil, sys\n"
            f"root = pathlib.Path({str(self.root)!r})\n"
            "for argument in sys.argv[1:]:\n"
            "    target = pathlib.Path(argument).resolve()\n"
            "    if not target.is_relative_to(root) or not target.name.startswith('.install-'):\n"
            "        raise SystemExit('refusing non-fixture cleanup')\n"
            "    shutil.rmtree(target)\n",
            encoding="utf-8",
        )
        trash.chmod(0o700)

    def env(self, **overrides: str) -> dict[str, str]:
        # Do not propagate real keys, provider selectors or runtime overrides.
        env = {
            "HOME": str(self.home),
            "PATH": os.pathsep.join((str(self.bin), str(Path(NODE).parent), "/usr/bin", "/bin")),
            "FIXTURE_RECORD": str(self.record),
            "PROOFREAD_RUNTIME_DIR": str(self.runtime),
        }
        env.update(overrides)
        return env

    def run_script(self, name: str, *args: str, env=None, cwd=None):
        return subprocess.run(
            ["/bin/sh", str(self.skill_dir / "scripts" / name), *args],
            cwd=cwd or self.skill_dir, env=env or self.env(),
            check=False, capture_output=True, text=True, timeout=15,
        )

    def install_runtime_stub(self, modules: Path) -> None:
        cli = modules / "tsx" / "dist" / "cli.mjs"
        cli.parent.mkdir(parents=True)
        cli.write_text(NATIVE_TS_LOADER, encoding="utf-8")

    def test_public_fixture_copy_excludes_unlisted_files(self) -> None:
        public_source = self.root / "public-source"
        copy_public_files(CLAUDE_SKILL, public_source)
        (public_source / ".env").write_text("owned sentinel; not a credential\n")
        (public_source / "ignored-private.txt").write_text("owned sentinel\n")
        destination = self.root / "public-copy"
        copy_public_files(public_source, destination)
        self.assertFalse((destination / ".env").exists())
        self.assertFalse((destination / "ignored-private.txt").exists())
        self.assertEqual(
            sorted(str(p.relative_to(destination)) for p in destination.rglob("*") if p.is_file()),
            sorted(PUBLIC_FILES),
        )

    def test_installer_stages_only_public_manifests_outside_drive(self) -> None:
        before = {p: (self.skill_dir / p).read_bytes() for p in PUBLIC_FILES}
        caller = self.drive / "unrelated-project"
        caller.mkdir()
        (caller / "package.json").write_text('{"name":"not-proofread"}')
        result = self.run_script("install-runtime.sh", cwd=caller, env=self.env(TMPDIR=str(self.drive / 'unused-temp')))
        self.assertEqual(result.returncode, 0, result.stderr)
        record = json.loads(self.record.read_text())
        stage = Path(record["cwd"])
        self.assertTrue(stage.is_relative_to(self.runtime))
        self.assertFalse(stage.is_relative_to(self.drive))
        self.assertEqual(record["files"], ["package.json", "tmp", "yarn.lock"])
        self.assertEqual(record["tmpdir"], str(stage / "tmp"))
        self.assertFalse((self.drive / "unused-temp").exists())
        self.assertEqual(json.loads(self.record.with_suffix('.version.json').read_text()), ["--no-default-rc", "--version"])
        self.assertEqual(record["package"], (self.skill_dir / "package.json").read_text())
        self.assertEqual(record["lock"], (self.skill_dir / "yarn.lock").read_text())
        for flag in ("--no-default-rc", "--frozen-lockfile", "--non-interactive", "--production=false"):
            self.assertIn(flag, record["args"])
        cache = Path(record["args"][record["args"].index("--cache-folder") + 1])
        self.assertTrue(cache.is_relative_to(stage))
        self.assertFalse(stage.exists())
        self.assertEqual(before, {p: (self.skill_dir / p).read_bytes() for p in PUBLIC_FILES})
        self.assertEqual(sorted(p.name for p in caller.iterdir()), ["package.json"])
        self.assertFalse((self.skill_dir / "node_modules").exists())

    def test_installer_failure_is_preserved_and_staging_is_cleaned(self) -> None:
        result = self.run_script("install-runtime.sh", env=self.env(FIXTURE_YARN_FAIL="1"))
        self.assertEqual(result.returncode, 23, result.stderr)
        stage = Path(json.loads(self.record.read_text())["cwd"])
        self.assertFalse(stage.exists())
        self.assertFalse((self.skill_dir / "yarn-error.log").exists())

    def test_installer_rejects_non_classic_yarn_without_install(self) -> None:
        result = self.run_script("install-runtime.sh", env=self.env(FIXTURE_YARN_VERSION="4.9.0"))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Yarn Classic 1.x", result.stderr)
        self.assertFalse(self.record.exists())

    def test_setup_and_execution_share_override_precedence(self) -> None:
        xdg = self.root / "xdg"
        env = self.env(XDG_DATA_HOME=str(xdg))
        setup = self.run_script("install-runtime.sh", env=env)
        self.assertEqual(setup.returncode, 0, setup.stderr)
        self.assertFalse(xdg.exists())
        script = self.root / "echo-runtime.mjs"
        script.write_text("console.log(process.env.PROOFREAD_RUNTIME_DIR);\n")
        result = self.run_script("run-with-runtime.sh", str(script), env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), str(self.runtime))

    def test_execution_preserves_external_node_modules_alias_runtime(self) -> None:
        self.runtime.mkdir()
        modules = self.root / "shared-dependencies"
        self.install_runtime_stub(modules)
        (self.runtime / "node_modules").symlink_to(modules, target_is_directory=True)
        package = modules / "fixture-package"
        package.mkdir()
        (package / "index.js").write_text("module.exports = 'owned fixture';\n")
        script = self.root / "resolve-package.mjs"
        script.write_text(
            "import { createRequire } from 'node:module';\n"
            "import { join } from 'node:path';\n"
            "const runtimeRequire = createRequire(join(process.env.PROOFREAD_RUNTIME_DIR, 'package.json'));\n"
            "console.log(runtimeRequire('fixture-package'));\n"
        )
        result = self.run_script("run-with-runtime.sh", str(script))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "owned fixture\n")

    def test_execution_rejects_missing_runtime(self) -> None:
        result = self.run_script("run-with-runtime.sh", "unused.ts")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("dependencies are not installed", result.stderr)
        self.assertFalse(self.runtime.exists())

    def assert_rejected(self, env: dict[str, str]) -> None:
        for name in ("install-runtime.sh", "run-with-runtime.sh"):
            with self.subTest(name=name):
                result = self.run_script(name, env=env)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("Refusing proofread runtime inside Google Drive", result.stderr)
                self.assertNotIn("\n    at ", result.stderr)
        self.assertFalse(self.record.exists())

    def test_setup_and_execution_reject_xdg_runtime_inside_drive(self) -> None:
        rejected = self.drive / "runtime-data"
        self.assert_rejected(self.env(PROOFREAD_RUNTIME_DIR="", XDG_DATA_HOME=str(rejected)))
        self.assertFalse(rejected.exists())

    def test_setup_and_execution_reject_runtime_symlink_into_drive(self) -> None:
        target = self.drive / "hidden-runtime"
        target.mkdir()
        self.runtime.symlink_to(target, target_is_directory=True)
        self.assert_rejected(self.env())
        self.assertFalse((target / "node_modules").exists())

    def test_setup_and_execution_reject_node_modules_symlink_into_drive(self) -> None:
        self.runtime.mkdir()
        target = self.drive / "node-modules-target"
        self.install_runtime_stub(target)
        (self.runtime / "node_modules").symlink_to(target, target_is_directory=True)
        self.assert_rejected(self.env())

    def test_setup_and_execution_reject_dangling_alias_into_drive(self) -> None:
        target = self.drive / "not-created" / "runtime"
        self.runtime.symlink_to(target, target_is_directory=True)
        self.assert_rejected(self.env())
        self.assertFalse(target.exists())


class ProofreadRuntimeMetadataTests(unittest.TestCase):
    def test_paired_public_runtime_files_match(self) -> None:
        for relative in PUBLIC_FILES:
            with self.subTest(relative=relative):
                self.assertEqual((CLAUDE_SKILL / relative).read_bytes(), (CODEX_SKILL / relative).read_bytes())

    def test_manifest_and_npm_lock_agree_without_dotenv(self) -> None:
        package = json.loads((CLAUDE_SKILL / "package.json").read_text())
        lock = json.loads((CLAUDE_SKILL / "package-lock.json").read_text())
        for kind in ("dependencies", "devDependencies"):
            self.assertEqual(package[kind], lock["packages"][""][kind])
        self.assertNotIn("dotenv", package["dependencies"])
        self.assertNotIn("node_modules/dotenv", lock["packages"])
        self.assertNotIn("dotenv@", (CLAUDE_SKILL / "yarn.lock").read_text())

    def test_typescript_checks_do_not_emit_into_drive(self) -> None:
        config = json.loads((CLAUDE_SKILL / "tsconfig.json").read_text())
        self.assertIs(config["compilerOptions"].get("noEmit"), True)
        self.assertIs(config["compilerOptions"].get("allowImportingTsExtensions"), True)

    def test_paired_docs_define_one_runtime_precedence_model(self) -> None:
        for skill_dir in (CLAUDE_SKILL, CODEX_SKILL):
            for name in ("SKILL.md", "README.md"):
                with self.subTest(skill_dir=skill_dir, name=name):
                    contents = (skill_dir / name).read_text()
                    self.assertIn("yarn -s setup-runtime", contents)
                    self.assertIn("PROOFREAD_RUNTIME_DIR, then XDG_DATA_HOME, then ~/.local/share/proofread", contents)
                    self.assertIn("Both the runtime root and node_modules destination are canonicalized", contents)


if __name__ == "__main__":
    unittest.main()
