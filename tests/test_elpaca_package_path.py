"""Exercise package-path containment through the real CLI with a fake resolver."""
from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "bin/elpaca-package-path"
FAKE_RESOLVER = """#!/usr/bin/env python3
import os
import sys
print(os.environ["PACKAGE_PATH_RESOLUTION"])
if os.environ.get("PACKAGE_PATH_RESOLVER_STATUS", "0") != "0":
    print("fixture resolver failed", file=sys.stderr)
sys.exit(int(os.environ.get("PACKAGE_PATH_RESOLVER_STATUS", "0")))
"""


class ElpacaPackagePathTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="elpaca-package-path-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()
        self.source = self.root / "source with spaces"
        self.source.mkdir()
        self.outside = self.root / "source with spaces-other"
        self.outside.mkdir()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.helper = self.bin / HELPER.name
        shutil.copyfile(HELPER, self.helper)
        self.helper.chmod(0o755)
        resolver = self.bin / "elpaca-package-resolve"
        resolver.write_text(FAKE_RESOLVER)
        resolver.chmod(0o755)
        self.env = dict(os.environ, PACKAGE_PATH_RESOLUTION=json.dumps({"source": str(self.source)}))

    def run_helper(self, *suffixes, resolution=None, resolver_status=0):
        env = dict(self.env, PACKAGE_PATH_RESOLVER_STATUS=str(resolver_status))
        if resolution is not None:
            env["PACKAGE_PATH_RESOLUTION"] = resolution
        return subprocess.run([str(self.helper), "fixture-package", *suffixes],
                              cwd=self.root, env=env, text=True, capture_output=True,
                              timeout=10, check=False)

    def assert_path(self, result, expected):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, str(expected) + "\n")

    def assert_refused(self, result):
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_plain_lookup_and_empty_suffix_preserve_package_root(self):
        self.assert_path(self.run_helper(), self.source)
        self.assert_path(self.run_helper(""), self.source)

    def test_existing_target_and_contained_dot_segments_are_canonical(self):
        directory = self.source / "docs"
        directory.mkdir()
        target = self.source / "package.el"
        target.write_text("fixture source\n")
        self.assert_path(self.run_helper("docs/../package.el"), target)
        self.assert_path(self.run_helper("./docs/../package.el"), target)

    def test_not_yet_existing_nested_child_is_allowed_without_creation(self):
        target = self.source / "new directory" / "nested" / "new.el"
        self.assert_path(self.run_helper("new directory/nested/new.el"), target)
        self.assertFalse(target.parent.exists())

    def test_source_alias_resolves_to_its_canonical_directory(self):
        alias = self.root / "source alias"
        alias.symlink_to(self.source, target_is_directory=True)
        resolution = json.dumps({"source": str(alias)})
        self.assert_path(self.run_helper(resolution=resolution), self.source)
        self.assert_path(self.run_helper("new.el", resolution=resolution), self.source / "new.el")

    def test_contained_symlink_parent_is_resolved_for_new_child(self):
        directory = self.source / "docs"
        directory.mkdir()
        (self.source / "alias").symlink_to(directory, target_is_directory=True)
        self.assert_path(self.run_helper("alias/not-yet-created.el"), directory / "not-yet-created.el")

    def test_parent_traversal_and_shared_prefix_sibling_are_rejected(self):
        for suffix in ["../outside.el", "../../outside.el", "../source with spaces-other/outside.el"]:
            with self.subTest(suffix=suffix):
                self.assert_refused(self.run_helper(suffix))

    def test_absolute_suffix_is_rejected_even_when_inside_source(self):
        for target in [self.outside / "outside.el", self.source / "inside.el"]:
            with self.subTest(target=target):
                self.assert_refused(self.run_helper(str(target)))

    def test_existing_symlink_escape_is_rejected(self):
        target = self.outside / "outside.el"
        target.write_text("independent fixture\n")
        (self.source / "outside.el").symlink_to(target)
        self.assert_refused(self.run_helper("outside.el"))
        self.assertEqual(target.read_text(), "independent fixture\n")

    def test_symlink_parent_escape_is_rejected_for_missing_descendants(self):
        for name, target in [("outside", self.outside), ("dangling", self.root / "missing outside")]:
            with self.subTest(name=name):
                (self.source / name).symlink_to(target, target_is_directory=True)
                self.assert_refused(self.run_helper(f"{name}/new directory/new.el"))
        self.assertFalse((self.outside / "new directory").exists())

    def test_symlink_loop_fails_without_a_path(self):
        (self.source / "loop").symlink_to("loop")
        self.assert_refused(self.run_helper("loop/new.el"))

    def test_resolver_nonzero_status_fails_even_with_valid_output(self):
        result = self.run_helper("new.el", resolver_status=7)
        self.assertEqual(result.returncode, 7)
        self.assertEqual(result.stdout, "")
        self.assertIn("fixture resolver failed", result.stderr)

    def test_invalid_or_unresolved_source_never_becomes_a_guessed_path(self):
        ordinary_file = self.root / "not a directory"
        ordinary_file.write_text("fixture file\n")
        for resolution in ["not json", "{}", '{"source":null}', '{"source":[]}',
                           '{"source":42}', '{"source":""}', '{"source":"relative"}',
                           json.dumps({"source": str(self.root / "missing source")}),
                           json.dumps({"source": str(ordinary_file)})]:
            with self.subTest(resolution=resolution):
                self.assert_refused(self.run_helper("new.el", resolution=resolution))


if __name__ == "__main__":
    unittest.main()
