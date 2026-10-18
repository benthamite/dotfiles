import importlib.machinery
import importlib.util
import io
import json
import os
import pathlib
import plistlib
import tempfile
import unittest
from contextlib import redirect_stdout


ROOT = pathlib.Path(__file__).resolve().parents[1]
SKILLS = [ROOT / "macos" / tool / "skills" / "security-audit" for tool in (".claude", ".codex")]
SCRIPT = SKILLS[0] / "scripts" / "lulu-rules.py"


def load_module():
    loader = importlib.machinery.SourceFileLoader("lulu_rules", str(SCRIPT))
    spec = importlib.util.spec_from_loader("lulu_rules", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_rules(path, rules):
    """Write rules as LuLu's NSKeyedArchiver layout: values referenced by UID."""
    objects = ["$null"]

    def ref(value):
        objects.append(value)
        return plistlib.UID(len(objects) - 1)

    for rule in rules:
        item = {}
        objects.append(item)
        for key, value in rule.items():
            item[key] = ref(value) if isinstance(value, str) else value
    with path.open("wb") as handle:
        plistlib.dump({"$version": 100000, "$archiver": "NSKeyedArchiver",
                       "$top": {"root": plistlib.UID(1)}, "$objects": objects}, handle,
                      fmt=plistlib.FMT_BINARY)


def rule(path, action=1, addr="*", port="*", disabled="$null"):
    return {"path": path, "action": action, "endpointAddr": addr, "endpointPort": port,
            "isDisabled": disabled}


class LuluRulesTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.base = pathlib.Path(self.directory.name)
        self.existing = self.base / "bin"
        self.existing.mkdir()
        for name in ("python3.14", "curl", "Rectangle"):
            (self.existing / name).touch()

    def tearDown(self):
        self.directory.cleanup()

    def report(self):
        output = io.StringIO()
        with redirect_stdout(output):
            self.mod.main(["--dir", str(self.base)])
        return json.loads(output.getvalue())

    def flagged(self):
        return {entry["path"]: entry for entry in self.report()["rules"]["flagged"]}

    def test_interpreter_allowed_to_any_endpoint_is_flagged(self):
        write_rules(self.base / "rules.plist", [rule(str(self.existing / "python3.14"))])
        [entry] = self.flagged().values()
        self.assertTrue(entry["generic_any_endpoint"])
        self.assertNotIn("stale", entry)

    def test_directory_rule_for_interpreter_package_is_flagged(self):
        write_rules(self.base / "rules.plist", [rule(str(self.base) + "/Cellar/node/*")])
        [entry] = self.flagged().values()
        self.assertTrue(entry["generic_any_endpoint"])
        self.assertTrue(entry["stale"])

    def test_scoped_endpoint_block_rule_and_specific_app_are_not_flagged(self):
        write_rules(self.base / "rules.plist", [
            rule(str(self.existing / "curl"), addr="api.example.com", port="443"),
            rule(str(self.existing / "python3.14"), action=0),
            rule(str(self.existing / "Rectangle")),
            rule(str(self.existing / "python3.14"), disabled=True),
        ])
        self.assertEqual(self.flagged(), {})

    def test_rule_on_deleted_program_is_stale(self):
        gone = str(self.base / "Cellar/foo/1.0/bin/foo")
        write_rules(self.base / "rules.plist", [rule(gone)])
        self.assertEqual(self.flagged()[gone], {"path": gone, "stale": True})

    def test_permissive_preferences_are_reported(self):
        with (self.base / "preferences.plist").open("wb") as handle:
            plistlib.dump({"allowApple": True, "allowInstalled": True, "blockMode": False,
                           "passiveMode": False}, handle)
        preferences = self.report()["preferences"]
        self.assertEqual(preferences["permissive"], {"allowApple": True, "allowInstalled": True})

    def test_unreadable_and_missing_files_are_distinct_from_empty(self):
        (self.base / "rules.plist").write_bytes(b"not a plist")
        report = self.report()
        self.assertEqual(report["rules"]["status"], "unreadable")
        self.assertEqual(report["preferences"]["status"], "missing")

    def test_helper_does_not_modify_the_rules(self):
        path = self.base / "rules.plist"
        write_rules(path, [rule(str(self.existing / "curl"))])
        before = (path.read_bytes(), os.stat(path).st_mtime_ns)
        self.report()
        self.assertEqual((path.read_bytes(), os.stat(path).st_mtime_ns), before)

    def test_claude_and_codex_copies_are_identical(self):
        self.assertEqual(*(skill.joinpath("scripts/lulu-rules.py").read_bytes() for skill in SKILLS))


if __name__ == "__main__":
    unittest.main()
