import importlib.machinery
import importlib.util
import pathlib
import plistlib
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "lulu-homebrew-rules"


def load_module():
    loader = importlib.machinery.SourceFileLoader("lulu_homebrew_rules", str(SCRIPT))
    spec = importlib.util.spec_from_loader("lulu_homebrew_rules", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def rule(path, action=1):
    return {"path": path, "action": action, "isDisabled": "$null"}


def write_archive(path, rules):
    """Write rules the way LuLu's NSKeyedArchiver does: values behind UIDs."""
    objects = ["$null"]
    for item in rules:
        entry = {}
        for key, value in item.items():
            objects.append(value)
            entry[key] = plistlib.UID(len(objects) - 1)
        objects.append(entry)
    with path.open("wb") as handle:
        plistlib.dump({"$archiver": "NSKeyedArchiver", "$objects": objects}, handle,
                      fmt=plistlib.FMT_BINARY)


class LuluHomebrewRulesTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def uncovered(self, rules):
        return self.mod.uncovered_packages(rules, installed=lambda _path: True)

    def test_versioned_rule_without_directory_rule_is_uncovered(self):
        rules = [rule("/opt/homebrew/Cellar/git/2.56.0/libexec/git-core/git-remote-http")]
        self.assertEqual(self.uncovered(rules), ["/opt/homebrew/Cellar/git"])

    def test_directory_rule_covers_every_version(self):
        rules = [rule("/opt/homebrew/Cellar/git/2.56.0/libexec/git-core/git-remote-http"),
                 rule("/opt/homebrew/Cellar/git/*")]
        self.assertEqual(self.uncovered(rules), [])

    def test_directory_rule_does_not_cover_a_package_sharing_its_prefix(self):
        rules = [rule("/opt/homebrew/Cellar/git-lfs/3.7.1/bin/git-lfs"),
                 rule("/opt/homebrew/Cellar/git/*")]
        self.assertEqual(self.uncovered(rules), ["/opt/homebrew/Cellar/git-lfs"])

    def test_casks_are_checked_and_block_rules_ignored(self):
        rules = [rule("/opt/homebrew/Caskroom/codex/0.155.1/bin/codex"),
                 rule("/opt/homebrew/Cellar/nmap/7.99/bin/nmap", action=0)]
        self.assertEqual(self.uncovered(rules), ["/opt/homebrew/Caskroom/codex"])

    def test_block_directory_rule_does_not_count_as_coverage(self):
        rules = [rule("/opt/homebrew/Cellar/node/26.3.0/bin/node"),
                 rule("/opt/homebrew/Cellar/node/*", action=0)]
        self.assertEqual(self.uncovered(rules), ["/opt/homebrew/Cellar/node"])

    def test_uninstalled_packages_are_ignored(self):
        rules = [rule("/opt/homebrew/Caskroom/claude-code/2.1.85/claude")]
        self.assertEqual(self.mod.uncovered_packages(rules, installed=lambda _path: False), [])

    def test_load_rules_resolves_archived_values(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "rules.plist"
            write_archive(path, [rule("/opt/homebrew/Cellar/gh/2.102.0/bin/gh")])
            loaded = self.mod.load_rules(path)
        self.assertEqual(loaded[0]["path"], "/opt/homebrew/Cellar/gh/2.102.0/bin/gh")
        self.assertEqual(loaded[0]["action"], 1)


if __name__ == "__main__":
    unittest.main()
