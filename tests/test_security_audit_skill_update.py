import importlib.machinery
import importlib.util
import json
import pathlib
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "macos" / ".claude" / "skills" / "security-audit" / "scripts" / "skill-update.py"
COMMIT = "a" * 40


def load_module():
    loader = importlib.machinery.SourceFileLoader("skill_update", str(SCRIPT))
    spec = importlib.util.spec_from_loader("skill_update", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class FakeUpstream:
    """Serve a published skill from memory through the script's fetch seam."""

    def __init__(self, module, files, tamper=None):
        self.module = module
        self.files = files
        self.tamper = tamper or {}
        self.requests = []

    def __call__(self, url):
        self.requests.append(url)
        m = self.module
        if url == f"{m.API}/commits/{m.BRANCH}":
            return json.dumps({"sha": COMMIT}).encode()
        if url.startswith(f"{m.API}/commits?"):
            return json.dumps([{"commit": {"message": "Change the skill\n\nbody",
                                           "committer": {"date": "2026-11-24T00:00:00Z"}}}]).encode()
        prefix = f"{m.API}/contents/"
        if url.startswith(prefix):
            directory = url[len(prefix):].split("?")[0]
            inside = directory[len(m.SKILL_PATH):].strip("/")
            entries, seen = [], set()
            for path, data in self.files.items():
                parts = pathlib.PurePosixPath(path).parts
                base = pathlib.PurePosixPath(inside).parts if inside else ()
                if parts[:len(base)] != base or len(parts) <= len(base):
                    continue
                name = parts[len(base)]
                full = "/".join((m.SKILL_PATH,) + base + (name,))
                if len(parts) == len(base) + 1:
                    entries.append({"type": "file", "path": full, "sha": m.blob_sha(data)})
                elif name not in seen:
                    seen.add(name)
                    entries.append({"type": "dir", "path": full, "sha": "d" * 40})
            return json.dumps(entries).encode()
        raw = f"{m.RAW}/{COMMIT}/{m.SKILL_PATH}/"
        if url.startswith(raw):
            path = url[len(raw):]
            return self.tamper.get(path, self.files[path])
        raise AssertionError(f"unexpected request {url}")


class SkillUpdateTests(unittest.TestCase):
    def setUp(self):
        self.m = load_module()
        self.m.is_source_checkout = lambda root: False
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = pathlib.Path(self.tmp.name) / "security-audit"
        self.backups = pathlib.Path(self.tmp.name) / "backups"

    def install(self, files):
        for path, data in files.items():
            target = self.root / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(data)

    def test_blob_hash_matches_git(self):
        self.assertEqual(self.m.blob_sha(b"hello\n"), "ce013625030ba8dba906f756967f9e9ca394464a")

    def test_identical_copy_is_up_to_date(self):
        files = {"SKILL.md": b"one\n", "references/a.md": b"a\n"}
        self.install(files)
        result = self.m.check(self.root, FakeUpstream(self.m, files))
        self.assertEqual(result["status"], "up-to-date")
        self.assertEqual(result["commit"], COMMIT)

    def test_changed_upstream_is_offered_and_applied(self):
        self.install({"SKILL.md": b"one\n", "references/a.md": b"a\n"})
        published = {"SKILL.md": b"two\n", "references/a.md": b"a\n", "scripts/new.py": b"print(1)\n"}
        upstream = FakeUpstream(self.m, published)
        result = self.m.check(self.root, upstream)
        self.assertEqual(result["status"], "update-available")
        self.assertEqual(result["modified"], ["SKILL.md"])
        self.assertEqual(result["added"], ["scripts/new.py"])
        self.assertEqual(result["local_edits"], "unknown")
        self.assertEqual(result["recent_upstream_commits"][0]["subject"], "Change the skill")
        self.assertEqual((self.root / "SKILL.md").read_bytes(), b"one\n", "check must change nothing")

        applied = self.m.apply(self.root, COMMIT, get=upstream, backup_root=self.backups)
        self.assertEqual(applied["status"], "updated")
        self.assertEqual((self.root / "SKILL.md").read_bytes(), b"two\n")
        self.assertEqual((self.root / "scripts/new.py").read_bytes(), b"print(1)\n")
        self.assertEqual((pathlib.Path(applied["backup"]) / "SKILL.md").read_bytes(), b"one\n")
        self.assertEqual(self.m.check(self.root, upstream)["status"], "up-to-date")

    def test_local_edits_block_an_update_unless_accepted(self):
        original = {"SKILL.md": b"one\n", "references/a.md": b"a\n"}
        self.install(original)
        self.m.write_manifest(self.root, COMMIT, {p: self.m.blob_sha(d) for p, d in original.items()})
        (self.root / "SKILL.md").write_bytes(b"mine\n")
        upstream = FakeUpstream(self.m, {"SKILL.md": b"two\n", "references/a.md": b"a\n"})
        result = self.m.check(self.root, upstream)
        self.assertEqual(result["status"], "local-edits")
        self.assertEqual(result["locally_modified"], ["SKILL.md"])
        with self.assertRaises(self.m.UpdateError):
            self.m.apply(self.root, COMMIT, get=upstream, backup_root=self.backups)
        self.assertEqual((self.root / "SKILL.md").read_bytes(), b"mine\n")
        self.m.apply(self.root, COMMIT, overwrite_local=True, get=upstream, backup_root=self.backups)
        self.assertEqual((self.root / "SKILL.md").read_bytes(), b"two\n")

    def test_removed_upstream_file_goes_only_if_an_update_installed_it(self):
        original = {"SKILL.md": b"one\n", "references/old.md": b"old\n"}
        self.install(original)
        self.install({"notes-of-mine.md": b"keep\n"})
        self.m.write_manifest(self.root, COMMIT, {p: self.m.blob_sha(d) for p, d in original.items()})
        upstream = FakeUpstream(self.m, {"SKILL.md": b"one\n"})
        result = self.m.check(self.root, upstream)
        self.assertEqual(result["removed"], ["references/old.md"])
        self.m.apply(self.root, COMMIT, get=upstream, backup_root=self.backups)
        self.assertFalse((self.root / "references/old.md").exists())
        self.assertEqual((self.root / "notes-of-mine.md").read_bytes(), b"keep\n")

    def test_a_file_that_fails_its_hash_installs_nothing(self):
        self.install({"SKILL.md": b"one\n", "references/a.md": b"a\n"})
        published = {"SKILL.md": b"two\n", "references/a.md": b"b\n"}
        upstream = FakeUpstream(self.m, published, tamper={"references/a.md": b"evil\n"})
        with self.assertRaises(self.m.UpdateError):
            self.m.apply(self.root, COMMIT, get=upstream, backup_root=self.backups)
        self.assertEqual((self.root / "SKILL.md").read_bytes(), b"one\n")
        self.assertEqual((self.root / "references/a.md").read_bytes(), b"a\n")

    def test_listing_outside_the_skill_is_refused(self):
        self.install({"SKILL.md": b"one\n"})
        m = self.m

        def get(url):
            if url == f"{m.API}/commits/{m.BRANCH}":
                return json.dumps({"sha": COMMIT}).encode()
            return json.dumps([{"type": "file", "path": "bin/evil", "sha": "e" * 40}]).encode()

        result = m.check(self.root, get)
        self.assertEqual(result["status"], "check-failed")

    def test_fetch_refuses_other_hosts(self):
        with self.assertRaises(self.m.UpdateError):
            self.m.fetch("https://example.com/x")

    def test_source_checkout_is_left_to_git(self):
        self.install({"SKILL.md": b"one\n"})
        self.m.is_source_checkout = lambda root: True

        def get(url):
            raise AssertionError("a source checkout must not contact upstream")

        self.assertEqual(self.m.check(self.root, get)["status"], "source-checkout")
        with self.assertRaises(self.m.UpdateError):
            self.m.apply(self.root, COMMIT, get=get, backup_root=self.backups)


if __name__ == "__main__":
    unittest.main()
