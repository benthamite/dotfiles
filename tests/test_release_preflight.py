"""bin/release-preflight flags exactly the checkouts a release would lose."""

import importlib.machinery
import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "release-preflight"


def load_module():
    loader = importlib.machinery.SourceFileLoader("release_preflight", str(SCRIPT))
    spec = importlib.util.spec_from_loader("release_preflight", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def git(cwd, *arguments):
    subprocess.run(["git", "-C", str(cwd), "-c", "user.name=T", "-c", "user.email=t@example.com",
                    "-c", "commit.gpgsign=false", *arguments],
                   check=True, capture_output=True, text=True)


class ReleasePreflightTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.root = Path(self.directory.name)
        self.remote = self.root / "remote.git"
        git(self.root, "init", "--quiet", "--bare", "-b", "main", str(self.remote))
        seed = self.root / "seed"
        git(self.root, "clone", "--quiet", str(self.remote), str(seed))
        (seed / "f").write_text("one\n")
        git(seed, "add", "f")
        git(seed, "commit", "--quiet", "-m", "one")
        git(seed, "push", "--quiet", "origin", "HEAD:main")

    def tearDown(self):
        self.directory.cleanup()

    def clone(self, name):
        path = self.root / name
        git(self.root, "clone", "--quiet", str(self.remote), str(path))
        return path

    def problems(self, repo):
        return self.mod.inspect(repo, set(), convert=False, fetch=True)["problems"]

    def test_clean_checkout_is_not_flagged(self):
        self.assertEqual([], self.problems(self.clone("clean")))

    def test_unpushed_commit_on_a_branch_is_flagged(self):
        repo = self.clone("ahead")
        (repo / "f").write_text("two\n")
        git(repo, "commit", "--quiet", "-am", "two")
        self.assertEqual(["1 commits not on upstream"], self.problems(repo))

    def test_detached_head_is_flagged_only_when_unpublished(self):
        repo = self.clone("detached")
        git(repo, "checkout", "--quiet", "--detach")
        self.assertEqual([], self.problems(repo))
        (repo / "f").write_text("local\n")
        git(repo, "commit", "--quiet", "-am", "local")
        self.assertEqual(1, len(self.problems(repo)))

    def test_github_slug(self):
        self.assertEqual("benthamite/x", self.mod.github_slug("https://github.com/benthamite/x.git"))
        self.assertEqual("tlon-team/y", self.mod.github_slug("git@github.com:tlon-team/y.git"))
        self.assertIsNone(self.mod.github_slug("https://codeberg.org/a/b.git"))


if __name__ == "__main__":
    unittest.main()
