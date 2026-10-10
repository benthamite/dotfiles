"""Exercise claude-pool's member fallback with stubbed Claude Code members.

The real script runs from a temporary copy of the repository layout it
expects (bin/, lib/python/agent_accounts.py, shell/shims/claude,
emacs/agent-accounts.json), so no real account,
Emacs or network is involved. Each stub member replays a canned reply.
"""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "claude-pool"
LIBRARY = Path(__file__).resolve().parents[1] / "lib" / "python" / "agent_accounts.py"

STUB = """#!/bin/sh
echo "$CLAUDE_CONFIG_DIR" >> "$POOL_TEST_CALLS"
cat "$CLAUDE_CONFIG_DIR/reply"
exit "$(cat "$CLAUDE_CONFIG_DIR/code")"
"""

# Replies as Claude Code prints them with --output-format json.
OK = json.dumps({"is_error": False, "result": "OK", "structured_output": {"links": []}})
EXPIRED = json.dumps({"is_error": True, "subtype": "success", "api_error_status": None,
                      "result": "Failed to authenticate: OAuth session expired and could not be refreshed"})
LIMITED = json.dumps({"is_error": True, "api_error_status": 429,
                      "result": "You've hit your weekly limit · resets Oct 9 at 1pm (America/Buenos_Aires)"})
NOT_LOGGED_IN = "Not logged in · Please run /login"
REFUSED = json.dumps({"is_error": True, "api_error_status": 400, "result": "API Error: 400 prompt is too long"})


class ClaudePoolTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory(prefix="claude-pool-test-")
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)
        (self.root / "bin").mkdir()
        shutil.copy(SCRIPT, self.root / "bin" / "claude-pool")
        (self.root / "lib" / "python").mkdir(parents=True)
        shutil.copy(LIBRARY, self.root / "lib" / "python" / "agent_accounts.py")
        shim = self.root / "shell" / "shims" / "claude"
        shim.parent.mkdir(parents=True)
        shim.write_text(STUB)
        shim.chmod(0o755)
        self.calls = self.root / "calls"

    def home(self, name, code, reply):
        home = self.root / "homes" / name
        home.mkdir(parents=True)
        (home / "code").write_text(str(code))
        (home / "reply").write_text(reply)
        return home

    def pool(self, *members, extra=()):
        """Declare members (name, code, reply) in order as pool test-pool.

EXTRA holds further entries (name, code, reply, form) outside the pool,
available as fallbacks; FORM is kept for the callers' readability."""
        claude = [{"name": name, "home": str(self.home(name, code, reply)), "pool": "test-pool"}
                  for name, code, reply in members]
        claude += [{"name": name, "home": str(self.home(name, code, reply))}
                   for name, code, reply, _form in extra]
        claude.append({"name": "other", "home": "~/.claude-other"})
        accounts = self.root / "emacs" / "agent-accounts.json"
        accounts.parent.mkdir()
        accounts.write_text(json.dumps({"claude": claude, "codex": []}))

    def run_pool(self, *options):
        env = {**os.environ, "POOL_TEST_CALLS": str(self.calls)}
        result = subprocess.run([str(self.root / "bin" / "claude-pool"), "run", "test-pool", *options, "--", "-p"],
                                input="prompt", capture_output=True, text=True, env=env, timeout=60)
        called = [Path(line).name for line in self.calls.read_text().split()] if self.calls.exists() else []
        return result, called

    def test_expired_login_falls_through_to_next_member(self):
        self.pool(("first", 1, EXPIRED), ("second", 0, OK))
        result, called = self.run_pool()
        self.assertEqual(called, ["first", "second"])
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, OK)
        self.assertIn("first is not logged in", result.stderr)
        self.assertIn("ran on second", result.stderr)

    def test_no_usable_member_exits_75_naming_each_reason(self):
        self.pool(("a", 1, LIMITED), ("b", 1, EXPIRED), ("c", 1, NOT_LOGGED_IN))
        result, called = self.run_pool()
        self.assertEqual(called, ["a", "b", "c"])
        self.assertEqual(result.returncode, 75)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr.strip().splitlines()[-1],
                         "claude-pool: no member of test-pool is usable (rate-limited: a; logged out: b, c)")

    def test_other_failures_are_returned_not_retried(self):
        self.pool(("first", 1, REFUSED), ("second", 0, OK))
        result, called = self.run_pool()
        self.assertEqual(called, ["first"])
        self.assertEqual((result.returncode, result.stdout), (1, REFUSED))

    def test_successful_reply_mentioning_authentication_is_kept(self):
        reply = json.dumps({"is_error": False, "result": "Failed to authenticate is a common error string"})
        self.pool(("first", 0, reply), ("second", 0, OK))
        result, called = self.run_pool()
        self.assertEqual(called, ["first"])
        self.assertEqual((result.returncode, result.stdout), (0, reply))

    def test_fallbacks_run_in_order_after_every_member(self):
        self.pool(("a", 1, LIMITED), ("b", 1, EXPIRED),
                  extra=[("tlon", 1, LIMITED, "pair"), ("spare", 0, OK, "plist")])
        result, called = self.run_pool("--fallback", "tlon", "--fallback", "spare")
        self.assertEqual(called, ["a", "b", "tlon", "spare"])
        self.assertEqual((result.returncode, result.stdout), (0, OK))
        self.assertIn("ran on spare", result.stderr)

    def test_fallback_is_unused_while_a_member_answers(self):
        self.pool(("a", 0, OK), extra=[("tlon", 0, OK, "pair")])
        result, called = self.run_pool("--fallback", "tlon")
        self.assertEqual((called, result.returncode), (["a"], 0))

    def test_unusable_fallbacks_exit_75_naming_each_reason(self):
        self.pool(("a", 1, LIMITED), extra=[("tlon", 1, NOT_LOGGED_IN, "pair")])
        result, called = self.run_pool("--fallback", "tlon")
        self.assertEqual((called, result.returncode, result.stdout), (["a", "tlon"], 75, ""))
        self.assertEqual(result.stderr.strip().splitlines()[-1],
                         "claude-pool: no member of test-pool or fallback tlon is usable "
                         "(rate-limited: a; logged out: tlon)")

    def test_unknown_or_duplicate_fallback_is_refused_before_any_run(self):
        self.pool(("a", 0, OK), extra=[("tlon", 0, OK, "pair")])
        for options in (["--fallback", "missing"], ["--fallback", "a"], ["--fallback", "tlon", "--fallback", "tlon"]):
            with self.subTest(options=options):
                result, called = self.run_pool(*options)
                self.assertEqual((result.returncode, called), (69, []))

    def test_malformed_fallback_option_prints_usage(self):
        self.pool(("a", 0, OK))
        result, called = self.run_pool("--fallback")
        self.assertEqual((result.returncode, called), (2, []))

    def test_members_lists_fallbacks_after_the_pool(self):
        self.pool(("a", 0, OK), extra=[("tlon", 0, OK, "pair")])
        result = subprocess.run([str(self.root / "bin" / "claude-pool"), "members", "test-pool", "--fallback", "tlon"],
                                capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0)
        self.assertEqual([m["name"] for m in json.loads(result.stdout)], ["a", "tlon"])
        self.assertEqual(json.loads(result.stdout)[1]["home"], str(self.root / "homes" / "tlon"))


if __name__ == "__main__":
    unittest.main()
