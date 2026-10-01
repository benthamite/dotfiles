import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, Call  # noqa: E402
from agent_guard.policies import elisp_skill  # noqa: E402


def decision(file_path, session):
    payload = {"tool_name": "Edit", "tool_input": {"file_path": file_path}, "session_id": session}
    return elisp_skill.decide(Call.from_payload(CLAUDE, payload)).decision


class ElispSkillTests(unittest.TestCase):
    def setUp(self):
        # The marker lives at a fixed /tmp path; a unique session id keeps it private to this test.
        handle, self.marker = tempfile.mkstemp(prefix="claude-elisp-skill-", dir="/tmp")
        os.close(handle)
        self.session = self.marker[len("/tmp/claude-elisp-skill-"):]

    def tearDown(self):
        os.remove(self.marker)

    def test_marker_allows(self):
        self.assertEqual(decision("/x/a.el", self.session), "allow")
        self.assertEqual(decision("/x/a.el", self.session + "-other"), "deny")

    def test_only_el_files(self):
        for path in ("/x/a.elc", "/x/a.el.org", "/x/a.org", ""):
            self.assertEqual(decision(path, "no-marker-session"), "allow", path)

    def test_reason(self):
        call = Call.from_payload(CLAUDE, {"tool_name": "Write", "tool_input": {"file_path": "a.el"}})
        self.assertEqual(elisp_skill.decide(call).reason, elisp_skill.REASON)


if __name__ == "__main__":
    unittest.main()
