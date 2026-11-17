"""lib/python/anger_lock.py: the lock, the block output and the fail-open path.

The classifier itself (a Haiku call) is replaced by a stub; its judgement was
checked live on sample prompts when the hook was written (2026-11-17).
"""

import io
import json
import sys
import tempfile
import time
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "lib/python"))

import anger_lock  # noqa: E402


class AngerLockTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        state = Path(self.tmp.name)
        for name, value in (("STATE_DIR", state), ("LOCK_FILE", state / "locked-until"),
                            ("LOG_FILE", state / "log.jsonl")):
            patcher = mock.patch.object(anger_lock, name, value)
            patcher.start()
            self.addCleanup(patcher.stop)
        notify = mock.patch.object(anger_lock, "notify")
        self.notify = notify.start()
        self.addCleanup(notify.stop)
        self.addCleanup(self.tmp.cleanup)

    def run_hook(self, prompt, agent="claude", verdict=(False, "", "")):
        with mock.patch.object(anger_lock, "classify", side_effect=verdict if isinstance(verdict, Exception)
                               else (lambda _prompt: verdict)), \
             mock.patch.object(sys, "stdin", io.StringIO(json.dumps({"prompt": prompt, "session_id": "s"}))):
            out = io.StringIO()
            with redirect_stdout(out):
                self.assertEqual(anger_lock.main(["anger_lock.py", agent]), 0)
        text = out.getvalue().strip()
        return json.loads(text) if text else None

    def test_calm_prompt_passes_silently(self):
        self.assertIsNone(self.run_hook("please add a test"))
        self.assertFalse(anger_lock.LOCK_FILE.exists())

    def test_angry_prompt_blocks_locks_and_shows_suggestion(self):
        out = self.run_hook("for fuck's sake", verdict=(True, "swearing", "Breathe out slowly."))
        self.assertEqual(out["decision"], "block")
        self.assertIn("Breathe out slowly.", out["reason"])
        self.assertIn("play with your dog", out["reason"])
        self.assertNotIn("systemMessage", out)
        self.assertGreater(anger_lock.lock_remaining(time.time()), 0)
        self.notify.assert_called_once()

    def test_codex_also_gets_a_system_message(self):
        out = self.run_hook("damn it", agent="codex", verdict=(True, "swearing", "Stand up."))
        self.assertEqual(out["systemMessage"], out["reason"])

    def test_calm_prompt_during_lock_is_blocked_without_extending(self):
        anger_lock.set_lock(time.time())
        until = anger_lock.LOCK_FILE.read_text()
        out = self.run_hook("ok, what next?")
        self.assertEqual(out["decision"], "block")
        self.assertIn("locked for another", out["reason"])
        self.assertEqual(anger_lock.LOCK_FILE.read_text(), until)

    def test_angry_prompt_during_lock_restarts_it(self):
        anger_lock.LOCK_FILE.parent.mkdir(parents=True, exist_ok=True)
        anger_lock.LOCK_FILE.write_text(f"{time.time() + 1}\n")
        self.run_hook("WHY", verdict=(True, "shouting", "Walk."))
        self.assertGreater(float(anger_lock.LOCK_FILE.read_text()), time.time() + anger_lock.LOCK_SECONDS - 1)

    def test_expired_lock_lets_prompts_through(self):
        anger_lock.LOCK_FILE.parent.mkdir(parents=True, exist_ok=True)
        anger_lock.LOCK_FILE.write_text(f"{time.time() - 1}\n")
        self.assertIsNone(self.run_hook("hello"))

    def test_classifier_failure_passes_with_visible_warning(self):
        out = self.run_hook("hello", verdict=anger_lock.ClassifierError("API HTTP 500"))
        self.assertNotIn("decision", out)
        self.assertIn("API HTTP 500", out["systemMessage"])
        self.assertIn("sent unchecked", out["systemMessage"])

    def test_classifier_failure_during_lock_still_blocks(self):
        anger_lock.set_lock(time.time())
        out = self.run_hook("hello", verdict=anger_lock.ClassifierError("API HTTP 500"))
        self.assertEqual(out["decision"], "block")

    def test_recent_suggestions_are_read_from_the_log(self):
        self.run_hook("damn", verdict=(True, "swearing", "First idea."))
        self.run_hook("shit", verdict=(True, "swearing", "Second idea."))
        self.assertEqual(anger_lock.recent_suggestions(), ["Second idea.", "First idea."])

    def test_duration_text(self):
        self.assertEqual(anger_lock.format_duration(5), "5 seconds")
        self.assertEqual(anger_lock.format_duration(300), "5 minutes")
        self.assertEqual(anger_lock.format_duration(1), "1 second")


if __name__ == "__main__":
    unittest.main()
