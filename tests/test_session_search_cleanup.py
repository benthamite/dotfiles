"""lib/python/session_search_cleanup.py: which search processes an ended
session owns, and the real hook run against live processes."""

import json
import os
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "lib/python"))

import session_search_cleanup as cleanup  # noqa: E402
from session_search_cleanup import Proc  # noqa: E402

AGENT, HOOK_SHELL, HOOK = 100, 101, 102
SESSION = "11111111-2222-3333-4444-555555555555"


def table(*extra):
    """launchd, an Emacs, the Claude agent (age 1000 s) and the hook under it."""
    procs = [Proc(1, 0, 9999, "/sbin/launchd"), Proc(50, 1, 5000, "/Applications/Emacs.app/Contents/MacOS/Emacs"),
             Proc(AGENT, 50, 1000, "/Users/u/.local/bin/claude"), Proc(HOOK_SHELL, AGENT, 1, "/bin/sh"),
             Proc(HOOK, HOOK_SHELL, 1, "/usr/bin/python3"), *extra]
    return {proc.pid: proc for proc in procs}


def targets(procs, environments=None, runtime="claude", event="SessionEnd", session=SESSION, sids=None):
    environments = environments or {}
    with mock.patch.object(cleanup, "_sid", side_effect=lambda pid: (sids or {}).get(pid)):
        found = cleanup.targets(procs, runtime, event, session, HOOK,
                                environ=lambda pid, names: {k: v for k, v in environments.get(pid, {}).items()
                                                            if k in names})
    return {proc.pid: why for proc, why in found}


class OwnershipTests(unittest.TestCase):
    def test_session_id_in_environment(self):
        procs = table(Proc(200, 1, 60, "ugrep"), Proc(201, 1, 60, "bfs"))
        env = {200: {"CLAUDE_CODE_SESSION_ID": SESSION, "CLAUDE_PID": str(AGENT)},
               201: {"CLAUDE_CODE_SESSION_ID": "other-session", "CLAUDE_PID": str(AGENT)}}
        self.assertEqual(list(targets(procs, env)), [200])
        # At SessionStart the ending session is not known; a live owner keeps both.
        self.assertEqual(targets(procs, env, event="SessionStart"), {})

    def test_name_alone_never_suffices(self):
        procs = table(*(Proc(200 + index, 1, 60, name) for index, name in enumerate(sorted(cleanup.SEARCH_TOOLS))))
        self.assertEqual(targets(procs), {})
        self.assertEqual(targets(procs, event="SessionStart"), {})

    def test_only_search_tools(self):
        procs = table(Proc(200, AGENT, 60, "/bin/sleep"), Proc(201, 1, 60, "/usr/bin/python3"))
        env = {200: {"CLAUDE_CODE_SESSION_ID": SESSION}, 201: {"CLAUDE_CODE_SESSION_ID": SESSION}}
        self.assertEqual(targets(procs, env), {})

    def test_descendants_of_the_ending_claude(self):
        procs = table(Proc(300, AGENT, 30, "/bin/zsh"), Proc(301, 300, 30, "bfs"), Proc(302, AGENT, 30, "rg"),
                      Proc(303, 50, 30, "rg"))
        self.assertEqual(sorted(targets(procs)), [301, 302])
        self.assertIn("descends from the ending Claude process 100", targets(procs)[301])
        # Only SessionEnd ends this process's session.
        self.assertEqual(targets(procs, event="SessionStart"), {})
        # Codex app-server processes host several threads: no ancestry rule.
        codex = table(Proc(301, AGENT, 30, "bfs"))
        codex[AGENT] = Proc(AGENT, 50, 1000, "/opt/homebrew/bin/codex")
        self.assertEqual(targets(codex, runtime="codex"), {})

    def test_orphans_in_the_claude_terminal_session(self):
        procs = table(Proc(400, 1, 30, "ugrep"), Proc(401, 1, 30, "ugrep"))
        sids = {AGENT: AGENT, 400: AGENT, 401: 50}
        self.assertEqual(list(targets(procs, sids=sids)), [400])
        # When Claude does not lead its terminal session, the session is not its own.
        self.assertEqual(targets(procs, sids={AGENT: 50, 400: 50, 401: 50}), {})

    def test_owner_gone(self):
        procs = table(Proc(500, 1, 600, "ugrep"), Proc(501, 1, 600, "ugrep"), Proc(502, 1, 600, "bfs"),
                      Proc(503, 1, 600, "rg"), Proc(504, 1, 600, "rg"), Proc(600, 1, 10, "/usr/bin/vim"),
                      Proc(601, 1, 10, "/Users/u/.local/share/claude/versions/2.1.291"),
                      Proc(602, 1, 600, "/Users/u/.local/bin/claude"))
        env = {500: {"CLAUDE_PID": "3305"},       # no such process
               501: {"CLAUDE_PID": "600"},        # pid reused by another program
               502: {"CLAUDE_PID": "601"},        # a Claude that started after the search
               503: {"CLAUDE_PID": "602"},        # its live Claude, the same age (whole seconds)
               504: {"CLAUDE_PID": str(AGENT)}}   # the live agent running the hook
        for event in ("SessionStart", "SessionEnd"):
            found = targets(procs, env, event=event)
            self.assertEqual(sorted(found), [500, 501, 502], event)
            self.assertIn("its Claude process 3305 is gone", found[500])
        # Codex sets no owner pid; a Claude variable does not make Codex act.
        self.assertEqual(targets(procs, env, runtime="codex"), {})

    def test_codex_thread_id(self):
        procs = table(Proc(700, 1, 30, "rg"), Proc(701, 1, 30, "rg"))
        procs[AGENT] = Proc(AGENT, 50, 1000, "/opt/homebrew/Caskroom/codex/0.160.1/bin/codex")
        env = {700: {"CODEX_THREAD_ID": SESSION}, 701: {"CODEX_THREAD_ID": "another-thread"}}
        self.assertEqual(list(targets(procs, env, runtime="codex")), [700])
        # A Claude session id never matches for Codex, nor the reverse.
        self.assertEqual(targets(procs, {700: {"CLAUDE_CODE_SESSION_ID": SESSION}}, runtime="codex"), {})
        self.assertEqual(targets(procs, env), {})

    def test_empty_session_id_matches_nothing(self):
        procs = table(Proc(800, 1, 30, "rg"))
        self.assertEqual(targets(procs, {800: {"CLAUDE_CODE_SESSION_ID": ""}}, session=""), {})

    def test_hook_ancestry_is_protected(self):
        # A hook run from inside a search (find -exec) must not kill its own ancestors.
        procs = table()
        procs[HOOK_SHELL] = Proc(HOOK_SHELL, 900, 1, "/bin/sh")
        procs[900] = Proc(900, AGENT, 5, "find")
        self.assertEqual(targets(procs, {900: {"CLAUDE_CODE_SESSION_ID": SESSION}}), {})


class HelperTests(unittest.TestCase):
    def test_etime(self):
        self.assertEqual(cleanup._seconds("00:07"), 7)
        self.assertEqual(cleanup._seconds("29:44"), 29 * 60 + 44)
        self.assertEqual(cleanup._seconds("01:02:03"), 3723)
        self.assertEqual(cleanup._seconds("2-01:02:03"), 2 * 86400 + 3723)
        self.assertIsNone(cleanup._seconds("x"))

    def test_agent_process(self):
        procs = table()
        self.assertEqual(cleanup.agent_process(procs, "claude", HOOK_SHELL).pid, AGENT)
        self.assertIsNone(cleanup.agent_process(procs, "codex", HOOK_SHELL))

    def test_main_ignores_other_events_and_bad_payloads(self):
        for text in ('{"hook_event_name": "Stop"}', "[]", "not json"):
            with mock.patch.object(cleanup, "process_table") as process_table, \
                    mock.patch("sys.stdin", new=__import__("io").StringIO(text)), \
                    mock.patch("sys.stderr", new=__import__("io").StringIO()):
                self.assertEqual(cleanup.main("claude"), 0)
                process_table.assert_not_called()


def _spawn(name, env=None, **kwargs):
    """A sleeping process named NAME (its argv[0]). Not /bin/sleep: macOS
    withholds a platform binary's environment from KERN_PROCARGS2."""
    return subprocess.Popen([name, "-c", "import time; time.sleep(60)"], executable=sys.executable, env=env,
                            **kwargs)


class LiveProcessTests(unittest.TestCase):
    """Real processes; only processes this test started are ever signalled."""

    def setUp(self):
        self.procs = []
        self.addCleanup(self._reap)

    def _reap(self):
        for proc in self.procs:
            if proc.poll() is None:
                proc.kill()
                proc.wait()

    def test_process_table_and_environment(self):
        child = _spawn("rg", env={"CLAUDE_CODE_SESSION_ID": "live-test", "OTHER": "x", "CLAUDE_PID": "1"})
        self.procs.append(child)
        time.sleep(0.2)
        row = cleanup.process_table()[child.pid]
        self.assertEqual(row.name, "rg")
        self.assertEqual(cleanup.environment_values(child.pid, ("CLAUDE_CODE_SESSION_ID", "CLAUDE_PID")),
                         {"CLAUDE_CODE_SESSION_ID": "live-test", "CLAUDE_PID": "1"})
        self.assertEqual(cleanup.environment_values(child.pid, ("ABSENT",)), {})
        self.assertEqual(cleanup.environment_values(999999, ("CLAUDE_PID",)), {})

    def test_platform_binary_environment_is_withheld(self):
        # The documented gap: /usr/bin/find and /usr/bin/grep cannot be matched by session id.
        child = subprocess.Popen(["find", "60"], executable="/bin/sleep", env={"CLAUDE_PID": "1"})
        self.procs.append(child)
        time.sleep(0.2)
        self.assertEqual(cleanup.environment_values(child.pid, ("CLAUDE_PID",)), {})

    def test_terminate_escalates_to_sigkill(self):
        stubborn = subprocess.Popen(["/usr/bin/python3", "-c",
                                     "import signal, time; signal.signal(signal.SIGTERM, signal.SIG_IGN); "
                                     "print('ready', flush=True); time.sleep(60)"], stdout=subprocess.PIPE)
        polite = _spawn("bfs")
        self.procs += [stubborn, polite]
        stubborn.stdout.readline()
        found = [(Proc(stubborn.pid, 0, 0, "find"), ""), (Proc(polite.pid, 0, 0, "bfs"), "")]
        self.assertEqual(cleanup.terminate(found, grace=0.3), [stubborn.pid, polite.pid])
        self.assertEqual(polite.wait(5), -signal.SIGTERM)
        self.assertEqual(stubborn.wait(5), -signal.SIGKILL)

    def test_hook_under_a_claude_process(self):
        """A process named `claude` (its own terminal-session leader) starts a
        search, an orphan in its session and the hook; the hook, in dry-run
        mode, names exactly the session's searches among this test's processes."""
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp) / "out"
            payload = json.dumps({"hook_event_name": "SessionEnd", "session_id": "live-" + SESSION})
            script = (
                "exec -a rg /bin/sleep 60 & echo $! > " + tmp + "/child\n"
                "( exec -a ugrep /bin/sleep 60 & echo $! > " + tmp + "/orphan ) \n"
                "sleep 0.3\n"
                "printf '%s' '" + payload + "' | /usr/bin/python3 -S " + str(ROOT / "lib/python/session_search_cleanup.py")
                + " claude --dry-run > " + str(out) + "\n"
                "sleep 60\n")
            agent = subprocess.Popen(["claude", "-c", script], executable="/bin/bash", start_new_session=True)
            self.procs.append(agent)
            matching = _spawn("find", env={"CLAUDE_CODE_SESSION_ID": "live-" + SESSION}, start_new_session=True)
            other = _spawn("find", env={"CLAUDE_CODE_SESSION_ID": "live-other"}, start_new_session=True)
            unrelated = _spawn("rg", start_new_session=True)
            self.procs += [matching, other, unrelated]
            deadline = time.time() + 10
            while not (out.exists() and out.stat().st_size) and time.time() < deadline:
                time.sleep(0.1)
            child = int((Path(tmp) / "child").read_text())
            orphan = int((Path(tmp) / "orphan").read_text())
            try:
                lines = out.read_text().splitlines()
                mine = {child, orphan, matching.pid, other.pid, unrelated.pid}
                named = {int(line.split()[0]): line for line in lines if int(line.split()[0]) in mine}
                self.assertEqual(set(named), {child, orphan, matching.pid}, lines)
                self.assertIn("descends from the ending Claude process", named[child])
                self.assertIn("terminal session", named[orphan])
                self.assertIn("CLAUDE_CODE_SESSION_ID is the ended session", named[matching.pid])
            finally:
                os.killpg(agent.pid, signal.SIGKILL)
                for pid in (orphan,):
                    try:
                        os.kill(pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass


if __name__ == "__main__":
    unittest.main()
