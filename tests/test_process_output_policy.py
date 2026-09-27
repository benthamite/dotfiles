"""Synthetic commands only: never inspect actual process arguments."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[1]
ALLOW = [
    "pgrep emacs", "pgrep -f mcp", "pgrep -l emacs",
    "ps -axo pid,comm", "ps axo pid,ppid,comm,etime", "ps -p 12 -o pid= -o comm=",
    "ps -axo pid,comm | rg emacs", "env command /bin/ps -axo pid,comm",
    "sudo --user root pgrep -f mcp", "sudo --group staff ps -axo pid,comm",
    "time -f '%s' ps -axo pid,comm", "/usr/bin/time -o /tmp/time.log ps -axo pid,comm",
    "bash -c 'ps -axo pid,comm'", "printf '%s' 'pgrep -fl mcp'",
    "rg 'pgrep -fl' claude", "python3 -c 'print(42)'",
    "python3 -c 'from pathlib import Path; print(Path(\"README.md\").read_text())'",
    "python3 <<'PY'\nfrom pathlib import Path\nprint(Path('README.md').read_text())\nPY\n",
    "cat <<'EOF'\npgrep -fl mcp\nEOF\n",
    "printf '%s' \"$(pgrep -f emacs)\"", "# pgrep -fl mcp\ntrue",
]
DENY = [
    "cat /proc/123/cmdline", "head /proc/123/environ",
    "cat <<'DATA'\nhello\nDATA\npython3 <<'PY'\nimport os\nos.system('ps aux')\nPY\n",
    "sudo --user root pgrep -fl mcp", "sudo --group staff ps aux",
    "time -f '%s' ps aux", "/usr/bin/time -o /tmp/time.log ps aux",
    "p\\grep -fl mcp", "p'g'rep -fl mcp", "p''s aux",
    "find . -exec ps aux \\;", "cat <<'EOF' | sh\npgrep -fl mcp\nEOF\n",
    "pgrep -fl mcp", "pgrep -l -f mcp", "pgrep --list-full mcp", "pgrep -a mcp",
    "ps", "ps aux", "ps -axo pid,etime,command", "ps -axo pid,args",
    "ps -axo pid,comm -E", "ps e -o pid,comm", "ps -O pid,comm",
    "ps -f -o pid,comm", "pgrep -fl mcp | rg xyz", "pgrep -fl mcp >/tmp/out",
    "sudo -u root env FOO=x nice -n 2 pgrep -fl mcp",
    "true && /usr/bin/pgrep -fl mcp", "bash -lc 'command pgrep -fl mcp'",
    "printf '%s' \"$(pgrep -fl mcp)\"", "echo `ps aux`",
    "cat <(pgrep -fl mcp)", "ps -o \"$FIELDS\"", "env -S 'pgrep -fl mcp'",
    "python3 -c 'import subprocess; subprocess.run([\"pgrep\",\"-fl\",\"mcp\"])'",
    "python3 <<'PY'\nimport os\nos.system('ps aux')\nPY\n",
    "sh <<'SH'\npgrep -fl mcp\nSH\n",
    "cat <<EOF\n$(pgrep -fl mcp)\nEOF\n",
]

class PolicyTests(unittest.TestCase):
    def test_cases(self):
        for family in ("claude", "codex"):
            path = ROOT / family / "hooks/lib-process-output-policy.py"
            spec = importlib.util.spec_from_file_location("process_policy_" + family, path)
            policy = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(policy)
            for command in ALLOW:
                with self.subTest(family=family, command=command): policy.classify(command)
            for command in DENY:
                with self.subTest(family=family, command=command):
                    with self.assertRaises(policy.Denied): policy.classify(command)

    def test_json_contract(self):
        path = ROOT / "claude/hooks/lib-process-output-policy.py"
        for command, decision in [("pgrep -fl mcp", "deny"), ("pgrep -f mcp", "allow")]:
            result = subprocess.run([sys.executable, str(path)], input=json.dumps(command), text=True, capture_output=True, check=True)
            self.assertEqual(json.loads(result.stdout)["decision"], decision)

    def test_copies_match(self):
        self.assertEqual((ROOT / "claude/hooks/lib-process-output-policy.py").read_bytes(), (ROOT / "codex/hooks/lib-process-output-policy.py").read_bytes())

if __name__ == "__main__": unittest.main()
