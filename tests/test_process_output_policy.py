"""Synthetic commands only: never inspect actual process arguments."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[1]
FUNCTION_LIST_COMMAND = """python3 - <<'PY'
import unicodedata
def mapped(t):
 cs=[];ps=[]
 for j,c in enumerate(t):
  for d in unicodedata.normalize('NFD',c.lower()):
   if unicodedata.combining(d):continue
   if d.isspace():
    if cs and cs[-1]!=' ':cs.append(' ');ps.append(j)
   else:cs.append(d);ps.append(j)
 return ''.join(cs),ps
t='Tángo';n,ps=mapped(t);a=0;q='tango';start,end=ps[a],ps[a+len(q)-1]+1
assert t[start:end]=='Tángo'
PY"""
PATH_READ_COMMAND = """python3 - <<'PY'
import json,pathlib
R=pathlib.Path('/Users/pablostafforini/repos/.worktrees/tangodb/biographical-sources');C=R/'research/source-catalog';ps=list((C/'acquisition-pass').glob('acq-forum5-*-reviewed-items.json'));print(len(ps));print(ps[0]);print(json.loads(ps[0].read_text()).keys());print(json.loads(ps[0].read_text())['items'][0]);base=pathlib.Path('/Users/pablostafforini/.local/share/tangodb/source-library/todotango-forum')
for name in ['standard-replay-unattempted-pass','standard-replay-after-redirect-fix','standard-replay-after-unknown-stop']:
 p=base/name/'captures.json';x=json.loads(p.read_text());print(name,x.keys());print(x['captures'][0])
PY"""
ALLOW = [
    FUNCTION_LIST_COMMAND,
    PATH_READ_COMMAND,
    "python3 - <<'PY'\nfor x in [1]:\n ps=[]\n ps.append(x)\n print(ps)\nPY",
    "python3 - <<'PY'\nimport pathlib,json,collections\nfor kind in ['sources','references']:\n ps=list(pathlib.Path('research/source-catalog',kind).glob('acq-forum3-*.json'));print(kind,len(ps),collections.Counter(p.stem.split('-')[2] for p in ps))\nPY",
    "python3 - <<'PY'\nlabel='á'; ps=[1,2]; print(len(ps))\nPY",
    "python3 - <<'PY'\nlabel='a\u2028xé';\nps=[]; print(len(ps))\nPY",
    "python3 - <<'PY'\nlabel='a\fxé';\nps=[]; print(len(ps))\nPY",
    "python3 - <<'PY'\nlabel='a'\rps=[]; print(len(ps))\nPY",
    "python3 - <<'PY'\npgrep=[1,2]; print(len(pgrep))\nPY",
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
    *["python3 - <<'PY'\n" + body + "\nPY" for body in (
        "ps=[]; ps.append(1); ps=object()", "ps=[]; ps.cmdline()",
        "ps=[]; globals()['ps']=object(); ps.append(1)",
        "ps=[]; del ps; ps.append(1)", "ps=[]; ps()",
        "ps=[]; import subprocess as ps; ps.append(1)",
        "ps=[]; ps.append('ps aux')", "ps=[]; exec('pass'); ps.append(1)",
        "ps=[]\ndef f(ps): ps.append(1)",
        "ps=[]\ntry: pass\nexcept Exception as ps: ps.append(1)",
        "ps=[]\nmatch object():\n case ps: ps.append(1)",
        "ps()", "ps.cmdline()", "ps.foo.bar()", "psutil = object()",
        "import subprocess as ps", "# ps command\nprint(1)",
        "import subprocess\nps='ps'; subprocess.run([ps,'aux'])",
        "import subprocess\ncommand='ps aux'; subprocess.run(command)",
        "ps = open('/proc/123/cmdline').read(); print(ps)", "ps = [",
    )],
    "python3 - <<PY\nps=[]; print('$(ps aux)')\nPY",
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
    def test_list_bindings_are_local_to_each_scope(self):
        allowed = [
            'ps=[];ps.append(1)\ndef f():\n ps=[];ps.append(2)\n return ps',
            'def f():\n ps=[]\n def g():\n  ps=[];ps.append(1)\n ps.append(2)\n return ps',
        ]
        denied = [
            'def f(ps):\n ps.append(1)',
            'def f(ps):\n ps=[];ps.append(1)',
            'def f():\n ps=[];ps=object();ps.append(1)',
            'ps=[]\ndef f():\n ps.append(1)',
            'ps=[]\ndef f():\n global ps\n ps=[];ps.append(1)',
            'def f():\n ps=[]\n def g():\n  nonlocal ps\n  ps.append(1)',
            'def f():\n ps=[]\n def g(ps):\n  ps.append(1)',
            'def f():\n ps=[]\n def g():\n  ps.append(1)',
            'def f():\n ps=[];locals().update({});ps.append(1)',
            'def f():\n ps=[];globals().update({});ps.append(1)',
            'def f():\n ps=[];exec("pass");ps.append(1)',
            'def f():\n ps=[];del ps;ps.append(1)',
            'def f():\n ps=[];import subprocess as ps;ps.append(1)',
            'def f():\n ps=[]\n try: pass\n except Exception as ps: ps.append(1)',
            'def f():\n ps=[]\n match object():\n  case ps: ps.append(1)',
            'class C:\n ps=[];ps.append(1)',
            'ps=[]\nf=lambda: ps.append(1)',
            'def f():\n ps=[];[ps.append(x) for x in [1]]',
            'def f():\n ps=[];ps.cmdline()',
            'def f():\n ps=[];ps()',
            'def f():\n ps=[];ps.append("ps aux")',
        ]
        for family in ('claude', 'codex'):
            spec = importlib.util.spec_from_file_location('scope_policy_' + family,
                ROOT / family / 'hooks/lib-process-output-policy.py')
            policy = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(policy)
            for body in allowed:
                policy.classify("python3 - <<'PY'\n" + body + '\nPY')
            for body in denied:
                with self.subTest(family=family, body=body):
                    with self.assertRaises(policy.Denied):
                        policy.classify("python3 - <<'PY'\n" + body + '\nPY')

    def test_indexed_path_read_boundaries(self):
        # Classify source only; none of these commands is executed.
        body = "import pathlib\nroot=pathlib.Path('/tmp');ps=list(root.glob('*.json'));print(ps[0].read_text())"
        mutations = [
            "list=object()\n" + body,
            body.replace("import pathlib", "import pathlib\npathlib=object()"),
            body.replace("import pathlib", "from other import pathlib"),
            body.replace("import pathlib", "import pathlib\nfrom other import list"),
            body.replace("pathlib.Path('/tmp')", "get_path()"),
            body.replace("pathlib.Path('/tmp')", "pathlib.Path(dynamic)"),
            body.replace("'*.json'", "pattern"),
            body.replace("ps[0]", "ps[index]"),
            body.replace("ps[0].read_text()", "ps[0].cmdline()"),
            body.replace("ps[0].read_text()", "ps.cmdline()"),
            body.replace("ps[0].read_text()", "ps()"),
            body.replace("print(ps", "ps=object();print(ps"),
            body.replace("print(ps", "ps[0]=object();print(ps"),
            body.replace("print(ps", "globals()['ps']=object();print(ps"),
            body.replace("print(ps", "exec('pass');print(ps"),
            body.replace("print(ps", "setattr(pathlib,'Path',object());print(ps"),
            body.replace("print(ps", "pathlib.__dict__.update({});print(ps"),
            body + "\nimport subprocess as ps",
            body.replace("print(ps", "\ndef f():\n print(ps"),
            body.replace("'/tmp'", "'/proc/123/cmdline'"),
            body + "\nimport subprocess;subprocess.run(['ps','aux'])",
        ]
        for family in ("claude", "codex"):
            spec = importlib.util.spec_from_file_location("path_policy_" + family,
                ROOT / family / "hooks/lib-process-output-policy.py")
            policy = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(policy)
            policy.classify("python3 - <<'PY'\n" + body + "\nPY")
            for source in mutations:
                with self.subTest(family=family, source=source):
                    with self.assertRaises(policy.Denied):
                        policy.classify("python3 - <<'PY'\n" + source + "\nPY")

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
