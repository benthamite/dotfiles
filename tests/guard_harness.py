"""Run guard policies in-process, as the former shell hooks ran.

The guard tests called the shell hooks in claude/hooks and codex/hooks as
subprocesses. Those hooks are replaced by lib/python/agent_guard
(docs/plans/2026-10-01-guard-engine.md), so the tests call the engine here:

- `run_guard(hook, payload)`: one former hook, by file name or path, through
  `agent_guard.engine.main` with that hook's policy as the whole chain;
- `run_engine(runtime, payload)`: the full chain the runtime registers, which
  is what production runs;
- `run_e2e(runtime, payload)`: the real `{claude,codex}/hooks/agent-guard.sh`
  as a subprocess, for the few tests that must prove wiring.

Each returns a `subprocess.CompletedProcess`-like result: stdout carries the
JSON decision (empty for a silent allow), and a failure is returncode 2 with
"Security hook failed; tool execution denied." on stderr.

ENV (default: the current environment) replaces os.environ for the call,
since policies and the secret index read it directly, and CWD is the process
directory, as the hook's subprocess cwd was. `agent_guard.core.HOME` is fixed at import, so a test that
changes HOME changes only what policies read from the environment.

Unless the caller's environment names one, SECRET_INDEX_PATH points at a
missing file, so no test reads the real known-secret index (or its Keychain
key).
"""

from __future__ import annotations

import contextlib
import io
import json
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from unittest import mock

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import engine  # noqa: E402
from agent_guard.core import CLAUDE, CODEX  # noqa: E402

POLICY_FOR = {
    "pretooluse-bash.sh": "dispatcher",
    "block-secret-leak.sh": "secret_leak",
    "block-sensitive-read.sh": "sensitive_read",
    "block-untrusted-execution.sh": "untrusted_execution",
    "block-destructive-command.sh": "destructive",
    "block-github-write-command.sh": "github_write",
    "block-dirty-tree-rewrite.sh": "dirty_tree",
    "block-walk-list-access.sh": "walk_list",
    "block-hookspath-override.sh": "hookspath",
    "block-elpaca-rebuild-uncommitted.sh": "elpaca_rebuild",
    "block-elpaca-dotfiles-edit.sh": "elpaca_dotfiles_edit",
    "block-unregistered-launchagent.sh": "launchagent",
    "block-unguarded-ahrefs-api.sh": "ahrefs",
    "block-texi-edit.sh": "texi_edit",
    "block-long-pipeline-run-unchecked.sh": "long_pipeline",
    "require-elisp-skill.sh": "elisp_skill",
    "block-retired-secrets-cli.py": "retired_secrets",
}


@dataclass
class GuardResult:
    returncode: int
    stdout: str
    stderr: str

    def decision(self) -> dict | None:
        """The parsed hookSpecificOutput, or None for a silent allow."""
        return json.loads(self.stdout)["hookSpecificOutput"] if self.stdout.strip() else None


def _runtime_of(hook: str | os.PathLike, runtime: str | None) -> str:
    if runtime:
        return runtime
    return CODEX if "codex/hooks" in Path(hook).as_posix() else CLAUDE


def _policy(hook: str | os.PathLike):
    name = Path(hook).name
    if name not in POLICY_FOR:
        raise KeyError(f"no policy replaces {name}")
    return __import__(f"agent_guard.policies.{POLICY_FOR[name]}", fromlist=["decide"])


def _stdin_text(payload) -> str:
    if isinstance(payload, (dict, list)):
        return json.dumps(payload)
    return payload.decode("utf-8", "surrogateescape") if isinstance(payload, bytes) else payload


MISSING_INDEX = "/nonexistent/agent-guard-test-secret-index.json"


def _with_test_index(env: dict | None) -> dict:
    env = dict(os.environ if env is None else env)
    env.setdefault("SECRET_INDEX_PATH", MISSING_INDEX)
    return env


@contextlib.contextmanager
def _environment(cwd, env):
    saved_env, saved_cwd = dict(os.environ), os.getcwd()
    try:
        effective = _with_test_index(env)
        os.environ.clear()
        os.environ.update(effective)
        if cwd is not None:
            os.chdir(cwd)
        yield
    finally:
        os.chdir(saved_cwd)
        os.environ.clear()
        os.environ.update(saved_env)


def _main(runtime: str, payload, cwd, env, decide=None) -> GuardResult:
    stdout, stderr = io.StringIO(), io.StringIO()
    patch = mock.patch.object(engine, "decide", decide) if decide else contextlib.nullcontext()
    with _environment(cwd, env), patch, \
            mock.patch.object(sys, "stdin", io.StringIO(_stdin_text(payload))), \
            contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
        returncode = engine.main(runtime)
    return GuardResult(returncode, stdout.getvalue(), stderr.getvalue())


def run_guard(hook, payload, *, runtime: str | None = None, cwd=None, env: dict | None = None) -> GuardResult:
    """What running the former shell HOOK on PAYLOAD (dict, or raw stdin text) printed."""
    policy = _policy(hook)
    return _main(_runtime_of(hook, runtime), payload, cwd, env, decide=policy.decide)


def run_engine(runtime: str, payload, *, cwd=None, env: dict | None = None) -> GuardResult:
    """The production decision: every policy RUNTIME registers for the tool."""
    return _main(runtime, payload, cwd, env)


def run_e2e(runtime: str, payload, *, cwd=None, env: dict | None = None, timeout: float = 30) -> GuardResult:
    """The real hook entry script as a subprocess, for wiring tests."""
    script = ROOT / runtime / "hooks/agent-guard.sh"
    result = subprocess.run([str(script)], input=_stdin_text(payload), capture_output=True, text=True,
                            cwd=cwd, env=_with_test_index(env), timeout=timeout)
    return GuardResult(result.returncode, result.stdout, result.stderr)
