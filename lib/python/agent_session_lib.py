"""Loader for agent.el's session library, which ships with the agent package.

The library used to live in this file.  Its only copy is now
lib/python/agent_session_lib.py in the active profile's agent checkout.  This
file runs that copy in its own namespace, so the consumers that load this path
(the orchestrate-review and request-review helpers and their tests) get the
same module object and patch points as before.
"""

import subprocess as _loader_subprocess
from pathlib import Path as _LoaderPath

_RESOLVER = _LoaderPath(__file__).resolve().parents[2] / "bin/elpaca-package-path"
_FOUND = _loader_subprocess.run(
    [str(_RESOLVER), "--on-disk", "agent", "lib/python/agent_session_lib.py"],
    capture_output=True, text=True, check=False)
if _FOUND.returncode != 0:
    raise ImportError("cannot locate the agent package's session library: "
                      + _FOUND.stderr.strip())
_SOURCE = _FOUND.stdout.strip()
with open(_SOURCE, encoding="utf-8") as _handle:
    exec(compile(_handle.read(), _SOURCE, "exec"), globals())
