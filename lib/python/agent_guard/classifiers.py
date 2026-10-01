"""In-process access to the existing Python guard classifiers.

The shell guards ran these as separate `python3` processes (one per check,
40-160 ms each). The engine imports them instead. Load them only through this
function so their location can change in one place:

- `lib-<name>.py`, identical copies in claude/hooks/ and codex/hooks/;
- `github-operation-authorization`, the bin/ tool whose `check` mode the
  GitHub-write guard ran on every shell command.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import sys
from functools import lru_cache
from types import ModuleType

from .core import DOTFILES

HOOK_LIBS = ("op-policy", "process-output-policy", "function-body-policy", "inert-mentions",
             "public-url-scan", "python-heredoc", "untrusted-execution")
BIN_TOOLS = ("github-operation-authorization",)


@lru_cache(maxsize=None)
def load(name: str) -> ModuleType:
    """The classifier module `name`, loaded once per process."""
    if name in HOOK_LIBS:
        path = DOTFILES / "claude/hooks" / f"lib-{name}.py"
    elif name in BIN_TOOLS:
        path = DOTFILES / "bin" / name
    else:
        raise KeyError(name)
    module_name = f"agent_guard_classifier_{name.replace('-', '_')}"
    loader = importlib.machinery.SourceFileLoader(module_name, str(path))
    spec = importlib.util.spec_from_loader(module_name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    # Some classifiers set sys.dont_write_bytecode when imported, to keep
    # __pycache__ out of Drive when run as scripts. In-process that would stop
    # bytecode caching (under the hooks' out-of-Drive pycache_prefix) for
    # everything imported later, so restore the flag.
    flag = sys.dont_write_bytecode
    try:
        loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = flag
    return module
