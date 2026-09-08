#!/usr/bin/env python3
"""Share and check the app-data denial used by the Automations broker.

macOS cannot apply a second restrictive sandbox. Callers that sandbox their
children must include --profile in their outer profile; --check verifies the
inherited denial without opening protected app data or trusting an env flag.
"""

import ctypes
import json
import os
from pathlib import Path
import sys


def container_paths(home: Path) -> list[Path]:
    parent = (home / "Library/Group Containers").resolve()
    names = {"2BUA8C4S2C.com.1password"}
    try:
        names.update(name for name in os.listdir(parent) if "2BUA8C4S2C" in name)
    except FileNotFoundError:
        pass
    # Do not stat these paths: the inherited sandbox may already deny metadata.
    return [parent / name for name in sorted(names)]


def profile(paths: list[Path]) -> str:
    clauses = " ".join(f"(subpath {json.dumps(str(path))})" for path in paths)
    return f"(deny file-read* file-write* {clauses})"


def inherited_denial(paths: list[Path]) -> bool:
    # sandbox_check is Apple's SPI, also declared by WebKit's SandboxSPI.h.
    # Querying policy avoids triggering TCC by probing the actual container.
    library = ctypes.CDLL("/usr/lib/libsandbox.1.dylib", use_errno=True)
    check = library.sandbox_check
    check.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int]
    check.restype = ctypes.c_int
    path_filter = 1 | ctypes.c_int.in_dll(library, "SANDBOX_CHECK_NO_REPORT").value
    # A literal-only denial on the directory is insufficient: op can probe
    # children directly. Check descendants too, without touching app data.
    # Use standard container children: sandbox_check resolves nonexistent
    # paths to an existing ancestor, so a made-up probe tests only the root.
    # Callers must compose --profile; these queries detect missing protection,
    # not arbitrary equivalence between policies with path-specific exceptions.
    probes = [path / child for path in paths for child in
              ("", ".com.apple.containermanagerd.metadata.plist", "Library", "Data")]
    for path in probes:
        for operation in (b"file-read-data", b"file-read-metadata", b"file-write-data"):
            result = check(os.getpid(), operation, path_filter,
                           ctypes.c_char_p(os.fsencode(path)))
            if result < 0:
                raise OSError(ctypes.get_errno(), "sandbox policy query failed")
            if result == 0:
                return False
    return True


def main() -> int:
    if sys.argv[1:] not in (["--profile"], ["--check"]):
        print("usage: op_automations_sandbox.py --profile|--check", file=sys.stderr)
        return 2
    try:
        paths = container_paths(Path.home())
        if sys.argv[1] == "--profile":
            print(profile(paths))
            return 0
        return 0 if inherited_denial(paths) else 1
    except (OSError, AttributeError, ValueError) as error:
        print(f"op-automations: cannot inspect app-data sandbox: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
