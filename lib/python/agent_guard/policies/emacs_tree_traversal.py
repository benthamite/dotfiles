"""Bulk file traversals from an agent running inside Emacs.

macOS's XProtect watches the process tree of scripted commands. When a
shell command under Emacs opens thousands of files one by one, XProtect
kills the whole "container", which is Emacs itself: no crash report, no
kill-emacs-hook. Seen 2026-09-13/14 (stafforini.com decision 025), twice on
2026-11-17 (`tr '\\n' '\\0' < files.txt | xargs -0 rg` over ~12,000 org-id
files) and 2026-11-20 (a full `export-notes.el` scan of 5,276 notes). The
rule XProtect matched is `<private>` in the log, so the shapes below are
the ones observed, not a proof of what triggers it.

Only sessions inside Emacs are checked (CLAUDE_BUFFER_NAME,
CODEX_BUFFER_NAME or INSIDE_EMACS in the environment). Denied when the file
list has more than MAX_FILES entries:
- `PRODUCER | ... | xargs CMD`, where the list comes from a file (`< F`,
  `cat F`, `xargs -a F`), `git ls-files`, `find`/`fd`/`bfs`, or
  `rg --files`;
- `for VAR in $(PRODUCER)` with the same producers.
Lists are counted at check time (a file's lines, `git ls-files`'s output, a
directory walk that stops at MAX_FILES + 1). A list the guard cannot resolve
statically is allowed: this catches accidents, not evasion. A single search
process over a directory (`rg PATTERN DIR`) is left to search_root.
"""

from __future__ import annotations

import os
import subprocess

from ..codex_payload import _extract_substitutions, _split_segments, _tokenize
from ..core import Call, Result
from ..shell_text import mask_heredoc_bodies
from .search_root import _command_start, _home, _shell_contexts, resolve

MAX_FILES = 500
EMACS_ENV = ("CLAUDE_BUFFER_NAME", "CODEX_BUFFER_NAME", "INSIDE_EMACS")
# xargs commands that do not open the listed files.
_HARMLESS = frozenset({"echo", "printf", "kill", "basename", "dirname"})
_WALKERS = frozenset({"find", "gfind", "bfs", "fd", "fdfind"})

REASON = ("BLOCKED: this command opens {count} files one by one from a shell inside Emacs ({what}). "
          "XProtect kills Emacs itself when a process under it does this (2026-09-14, 2026-11-17, "
          "2026-11-20). Instead: work on the few files you need; use the project's own entry point "
          "(e.g. stafforini.com's scripts/export-notes.sh, or EXPORT_FILE_LIST for one note); or use "
          "one search process over a directory (`rg PATTERN DIR`, `git grep`) rather than a file list. "
          f"Limit: {MAX_FILES} files ({__name__}).")


def inside_emacs(call: Call) -> bool:
    return any(call.env.get(name) for name in EMACS_ENV)


def _count_lines(path: str, cap: int) -> int:
    count = 0
    with open(path, "rb") as handle:
        for _ in handle:
            count += 1
            if count > cap:
                break
    return count


def _walk_count(root: str, cap: int) -> int:
    count = 0
    for _dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d != ".git"]
        count += len(filenames)
        if count > cap:
            break
    return count


def _git_count(args: list[str], cwd: str | None) -> int | None:
    if cwd is None:
        return None
    try:
        out = subprocess.run(["git", "-C", cwd, "ls-files", *[a for a in args if not a.startswith("-")]],
                             capture_output=True, timeout=5, check=False).stdout
    except (OSError, subprocess.SubprocessError):
        return None
    return out.count(b"\n") + out.count(b"\0")


def _producer_count(words: list[str], cwd: str | None, home: str) -> tuple[int, str] | None:
    """(file count, description) of a file-list producer, or None."""
    pos = _command_start(words)
    if pos >= len(words):
        return None
    tool, args = os.path.basename(words[pos]), words[pos + 1:]
    if "<" in args[:-1]:
        path = resolve(args[args.index("<") + 1], cwd, home)
        if path and os.path.isfile(path):
            return _count_lines(path, MAX_FILES), f"a list read from {path}"
    if tool == "cat" and args and not args[0].startswith("-"):
        path = resolve(args[0], cwd, home)
        if path and os.path.isfile(path):
            return _count_lines(path, MAX_FILES), f"a list read from {path}"
    if tool == "git" and args[:1] == ["ls-files"]:
        count = _git_count(args[1:], cwd)
        return (count, "`git ls-files`") if count is not None else None
    if tool in _WALKERS or (tool == "rg" and "--files" in args):
        if any(a in ("-maxdepth", "--max-depth", "-d") for a in args):
            return None
        if tool in ("find", "gfind", "bfs"):    # roots come before the first expression word
            roots = []
            for arg in args:
                if arg.startswith("-") or arg in ("!", "(", ")"):
                    break
                roots.append(arg)
        else:
            operands = [a for a in args if not a.startswith("-")]
            roots = operands[1:] if tool in ("fd", "fdfind") else operands    # fd PATTERN PATH...
        roots = roots or ["."]
        total = 0
        for root in roots:
            path = resolve(root, cwd, home)
            if not path or not os.path.isdir(path):
                return None
            total += _walk_count(path, MAX_FILES)
        return total, f"`{tool}` over {', '.join(roots)}"
    return None


def _xargs_findings(segments, cwd, home) -> list[str]:
    findings = []
    pipeline: list[list[str]] = []
    for words, terminator in segments:
        pipeline.append(words)
        pos = _command_start(words)
        if pos < len(words) and os.path.basename(words[pos]) == "xargs":
            args = words[pos + 1:]
            source = None
            for flag in ("-a", "--arg-file"):
                if flag in args[:-1]:
                    path = resolve(args[args.index(flag) + 1], cwd, home)
                    if path and os.path.isfile(path):
                        source = (_count_lines(path, MAX_FILES), f"a list read from {path}")
            if source is None and len(pipeline) > 1:
                for upstream in pipeline[:-1]:
                    source = _producer_count(upstream, cwd, home) or source
            command = next((a for a in args if not a.startswith("-")), "echo")
            if source and source[0] > MAX_FILES and os.path.basename(command) not in _HARMLESS:
                findings.append(REASON.format(count=f"more than {MAX_FILES}",
                                              what=f"{source[1]} piped to `xargs {command}`"))
        if terminator != "|":
            pipeline = []
    return findings


def _for_findings(segments, substitutions, cwd, home) -> list[str]:
    findings = []
    for words, _terminator in segments:
        if len(words) >= 4 and words[0] == "for" and words[2] == "in":
            for word in words[3:]:
                if word.startswith("__CODEX_SUB_"):
                    index = int(word.strip("_").rpartition("_")[2])
                    for sub_words, _ in _split_segments(_tokenize(substitutions[index])):
                        source = _producer_count(sub_words, cwd, home)
                        if source and source[0] > MAX_FILES:
                            findings.append(REASON.format(count=f"more than {MAX_FILES}",
                                                          what=f"a `for` loop over {source[1]}"))
    return findings


def findings(command: str, cwd: str | None, home: str) -> list[str]:
    command = mask_heredoc_bodies(command.replace("\\\n", ""))
    masked, substitutions, invalid = _extract_substitutions(command)
    if invalid:
        return []
    segments = list(_split_segments(_tokenize(masked)))
    for words, _ in segments:    # a leading `cd DIR` sets the directory later lists are read from
        pos = _command_start(words)
        if pos < len(words) and words[pos] == "cd" and len(words) > pos + 1:
            cwd = resolve(words[pos + 1], cwd, home) or cwd
            break
    return _xargs_findings(segments, cwd, home) + _for_findings(segments, substitutions, cwd, home)


def decide(call: Call) -> Result:
    if not call.is_shell or not inside_emacs(call):
        return Result.allow()
    home = _home(call)
    lines = []
    for command, workdir in _shell_contexts(call):
        cwd = resolve(workdir, None, home) if workdir else None
        lines += findings(command, cwd, home)
    return Result.deny("\n\n".join(dict.fromkeys(lines))) if lines else Result.allow()
