"""Heredoc and commit-message masking, ported from claude/hooks/lib-heredoc.sh.

Both functions return what ``$(mask_... "$COMMAND")`` gave the shell guards:
the masked text with trailing newlines stripped. They work on UTF-8 bytes
because macOS awk (20200816) and perl without -C count and match bytes, not
characters.
"""

from __future__ import annotations

import re

# `[[:space:]]` as /bin/bash 3.2 and Homebrew bash 5 match it in `case` and
# parameter-expansion patterns under the hooks' en_GB.UTF-8 locale, measured
# over U+0001-U+0020, U+007F, U+0085, U+00A0, U+1680, U+180E, U+2000-U+200B,
# U+2028, U+2029, U+202F, U+205F, U+3000 and U+FEFF. U+0085, U+180E, U+200B,
# U+FEFF and U+001C-U+001F are not members, unlike Python's str.isspace().
BASH_SPACE = ("\t\n\v\f\r \u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a"
              "\u2028\u2029\u202f\u205f\u3000")
# `[[:space:]]` in /usr/bin/grep -E (BSD grep 2.6.0) under the same locale,
# measured over the same characters: BASH_SPACE without the newline, which
# grep never sees because it matches one line at a time. Apply it per line
# (or with re.MULTILINE anchors), never across a newline.
GREP_SPACE = BASH_SPACE.replace("\n", "")

_SINKS = frozenset(
    b"cat tee git gh head tail wc sort uniq grep rg diff cmp tr cut fold less more md5 md5sum "
    b"shasum sha256sum column nl paste copy-slack-draft kill-ring-put".split())
_PREFIX_WORDS = frozenset(b"command env sudo timeout nice exec nohup time builtin".split())
_NONSHELL = re.compile(
    rb"(python[0-9.]*|node|nodejs|deno|bun|ruby|perl|php|osascript|emacs|emacsclient|sqlite3|psql|mysql"
    rb"|jq|yq|Rscript|lua|luajit|swift|julia|g?awk|mawk|sed|bc|dc)")
_NETWORK_PREFIX = re.compile(rb"[ \t]*cat[ \t]+>>?[ \t]*[A-Za-z0-9_./-]+\.(md|org|txt|json)[ \t]*")
_ASSIGNMENT = re.compile(rb"[A-Za-z_][A-Za-z0-9_]*=")
_DURATION = re.compile(rb"[0-9]+[smhd]?")
_PYENV = re.compile(rb"(^|/)pyenv\Z")
_BLANK = re.compile(rb"[ \t]*")
_SQ_WORD = re.compile(rb"'[^']*'")
_DQ_WORD = re.compile(rb'"[^"]*"')
_BARE_WORD = re.compile(rb"[A-Za-z_][A-Za-z0-9_.-]*")
_COMMIT_MESSAGE = re.compile(
    rb"(\bcommit\b[^;&|\n]*?\s(?:-[a-zA-Z]*m|--message)(?:=|\s+))"
    rb"(?:'[^'`]*'|\"(?:[^\"`\\$]|\\.|\$(?!\())*\")")
_SUBST = re.compile(rb"[(`]")


def _encode(text: str) -> bytes:
    return text.encode("utf-8", "surrogateescape")


def _decode(data: bytes) -> str:
    return data.decode("utf-8", "surrogateescape").rstrip("\n")


def _neutralize(text: bytes) -> bytes:
    """Replace quoted spans and escapes with `x`, as the awk `neutralize` did.

    A trailing backslash still yields two bytes, so the result can be one byte
    longer than TEXT; `_maskable` keeps the awk arithmetic that depends on it.
    """
    out = bytearray()
    sq = dq = False
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if sq:
            out += b"x"
            sq = c != 0x27
        elif c == 0x5C:
            out += b"xx"
            i += 1
        elif dq:
            out += b"x"
            dq = c != 0x22
        elif c == 0x27:
            sq = True
            out += b"x"
        elif c == 0x22:
            dq = True
            out += b"x"
        else:
            out.append(c)
        i += 1
    return bytes(out)


def _maskable(prefix: bytes, suffix: bytes, mode: str) -> bool:
    if mode == "all":
        return True
    if re.search(rb"[|(`]", suffix):
        return False
    seg = _neutralize(prefix)
    last = max(seg.rfind(b";"), seg.rfind(b"|"), seg.rfind(b"&"))
    seg = seg[last + 1:]
    tokens = [tok for tok in re.split(rb"[ \t]+", seg) if tok]
    word = b""
    k = 0
    while k < len(tokens):
        tok = tokens[k]
        if _ASSIGNMENT.match(tok) or tok in _PREFIX_WORDS:
            k += 1
            continue
        if _PYENV.search(tok) and k + 1 < len(tokens) and tokens[k + 1] == b"exec":
            k += 2
            continue
        if tok.startswith(b"-") or _DURATION.fullmatch(tok):
            k += 1
            continue
        word = tok
        break
    if not word:
        return False
    program = word.rsplit(b"/", 1)[-1]
    owner = prefix[max(len(prefix) - len(seg), 0):]
    prefix_subst = bool(_SUBST.search(prefix))
    if mode != "nonshell" and prefix_subst:
        return False
    if _SUBST.search(owner):
        return False
    if mode == "nonshell" and _NONSHELL.fullmatch(program):
        return True
    if prefix_subst:
        return False
    if mode == "network":
        return bool(_NETWORK_PREFIX.fullmatch(prefix) and _BLANK.fullmatch(suffix))
    return program in _SINKS


def mask_heredoc_bodies(command: str, mode: str = "sinks") -> str:
    """Drop heredoc bodies the guard can prove are inert data.

    Modes as documented in lib-heredoc.sh: "sinks" (default), "all",
    "nonshell" and "network".
    """
    lines = (_encode(command) + b"\n").split(b"\n")[:-1]
    out: list[bytes] = []
    queue: list[list] = []          # [terminator, strip_tabs, drop, identity]
    in_sq = in_dq = False
    sequence = 0
    last_nonempty = 0
    terminal_end = 0
    terminal_id = 0
    masked: dict[int, int] = {}
    for nr, line in enumerate(lines, 1):
        if mode == "network" and not _BLANK.fullmatch(line):
            last_nonempty = nr
        if queue:
            term, strip, drop, identity = queue[0]
            t = line.lstrip(b"\t") if strip else line
            if t == term:
                if mode == "network" and drop:
                    terminal_end, terminal_id = nr, identity
                queue.pop(0)
                if mode != "network":
                    out.append(line)
            elif mode == "network":
                if drop:
                    masked[nr] = identity
            elif not drop:
                out.append(line)
            continue
        if mode != "network":
            out.append(line)
        m, j = len(line), 0
        while j < m:
            c = line[j]
            if in_sq:
                in_sq = c != 0x27
                j += 1
                continue
            if in_dq:
                if c == 0x5C:
                    j += 2
                    continue
                in_dq = c != 0x22
                j += 1
                continue
            if c == 0x5C:
                j += 2
                continue
            if c == 0x27:
                in_sq = True
                j += 1
                continue
            if c == 0x22:
                in_dq = True
                j += 1
                continue
            if (line[j:j + 2] == b"<<" and line[j + 2:j + 3] != b"<"
                    and (j == 0 or line[j - 1] != 0x3C)):
                rest0 = line[j + 2:]
                rest = rest0
                strip = rest.startswith(b"-")
                if strip:
                    rest = rest[1:]
                rest = rest.lstrip(b" \t")
                quoted = False
                match = _SQ_WORD.match(rest) or _DQ_WORD.match(rest)
                if match:
                    word, consumed, quoted = match.group()[1:-1], match.end(), True
                else:
                    match = _BARE_WORD.match(rest)
                    word, consumed = (match.group(), match.end()) if match else (b"", 0)
                if not word:
                    j += 2
                    continue
                sequence += 1
                previous = lines[nr - 2] if nr > 1 else b""
                drop = _maskable(line[:j], rest[consumed:], mode) and (
                    mode != "network" or (quoted and not previous.endswith(b"\\")))
                queue.append([word, strip, drop, sequence])
                j += 2 + (len(rest0) - len(rest)) + consumed
                continue
            j += 1
    if mode == "network":
        out = [line for nr, line in enumerate(lines, 1)
               if not (terminal_id and terminal_end == last_nonempty and masked.get(nr) == terminal_id)]
    return _decode(b"".join(line + b"\n" for line in out))


def mask_git_commit_messages(command: str) -> str:
    """Replace quoted `git commit -m` / `--message` arguments by COMMIT_MESSAGE."""
    data = _encode(command)
    while match := _COMMIT_MESSAGE.search(data):
        data = data[:match.start()] + match.group(1) + b"COMMIT_MESSAGE" + data[match.end():]
    return _decode(data)
