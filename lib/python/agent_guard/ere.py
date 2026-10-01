"""`grep -E` as the shell guards ran it: /usr/bin/grep (BSD grep 2.6.0-FreeBSD,
macOS regex) under the hooks' en_GB.UTF-8 locale.

Python's `re` differs from that grep in ways that change guard decisions, so
patterns are translated rather than reused. Measured over every code point
(U+0001-U+10FFFF except LF and surrogates) with /usr/bin/grep:

- `[[:NAME:]]` is libc `iswctype(c, wctype(NAME))`; for `digit` and `xdigit`
  that includes non-ASCII digits (680 and 692 characters), unlike iswdigit().
- `\\w` is `[[:alnum:]_]` (136,785 characters, 5,499 of them classified
  differently from Python's `\\w`), `\\W` its complement; `\\b`, `\\B`, `\\<`,
  `\\>` use that word set. `\\s`/`\\S` are `[[:space:]]` (23 characters, the
  shell_text.GREP_SPACE set) and its complement; `\\d`/`\\D` are `[[:digit:]]`.
- `\\t \\r \\f \\a \\e` are TAB CR FF BEL ESC, but `\\v` is a literal v and
  any other escaped letter or punctuation is itself; `\\n`, `\\x`, `\\Q` and
  backreferences are not supported here and raise ValueError.
- Inside brackets a backslash is an ordinary character: `[ \\t]` matches
  space, backslash and t, not TAB. Ranges compare code points (`[a-z]` is
  the 26 ASCII letters).
- `.` matches every character. grep matches one line at a time, so no
  pattern spans a newline.
- With -i, a literal pattern character matches itself plus towlower() of an
  uppercase or towupper() of a lowercase one; a bracket or class matches a
  text character that is in it, or whose towlower() (if it is uppercase) or
  towupper() (if it is lowercase) is; a negated bracket is the complement of
  that. So `-i k` misses KELVIN SIGN but `-i [a-z]` matches it.

Class membership for non-ASCII text depends on the characters present, so
`Ere` compiles one Python pattern per set of non-ASCII characters seen
(nearly always just the ASCII one).

`grep_only` (`grep -o`) uses Python's leftmost-first matches; POSIX grep
reports leftmost-longest. They agree for patterns whose alternatives cannot
both match at one start and whose repetitions are greedy to the end, which
holds for every `grep -o` pattern in the hooks; check a new one before use.
"""

from __future__ import annotations

import functools
import re

CLASS_NAMES = ("alnum", "alpha", "blank", "cntrl", "digit", "graph", "lower", "print", "punct", "space", "upper",
               "xdigit")


def _span(*ranges: tuple[int, int]) -> frozenset:
    return frozenset(chr(code) for lo, hi in ranges for code in range(lo, hi + 1))


# `[[:NAME:]]` over U+0001-U+007F: libc iswctype under en_GB.UTF-8, which is
# the POSIX ASCII table. Checked against libc by tests/agent_guard/test_ere.py.
ASCII_CLASSES = {
    "alnum": _span((0x30, 0x39), (0x41, 0x5A), (0x61, 0x7A)),
    "alpha": _span((0x41, 0x5A), (0x61, 0x7A)),
    "blank": _span((0x09, 0x09), (0x20, 0x20)),
    "cntrl": _span((0x01, 0x1F), (0x7F, 0x7F)),
    "digit": _span((0x30, 0x39)),
    "graph": _span((0x21, 0x7E)),
    "lower": _span((0x61, 0x7A)),
    "print": _span((0x20, 0x7E)),
    "punct": _span((0x21, 0x2F), (0x3A, 0x40), (0x5B, 0x60), (0x7B, 0x7E)),
    "space": _span((0x09, 0x0D), (0x20, 0x20)),
    "upper": _span((0x41, 0x5A)),
    "xdigit": _span((0x30, 0x39), (0x41, 0x46), (0x61, 0x66)),
}
_ASCII = _span((0x01, 0x7F))
_ASCII_WORD = ASCII_CLASSES["alnum"] | {"_"}


@functools.lru_cache(maxsize=None)
def _libc():
    """libc's wide-character functions, loaded only when non-ASCII text needs them."""
    import ctypes  # noqa: PLC0415 - ctypes costs a few ms; most commands are ASCII

    libc = ctypes.CDLL(None)
    libc.wctype.argtypes = [ctypes.c_char_p]
    libc.wctype.restype = ctypes.c_ulong
    libc.iswctype.argtypes = [ctypes.c_int, ctypes.c_ulong]
    libc.towlower.argtypes = libc.towupper.argtypes = [ctypes.c_int]
    libc.towlower.restype = libc.towupper.restype = ctypes.c_int
    return libc


@functools.lru_cache(maxsize=None)
def _wctype(name: str) -> int:
    return _libc().wctype(name.encode())


@functools.lru_cache(maxsize=65536)
def in_class(name: str, char: str) -> bool:
    """`[[:NAME:]]` membership, as grep tests it."""
    if char < "\x80":
        return char in ASCII_CLASSES[name]
    return bool(_libc().iswctype(ord(char), _wctype(name)))


@functools.lru_cache(maxsize=65536)
def _folds(char: str) -> tuple[str, ...]:
    """The other-case forms grep tries for a text character under -i."""
    if char < "\x80":
        return (char.swapcase(),) if char.isalpha() else ()
    if in_class("upper", char):
        return (chr(_libc().towlower(ord(char))),)
    if in_class("lower", char):
        return (chr(_libc().towupper(ord(char))),)
    return ()


class _Set:
    """A character set: a bracket expression or a class escape."""

    def __init__(self, chars=(), ranges=(), classes=(), word=False, negated=False, everything=False, exact=False):
        self.chars, self.ranges, self.classes = frozenset(chars), tuple(ranges), tuple(classes)
        self.word, self.negated, self.everything = word, negated, everything
        self.exact = exact  # a -i literal's case forms: no further folding of the text
        self._ascii: dict[bool, frozenset] = {}

    def _contains(self, char: str) -> bool:
        return (self.everything or char in self.chars or any(lo <= char <= hi for lo, hi in self.ranges)
                or any(in_class(name, char) for name in self.classes) or (self.word and _is_word(char)))

    def matches(self, char: str, icase: bool) -> bool:
        hit = self._contains(char) or (icase and not self.exact and any(self._contains(f) for f in _folds(char)))
        return hit != self.negated

    def ascii_members(self, icase: bool) -> frozenset:
        """`matches` over U+0001-U+007F, by set algebra on the static tables."""
        if icase not in self._ascii:
            if self.everything:
                hit = _ASCII
            else:
                hit = set(self.chars & _ASCII)
                for lo, hi in self.ranges:
                    hit.update(chr(code) for code in range(max(ord(lo), 1), min(ord(hi), 0x7F) + 1))
                for name in self.classes:
                    hit |= ASCII_CLASSES[name]
                if self.word:
                    hit |= _ASCII_WORD
                if icase and not self.exact:
                    # An ASCII letter folds to an ASCII letter, and to nothing else.
                    hit |= {char for char in _ASCII if char.isalpha() and self._contains(char.swapcase())}
            self._ascii[icase] = frozenset(_ASCII - hit if self.negated else hit)
        return self._ascii[icase]


def _is_word(char: str) -> bool:
    return char == "_" or in_class("alnum", char)


_WORD = _Set(word=True)
_ESCAPE_SETS = {"w": _WORD, "W": _Set(word=True, negated=True), "s": _Set(classes=("space",)),
                "S": _Set(classes=("space",), negated=True), "d": _Set(classes=("digit",)),
                "D": _Set(classes=("digit",), negated=True)}
_ESCAPE_CHARS = {"t": "\t", "r": "\r", "f": "\f", "a": "\a", "e": "\x1b"}
_UNSUPPORTED_ESCAPES = set("nxQ123456789")
_ASSERTIONS = {"b": "b", "B": "B", "<": "<", ">": ">"}
_SED_ESCAPE_CHARS = {"t": "\t", "r": "\r", "n": "\n"}
_INTERVAL = re.compile(r"\{[0-9]+(,[0-9]*)?\}")


def _parse_bracket(pattern: str, pos: int) -> tuple[_Set, int]:
    """Parse the bracket expression whose `[` is at POS; return it and the index after `]`."""
    i = pos + 1
    negated = pattern[i:i + 1] == "^"
    if negated:
        i += 1
    chars, ranges, classes = set(), [], []
    first = True
    while True:
        if i >= len(pattern):
            raise ValueError("unterminated bracket expression")
        c = pattern[i]
        if c == "]" and not first:
            return _Set(chars, ranges, classes, negated=negated), i + 1
        first = False
        if c == "[" and pattern[i + 1:i + 2] in (":", "=", "."):
            kind = pattern[i + 1]
            end = pattern.find(kind + "]", i + 2)
            if end < 0:
                raise ValueError("unterminated bracket class")
            if kind != ":" or pattern[i + 2:end] not in CLASS_NAMES:
                raise ValueError(f"unsupported bracket item {pattern[i:end + 2]}")
            classes.append(pattern[i + 2:end])
            i = end + 2
            continue
        if pattern[i + 1:i + 2] == "-" and pattern[i + 2:i + 3] not in ("", "]"):
            if pattern[i + 2] == "[":
                raise ValueError("unsupported range endpoint")
            ranges.append((c, pattern[i + 2]))
            i += 3
            continue
        chars.add(c)
        i += 1


def _parse(pattern: str, icase: bool, dialect: str) -> list:
    """Tokens: strings of Python regex source, `_Set`s and `("assert", kind)`."""
    tokens: list = []
    i = 0
    n = len(pattern)
    while i < n:
        c = pattern[i]
        if c == "[":
            item, i = _parse_bracket(pattern, i)
            tokens.append(item)
            continue
        if c == "\\":
            if i + 1 >= n:
                raise ValueError("trailing backslash")
            e = pattern[i + 1]
            i += 2
            if dialect == "sed":
                if e in "123456789":
                    raise ValueError(f"unsupported backreference \\{e}")
                tokens.append(_literal(_SED_ESCAPE_CHARS.get(e, e), icase))
            elif e in _ESCAPE_SETS:
                tokens.append(_ESCAPE_SETS[e])
            elif e in _ASSERTIONS:
                tokens.append(("assert", e))
            elif e in _UNSUPPORTED_ESCAPES:
                raise ValueError(f"unsupported escape \\{e}")
            else:
                tokens.append(_literal(_ESCAPE_CHARS.get(e, e), icase))
            continue
        if c == ".":
            tokens.append(_Set(everything=True))
        elif c == "$":
            tokens.append(r"\Z")  # Python's `$` also matches before a final newline
        elif c in "^|()":
            if c == "(" and pattern[i + 1:i + 2] in ("*", "+", "?"):
                raise ValueError("repetition operator after (")
            if c == "|" and (not tokens or tokens[-1] in ("|", "(") or pattern[i + 1:i + 2] in ("", "|", ")")):
                raise ValueError("empty alternative")  # grep and sed both reject it
            tokens.append(c)
        elif c in "*+?":
            tokens.append(c)
        elif c == "{":
            match = _INTERVAL.match(pattern, i)
            if match is None:
                raise ValueError("unsupported brace")
            tokens.append(match.group())
            i = match.end()
            continue
        else:
            tokens.append(_literal(c, icase))
        i += 1
    return tokens


class _Lit(str):
    """A literal character's Python source; `char` is the character itself."""

    char: str


def _literal(char: str, icase: bool):
    if icase:
        forms = {char, *_folds(char)}
        if len(forms) > 1:
            return _Set(forms, exact=True)
    token = _Lit(re.escape(char))
    token.char = char
    return token


def _required_literals(tokens: list) -> tuple[frozenset, ...]:
    """Clauses every match satisfies: in each, at least one of the literal
    strings occurs in the matched line. A text failing a clause cannot match,
    so it needs no compiled pattern. (No _Lit equals a syntax token: re.escape
    escapes ( ) | * + ? {.)"""
    branches, _ = _branches(tokens, 0)
    return tuple(_alternation(branches))


def _alternation(branches: list) -> list:
    if len(branches) == 1:
        return branches[0]
    # Each branch needs its own first clause, so one of those must occur.
    return [frozenset().union(*(branch[0] for branch in branches))] if all(branches) else []


def _branches(tokens: list, pos: int) -> tuple[list, int]:
    """The clause lists of the `|` branches from POS up to an unmatched `)` or the end."""
    branches = []
    while True:
        clauses, pos = _sequence(tokens, pos)
        branches.append(clauses)
        if pos < len(tokens) and tokens[pos] == "|":
            pos += 1
        else:
            return branches, pos


def _sequence(tokens: list, pos: int) -> tuple[list, int]:
    clauses, run = [], ""
    while pos < len(tokens) and tokens[pos] not in ("|", ")"):
        token = tokens[pos]
        if token == "(":
            branches, pos = _branches(tokens, pos + 1)
            pos += 1  # past `)`
            if run:
                clauses.append(frozenset((run,)))
            run = ""
            if not _optional(tokens, pos):
                clauses.extend(_alternation(branches))
            continue
        pos += 1
        if isinstance(token, _Lit) and not _optional(tokens, pos):
            run += token.char
            if pos < len(tokens) and tokens[pos] == "+":
                clauses.append(frozenset((run,)))
                run = ""
        elif run:
            clauses.append(frozenset((run,)))
            run = ""
    if run:
        clauses.append(frozenset((run,)))
    return clauses, pos


def _optional(tokens: list, pos: int) -> bool:
    """Whether the atom ending before POS may match zero times."""
    following = tokens[pos] if pos < len(tokens) else ""
    return isinstance(following, str) and not isinstance(following, _Lit) and (
        following in ("*", "?") or following.startswith("{"))


def _class_source(item: _Set, extra: tuple[str, ...], icase: bool) -> str:
    """A Python character class for the ASCII members plus the members among
    EXTRA (the non-ASCII characters present)."""
    members = item.ascii_members(icase)
    if extra:
        members = members | {char for char in extra if item.matches(char, icase)}
    return _ranges(members)


@functools.lru_cache(maxsize=None)
def _ranges(members: frozenset) -> str:
    """MEMBERS as a Python class of code point ranges; one set recurs across patterns."""
    codes = sorted(map(ord, members))
    if not codes:
        return "(?!)"
    out, start, prev = [], codes[0], codes[0]
    for code in codes[1:] + [None]:
        if code is not None and code == prev + 1:
            prev = code
            continue
        lo, hi = re.escape(chr(start)), re.escape(chr(prev))
        out.append(lo if start == prev else lo + hi if prev == start + 1 else f"{lo}-{hi}")
        if code is not None:
            start = prev = code
    return "[" + "".join(out) + "]"


# For pure-ASCII text: re.ASCII's \b and \w are [0-9A-Za-z_] boundaries.
_ASCII_ASSERTIONS = {"b": r"\b", "B": r"\B", "<": r"\b(?=\w)", ">": r"\b(?<=\w)"}


class Ere:
    """A grep -E (or, with DIALECT "sed", sed -E) pattern. `search`, `finditer`
    and `fullmatch` take a single line, as grep sees it.

    Parsed on first use (unsupported syntax raises ValueError then) and
    compiled to Python only for a text holding every required literal, one
    pattern per set of non-ASCII characters seen; pure-ASCII text needs no
    libc call. Hooks build ~160 of these per process and use about half.
    """

    def __init__(self, pattern: str, ignore_case: bool = False, dialect: str = "grep"):
        self.pattern, self.ignore_case, self.dialect = pattern, ignore_case, dialect
        self._compiled: dict[tuple[str, ...], re.Pattern] = {}

    @functools.cached_property
    def _tokens(self) -> list:
        return _parse(self.pattern, self.ignore_case, self.dialect)

    @functools.cached_property
    def required(self) -> tuple[frozenset, ...]:
        return _required_literals(self._tokens)

    def excludes(self, text: str) -> bool:
        """Whether no line of TEXT can match: a required literal is absent."""
        return any(not any(literal in text for literal in clause) for clause in self.required)

    def _compile(self, extra: tuple[str, ...]) -> re.Pattern:
        compiled = self._compiled.get(extra)
        if compiled is None:
            out = []
            if extra:
                word = _class_source(_WORD, extra, False)
                assertions = {"b": f"(?:(?<={word})(?!{word})|(?<!{word})(?={word}))",
                              "B": f"(?:(?<={word})(?={word})|(?<!{word})(?!{word}))",
                              "<": f"(?<!{word})(?={word})", ">": f"(?<={word})(?!{word})"}
            else:
                assertions = _ASCII_ASSERTIONS
            for token in self._tokens:
                if isinstance(token, str):
                    out.append(token)
                elif isinstance(token, _Set):
                    out.append(_class_source(token, extra, self.ignore_case))
                else:
                    out.append(assertions[token[1]])
            # Under re.ASCII, Python's \w is grep's ASCII word set exactly.
            compiled = self._compiled[extra] = re.compile("".join(out), 0 if extra else re.ASCII)
        return compiled

    def compile(self, text: str) -> re.Pattern:
        """The Python pattern valid for lines drawn from TEXT's characters."""
        if text.isascii():
            return self._compile(())
        return self._compile(tuple(sorted({char for char in text if char > "\x7f"})))

    def search(self, line: str):
        return None if self.excludes(line) else self.compile(line).search(line)

    def finditer(self, line: str):
        return iter(()) if self.excludes(line) else self.compile(line).finditer(line)

    def fullmatch(self, line: str):
        return None if self.excludes(line) else self.compile(line).fullmatch(line)


@functools.lru_cache(maxsize=512)
def ere(pattern: str, ignore_case: bool = False) -> Ere:
    return Ere(pattern, ignore_case)


_ECHO_OPTIONS = re.compile("-[neE]+")


def lines(text: str, echo: bool = False) -> list[str]:
    """The lines grep reads from `printf '%s' TEXT` or, with ECHO, `echo "$TEXT"`.

    bash's builtin echo (3.2 and 5.3 alike, xpg_echo off) takes a TEXT made
    only of option letters, like `-n` or `-eE`, as options: it then prints
    nothing if they include n and a lone newline otherwise.
    """
    if echo and _ECHO_OPTIONS.fullmatch(text):
        return [] if "n" in text else [""]
    lines = text.split("\n")
    if not echo and lines[-1] == "":
        lines.pop()
    return lines


def _pattern(pattern, ignore_case: bool) -> Ere:
    return pattern if isinstance(pattern, Ere) else ere(pattern, ignore_case)


def grep(pattern, text: str, ignore_case: bool = False, echo: bool = False) -> bool:
    """`printf '%s' TEXT | grep -qE PATTERN`, or with ECHO `echo "$TEXT" | grep -qE
    PATTERN` (see `_lines`). PATTERN is a string or an Ere."""
    ere_ = _pattern(pattern, ignore_case)
    if ere_.excludes(text):
        return False
    compiled = ere_.compile(text)
    return any(compiled.search(line) for line in lines(text, echo))


def grep_lines(pattern, text: str, ignore_case: bool = False, echo: bool = False, invert: bool = False) -> list[str]:
    """The lines `grep -E` (`-v` with INVERT) prints, without their newlines."""
    ere_ = _pattern(pattern, ignore_case)
    if ere_.excludes(text):
        return lines(text, echo) if invert else []
    compiled = ere_.compile(text)
    return [line for line in lines(text, echo) if (compiled.search(line) is None) == invert]


def grep_only(pattern, text: str, ignore_case: bool = False, echo: bool = False) -> list[str]:
    """The matches `grep -oE` prints, one per element; empty matches are not printed."""
    ere_ = _pattern(pattern, ignore_case)
    if ere_.excludes(text):
        return []
    compiled = ere_.compile(text)
    return [m.group() for line in lines(text, echo) for m in compiled.finditer(line) if m.group()]


# --- sed -E -----------------------------------------------------------------

_SED_FLAGS = ("", "g")


@functools.lru_cache(maxsize=512)
def _sed_rule(pattern: str, replacement: str, flags: str):
    if flags not in _SED_FLAGS:
        raise ValueError(f"unsupported sed flags {flags!r}")
    compiled = Ere(pattern, dialect="sed")
    groups = compiled.compile("").groups
    parts: list = []  # strings and group numbers (0 for `&`)
    i = 0
    while i < len(replacement):
        c = replacement[i]
        if c == "&":
            parts.append(0)
        elif c == "\\" and i + 1 < len(replacement):
            i += 1
            e = replacement[i]
            if e in "123456789":
                if int(e) > groups:
                    raise ValueError(f"\\{e} not defined in the RE")
                parts.append(int(e))
            else:
                parts.append("\n" if e == "n" else e)
        else:
            parts.append(c)
        i += 1
    return compiled, tuple(parts), flags == "g"


def _expand(parts: tuple, match: re.Match) -> str:
    return "".join(part if isinstance(part, str) else (match.group(part) or "") for part in parts)


def _substitute(compiled: re.Pattern, parts: tuple, every: bool, space: str) -> str:
    """One `s///` over a pattern space, as BSD sed's substitute() walks it: an
    empty match right after a non-empty one is not replaced, and the scan
    steps one character past every empty match."""
    match = compiled.search(space)
    if match is None:
        return space
    out, end, last_empty = [], 0, True
    while match is not None:
        start, stop = match.span()
        out.append(space[end:start])
        replaced = last_empty or start != end or start != stop
        if replaced:
            out.append(_expand(parts, match))
        end = stop
        if start == stop:
            if end >= len(space):
                break
            out.append(space[end])
            end += 1
            last_empty = True
        else:
            last_empty = False
        if replaced and not every:
            break
        match = compiled.search(space, end)
    out.append(space[end:])
    return "".join(out)


def sed(text: str, *rules: tuple[str, str, str]) -> str:
    """stdout of `printf '%s' TEXT | sed -E -e 's/P1/R1/F1' -e ...` for RULES of
    (P, R, F), F being "" or "g". Callers capturing it with `$(...)` apply
    codex_payload.command_substitution.

    Measured against /usr/bin/sed under en_GB.UTF-8: classes and brackets are
    grep's, but every escape outside brackets except `\\t`, `\\r` and `\\n` is the
    literal character (`\\s`, `\\b`, `\\w`, `\\d` match s, b, w, d). In the
    replacement `&` is the match, `\\1`-`\\9` groups, `\\n` a newline and `\\X` is
    X. Each input line is one pattern space, so `.` and `[^x]` match a newline
    an earlier rule inserted, while `^` and `$` match only at its ends.

    Group contents follow Python's leftmost-first choice, POSIX sed's
    leftmost-longest subexpressions differ when an alternation is ambiguous
    (`(a|ab)(c|bcd)` on abcd); the rules the hooks use are checked in
    tests/agent_guard/test_ere.py.
    """
    # Replacements can bring characters the text lacks into later rules' input.
    alphabet = text + "".join(replacement for _, replacement, _ in rules)
    compiled = [(rule.compile(alphabet), parts, every) for rule, parts, every in (_sed_rule(*r) for r in rules)]
    lines = text.split("\n")
    out = []
    for index, line in enumerate(lines):
        if index == len(lines) - 1 and line == "":
            break
        for pattern, parts, every in compiled:
            line = _substitute(pattern, parts, every, line)
        out.append(line)
    return "\n".join(out) + ("\n" if text.endswith("\n") else "")


def sub(pattern: str, replacement: str, text: str, flags: str = "") -> str:
    """`printf '%s' TEXT | sed -E 's/PATTERN/REPLACEMENT/FLAGS'` (see `sed`)."""
    return sed(text, (pattern, replacement, flags))
