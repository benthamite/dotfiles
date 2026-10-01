"""agent_guard.ere against /usr/bin/grep -E and bash's echo, on generated lines.

The lines include every character whose word-character status differs
between libc (grep) and Python's `\\w`, the measured space set, non-ASCII
digits and the characters whose case folding grep treats specially. Run by
`bin/test-guards --differential`; the fast suite has test_ere.py.
"""

import ctypes
import locale
import os
import random
import subprocess
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import ere  # noqa: E402

GREP = "/usr/bin/grep"
LOCALE = "en_GB.UTF-8"
CODE_POINTS = [c for c in range(1, 0x110000) if c != 10 and not 0xD800 <= c <= 0xDFFF]

PATTERNS = [
    r"\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b",
    r"(^|[ \t|;&(/])(eval|bash|sh)([ \t]|$)",
    r"(^|[[:space:]])ALLOW_CLONE=1([[:space:]]|$)",
    r"\w", r"\W", r"\s", r"\S", r"\d", r"\D", r"\Brm", r"\<rm\>", r"rm\b", r"\brm",
    "[[:alnum:]]", "[[:alpha:]]", "[[:upper:]]", "[[:lower:]]", "[[:digit:]]", "[[:punct:]]", "[[:xdigit:]]",
    "[[:blank:]]", "[[:cntrl:]]", "[[:print:]]", "[[:graph:]]", "[[:space:]]",
    "[^[:alnum:]_-]", "[a-z]", "[^a-z]", "[]x]", "[^]x]", "[a-]", r"[\.]", r"a\.b", r"\t", r"\v", ".", "^$",
    "x{2,3}", "(a|ab)c", "[$][{]?([(][a-zA-Z@]*[)])?(dis_)?functions", "&[[:space:]]*$",
    r"""<<-?[[:space:]]*[A-Za-z_'"]""", r"git\s+push[^|;&]*(\s-f\b|\s--force\b)",
    # Required-literal prefilter shapes.
    "(ab|c)d|e", "a?b+c*de", "(x|y)?z", r"\.env(\.[a-z]+)*\.example$", r"\bgit\s+push", "which|where|whence",
    "(a(b|c))d", "x{2}bc", r"\(x\)|a\|b",
]
ICASE_PATTERNS = ["rm", "k", "s", "i", "[a-z]", "[[:lower:]]", "[[:upper:]]", "[[:alpha:]]", "[^[:alnum:]_-]",
                  r"\w", r"\brm\b", "git[[:space:]][^|;&]*config", "ǅ", "ß", "K", "[^a-z]"]
ONLY_PATTERNS = [
    (r"git\s+add\s+[^;&|]*", False), (r"(^|[/ ])[^ ]*doc/[^ ]*\.org", False),
    ("(^|[[:space:];|&(])(cd|pushd)[[:space:]]", False), (r"--body-file[ =]+[^ ]+", False),
    ("git[[:space:]]+add[[:space:]]+[^;&|]*", False),
    ("(^|[^[:alnum:]_-])git[[:space:]][^|;&]*config[[:space:]][^|;&]*core\\.hookspath[^|;&]*", True),
]
ONLY_WORDS = ["git", "add", "cd", "pushd", "config", "core.hookspath", "core.hooksPath", "doc/a.org", "x/doc/b.org",
              "--body-file", "=", "f.md", " ", " ", "  ", "\t", ";", "|", "&", "(", "-", "_", "x", " ", "²",
              "GIT", "K"]


def _special_characters() -> list[str]:
    iswalnum = ctypes.CDLL(None).iswalnum
    iswalnum.argtypes = [ctypes.c_int]
    word_diff = [chr(c) for c in CODE_POINTS if (chr(c) == "_" or chr(c).isalnum()) != (c == 95 or bool(iswalnum(c)))]
    extra = (" \u0085     　᠎​﻿\x0b\x0c\r\x1c"
             "٠٩۰०０KİıſǄǅǆßẞͅ΍"
             "ᾈᾀᾼςµéÉ\U00011f02\U0001d400")
    return word_diff + list(extra)


def _run_grep(flags: str, pattern: str, text: str) -> bytes:
    env = {**os.environ, "LANG": LOCALE, "LC_ALL": LOCALE}
    result = subprocess.run([GREP, flags, "--", pattern], input=text.encode(), capture_output=True, env=env)
    if result.returncode not in (0, 1):
        raise AssertionError(f"grep failed on {pattern!r}: {result.stderr!r}")
    return result.stdout


@unittest.skipUnless(os.path.exists(GREP), "needs /usr/bin/grep")
class GrepDifferentialTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            locale.setlocale(locale.LC_CTYPE, LOCALE)
        except locale.Error:
            raise unittest.SkipTest(f"{LOCALE} unavailable")
        rnd = random.Random(5)
        special = _special_characters()
        lines = []
        for char in special:
            lines += [char + "rm", "rm" + char, "a" + char + "b", char]
        alphabet = list("rmfaxbk-_. \t|;&(/$'\"<{}]^\\") + ["rm", "-rf", "bash", "sh", "eval", "ALLOW_CLONE=1",
                                                            "git", "push", "-f", "--force", "functions", "<<",
                                                            "ab", "c", "d", "e", "de", "z", "whence", ".env",
                                                            ".example", "(x)", "a|b", "bc", "x"]
        for _ in range(3000):
            lines.append("".join(rnd.choice(alphabet + special[:40] + rnd.sample(special, 3))
                                 for _ in range(rnd.randint(0, 12))))
        cls.lines = lines
        cls.text = "\n".join(lines) + "\n"
        cls.only_text = "\n".join("".join(rnd.choice(ONLY_WORDS) for _ in range(rnd.randint(1, 14)))
                                  for _ in range(3000)) + "\n"

    def compare(self, pattern, ignore_case):
        flags = "-niE" if ignore_case else "-nE"
        expected = [int(line.split(b":", 1)[0]) - 1 for line in _run_grep(flags, pattern, self.text).split(b"\n")[:-1]]
        # One compile for the whole alphabet; the required-literal prefilter per line, as grep() applies it.
        pattern_ = ere.ere(pattern, ignore_case)
        compiled = pattern_.compile(self.text)
        got = [i for i, line in enumerate(self.lines) if not pattern_.excludes(line) and compiled.search(line)]
        self.assertEqual(got, expected, f"{pattern!r} ignore_case={ignore_case}")

    def test_patterns(self):
        for pattern in PATTERNS:
            self.compare(pattern, False)

    def test_ignore_case(self):
        for pattern in ICASE_PATTERNS:
            self.compare(pattern, True)

    def test_grep_lines_matches_grep_output(self):
        for pattern in (r"\brm\b", "[[:space:]]", r"\W"):
            expected = _run_grep("-E", pattern, self.text).decode().split("\n")[:-1]
            self.assertEqual(ere.grep_lines(pattern, self.text), expected, pattern)

    def test_grep_only_on_the_hooks_patterns(self):
        for pattern, ignore_case in ONLY_PATTERNS:
            expected = _run_grep("-oiE" if ignore_case else "-oE", pattern, self.only_text).decode().split("\n")[:-1]
            self.assertEqual(ere.grep_only(pattern, self.only_text, ignore_case), expected, pattern)


ECHO_VALUES = ["-n", "-e", "-E", "-neE", "-nnn", "-en", "-nx", "--", "-", "-n ", " -n", "-n\n", "a", "", "\n",
               "-e\\t", "a\\tb", "-E\n-n", "x\n"]
ECHO_PATTERNS = [".", "^$", "n", "^-", "^"]


class EchoDifferentialTests(unittest.TestCase):
    """grep(..., echo=True) against `echo "$X" | grep -qE` in each bash."""

    def test_echo_values(self):
        shells = [b for b in ("/bin/bash", "/opt/homebrew/bin/bash") if os.path.exists(b)]
        for shell in shells:
            for value in ECHO_VALUES:
                for pattern in ECHO_PATTERNS:
                    real = subprocess.run([shell, "-c", 'echo "$1" | /usr/bin/grep -qE -- "$2"', "_", value, pattern],
                                          env={**os.environ, "LANG": LOCALE}).returncode == 0
                    self.assertEqual(ere.grep(pattern, value, echo=True), real, (shell, value, pattern))


if __name__ == "__main__":
    unittest.main()
