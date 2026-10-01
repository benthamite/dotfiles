"""agent_guard.ere in-process: the measured grep -E, sed -E and echo facts as
fixed expectations. The comparisons against the real tools are in
differential/ (`bin/test-guards --differential`).
"""

import ctypes
import locale
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import ere  # noqa: E402
from agent_guard.shell_text import GREP_SPACE  # noqa: E402

LOCALE = "en_GB.UTF-8"
# Characters libc (and so grep) counts as word characters and Python's \w does not, and the reverse.
GREP_ONLY_WORD = "\U0001123f\U00011240\U00011f02\U00011f04"
PYTHON_ONLY_WORD = "²³¹¼½¾৴୲౸൘"


class LocaleTests(unittest.TestCase):
    """Facts that depend on libc's character classes, measured under en_GB.UTF-8."""

    @classmethod
    def setUpClass(cls):
        try:
            locale.setlocale(locale.LC_CTYPE, LOCALE)
        except locale.Error:
            raise unittest.SkipTest(f"{LOCALE} unavailable")

    def test_static_ascii_tables_are_libc(self):
        libc = ctypes.CDLL(None)
        libc.wctype.argtypes = [ctypes.c_char_p]
        libc.wctype.restype = ctypes.c_ulong
        libc.iswctype.argtypes = [ctypes.c_int, ctypes.c_ulong]
        for name in ere.CLASS_NAMES:
            kind = libc.wctype(name.encode())
            measured = {chr(c) for c in range(1, 128) if libc.iswctype(c, kind)}
            self.assertEqual(ere.ASCII_CLASSES[name], measured, name)

    def test_word_characters_are_libc_alnum(self):
        for char in GREP_ONLY_WORD:
            self.assertFalse(ere.grep(r"\brm", char + "rm"), hex(ord(char)))
            self.assertTrue(ere.grep(r"^\w$", char), hex(ord(char)))
        for char in PYTHON_ONLY_WORD:
            self.assertTrue(ere.grep(r"\brm", char + "rm"), hex(ord(char)))
            self.assertFalse(ere.grep(r"^[[:alnum:]_]$", char), hex(ord(char)))
        self.assertTrue(ere.grep(r"\Brm", "xrm"))
        self.assertTrue(ere.grep(r"\<rm\>", "a rm b"))
        self.assertFalse(ere.grep(r"\<rm", "xrm"))

    def test_space_class_is_grep_space(self):
        for char in GREP_SPACE:
            self.assertTrue(ere.grep(r"^\s$", char), hex(ord(char)))
            self.assertTrue(ere.grep(r"^[[:space:]]$", char), hex(ord(char)))
        for char in "\u0085᠎​﻿\x1c\x1f":
            self.assertTrue(ere.grep(r"^\S$", char), hex(ord(char)))

    def test_digits_include_non_ascii(self):
        for char in "٠۰०０":
            self.assertTrue(ere.grep(r"^\d$", char), hex(ord(char)))
            self.assertTrue(ere.grep("^[[:digit:]]$", char), hex(ord(char)))

    def test_ignore_case(self):
        self.assertFalse(ere.grep("k", "K", ignore_case=True))  # a literal folds the pattern
        self.assertTrue(ere.grep("[a-z]", "K", ignore_case=True))  # a bracket folds the text
        self.assertTrue(ere.grep("[a-z]", "İ", ignore_case=True))
        self.assertFalse(ere.grep("i", "İ", ignore_case=True))
        self.assertTrue(ere.grep("K", "k", ignore_case=True))
        self.assertFalse(ere.grep("^[[:upper:]]$", "ǅ", ignore_case=True))  # titlecase is not folded
        self.assertTrue(ere.grep("^[[:upper:]]$", "ǆ", ignore_case=True))
        self.assertFalse(ere.grep("^[^a-z]$", "K", ignore_case=True))


class SyntaxTests(unittest.TestCase):
    def test_backslash_is_literal_in_brackets(self):
        self.assertTrue(ere.grep(r"(^|[ \t])bash([ \t]|$)", "bashtx"))
        self.assertFalse(ere.grep(r"(^|[ \t])bash([ \t]|$)", "bash\tx"))
        self.assertTrue(ere.grep(r"bash\t", "bash\tx"))
        self.assertTrue(ere.grep(r"^[\.]$", "\\"))

    def test_escapes_outside_brackets(self):
        self.assertTrue(ere.grep(r"^\v$", "v"))
        self.assertFalse(ere.grep(r"^\v$", "\v"))
        self.assertTrue(ere.grep(r"^\r\f\a\e$", "\r\f\a\x1b"))
        self.assertTrue(ere.grep(r"^\q$", "q"))

    def test_lines_are_independent(self):
        self.assertFalse(ere.grep("a.b", "a\nb"))
        self.assertTrue(ere.grep("^b$", "a\nb"))

    def test_grep_lines_and_only(self):
        self.assertEqual(ere.grep_lines("x", "x\nyx\nz"), ["x", "yx"])
        self.assertEqual(ere.grep_lines("x", "x\nz", invert=True), ["z"])
        self.assertEqual(ere.grep_only("git[[:space:]]+add[[:space:]]+[^;&|]*", "x; git  add a && y"),
                         ["git  add a "])

    def test_unsupported_syntax_raises(self):
        for pattern in (r"a\n", r"\x41", r"(a)\1", "[[=a=]]", "[[:nope:]]", "a{", "(*a)", "[ab", "a|", "(|a)",
                        "a||b"):
            with self.assertRaises(ValueError, msg=pattern):
                ere.grep(pattern, "a")


class PrefilterTests(unittest.TestCase):
    """Required literals let a text without them skip compiling; they must be necessary."""

    def test_required_clauses(self):
        cases = {r"\bgit\s+push": [{"git"}, {"push"}], "which|where": [{"which", "where"}],
                 "a?b+c*de": [{"b"}, {"de"}], "(x|y)?z": [{"z"}], "(ab|c)d|e": [{"ab", "c", "e"}],
                 "(a|)b": None, "x{2}bc": [{"bc"}], r"\(x\)": [{"(x)"}], "[ab]c": [{"c"}], "(^|x)y": [{"y"}]}
        for pattern, expected in cases.items():
            if expected is None:
                with self.assertRaises(ValueError):
                    ere.Ere(pattern).required
            else:
                self.assertEqual([set(c) for c in ere.Ere(pattern).required], expected, pattern)

    def test_excluded_text_does_not_compile(self):
        pattern = ere.Ere(r"\bgit\s+push")
        self.assertFalse(ere.grep(pattern, "ls -la"))
        self.assertEqual(pattern._compiled, {})
        self.assertTrue(ere.grep(pattern, "x; git  push"))
        self.assertEqual(ere.sub("git", "G", "ls"), "ls")


class EchoTests(unittest.TestCase):
    """bash 3.2 and 5.3 builtin echo, xpg_echo off: an all-option argument is options."""

    def test_printf_and_echo_lines(self):
        self.assertEqual(ere.lines(""), [])
        self.assertEqual(ere.lines("", echo=True), [""])
        self.assertEqual(ere.lines("a\n"), ["a"])
        self.assertEqual(ere.lines("a\n", echo=True), ["a", ""])

    def test_option_values(self):
        for value in ("-n", "-neE", "-nnn", "-en"):
            self.assertEqual(ere.lines(value, echo=True), [], value)
        for value in ("-e", "-E", "-eE"):
            self.assertEqual(ere.lines(value, echo=True), [""], value)
        for value in ("-nx", "--", "-", "-n ", " -n", "-n\n"):
            self.assertEqual(ere.lines(value, echo=True), value.split("\n"), value)
        self.assertFalse(ere.grep(".", "-n", echo=True))
        self.assertTrue(ere.grep(".", "-n"))


class SedTests(unittest.TestCase):
    def test_sed_dialect_escapes_are_mostly_literal(self):
        self.assertEqual(ere.sub(r"\s\b\w\d\v", "#", "a sbwdv"), "a #")
        self.assertEqual(ere.sub(r"\t", "#", "a\tb"), "a#b")
        self.assertEqual(ere.sub(r"[\t]", "#", "\\t", "g"), "##")

    def test_replacement_escapes(self):
        self.assertEqual(ere.sub("b", r"[&][\&][\\][\n][\q]", "abc"), "a[b][&][\\][\n][q]c")
        self.assertEqual(ere.sub("(x)?b", r"[\1]", "abc"), "a[]c")
        with self.assertRaises(ValueError):
            ere.sub("a", r"\1", "a")

    def test_empty_matches(self):
        self.assertEqual(ere.sub("x*", "-", "abxc", "g"), "-a-b-c-")
        self.assertEqual(ere.sub("x*", "-", "xx", "g"), "-")
        self.assertEqual(ere.sub("x*", "-", "abc"), "-abc")

    def test_lines_and_pattern_space(self):
        self.assertEqual(ere.sub("$", "!", "a\nb"), "a!\nb!")
        self.assertEqual(ere.sub("^", "X", ""), "")
        self.assertEqual(ere.sub("a", "b", "a\n"), "b\n")
        self.assertEqual(ere.sed("ab\n", ("a", r"x\ny", ""), ("^y", "Z", ""), ("x.y", "Q", "")), "Qb\n")


if __name__ == "__main__":
    unittest.main()
