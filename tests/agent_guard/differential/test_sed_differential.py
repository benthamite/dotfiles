"""agent_guard.ere.sed against /usr/bin/sed -E: the hooks' own rules, engine
edge cases and chains, on generated texts. Run by `bin/test-guards --differential`.
"""

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

LOCALE = "en_GB.UTF-8"

SED_RULES = [
    # The hooks' own sed -E substitutions.
    (r"((^|[[:space:];|&(])(bash|sh|zsh|dash|ksh)[[:space:]]+-[a-zA-Z]*c[a-zA-Z]*[[:space:]]+)['\"]", r"\1", "g"),
    (r"GH_REPO=['\"]([^'\"]+)['\"]", r"GH_REPO=\1", "g"),
    (r"(^|[;&|])[[:space:]]*(if|then|elif|while|until|do)[[:space:]]+", r"\1 ", "g"),
    (r"['\"\\]", "", "g"),
    (r"[[:space:]]*[12]?&?>>?[[:space:]]*[^[:space:];&|<>()]+", "", "g"),
    (r"--body-file[ =]+", "", ""),
    (r"(case[[:space:]]+[^;&|()]+[[:space:]]+in[[:space:]]*)\*[[:space:]]*\)", r"\1CASE_DEFAULT)", "g"),
    (r"(^[[:space:]]*|;;[[:space:]]*)\*[[:space:]]*\)", r"\1CASE_DEFAULT)", "g"),
    (r"\$\?", "EXIT_STATUS", "g"),
    (r"(^[[:space:]]*|[;&|(][[:space:]]*)([A-Za-z_][A-Za-z0-9_]*)=[^;&|[:space:]]*([;&|]|$)", r"\1\2=ASSIGNED\3", "g"),
    # Engine behaviour: empty matches, anchors, replacement escapes, sed's literal escapes.
    ("x*", "-", "g"), ("x*", "-", ""), ("^", "<", "g"), ("$", ">", "g"), ("a|b", "_", "g"),
    ("(x)?a", r"[\1&\&\\\n\q]", "g"), (r"\s\b\w\d\t", "#", "g"), ("[[:alpha:]]+", "W", "g"), ("[^a]", ".", ""),
]
SED_CHAINS = [[("a", r"x\ny", ""), ("^y", "Z", "g"), ("x.y", "Q", "g"), ("[^Q]$", "!", "")],
              SED_RULES[6:10]]
SED_WORDS = ["a", "x", "xx", "b", "case ", " in ", "*", ")", ";;", " ", "\t", ";", "&", "|", "(", "if ", "then",
             "bash -c '", "sh -xc \"", "GH_REPO='o/r'", "2>&1", ">>", " >f", "$?", "A=1", "B=", "--body-file=x",
             "'", '"', "\\", "s", " ", "é", "K", "²", "\U00011f02", "\n", "\n"]
@unittest.skipUnless(os.path.exists("/usr/bin/sed"), "needs /usr/bin/sed")
class SedDifferentialTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            locale.setlocale(locale.LC_CTYPE, LOCALE)
        except locale.Error:
            raise unittest.SkipTest(f"{LOCALE} unavailable")
        rnd = random.Random(9)
        cls.texts = ["", "\n", "a", "a\n", "x\n\n"] + [
            "".join(rnd.choice(SED_WORDS) for _ in range(rnd.randint(0, 25))) for _ in range(150)]

    def run_sed(self, rules, text):
        args = ["/usr/bin/sed", "-E"]
        for pattern, replacement, flags in rules:
            args += ["-e", f"s/{pattern.replace('/', chr(92) + '/')}/{replacement}/{flags}"]
        env = {**os.environ, "LANG": LOCALE, "LC_ALL": LOCALE}
        result = subprocess.run(args, input=text.encode(), capture_output=True, env=env, check=True)
        return result.stdout.decode()

    def test_single_rules(self):
        for rule in SED_RULES:
            # One sed run per rule over all texts joined, then per-text spot checks.
            joined = "\n".join(t.replace("\n", " ") for t in self.texts) + "\n"
            self.assertEqual(ere.sed(joined, rule), self.run_sed([rule], joined), rule)
            for text in self.texts[:20]:
                self.assertEqual(ere.sed(text, rule), self.run_sed([rule], text), (rule, text))

    def test_chains(self):
        for rules in SED_CHAINS:
            for text in self.texts[:60]:
                self.assertEqual(ere.sed(text, *rules), self.run_sed(rules, text), (rules, text))

    def test_undefined_group_raises(self):
        with self.assertRaises(ValueError):
            ere.sub("a", r"\1", "a")


if __name__ == "__main__":
    unittest.main()
