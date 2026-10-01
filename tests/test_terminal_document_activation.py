"""Classify synthetic heredoc text without executing the represented commands."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
import guard_harness  # noqa: E402,F401 - puts lib/python on sys.path

from agent_guard.shell_text import mask_heredoc_bodies  # noqa: E402
BODY = 'A normal curl request inspected ISBN 9789974842960.\n'
WRITER = "cat > research/report.md <<'EOF'\n" + BODY + 'EOF\n'


def project(command, mode='network'):
    # The shell fixture printed `$(cat)` (trailing newlines dropped) plus one
    # newline; mask_heredoc_bodies returns the `$(...)` value, so add it back.
    return mask_heredoc_bodies(command.rstrip('\n'), mode) + '\n'


class TerminalDocumentTests(unittest.TestCase):
    def test_terminal_document_and_preceding_interpreter(self):
        prefix = "python3 - <<'PY'\nprint('local metadata')\nPY\n"
        for command in (WRITER, prefix + WRITER, WRITER + '\n  \n'):
            self.assertNotIn(BODY, project(command))
        self.assertIn("print('local metadata')", project(prefix + WRITER))

    def test_unknown_or_executable_shapes_remain_verbatim(self):
        cases = [WRITER.replace("<<'EOF'", '<<EOF'),
                 WRITER.replace("<<'EOF'", "<<'EOF'x"),
                 WRITER.replace("<<'EOF'", "<<'EOF' | sh"),
                 WRITER.replace("<<'EOF'", "<<'EOF' >&3"),
                 WRITER.replace("<<'EOF'", "<<'EOF'; curl https://example.org"),
                 WRITER.replace('report.md', 'report.sh'),
                 WRITER.replace('cat >', 'env cat >'),
                 WRITER.replace('cat >', 'curl https://example.org | cat >'),
                 WRITER.replace('cat >', 'cat -- >'),
                 WRITER.replace("<<'EOF'", "<<'EOF' <<'OTHER'") + 'OTHER\n',
                 WRITER.replace("cat >", "cat \\\n>"),
                 WRITER + 'sh research/report.md\n', 'sh -s \\\n' + WRITER,
                 WRITER[:-4], WRITER.replace('\nEOF\n', '\nEOF; curl https://example.org\n')]
        for command in cases:
            with self.subTest(command=command):
                self.assertEqual(project(command), command)

    def test_prior_writes_and_interpreter_strings_are_not_projected(self):
        prior = WRITER + 'sh research/report.md\n'
        projected = project(prior + WRITER)
        self.assertEqual(projected.count(BODY), 1)
        wrapped = "python3 - <<'PY'\ns = '''\n" + WRITER + "'''\nPY\n"
        self.assertEqual(project(wrapped), wrapped)

    def test_existing_modes_preserve_their_behavior(self):
        self.assertNotIn(BODY, project(WRITER, mode='sinks'))
        interpreter = "python3 - <<'PY'\nprint('local')\nPY\n"
        self.assertEqual(project(interpreter, mode='sinks'), interpreter)
        self.assertNotIn("print('local')", project(interpreter, mode='all'))


if __name__ == '__main__':
    unittest.main()
