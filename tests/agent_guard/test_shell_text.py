import subprocess
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.shell_text import BASH_SPACE, GREP_SPACE, mask_git_commit_messages, mask_heredoc_bodies  # noqa: E402

CANDIDATES = [chr(c) for c in [*range(1, 0x21), 0x7F, 0x85, 0xA0, 0x1680, 0x180E, *range(0x2000, 0x200C),
                                0x2028, 0x2029, 0x202F, 0x205F, 0x3000, 0xFEFF]]
ENV = {"LANG": "en_GB.UTF-8", "PATH": "/usr/bin:/bin"}


class MaskHeredocBodiesTests(unittest.TestCase):
    def test_sink_body_dropped_terminator_kept(self):
        self.assertEqual(mask_heredoc_bodies("cat <<EOF > f\nsecret\nEOF\necho done"), "cat <<EOF > f\nEOF\necho done")

    def test_interpreter_body_kept_except_in_all_and_nonshell(self):
        command = "python3 - <<'EOF'\nimport os\nEOF"
        self.assertEqual(mask_heredoc_bodies(command), command)
        self.assertEqual(mask_heredoc_bodies(command, "all"), "python3 - <<'EOF'\nEOF")
        self.assertEqual(mask_heredoc_bodies(command, "nonshell"), "python3 - <<'EOF'\nEOF")
        self.assertEqual(mask_heredoc_bodies("bash <<EOF\nrm x\nEOF", "nonshell"), "bash <<EOF\nrm x\nEOF")

    def test_pipe_or_substitution_keeps_body(self):
        for command in ("cat <<EOF | sh\nx\nEOF", "x=$(cat <<EOF\nx\nEOF\n)", "f=`ls` && cat <<EOF\nx\nEOF"):
            self.assertEqual(mask_heredoc_bodies(command), command)

    def test_earlier_substitution_only_blocks_sinks_not_nonshell(self):
        command = "f=$(ls) && python - <<EOF\nx\nEOF"
        self.assertEqual(mask_heredoc_bodies(command, "nonshell"), "f=$(ls) && python - <<EOF\nEOF")
        self.assertEqual(mask_heredoc_bodies("f=$(ls) && cat <<EOF\nx\nEOF"), "f=$(ls) && cat <<EOF\nx\nEOF")

    def test_prefix_words_and_pyenv_exec_are_skipped(self):
        self.assertEqual(mask_heredoc_bodies("env A=1 timeout 5s sudo -E tee f <<EOF\nx\nEOF"),
                         "env A=1 timeout 5s sudo -E tee f <<EOF\nEOF")
        self.assertEqual(mask_heredoc_bodies("pyenv exec cat <<EOF\nx\nEOF"), "pyenv exec cat <<EOF\nEOF")

    def test_quoted_operator_and_herestring_are_not_heredocs(self):
        for command in ("echo '<<EOF'\nx\nEOF", "cat <<<EOF\nx\nEOF", "echo \\<<EOF\nx\nEOF", "cat <<''\nx"):
            self.assertEqual(mask_heredoc_bodies(command), command)

    def test_strip_tabs_and_queued_heredocs(self):
        self.assertEqual(mask_heredoc_bodies("cat <<-EOF\n\tx\n\tEOF\ny"), "cat <<-EOF\n\tEOF\ny")
        self.assertEqual(mask_heredoc_bodies("cat <<A; python <<B\n1\nA\n2\nB\nz"), "cat <<A; python <<B\nA\n2\nB\nz")

    def test_unterminated_body_runs_to_end(self):
        self.assertEqual(mask_heredoc_bodies("cat <<EOF\nx\ny"), "cat <<EOF")

    def test_trailing_newlines_dropped_as_command_substitution_did(self):
        self.assertEqual(mask_heredoc_bodies("a\n\n"), "a")
        self.assertEqual(mask_heredoc_bodies(""), "")

    def test_network_drops_only_terminal_quoted_literal_document_write(self):
        self.assertEqual(mask_heredoc_bodies("cat > notes.md <<'EOF'\nbody\nEOF\n\n", "network"),
                         "cat > notes.md <<'EOF'\nEOF")
        for command in ("cat > notes.md <<EOF\nbody\nEOF", "cat > notes.md <<'EOF'\nbody\nEOF\necho next",
                        "cat > notes.sh <<'EOF'\nbody\nEOF", "x \\\ncat > a.md <<'EOF'\nb\nEOF"):
            self.assertEqual(mask_heredoc_bodies(command, "network"), command)

    def test_counts_bytes_like_macos_awk(self):
        # A trailing backslash neutralizes to two bytes; the owner slice must
        # still find the program after a multibyte quoted argument.
        self.assertEqual(mask_heredoc_bodies("echo 'é' 'ü\\' <<EOF\nx\nEOF"), "echo 'é' 'ü\\' <<EOF\nx\nEOF")
        self.assertEqual(mask_heredoc_bodies("cat 'é' <<EOF\nx\nEOF"), "cat 'é' <<EOF\nEOF")


class SpaceSetTests(unittest.TestCase):
    """Re-measure the constants on this machine, as their comments describe."""

    def test_bash_space_matches_bash_character_class(self):
        script = 'while IFS= read -r -d "" c; do case "$c" in [[:space:]]) printf Y;; *) printf N;; esac; done'
        data = "".join(c + "\0" for c in CANDIDATES).encode()
        for bash in (b for b in ("/bin/bash", "/opt/homebrew/bin/bash") if Path(b).exists()):
            out = subprocess.run([bash, "-c", script], input=data, capture_output=True, env=ENV).stdout.decode()
            self.assertEqual({c for c, o in zip(CANDIDATES, out) if o == "Y"}, set(BASH_SPACE), bash)

    def test_grep_space_matches_bsd_grep_within_a_line(self):
        matched = {c for c in CANDIDATES if c != "\n" and subprocess.run(
            ["/usr/bin/grep", "-qE", "^a[[:space:]]b$"], input=f"a{c}b\n".encode(), env=ENV).returncode == 0}
        self.assertEqual(matched, set(GREP_SPACE))


class MaskGitCommitMessagesTests(unittest.TestCase):
    def test_quoted_messages_masked(self):
        self.assertEqual(mask_git_commit_messages("git commit -m 'a ~/.ssh/id' && git commit --message=\"b\""),
                         "git commit -m COMMIT_MESSAGE && git commit --message=COMMIT_MESSAGE")
        self.assertEqual(mask_git_commit_messages("git commit -am 'x' -m \"y\""),
                         "git commit -am COMMIT_MESSAGE -m COMMIT_MESSAGE")

    def test_substitutions_files_and_other_commands_kept(self):
        for command in ('git commit -m "$(cat f)"', "git commit -m `x`", "git commit -F f", "less -m 'f'",
                        "echo -m 'b'", "recommit -m 'x'"):
            self.assertEqual(mask_git_commit_messages(command), command)
        self.assertEqual(mask_git_commit_messages('git commit -m "a $HOME \\"b\\""'), "git commit -m COMMIT_MESSAGE")

    def test_perl_whitespace_includes_vertical_tab(self):
        self.assertEqual(mask_git_commit_messages("commit\v-m\v'x'"), "commit\v-m\vCOMMIT_MESSAGE")


if __name__ == "__main__":
    unittest.main()
