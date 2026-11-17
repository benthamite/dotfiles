import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import applescript_reads as reads  # noqa: E402

EVENTS = 'tell application "System Events"'

READS = [
    f"{EVENTS} to get name of first application process whose frontmost is true",
    f'{EVENTS} to tell process "LuLu" to get {{name of windows, visible, frontmost}}',
    f'{EVENTS} to tell process "Google Chrome" to get title of every window',
    f"{EVENTS} to get the name of every login item",
    f"{EVENTS} to get UI elements enabled",
    f"{EVENTS} to tell (first process whose unix id is 42740) to get {{name, count of windows, "
    "name of menu bar items of menu bar 1}",
    f'{EVENTS} to tell process "LuLu" to count (windows whose name is "LuLu Alert")',
    f'{EVENTS} to exists process "LuLu"',
    f'{EVENTS} to (name of processes) contains "LuLu"',
    f'{EVENTS} to return bundle identifier of first application process whose frontmost is true',
    f'{EVENTS} to get value of attribute "AXFocusedWindow" of process "Emacs"',
    f'{EVENTS} to get {{position, size, subrole}} of every window of process "Google Chrome"',
    f'{EVENTS} to get entire contents of window 1 of process "LuLu"',
    f'{EVENTS} to get properties of window 1 of process "LuLu" -- a look\n# nothing else',
    f"{EVENTS} to get dark mode of appearance preferences",
    f'{EVENTS} to get POSIX path of (file of process "Finder")',
    f"TELL APPLICATION \"system events\" TO GET NAME OF EVERY PROCESS WHOSE BACKGROUND ONLY IS FALSE",
    'tell app "System Events" to get name of processes',
    'tell application id "com.apple.systemevents" to count processes',
    'tell application "System Settings" to activate',
    'tell application "System Settings" to get name of current pane',
    'tell application "System Preferences" to get name of every anchor of current pane',
    'tell application "LuLu" to activate',
    'tell application "Google Chrome" to activate\n'
    f'{EVENTS} to get title of window 1 of process "Google Chrome"',
    f'activate application "Preview"\ndelay 0.5\n{EVENTS} to get name of window 1 of process "Preview"',
    f'{EVENTS} to tell process "LuLu"\n'
    '  set w to (first window whose name starts with "LuLu Rules")\n'
    "  set o to outline 1 of scroll area 1 of w\n"
    '  set out to ""\n'
    "  repeat with r in rows of o\n"
    "    set out to out & (value of static text 1 of r) & linefeed\n"
    "  end repeat\n"
    "  return out\n"
    "end tell",
    f"{EVENTS}\n"
    '  tell process "LuLu"\n'
    "    set n to 0\n"
    "    repeat with i from 1 to (count of windows)\n"
    "      try\n"
    '        if name of window i is "LuLu Alert" then set n to n + 1\n'
    "      on error msg number num\n"
    "        log msg\n"
    "      end try\n"
    "    end repeat\n"
    "    if n > 0 then\n"
    "      return n\n"
    "    else if (count of windows) is 0 then\n"
    '      return "none"\n'
    "    else\n"
    "      return {n, name of windows}\n"
    "    end if\n"
    "  end tell\n"
    "end tell",
    f'{EVENTS} to tell process "Finder"\n  set {{x, y}} to position of window 1\n  repeat 2 times\n    delay 1\n'
    "  end repeat\n  repeat while x < 0\n    exit repeat\n  end repeat\n  return x & y as text\nend tell",
    "",
]

ACTIONS = [
    f"{EVENTS} to key code 36",
    f'{EVENTS} to keystroke "a"',
    f'{EVENTS} to keystroke "q" using command down',
    f'{EVENTS} to click button 1 of window 1 of process "LuLu"',
    f'{EVENTS} to click at {{10, 20}}',
    f'{EVENTS} to perform action "AXPress" of button 1 of window 1 of process "LuLu"',
    f'{EVENTS} to select row 1 of table 1 of window 1 of process "LuLu"',
    f'{EVENTS} to pick menu item 1 of menu 1 of menu bar 1 of process "LuLu"',
    f'{EVENTS} to set frontmost of process "X" to true',
    f'{EVENTS} to tell process "X" to set frontmost to true',
    f'{EVENTS} to tell process "X" to set its frontmost to true',
    f'{EVENTS} to tell process "X" to tell text field 1 of window 1 to set value to "a"',
    f'{EVENTS} to set value of text field 1 of window 1 of process "X" to "a"',
    f"{EVENTS} to tell appearance preferences to set dark mode to true",
    f"{EVENTS} to tell dock preferences to set autohide to true",
    f"{EVENTS} to set autohide to true",
    f'{EVENTS} to copy true to frontmost of process "X"',
    f'{EVENTS} to make new login item at end with properties {{path:"/x"}}',
    f'{EVENTS} to delete login item "x"',
    f'{EVENTS} to do shell script "id"',
    f'{EVENTS} to run script "beep"',
    f"{EVENTS} to «event prcsclic» button 1",
    f'{EVENTS} to get (click button 1 of window 1 of process "LuLu")',
    f'{EVENTS} to return keystroke "a"',
    f'{EVENTS} to count (delete every login item)',
    f'{EVENTS} to if (click button 1 of window 1 of process "X") is missing value then return 1',
    f'{EVENTS} to system attribute "HOME"',
    f"{EVENTS} to get the clipboard",
    f"{EVENTS} to shut down",
    f"{EVENTS} to log out",
    f"{EVENTS} to sleep",
    # A dictionary term is never a variable, whatever the script declares.
    f'{EVENTS}\n  set out to ""\n  log out\nend tell',
    f'set click to 1\n{EVENTS} to click button 1 of window 1 of process "X"',
    f"set key to 1\n{EVENTS} to key up 1",
    f"set visible to false\n{EVENTS} to get name of processes",
    # An action after, between or inside reads.
    f'{EVENTS} to get name of processes\n{EVENTS} to keystroke "q" using command down',
    f'{EVENTS}\n  tell process "LuLu"\n    get name of windows\n    click button "Allow" of window 1\n  end tell\nend tell',
    f'{EVENTS}\n  repeat with w in windows of process "LuLu"\n    try\n      click button 1 of w\n    end try\n'
    "  end repeat\nend tell",
    # Other applications take only `activate`; code loading and handlers are not reads.
    'tell application "LuLu" to quit',
    'tell application "System Settings" to quit',
    'tell application "System Settings" to reveal pane id "com.apple.settings.PrivacySecurity.extension"',
    'tell application "System Settings" to set current pane to pane id "x"',
    f'tell application "Finder" to get name of front window\n{EVENTS} to get name of processes',
    f'tell application "Google Chrome" to close window 1\n{EVENTS} to get name of processes',
    f'tell application "Google Chrome"\n  activate\nend tell\n{EVENTS} to get name of processes',
    f"on run argv\n  {EVENTS} to get name of processes\nend run",
    f"{EVENTS} to my look()",
    f'do shell script "true"\n{EVENTS} to get name of processes',
    f'run script "x"\n{EVENTS} to get name of processes',
    f'tell application "Safari" to do JavaScript "1" in document 1\n{EVENTS} to get name of processes',
    f'{EVENTS} to get name of |process| 1',
    f"{EVENTS} to get name of processes ¬\n  whose visible is true",
    f"{EVENTS} to get name of current application",
    f'set AppleScript\'s text item delimiters to ","\n{EVENTS} to get name of processes',
    f'{EVENTS} to get name of processes; {EVENTS} to keystroke "a"',
    f'use framework "Foundation"\n{EVENTS} to get name of processes',
    f'{EVENTS} to get name of widgets of process "X"',
]


class ReadsTests(unittest.TestCase):
    def test_reads_and_control_flow_pass(self):
        for script in READS:
            with self.subTest(script=script):
                self.assertIsNone(reads.problem(script))

    def test_actions_and_unknown_forms_fail(self):
        for script in ACTIONS:
            with self.subTest(script=script):
                self.assertIsNotNone(reads.problem(script))

    def test_problem_names_what_was_not_recognized(self):
        self.assertEqual(reads.problem(f"{EVENTS} to click button 1"), "`click`")
        self.assertEqual(reads.problem(f'{EVENTS}\nset out to ""\nlog out\nend tell'), "`log out`")
        self.assertEqual(reads.problem(f"{EVENTS} to set frontmost to true"), "`set` on a property or dictionary term")
        self.assertEqual(reads.problem(f"{EVENTS} to «event x»"), "a character outside plain AppleScript")


class VocabularyTests(unittest.TestCase):
    """What keeps the allowlist closed whatever words are added to it later."""

    def test_no_command_is_an_expression_word(self):
        commands = reads._DICTIONARY_COMMANDS | reads._LANGUAGE_COMMANDS
        self.assertEqual(reads._VOCABULARY & commands, reads.READ_COMMANDS)

    def test_statement_words_stay_out_of_expressions(self):
        self.assertEqual(reads._VOCABULARY & reads._STATEMENT_WORDS, frozenset())

    def test_no_command_phrase_is_spelled_by_expression_words_alone(self):
        spelled = [phrase for phrase in reads._COMMAND_PHRASES if all(word in reads._VOCABULARY for word in phrase)]
        self.assertEqual(spelled, [("path", "to")])  # caught by the phrase check, as the tests above show

    def test_path_to_is_refused(self):
        self.assertEqual(reads.problem(f"{EVENTS} to get path to startup disk"), "`path to`")

    def test_dictionary_terms_cannot_be_variables(self):
        reader = reads._Reader()
        for name in ("visible", "autohide", "volume", "click", "name", "get", "then", "contents", "magnification"):
            self.assertFalse(reader.free(name), name)
        for name in ("out", "w", "n", "thename", "row_text", "key"):
            self.assertTrue(reader.free(name), name)


if __name__ == "__main__":
    unittest.main()
