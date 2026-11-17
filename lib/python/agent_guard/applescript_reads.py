"""Whether an AppleScript only looks: the allowlist behind security_controls rule 1.

`problem(script)` returns None when every statement of SCRIPT is a recognized
read or plain control flow, else a few words naming what was not recognized.
It is an allowlist. A script passes only if each of its words is known:

- statements: `tell` (block or one line), `set VARIABLE to EXPRESSION`,
  `get`, `count`, `exists`, `return`, `log`, `delay`, `activate`, `if`,
  `else`, `repeat`, `exit repeat`, `try`, `on error`, `end`, or a bare
  expression;
- expressions: strings, numbers, punctuation, variables the script itself
  assigned, and the words of _VOCABULARY (reference forms, comparison and
  text operators, the classes and properties of processes and their windows).

Everything else is unrecognized, so it is denied: every command that acts
(`click`, `keystroke`, `key code`, `perform`, `select`, `pick`, `make`,
`delete`, `quit`, `open` ...), `do shell script`, `run script`,
`do JavaScript`, raw event codes, handlers, and `set` on anything but a
plain variable. Three properties make that hold when AppleScript's own
parser prefers dictionary terms over variables:

- a variable may not be a single-word term of the dictionaries below, so
  `set visible to false` or `set autohide to true` is never a variable
  assignment;
- no run of adjacent words may spell a multi-word command of those
  dictionaries, so `log out` logs nothing even after `set out to ""`;
- `tell application` takes only System Events, System Settings, System
  Preferences and LuLu, whose dictionaries these are; another application is
  accepted only as `tell application "X" to activate`.

_TERMS, _DICTIONARY_COMMANDS and _COMMAND_PHRASES are the class, property,
enumerator and command names of System Events, System Settings and
StandardAdditions, read from their scripting definitions on macOS 27 (the
`.sdef` and `.scriptTerminology` files in each bundle, and CocoaStandard.sdef).
LuLu has no dictionary of its own. Regenerate them after a macOS upgrade that
changes those dictionaries.
"""

from __future__ import annotations

import re

_TERMS = frozenset("""
access action actions active alias aliases anchor animate appearance appletalk application applications
architecture ask attribute attributes auto autohide automatic blue bottom bounds browser browsers button buttons
capacity caution checkbox checkboxes class classic closeable collating color column columns command
configuration configurations connected container contents control copies critical dark data description desktop
desktops detailed disk disks document domain domains down drawer drawers duplex dynamic editors ejectable
enabled enumerated eof extensions file files focused folder folders fonts format frontmost genie gold graphite
green group groups help hidden host id ignore image images incrementor incrementors index informational
interface interfaces ip item items kind left light list listed lists location locations locked magnification
medium menu menus miniaturizable miniaturized minimize modified mtu name no none note off option optional orange
orientation outline outlines pane password path picture plugins port position preferences printmonitor process
processes properties purple red resizable right role row rows running scale scheme selected server service
services settable sheet sheets shift short silver size slider sliders speed splitter splitters standard startup
stationery stop strong subrole superclass table tables text title toolbar toolbars trash unknown up url user
users value version visible voices volume warning window windows yes zone zoom zoomable zoomed
""".split())

_DICTIONARY_COMMANDS = frozenset("""
authorize beep cancel click close confirm connect count decrement delay delete disconnect duplicate exists
increment keystroke make move offset open perform pick print quit read restart reveal round save say select
sleep start stop summarize timedload write
""".split())

_COMMAND_PHRASES = frozenset(tuple(phrase.split()) for phrase in (
    "abort transaction", "adding folder items to", "ascii character", "ascii number", "attach action to",
    "attached scripts", "begin transaction", "choose application", "choose color", "choose file",
    "choose file name", "choose folder", "choose from list", "choose remote application", "choose url",
    "clipboard info", "close access", "closing folder window for", "current date", "display alert",
    "display dialog", "display notification", "do folder action", "do shell script", "edit action of",
    "end transaction", "get eof", "get volume settings", "handle cgi request", "info for", "key code",
    "key down", "key up", "list disks", "list folder", "load script", "localized string", "log out",
    "mount volume", "moving folder window for", "open for access", "open location", "opening folder", "path to",
    "path to resource", "random number", "remove action from", "removing folder items from", "run script",
    "scripting components", "set eof", "set the clipboard to", "set volume", "shut down", "store script",
    "system attribute", "system info", "the clipboard", "time to gmt",
))

# AppleScript's own commands and statement words, which no dictionary lists.
_LANGUAGE_COMMANDS = frozenset("""
activate considering continue copy error exit get ignoring launch log reopen run set tell using
""".split())
_STATEMENT_WORDS = frozenset("""
activate delay else end error exit if log on repeat set tell then times try until while
""".split())
# The only commands an expression may hold (tests check that _VOCABULARY has no other).
READ_COMMANDS = frozenset(("count", "exists", "get"))

# Words an expression may hold.
_VOCABULARY = frozenset("""
a an the of in at to from thru through by whose where that it its result
every some first second third fourth fifth sixth seventh eighth ninth tenth last front back middle before after
named id index item items
is not and or equal equals greater less than contains contain does starts begins ends with as mod div
missing value true false count number length reverse rest exists exist get
class text string integer real boolean list record character characters word words paragraph paragraphs
linefeed return tab space quote
application applications app process processes window windows sheet sheets drawer drawers
button buttons checkbox checkboxes radio group groups pop up menu menus bar bars static texts
field fields area areas scroll outline outlines row rows column columns table tables lists image images
toolbar toolbars tool splitter splitters slider sliders incrementor incrementors browser browsers grow
busy progress relevance indicator indicators color well combo box boxes
ui element elements attribute attributes action actions login
name title description role subrole help position size properties frontmost visible enabled focused selected
orientation minimum maximum entire contents bounds unix bundle identifier displayed short file posix path
accessibility settable background only kind version
appearance preferences dark mode current pane panes anchor anchors
""".split())

_APPLICATIONS = frozenset(("system events", "system settings", "system preferences", "lulu"))
_APPLICATION_IDS = frozenset(("com.apple.systemevents", "com.apple.systempreferences", "com.objective-see.lulu.app"))
_AFTER_APPLICATION = frozenset(("process", "processes", "file"))
_BLOCKS = frozenset(("tell", "if", "repeat", "try"))

_TOKEN = re.compile(r'''(?P<blank>[ \t]+)|(?P<comment>--[^\n]*|\#[^\n]*)|(?P<string>"(?:[^"\\\n]|\\.)*")'''
                    r'''|(?P<number>\d+(?:\.\d+)?)|(?P<word>[A-Za-z_][A-Za-z0-9_]*)|(?P<possessive>'s\b)'''
                    r'''|(?P<punct>[(){},:&+\-*/^=<>])|(?P<newline>\n)''')


def _lines(script: str) -> list[list[tuple[str, str]]] | None:
    """SCRIPT as lines of (kind, text) tokens, words in lower case; None if a
    character outside the recognized ones appears (a raw event code's
    chevrons, a `|quoted|` identifier, a continuation mark ...)."""
    lines, line, position = [], [], 0
    while position < len(script):
        match = _TOKEN.match(script, position)
        if match is None:
            return None
        position, kind = match.end(), match.lastgroup
        if kind == "newline":
            lines.append(line)
            line = []
        elif kind == "word":
            line.append(("w", match.group().lower()))
        elif kind == "string":
            line.append(("s", match.group()[1:-1]))
        elif kind == "number":
            line.append(("n", match.group()))
        elif kind in ("punct", "possessive"):
            line.append(("p", match.group()))
    lines.append(line)
    return lines


def _word(text: str) -> tuple[str, str]:
    return ("w", text)


def _split(tokens: list, word: str) -> int | None:
    """The index of the first WORD outside parentheses and braces."""
    depth = 0
    for index, (kind, text) in enumerate(tokens):
        if kind == "p" and text in "({":
            depth += 1
        elif kind == "p" and text in ")}":
            depth -= 1
        elif depth == 0 and kind == "w" and text == word:
            return index
    return None


def _application(tokens: list) -> str | None:
    """"name" or "id" when TOKENS are exactly `application "X"` or `application id "X"`."""
    if len(tokens) == 2 and tokens[0] in (_word("application"), _word("app")) and tokens[1][0] == "s":
        return "name"
    if len(tokens) == 3 and tokens[0] in (_word("application"), _word("app")) and tokens[1] == _word("id") \
            and tokens[2][0] == "s":
        return "id"
    return None


class _Reader:
    """One script's check. `variables` are the names it has assigned so far."""

    def __init__(self) -> None:
        self.variables: set[str] = set()

    def free(self, name: str) -> bool:
        """Whether NAME can only be a variable: no dictionary or language word."""
        return name not in _VOCABULARY and name not in _STATEMENT_WORDS and name not in _TERMS \
            and name not in _DICTIONARY_COMMANDS and name not in _LANGUAGE_COMMANDS

    def phrase(self, tokens: list) -> str | None:
        """A multi-word command spelled by adjacent words of TOKENS, if any."""
        run: list[str] = []
        for kind, text in [*tokens, ("p", "")]:
            if kind == "w":
                run.append(text)
                continue
            for start in range(len(run)):
                for size in range(2, 6):
                    if tuple(run[start:start + size]) in _COMMAND_PHRASES:
                        return " ".join(run[start:start + size])
            run = []
        return None

    def expression(self, tokens: list) -> str | None:
        """None when TOKENS hold only known words, else what is not known."""
        if not tokens:
            return "an empty expression"
        for index, (kind, text) in enumerate(tokens):
            if kind != "w":
                continue
            if text not in _VOCABULARY and text not in self.variables:
                return f"`{text}`"
            if text in ("application", "app"):
                following = tokens[index + 1:index + 3]
                if following and following[0][0] == "s":
                    known = following[0][1].lower() in _APPLICATIONS
                elif len(following) == 2 and following[0] == _word("id") and following[1][0] == "s":
                    known = following[1][1].lower() in _APPLICATION_IDS
                else:
                    known = bool(following) and following[0][0] == "w" and following[0][1] in _AFTER_APPLICATION
                if not known:
                    return "an application other than System Events, System Settings or LuLu"
        return None

    def statement(self, tokens: list) -> str | None:
        """None when TOKENS are a recognized read or control-flow statement."""
        if not tokens:
            return None
        phrase = self.phrase(tokens)
        if phrase:
            return f"`{phrase}`"
        (kind, head), rest = tokens[0], tokens[1:]
        if kind != "w":
            return self.expression(tokens)
        if head == "end":
            return None if not rest or (len(rest) == 1 and rest[0][0] == "w" and rest[0][1] in _BLOCKS) else "`end`"
        if head == "try":
            return None if not rest else "`try`"
        if head == "exit":
            return None if rest == [_word("repeat")] else "`exit`"
        if head == "else":
            if not rest:
                return None
            return self.condition(rest[1:]) if rest[0] == _word("if") else "`else`"
        if head == "if":
            return self.condition(rest)
        if head == "on":
            return self.on_error(rest)
        if head == "repeat":
            return self.repeat(rest)
        if head == "tell":
            return self.tell(rest)
        if head == "set":
            return self.assignment(rest)
        if head == "activate":
            return None if not rest or _application(rest) else "`activate` with more than an application"
        if head in ("log", "delay"):
            return self.expression(rest)
        if head == "return":
            return self.expression(rest) if rest else None
        return self.expression(tokens)

    def condition(self, tokens: list) -> str | None:
        then = _split(tokens, "then")
        if then is None:
            return "`if` without `then`"
        return self.expression(tokens[:then]) or (self.statement(tokens[then + 1:]) if tokens[then + 1:] else None)

    def on_error(self, tokens: list) -> str | None:
        if not tokens or tokens[0] != _word("error"):
            return "a handler"
        names = [text for kind, text in tokens[1:] if kind == "w" and text != "number"]
        shape = [text if text == "number" else "name" for kind, text in tokens[1:] if kind == "w"]
        if len(tokens) - 1 != len(shape) or shape not in ([], ["name"], ["number", "name"], ["name", "number", "name"]):
            return "`on error`"
        if not all(self.free(name) for name in names):
            return "an `on error` variable that is a dictionary term"
        self.variables.update(names)
        return None

    def repeat(self, tokens: list) -> str | None:
        if not tokens:
            return None
        if tokens[0] in (_word("while"), _word("until")):
            return self.expression(tokens[1:])
        if tokens[0] == _word("with"):
            if len(tokens) < 4 or tokens[1][0] != "w" or tokens[2] not in (_word("in"), _word("from")):
                return "`repeat with`"
            if not self.free(tokens[1][1]):
                return f"the loop variable `{tokens[1][1]}`, a dictionary term"
            self.variables.add(tokens[1][1])
            return self.expression(tokens[3:])
        if tokens[-1] == _word("times"):
            return self.expression(tokens[:-1])
        return "`repeat`"

    def tell(self, tokens: list) -> str | None:
        to = _split(tokens, "to")
        target, statement = (tokens, None) if to is None else (tokens[:to], tokens[to + 1:])
        unknown = self.expression(target)
        if unknown:
            # Another application's dictionary is unknown, so it takes only `activate`.
            return None if _application(target) and statement == [_word("activate")] else unknown
        if statement is None:
            return None
        return self.statement(statement) if statement else "`tell ... to` nothing"

    def assignment(self, tokens: list) -> str | None:
        to = _split(tokens, "to")
        if to is None:
            return "`set` without `to`"
        target, value = tokens[:to], tokens[to + 1:]
        if len(target) == 1:
            names = [target[0]]
        elif len(target) >= 3 and target[0] == ("p", "{") and target[-1] == ("p", "}"):
            inner = target[1:-1]
            if any(token != ("p", ",") for token in inner[1::2]) or len(inner) % 2 == 0:
                return "`set` on something other than plain variables"
            names = inner[0::2]
        else:
            return "`set` on something other than a plain variable"
        if not all(kind == "w" and self.free(text) for kind, text in names):
            return "`set` on a property or dictionary term"
        problem = self.expression(value)
        if problem:
            return problem
        self.variables.update(text for _, text in names)
        return None


def problem(script: str) -> str | None:
    """None when SCRIPT only reads; else what in it is not a recognized read."""
    lines = _lines(script)
    if lines is None:
        return "a character outside plain AppleScript"
    reader = _Reader()
    for line in lines:
        found = reader.statement(line)
        if found:
            return found
    return None
