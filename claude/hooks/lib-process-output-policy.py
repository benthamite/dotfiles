#!/usr/bin/env python3
"""Conservative static process-output policy; not a general execution sandbox.

Read a JSON command string (or {"cmd": string}); emit allow/deny JSON.
Literal wrappers, compound commands, shell -c, and substitutions are covered.
Computed executable names and programs loaded from external files are not.
"""
import importlib.util
import json
from pathlib import Path
import re
import sys

sys.dont_write_bytecode = True
SPEC = importlib.util.spec_from_file_location("process_shell_tokens", Path(__file__).with_name("lib-op-policy.py"))
TOKENS = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = TOKENS
SPEC.loader.exec_module(TOKENS)
SHELLS = TOKENS.SHELLS | {"fish"}
FIELDS = set("pid ppid pgid pgrp sess sid uid euid ruid gid egid rgid user ruser group rgroup comm ucomm etime etimes time cputime cpu pcpu pmem %cpu %mem rss vsz vsize state stat tty tt tname nice ni pri flags f start started lstart wchan jobc nlwp thcount".split())
RELEVANT = re.compile(r"(?<![\w-])(?:ps|pgrep)(?![\w-])|psutil|/proc/[^\s]+/(?:cmdline|environ)")
INTERPRETERS = {"node", "nodejs", "perl", "ruby", "awk", "osascript"}
WRAPPER_VALUES = {
    "env": {"-u", "--unset", "-C", "--chdir"},
    "nice": {"-n", "--adjustment"},
    "timeout": {"-s", "--signal", "-k", "--kill-after"},
    "time": {"-f", "--format", "-o", "--output"},
    "sudo": {"-u", "--user", "-g", "--group", "-h", "--host", "-p", "--prompt",
             "-C", "--close-from", "-r", "--role", "-t", "--type", "-D", "--chdir",
             "-R", "--chroot", "-T", "--command-timeout", "-U", "--other-user"},
    "exec": {"-a"},
}

class Denied(Exception):
    pass

def fields(value):
    names = [part.partition("=")[0] for part in re.split(r"[,\s]+", value) if part]
    if not names or any(name not in FIELDS for name in names):
        raise Denied("ps must select only approved non-secret fields, such as pid,ppid,comm,etime")

def check_ps(arguments):
    selected = False
    index = 0
    while index < len(arguments):
        value = arguments[index]
        if value.startswith("--"):
            if value.startswith("--format="):
                fields(value.split("=", 1)[1]); selected = True
            elif value == "--format" and index + 1 < len(arguments):
                index += 1; fields(arguments[index]); selected = True
            elif value not in {"--no-headers", "--headers"}:
                raise Denied("unclassified ps option may expose process arguments or environment")
        else:
            if not value.startswith("-") and "e" in value:
                raise Denied("BSD ps environment output is not allowed")
            flags = value.lstrip("-")
            position = 0
            while position < len(flags):
                flag = flags[position]
                if flag in "oGgptUu":
                    operand = flags[position + 1:]
                    if not operand:
                        index += 1
                        if index >= len(arguments): raise Denied("ps option has no value")
                        operand = arguments[index]
                    if flag == "o": fields(operand); selected = True
                    break
                if flag not in "AaCcdehrSTwXx":
                    raise Denied("ps display modifiers may expose process arguments or environment")
                position += 1
        index += 1
    if not selected: raise Denied("ps requires explicit safe fields; use ps -axo pid,ppid,comm,etime")

def check_pgrep(arguments):
    full = listing = False
    index = 0
    while index < len(arguments):
        value = arguments[index]
        if value == "--": break
        if value.startswith("--"):
            if value == "--full": full = True
            elif value == "--list-name": listing = True
            elif value in {"--list-full", "--echo"}: raise Denied("pgrep full-command output can expose credentials")
            elif value.split("=", 1)[0] in {"--delimiter", "--pgroup", "--group", "--parent", "--session", "--terminal", "--euid", "--uid", "--pidfile", "--runstates"}:
                if "=" not in value: index += 1
            elif value not in {"--count", "--ignore-case", "--newest", "--oldest", "--inverse", "--exact", "--lightweight"}:
                raise Denied("unclassified pgrep option may expose process arguments")
        elif value.startswith("-"):
            for position, flag in enumerate(value[1:], 1):
                if flag in "FGPUdgstu":
                    if position == len(value) - 1: index += 1
                    break
                if flag == "a": raise Denied("pgrep -a prints full arguments on Linux; use PID-only output")
                if flag == "f": full = True
                elif flag == "l": listing = True
                elif flag not in "Linoqvxcw": raise Denied("unclassified pgrep option may expose process arguments")
        index += 1
    if full and listing: raise Denied("pgrep -fl exposes full process arguments on macOS; use pgrep -f or pgrep -l")

def unwrap(words):
    words = list(words)
    while words and Path(words[0].text).name in TOKENS.WRAPPERS:
        base = Path(words.pop(0).text).name
        valued = WRAPPER_VALUES.get(base, set())
        while words:
            value = words[0].text
            if value == "--": words.pop(0); break
            if value == "--split-string" or value.startswith("-S") or value.startswith("--split-string="):
                raise Denied("split-string execution involving process inspection cannot be classified")
            if value in valued: del words[:2]
            elif value.startswith("-") or (base == "env" and "=" in value): words.pop(0)
            elif base == "timeout" and re.fullmatch(r"\d+(?:\.\d+)?[smhd]?", value): words.pop(0)
            else: break
    return words

def classify(source, depth=0):
    if depth > 8: raise Denied("nested process inspection exceeds the classifier limit")
    relevant = RELEVANT.search(re.sub(r"[\\'\"]", "", source))
    if not relevant: return
    # Quoted data heredocs are inert; executable interpreter bodies are checked.
    match = re.search(r"<<-?\s*(['\"]?)(\w+)\1[^\n]*\n", source)
    while match:
        prefix = source[:match.start()]
        end = re.search(r"(?m)^\t*" + re.escape(match[2]) + r"[ \t]*$", source[match.end():])
        if end is None: raise Denied("unclassified relevant heredoc")
        body = source[match.end():match.end() + end.start()]
        if "|" in source[:match.end()] and RELEVANT.search(body):
            raise Denied("piped process-inspection heredoc cannot be classified")
        head = unwrap(TOKENS._split_simple(TOKENS._tokenize(prefix))[-1][-1].words)
        base = Path(head[0].text).name if head else ""
        if base in SHELLS: classify(body, depth + 1)
        elif not match[1] or base not in {"cat", "tee", "sed", "grep", "rg"}:
            if RELEVANT.search(body): raise Denied("process inspection inside interpreter source requires a directly checked shell command")
        source = prefix + "\n" + source[match.end() + end.end():]
        match = re.search(r"<<-?\s*(['\"]?)(\w+)\1[^\n]*\n", source)
    try:
        lifted, captures, procsub = TOKENS._lift_substitutions(source)
        if procsub and relevant: raise Denied("process inspection in process substitution cannot be classified")
        for capture in captures: classify(capture.inner, depth + 1)
        tokens = []
        comment = False
        for token in TOKENS._tokenize(lifted):
            if isinstance(token, str) and token == "\n": comment = False
            elif not isinstance(token, str) and token.raw.startswith("#"): comment = True
            if not comment: tokens.append(token)
        for pipeline in TOKENS._split_simple(tokens):
            for simple in pipeline:
                try:
                    words = unwrap(simple.words)
                except Denied:
                    if relevant: raise
                    continue
                if not words: continue
                base = Path(words[0].text).name
                arguments = words[1:]
                if base in {"cat", "head", "tail", "less", "more", "od", "xxd", "strings", "grep", "rg", "sed", "awk"} and any(re.search(r"/proc/[^\s]+/(?:cmdline|environ)(?:$|/)", word.text) for word in arguments):
                    raise Denied("process command-line and environment files can expose credentials")
                if base in {"ps", "pgrep"}:
                    if any(word.expands or "\x00" in word.text for word in arguments): raise Denied("dynamic process-listing options cannot be classified")
                    (check_ps if base == "ps" else check_pgrep)([word.text for word in arguments])
                elif base in SHELLS:
                    for index, word in enumerate(arguments):
                        if word.text.startswith("-") and not word.text.startswith("--") and "c" in word.text:
                            if index + 1 < len(arguments): classify(arguments[index + 1].text, depth + 1)
                            break
                    if simple.here_string: classify(simple.here_string.text, depth + 1)
                elif base == "eval": classify(" ".join(word.text for word in arguments), depth + 1)
                elif base == "find" and any(word.text in {"-exec", "-execdir", "-ok", "-okdir"} for word in arguments) and relevant:
                    raise Denied("process inspection inside find execution cannot be classified")
                elif base in INTERPRETERS or re.fullmatch(r"python[0-9.]*", base) or base in {"xargs", "watch"}:
                    if any(RELEVANT.search(word.text) for word in arguments): raise Denied("process inspection inside interpreter or launcher source cannot be classified")
    except TOKENS.Deny:
        if relevant: raise Denied("relevant process inspection could not be parsed") from None

def main():
    try:
        data = json.load(sys.stdin)
        source = data if isinstance(data, str) else data["cmd"]
        if not isinstance(source, str): raise ValueError()
        classify(source)
        result = {"decision": "allow"}
    except Denied as error: result = {"decision": "deny", "reason": str(error)}
    except (ValueError, TypeError, KeyError, IndexError, TOKENS.Deny): result = {"decision": "deny", "reason": "process-output policy input could not be classified"}
    print(json.dumps(result))

if __name__ == "__main__": main()
