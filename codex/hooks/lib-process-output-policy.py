#!/usr/bin/env python3
"""Conservative static process-output policy; not a general execution sandbox.

Read a JSON command string (or {"cmd": string}); emit allow/deny JSON.
Literal wrappers, compound commands, shell -c, and substitutions are covered.
Computed executable names and programs loaded from external files are not.
"""
import ast
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

def project_python_names(source):
    """Mask only ordinary ps/pgrep variable names, never executable evidence."""
    try:
        normalized = source.replace("\r\n", "\n").replace("\r", "\n")
        tree = ast.parse(normalized)
    except (SyntaxError, ValueError, RecursionError):
        return source
    source = normalized
    nodes = list(ast.walk(tree))
    parents = {child: parent for parent in nodes for child in ast.iter_child_nodes(parent)}
    def module_scope(node):
        while node in parents:
            node = parents[node]
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)):
                return False
        return True
    # Only the observed data-list case: one module-scope empty-list binding.
    # Rebinding, shadowing and dynamic namespace mutation remain unclassified.
    lists = set()
    dynamic = any(isinstance(node, ast.Name) and node.id in {"exec", "eval", "globals", "locals", "vars"}
                  or isinstance(node, ast.ImportFrom) and any(alias.name == "*" for alias in node.names)
                  for node in nodes)
    for name in ("ps", "pgrep"):
        stores = [node for node in nodes if isinstance(node, ast.Name)
                  and node.id == name and isinstance(node.ctx, (ast.Store, ast.Del))]
        if not dynamic and len(stores) == 1:
            binding = parents.get(stores[0])
            if (isinstance(binding, ast.Assign) and len(binding.targets) == 1
                    and isinstance(binding.value, ast.List) and not binding.value.elts
                    and module_scope(binding)):
                lists.add(name)
    def data_append(node):
        return (isinstance(node, ast.Attribute) and node.attr == "append"
                and isinstance(node.value, ast.Name) and node.value.id in lists
                and module_scope(node))
    protected = set()
    for node in nodes:
        if isinstance(node, ast.Attribute) and not data_append(node):
            protected.update(ast.walk(node.value))
        elif isinstance(node, ast.Call) and not data_append(node.func):
            protected.update(ast.walk(node.func))
    lines = source.split("\n")
    offsets, total = [], 0
    for line in lines:
        offsets.append(total)
        total += len(line.encode("utf-8")) + 1
    data = bytearray(source.encode("utf-8"))
    for node in ast.walk(tree):
        if (isinstance(node, ast.Name) and node.id in {"ps", "pgrep"}
                and node not in protected):
            start = offsets[node.lineno - 1] + node.col_offset
            end = offsets[node.end_lineno - 1] + node.end_col_offset
            data[start:end] = b"_" * (end - start)
    return data.decode("utf-8")


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
            inspected = body
            if (match[1] and re.fullmatch(r"python[0-9.]*", base)
                    and [word.text for word in head[1:]] in ([], ["-"])):
                inspected = project_python_names(body)
            if RELEVANT.search(inspected): raise Denied("process inspection inside interpreter source requires a directly checked shell command")
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
