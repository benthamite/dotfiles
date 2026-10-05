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
# Python modules that can neither start a process nor evaluate text. A program
# importing only these, with no dynamic builtins, cannot run a string literal.
INERT_MODULES = {"__future__", "base64", "collections", "dataclasses", "datetime", "difflib", "enum",
                 "fnmatch", "functools", "hashlib", "itertools", "json", "math", "pathlib", "re",
                 "string", "textwrap", "typing", "unicodedata"}
EVALUATORS = {"__import__", "compile", "getattr", "breakpoint", "exec", "eval", "globals", "locals",
              "vars", "__builtins__"}
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
    dynamic = any(isinstance(node, ast.Name) and node.id in {"exec", "eval", "globals", "locals", "vars"}
                  or isinstance(node, ast.ImportFrom) and any(alias.name == "*" for alias in node.names)
                  for node in nodes)
    def lexical_scope(node):
        while node in parents:
            node = parents[node]
            if isinstance(node, (ast.Module, ast.FunctionDef, ast.AsyncFunctionDef,
                                 ast.ClassDef, ast.Lambda, ast.ListComp, ast.SetComp,
                                 ast.DictComp, ast.GeneratorExp)):
                return node
        return tree

    bindings, scoped_bindings = {}, {}
    for node in nodes:
        name = (node.id if isinstance(node, ast.Name) and isinstance(node.ctx, (ast.Store, ast.Del))
                else node.asname or node.name.split(".")[0] if isinstance(node, ast.alias)
                else node.arg if isinstance(node, ast.arg)
                else node.name if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.ExceptHandler))
                else node.name if isinstance(node, (ast.MatchAs, ast.MatchStar)) else None)
        if name:
            bindings.setdefault(name, []).append(node)
            scoped_bindings.setdefault((lexical_scope(node), name), []).append(node)
    # A list must be proved in the receiver's own lexical scope. Do not infer
    # closure values, or confuse a function local with the caller's binding.
    redirected = {lexical_scope(node) for node in nodes
                  if isinstance(node, (ast.Global, ast.Nonlocal))
                  and {"ps", "pgrep"}.intersection(node.names)}
    lists = {}
    for (scope, name), bound in scoped_bindings.items():
        if (not dynamic and name in {"ps", "pgrep"} and len(bound) == 1
                and scope not in redirected
                and isinstance(scope, (ast.Module, ast.FunctionDef, ast.AsyncFunctionDef))):
            binding = parents.get(bound[0])
            if (isinstance(binding, ast.Assign) and len(binding.targets) == 1
                    and binding.targets[0] is bound[0]
                    and isinstance(binding.value, ast.List) and not binding.value.elts):
                lists[(scope, name)] = binding

    def data_append(node):
        if not (isinstance(node, ast.Attribute) and node.attr == "append"
                and isinstance(node.value, ast.Name)):
            return False
        binding = lists.get((lexical_scope(node), node.value.id))
        return (binding is not None
                and (binding.lineno, binding.col_offset) < (node.lineno, node.col_offset))

    # Prove only indexed reads from a uniquely bound literal pathlib-glob list.
    # Other receivers stay conservative, including nested/dynamic namespaces.
    path_eligible = not dynamic and not any(
        isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda,
                          ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp, ast.Match))
        or isinstance(node, (ast.Attribute, ast.Subscript)) and isinstance(node.ctx, (ast.Store, ast.Del))
        or isinstance(node, ast.Attribute) and node.attr == "__dict__"
        or isinstance(node, ast.Name) and node.id in {"setattr", "delattr", "__builtins__"}
        for node in nodes)
    imports = bindings.get("pathlib", [])
    path_eligible = (path_eligible and "list" not in bindings and len(imports) == 1
                     and isinstance(imports[0], ast.alias) and imports[0].name == "pathlib"
                     and imports[0].asname is None and isinstance(parents[imports[0]], ast.Import))

    def assignment(name, before):
        bound = bindings.get(name, [])
        if len(bound) == 1:
            statement = parents.get(bound[0])
            if (isinstance(statement, ast.Assign) and len(statement.targets) == 1
                    and statement.targets[0] is bound[0] and module_scope(statement)
                    and (statement.lineno, statement.col_offset) < (before.lineno, before.col_offset)):
                return statement.value
        return None

    def literal_text(node):
        return isinstance(node, ast.Constant) and isinstance(node.value, str)

    def literal_path(node, seen=frozenset()):
        if isinstance(node, ast.Name) and node.id not in seen:
            value = assignment(node.id, node)
            return value is not None and literal_path(value, seen | {node.id})
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Div):
            return literal_path(node.left, seen) and literal_text(node.right)
        return (isinstance(node, ast.Call) and not node.keywords and len(node.args) == 1
                and literal_text(node.args[0]) and isinstance(node.func, ast.Attribute)
                and node.func.attr == "Path" and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "pathlib")

    path_reads = set()
    if path_eligible:
        for node in nodes:
            if not (isinstance(node, ast.Name) and node.id in {"ps", "pgrep"}):
                continue
            sub = parents.get(node)
            attr = parents.get(sub)
            call = parents.get(attr)
            value = assignment(node.id, node)
            if not (isinstance(sub, ast.Subscript) and sub.value is node
                    and isinstance(sub.slice, ast.Constant) and type(sub.slice.value) is int
                    and isinstance(attr, ast.Attribute) and attr.value is sub and attr.attr == "read_text"
                    and isinstance(call, ast.Call) and call.func is attr and not call.args and not call.keywords
                    and isinstance(value, ast.Call) and isinstance(value.func, ast.Name)
                    and value.func.id == "list" and len(value.args) == 1 and not value.keywords):
                continue
            glob = value.args[0]
            if (isinstance(glob, ast.Call) and not glob.keywords and len(glob.args) == 1
                    and literal_text(glob.args[0]) and isinstance(glob.func, ast.Attribute)
                    and glob.func.attr == "glob" and literal_path(glob.func.value)):
                path_reads.add(node)

    # A `ps` holding parsed JSON or a builtin container is data: `ps.values()`
    # and `ps.get(k)` call builtin methods that start nothing. Proven like the
    # list appends above, in the receiver's own module or function scope:
    # every binding of each name involved is a plain assignment in that scope
    # whose value is such data (a name stands for all of its bindings there).
    # json is the stdlib module imported once at the top level; json.load
    # without hooks returns only dicts, lists and scalars.
    json_imports = bindings.get("json", [])
    real_json = (len(json_imports) == 1 and isinstance(json_imports[0], ast.alias)
                 and json_imports[0].name == "json" and json_imports[0].asname is None
                 and isinstance(parents[json_imports[0]], ast.Import) and module_scope(json_imports[0]))
    imported_roots = ({alias.name.split(".")[0] for node in nodes if isinstance(node, ast.Import)
                       for alias in node.names}
                      | {(node.module or "").split(".")[0] for node in nodes if isinstance(node, ast.ImportFrom)})
    tampering = (dynamic or "builtins" in imported_roots or any(
        isinstance(node, ast.Name) and node.id in {"__builtins__", "setattr", "delattr"}
        or isinstance(node, ast.Attribute) and node.attr in {"__dict__", "modules"}
        for node in nodes))
    BUILTIN_CONSTRUCTORS = {"list", "dict", "set", "tuple", "sorted", "frozenset"}

    def name_bound_to(name, predicate, scope, seen):
        key = (predicate.__name__, name)
        if key in seen: return True
        bound = bindings.get(name, [])
        if not bound: return False
        for node in bound:
            statement = parents.get(node)
            if not (isinstance(node, ast.Name) and isinstance(node.ctx, ast.Store)
                    and lexical_scope(node) is scope
                    and (isinstance(statement, ast.Assign) and statement.targets == [node]
                         or isinstance(statement, ast.AnnAssign) and statement.target is node
                         and statement.value is not None)
                    and predicate(statement.value, scope, seen | {key})):
                return False
        return True

    def json_value(node, scope, seen):
        """NODE evaluates to a value json.load could return."""
        if isinstance(node, ast.Constant): return True
        if isinstance(node, ast.Name): return name_bound_to(node.id, json_value, scope, seen)
        if isinstance(node, ast.Subscript): return json_value(node.value, scope, seen)
        if isinstance(node, ast.IfExp):
            return json_value(node.body, scope, seen) and json_value(node.orelse, scope, seen)
        if isinstance(node, ast.BoolOp): return all(json_value(value, scope, seen) for value in node.values)
        if isinstance(node, ast.Call) and not node.keywords and isinstance(node.func, ast.Attribute):
            receiver = node.func.value
            if (real_json and node.func.attr in {"load", "loads"} and len(node.args) == 1
                    and isinstance(receiver, ast.Name) and receiver.id == "json"):
                return True
            return (node.func.attr == "get" and 1 <= len(node.args) <= 2 and json_value(receiver, scope, seen)
                    and all(json_value(arg, scope, seen) for arg in node.args[1:]))
        return False

    def builtin_value(node, scope, seen):
        """NODE evaluates to JSON data or a builtin container."""
        if json_value(node, scope, seen): return True
        if isinstance(node, (ast.List, ast.Tuple, ast.Set, ast.Dict,
                             ast.ListComp, ast.SetComp, ast.DictComp)): return True
        if isinstance(node, ast.Name): return name_bound_to(node.id, builtin_value, scope, seen)
        if isinstance(node, ast.IfExp):
            return builtin_value(node.body, scope, seen) and builtin_value(node.orelse, scope, seen)
        return (isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                and node.func.id in BUILTIN_CONSTRUCTORS and node.func.id not in bindings)

    container_methods = {name for kind in (dict, list, set, tuple, str)
                         for name in dir(kind) if not name.startswith("_")}
    builtin_receivers = set()
    if not tampering:
        for node in nodes:
            parent = parents.get(node)
            scope = lexical_scope(node)
            if (isinstance(node, ast.Name) and node.id in {"ps", "pgrep"}
                    and isinstance(parent, ast.Attribute) and parent.value is node
                    and parent.attr in container_methods
                    and isinstance(scope, (ast.Module, ast.FunctionDef, ast.AsyncFunctionDef))
                    and scope not in redirected
                    and builtin_value(node, scope, frozenset())):
                builtin_receivers.add(node)

    protected = set()
    for node in nodes:
        if isinstance(node, ast.Attribute) and not data_append(node):
            protected.update(ast.walk(node.value))
        elif isinstance(node, ast.Call) and not data_append(node.func):
            protected.update(ast.walk(node.func))
    protected -= builtin_receivers
    lines = source.split("\n")
    offsets, total = [], 0
    for line in lines:
        offsets.append(total)
        total += len(line.encode("utf-8")) + 1
    # String literals are data when the program cannot start a process or
    # evaluate text: only inert imports, no relative imports, no evaluators.
    # Literals naming psutil or /proc stay visible. f-string parts are skipped
    # because their positions are not reliable before Python 3.12.
    imported = ({alias.name.split(".")[0] for node in nodes if isinstance(node, ast.Import)
                 for alias in node.names}
                | {(node.module or "").split(".")[0] for node in nodes if isinstance(node, ast.ImportFrom)})
    inert_strings = (not dynamic and imported <= INERT_MODULES
                     and not any(isinstance(node, ast.ImportFrom) and node.level for node in nodes)
                     and not any(isinstance(node, ast.Name) and node.id in EVALUATORS for node in nodes))
    data = bytearray(source.encode("utf-8"))
    for node in ast.walk(tree):
        if ((isinstance(node, ast.Name) and node.id in {"ps", "pgrep"}
                and (node not in protected or node in path_reads))
                or (inert_strings and isinstance(node, ast.Constant) and isinstance(node.value, str)
                    and not isinstance(parents.get(node), ast.JoinedStr)
                    and not re.search(r"psutil|/proc/", node.value))):
            start = offsets[node.lineno - 1] + node.col_offset
            end = offsets[node.end_lineno - 1] + node.end_col_offset
            data[start:end] = b"_" * (end - start)
    return data.decode("utf-8")


def classify(source, depth=0):
    if depth > 8: raise Denied("nested process inspection exceeds the classifier limit")
    # Deleting quotes exposes split names (p''s); treating them as separators
    # keeps prefixed literals (f'ps', rb"pgrep") from merging into one word.
    relevant = (RELEVANT.search(re.sub(r"[\\'\"]", "", source))
                or RELEVANT.search(re.sub(r"[\\'\"]", " ", source)))
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
        # After a `#` the operator may be a comment, so the "body" may be shell.
        if "#" in prefix[prefix.rfind("\n") + 1:] and RELEVANT.search(body):
            raise Denied("process inspection after a possible comment cannot be classified")
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
