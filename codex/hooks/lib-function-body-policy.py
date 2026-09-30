#!/usr/bin/env python3
"""Deny shell introspection that would print a shell function's body.

Read a JSON command string (or {"cmd": string}); emit allow/deny JSON.

On 2026-09-30 an agent ran `which mbsync-passcmd gmail-maildir-sync`. In zsh,
`which` is `whence -c`, which prints a function's full definition, and the
function held literal OAuth credentials, so they reached the transcript. The
output redactor only masks known credential shapes; a function body can hold
anything. So this denies, before execution:

- dumpers that print function bodies by design: `functions` (except `+`
  name listings), `typeset/declare/local/export/readonly -f`, and the
  `$functions` / `${functions[...]}` parameter (except the `(k)` key flag);
- `which`, `where`, `whence -c/-f/-x` and `type` (bash `type` prints bodies)
  on a name that is a shell function, established by asking a non-interactive
  zsh child `whence -w` (which prints only the name and its kind). That child
  sees functions from `.zshenv` and the files it sources, including
  `.zshenv-secrets`; functions defined only in `.zshrc` are not detected, so
  Claude's output redaction remains the backstop for those.

Paths, builtins and commands stay available, as do `whence -w NAME`,
`command -v NAME` and `type -w NAME`, which never print a body. Heredoc bodies
count only when a shell reads them; computed command names are not covered.
This is a static check, not a sandbox.
"""
import importlib.util
import json
from pathlib import Path
import re
import subprocess
import sys

sys.dont_write_bytecode = True
SPEC = importlib.util.spec_from_file_location("function_body_shell_tokens", Path(__file__).with_name("lib-op-policy.py"))
TOKENS = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = TOKENS
SPEC.loader.exec_module(TOKENS)

RELEVANT = re.compile(r"(?<![\w-])(?:which|where|whence|type|functions|typeset|declare|local|export|readonly|eval)(?![\w-])")
# `$functions`, `${functions[x]}`, `${(kv)functions}`, `$functions_source` ...
# A `(k)`-only flag yields names, not bodies.
PARAMETER = re.compile(r"\$\{?(\([a-zA-Z@]*\))?(?:dis_)?functions\b")
HEREDOC = re.compile(r"<<-?\s*(['\"]?)(\w+)\1[^\n]*\n")
DECLARATIONS = {"typeset", "declare", "local", "export", "readonly"}
LOOKUPS = {"which", "where", "whence", "type"}
FALLBACK = re.compile(r"(?:^|[;&|(!`\s])(which|where|whence|type|functions|typeset|declare|local|export|readonly)"
                      r"((?:[ \t]+[^\s;&|<>()`'\"]+)*)")


class Denied(Exception):
    pass


def shell_functions(names):
    """The subset of NAMES that are shell functions in a non-interactive zsh."""
    names = [name for name in names if name and "/" not in name]
    if not names:
        return []
    try:
        result = subprocess.run(["zsh", "-c", 'whence -w -- "$@"', "zsh", *names],
                                capture_output=True, text=True, timeout=10)
    except (OSError, subprocess.TimeoutExpired):
        raise Denied("could not check whether the named commands are shell functions") from None
    kinds = dict(line.rsplit(": ", 1) for line in result.stdout.splitlines() if ": " in line)
    if set(kinds) != set(names):
        raise Denied("could not check whether the named commands are shell functions")
    return [name for name in names if kinds[name] == "function"]


def expanded_text(text):
    """TEXT without single-quoted spans and escaped `$`, which never expand."""
    out, i, double = [], 0, False
    while i < len(text):
        c = text[i]
        if c == "\\":
            i += 2
            continue
        if c == "'" and not double:
            end = text.find("'", i + 1)
            i = len(text) if end < 0 else end + 1
            continue
        if c == '"':
            double = not double
        out.append(c)
        i += 1
    return "".join(out)


def check_parameter(text):
    for match in PARAMETER.finditer(expanded_text(text)):
        flags = match.group(1) or ""
        if not re.fullmatch(r"\(k\)", flags):
            raise Denied("the $functions parameter holds shell function bodies")


def check_simple(base, arguments):
    options = [word for word in arguments if word.startswith(("-", "+")) and word not in {"-", "--"}]
    operands = [word for word in arguments if word not in options and word != "--"]
    letters = "".join(option[1:] for option in options if option.startswith("-"))
    if base == "functions":
        if not (options and all(option == "+" or option.startswith("+") for option in options)):
            raise Denied("`functions` prints shell function bodies")
    elif base in DECLARATIONS:
        if "f" in letters:
            raise Denied(f"`{base} -f` prints shell function bodies")
    elif base in LOOKUPS:
        if "w" in letters:
            return
        prints_body = (base in {"which", "where"} or any(flag in letters for flag in "cfx")
                       or base == "type")
        if prints_body:
            functions = shell_functions(operands)
            if functions:
                raise Denied(f"`{base}` would print the body of shell function {', '.join(functions)}")


def classify(source, depth=0):
    if depth > 8:
        raise Denied("nested shell introspection exceeds the classifier limit")
    if not RELEVANT.search(source) and not PARAMETER.search(source):
        return
    # A heredoc body is data unless a shell reads it as its program.
    match = HEREDOC.search(source)
    while match:
        prefix = source[:match.start()]
        end = re.search(r"(?m)^\t*" + re.escape(match[2]) + r"[ \t]*$", source[match.end():])
        if end is None:
            raise Denied("unterminated heredoc near shell introspection cannot be classified")
        body = source[match.end():match.end() + end.start()]
        try:
            head = TOKENS._strip_wrappers(TOKENS._split_simple(TOKENS._tokenize(prefix))[-1][-1].words)
        except (TOKENS.Deny, IndexError):
            head = []
        if head and TOKENS._basename(head[0].text) in TOKENS.SHELLS:
            classify(body, depth + 1)
        source = prefix + "\n" + source[match.end() + end.end():]
        match = HEREDOC.search(source)
    check_parameter(source)
    try:
        lifted, captures, _ = TOKENS._lift_substitutions(source)
        for capture in captures:
            classify(capture.inner, depth + 1)
        for pipeline in TOKENS._split_simple(TOKENS._tokenize(lifted)):
            for simple in pipeline:
                words = TOKENS._strip_wrappers(simple.words)
                if not words or words[0].quoted and words[0].expands:
                    continue
                base = TOKENS._basename(words[0].text)
                arguments = [word.text for word in words[1:]]
                if base in TOKENS.SHELLS:
                    for index, argument in enumerate(arguments):
                        if argument.startswith("-") and not argument.startswith("--") and "c" in argument:
                            if index + 1 < len(arguments):
                                classify(arguments[index + 1], depth + 1)
                            break
                elif base == "eval":
                    classify(" ".join(arguments), depth + 1)
                else:
                    check_simple(base, arguments)
    except TOKENS.Deny:
        # The tokenizer refuses some shapes (process substitution, unclosed
        # quotes). Fall back to the command positions a regex can still see.
        for match in FALLBACK.finditer(source):
            check_simple(match.group(1), match.group(2).split())


def main():
    try:
        data = json.load(sys.stdin)
        source = data if isinstance(data, str) else data["cmd"]
        if not isinstance(source, str):
            raise ValueError()
        classify(source)
        result = {"decision": "allow"}
    except Denied as error:
        result = {"decision": "deny", "reason": str(error)}
    except Exception:  # noqa: BLE001 - fail closed on malformed input
        result = {"decision": "deny", "reason": "function-body classifier received malformed input"}
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    sys.exit(main())
