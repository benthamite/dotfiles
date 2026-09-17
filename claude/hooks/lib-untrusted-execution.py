#!/usr/bin/env python3
"""Classify recognizable untrusted shell execution without running its code.

This routing guard covers transient package runners, downloaded shell input,
and the designated staging directory. It is not a general shell sandbox or a
proof of arbitrary interpreter-program safety. The VM runner supplies the
execution boundary; trusted host execution remains available.
"""

from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import re
import sys


HERE = Path(__file__).resolve().parent
RUNNER = HERE.parents[1] / "bin/untrusted-run"
STAGING = Path.home() / ".local/share/agent-untrusted"
# Reuse the existing quote-preserving static tokenizer, not its broker policy.
sys.dont_write_bytecode = True
SPEC = importlib.util.spec_from_file_location("untrusted_shell_tokens", HERE / "lib-op-policy.py")
TOKENS = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = TOKENS
SPEC.loader.exec_module(TOKENS)

SHELLS = {"sh", "bash", "zsh", "dash", "ksh", "fish"}
TRANSIENT = {"npx", "pnpx", "uvx", "bunx"}
WRAPPERS = {"env", "command", "builtin", "exec", "nohup", "nice", "timeout", "time", "sudo"}
WRAPPER_VALUES = {
    "env": {"-u", "--unset", "-C", "--chdir"},
    "nice": {"-n", "--adjustment"},
    "timeout": {"-s", "--signal", "-k", "--kill-after"},
    "time": {"-f", "--format", "-o", "--output"},
    "sudo": {"-u", "--user", "-g", "--group", "-h", "--host", "-p", "--prompt",
             "-C", "--close-from", "-r", "--role", "-t", "--type", "-D", "--chdir",
             "-R", "--chroot", "-T", "--command-timeout"},
    "exec": {"-a"},
}
MANAGER_VALUES = {"--prefix", "--cache", "--userconfig", "--registry", "--dir", "-C", "--cwd", "--directory", "--project", "--package", "-p"}
READERS = {"cat", "head", "tail", "wc", "ls", "stat", "file", "readlink", "pwd", "grep", "rg", "ripgrep", "echo", "printf", "true", "false"}
INTERPRETERS = SHELLS | {"node", "nodejs", "ruby", "perl", "source", ".", "eval"}
CODE_ENV = {"PATH", "PYTHONPATH", "NODE_PATH", "NODE_OPTIONS", "BASH_ENV", "ENV", "LD_PRELOAD", "DYLD_INSERT_LIBRARIES"}


class Denied(Exception):
    """A fixed diagnostic, never the command or values supplied by the caller."""


def expanded_home(value: str) -> str:
    for prefix in ("${HOME}", "$HOME", "~"):
        if value == prefix or value.startswith(prefix + "/"):
            return str(Path.home()) + value[len(prefix):]
    return value


def normalized(value: str, cwd: Path) -> Path:
    value = expanded_home(value)
    path = Path(value)
    return (path if path.is_absolute() else cwd / path).resolve(strict=False)


def staged(value: str, cwd: Path) -> bool:
    # Treat an option's explicit path value in the same way as a positional path.
    if value.startswith("-") and "=" in value:
        value = value.partition("=")[2]
    try:
        lexical = Path(os.path.normpath(os.path.join(str(cwd), expanded_home(value))))
        if lexical == STAGING or STAGING in lexical.parents:
            return True
        path = normalized(value, cwd)
    except (ValueError, OSError):
        # Ordinary inline program/data arguments need not be filesystem paths.
        return "agent-untrusted" in value
    root = STAGING.resolve(strict=False)
    return path == root or root in path.parents


def relevant(source: str, cwd: Path) -> bool:
    return (staged(str(cwd), cwd) or "agent-untrusted" in source
            or re.search(r"\b(?:npx|pnpx|uvx|bunx|npm|pnpm|yarn|uv|curl|wget|untrusted-run)\b", source) is not None)


def without_comments(tokens):
    skipping = False
    for token in tokens:
        if isinstance(token, str) and token == "\n":
            skipping = False
        if not isinstance(token, str) and token.raw.startswith("#"):
            skipping = True
        if not skipping:
            yield token


def unwrap(words, cwd):
    """Resolve ordinary literal command wrappers, including their workdirs."""
    words = list(words)
    while words and Path(words[0].text).name in WRAPPERS:
        wrapper = Path(words.pop(0).text).name
        while words:
            value = words[0].text
            if value == "--":
                words.pop(0)
                break
            if value.startswith("-S") or value == "--split-string" or value.startswith("--split-string="):
                raise Denied("a split-string command wrapper cannot be classified")
            if value in WRAPPER_VALUES.get(wrapper, set()):
                if len(words) < 2:
                    raise Denied("a command wrapper has a missing option value")
                if ((wrapper == "env" and value in {"-C", "--chdir"})
                        or (wrapper == "sudo" and value in {"-D", "--chdir"})):
                    cwd = normalized(words[1].text, cwd)
                del words[:2]
            elif value.startswith("--chdir=") and wrapper == "env":
                cwd = normalized(value.partition("=")[2], cwd)
                words.pop(0)
            elif value.startswith("-C") and wrapper == "env":
                cwd = normalized(value[2:], cwd)
                words.pop(0)
            elif value.startswith("-") or re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=.*", value):
                words.pop(0)
            elif wrapper == "timeout" and re.fullmatch(r"\d+(?:\.\d+)?[smhd]?", value):
                words.pop(0)
            else:
                break
    return words, cwd


def subcommand(arguments):
    index = 0
    while index < len(arguments):
        word = arguments[index].text
        if word == "--":
            return arguments[index + 1:]
        if word in MANAGER_VALUES:
            index += 2
        elif word.startswith("-"):
            index += 1
        else:
            return arguments[index:]
    return []


def interpreter(base):
    return base in INTERPRETERS or re.fullmatch(r"python(?:[0-9.]+)?", base) is not None


def consumes_code(base, arguments):
    """A fixed interpreter program consumes data rather than downloaded code."""
    if not interpreter(base):
        return False
    if base in {"source", ".", "eval"}:
        return True
    texts = [word.text for word in arguments]
    if "-c" in texts or "-e" in texts or "--eval" in texts or "-m" in texts:
        return False
    if base in SHELLS:
        flags = [value for value in texts if value.startswith("-") and not value.startswith("--")]
        if any("c" in flag for flag in flags):
            return False
        if any("s" in flag for flag in flags):
            return True
    # A literal script operand supplies the code independently of stdin.
    operands = [value for value in texts if not value.startswith("-") or value == "-"]
    return not operands or operands[0] in {"-", "/dev/stdin", "/dev/fd/0"}


def untrusted_input_capture(source, cwd):
    """Recognize a substitution producing downloaded or staged input."""
    lifted, _, _ = TOKENS._lift_substitutions(source)
    for pipeline in TOKENS._split_simple(list(without_comments(TOKENS._tokenize(lifted)))):
        for simple in pipeline:
            words, directory = unwrap(simple.words, cwd)
            if words and Path(words[0].text).name in {"curl", "wget"}:
                return True
            values = [word.text for word in words[1:]]
            values.extend(redirect.target.strip("\"'") for redirect in simple.redirects
                          if redirect.op.startswith("<"))
            if any(staged(value, directory) for value in values):
                return True
    return False


def literal_heredocs(source):
    """Remove quoted data/program bodies that are not shell command words.

    Only a plain, single quoted-delimiter heredoc is recognized. Complex and
    unquoted forms remain in the scan. Shell programs remain executable input.
    General non-shell program analysis is outside this routing guard's scope.
    """
    lines = source.splitlines(keepends=True)
    index = 0
    while index < len(lines):
        match = re.fullmatch(r"([^<>;|&`\n]+)<<(-)?\s*(['\"])([A-Za-z_][A-Za-z0-9_]*)\3\s*", lines[index])
        if not match:
            index += 1
            continue
        prefix = match[1]
        commands = TOKENS._split_simple(TOKENS._tokenize(prefix))
        if len(commands) != 1 or len(commands[0]) != 1 or not commands[0][0].words:
            index += 1
            continue
        base = Path(commands[0][0].words[0].text).name
        if base not in READERS | {"tee", "node", "ruby", "perl", "jq"} and not re.fullmatch(r"python[0-9.]*", base):
            index += 1
            continue
        end = next((position for position in range(index + 1, len(lines))
                    if (lines[position].lstrip("\t") if match[2] else lines[position]).rstrip("\n") == match[4]), None)
        if end is None:
            raise TOKENS.Deny("unterminated heredoc")
        lines[index:end + 1] = [prefix + "\n"]
        index += 1
    return "".join(lines)


def classify(source: str, cwd: Path, depth=0):
    if depth > 5:
        raise Denied("nested execution exceeds the routing guard's limit")
    try:
        source = literal_heredocs(source)
        lifted, captures, process_substitution = TOKENS._lift_substitutions(source)
        tokens = list(without_comments(TOKENS._tokenize(lifted)))
        pipelines = TOKENS._split_simple(tokens)
    except TOKENS.Deny:
        if relevant(source, cwd):
            raise Denied("a relevant shell command could not be parsed") from None
        return
    if process_substitution and relevant(source, cwd):
        raise Denied("a relevant process substitution cannot be safely routed")
    if (staged(str(cwd), cwd) or "agent-untrusted" in source) and any(token in {"(", ")", "{", "}", "&"} for token in tokens if isinstance(token, str)):
        raise Denied("grouped or background execution involving untrusted staging requires an explicit runner command")
    current = cwd
    for pipeline in pipelines:
        downloaded = False
        staged_input = False
        for simple in pipeline:
            assignments = [(name, word.text) for name, word in simple.assignments]
            assignments.extend((word.text.partition("=")[0], word.text.partition("=")[2])
                               for word in simple.words if "=" in word.text)
            if any(name in CODE_ENV and any(staged(part, current) for part in value.split(":"))
                   for name, value in assignments):
                raise Denied("an executable search path or loader references untrusted staging")
            try:
                words, invocation_dir = unwrap(simple.words, current)
            except Denied:
                wrapper_text = " ".join(word.text[2:] if word.text.startswith("-S") else word.text
                                        for word in simple.words)
                if relevant(source, current) or relevant(wrapper_text, current):
                    raise
                continue  # Unrelated wrapper syntax is outside this guard's scope.
            # Expansions use this invocation's directory, including prior cd.
            # They run on the host even in arguments to the VM runner.
            expansion_inputs = ([word.text for word in simple.words]
                                + [word.text for _, word in simple.assignments]
                                + [redirect.target for redirect in simple.redirects])
            if simple.here_string is not None:
                expansion_inputs.append(simple.here_string.text)
            for capture in captures:
                if any(capture.placeholder in value for value in expansion_inputs):
                    classify(capture.inner, current, depth + 1)
            if not words:
                continue
            head, arguments = words[0], words[1:]
            base = Path(head.text).name
            if simple.here_string is not None and consumes_code(base, arguments):
                if any(capture.placeholder in simple.here_string.text and untrusted_input_capture(capture.inner, current)
                       for capture in captures):
                    raise Denied("downloaded or staged input is being executed through a host here-string")
            if head.expands and expanded_home(head.text) == head.text and relevant(source, invocation_dir):
                raise Denied("an indirect executable in a relevant command cannot be classified")
            if base == "untrusted-run":
                if normalized(head.text, invocation_dir) != RUNNER:
                    raise Denied("use the canonical untrusted-run executable")
                # Its arguments describe container execution, not host execution.
                # Substitutions were checked before this per-command exemption.
                continue
            metadata = arguments and all(word.text in {"--help", "-h", "--version", "-V", "-v"} for word in arguments)
            if base in TRANSIENT and not metadata:
                raise Denied("a transient package executable requires isolation")
            command = subcommand(arguments)
            values = [word.text for word in command]
            if ((base == "npm" and values[:1] in (["exec"], ["x"]))
                    or (base in {"pnpm", "yarn"} and values[:1] == ["dlx"])
                    or (base == "uv" and values[:2] == ["tool", "run"])):
                raise Denied("a transient package executable requires isolation")
            if base == "cd":
                if len(pipeline) > 1 and (staged(str(invocation_dir), invocation_dir) or any(staged(word.text, invocation_dir) for word in arguments)):
                    raise Denied("a piped directory change involving untrusted staging cannot be classified")
                if len(arguments) == 1 and not arguments[0].expands:
                    current = normalized(arguments[0].text, invocation_dir)
                elif len(arguments) == 1 and expanded_home(arguments[0].text) != arguments[0].text:
                    current = normalized(arguments[0].text, invocation_dir)
                elif relevant(source, invocation_dir):
                    raise Denied("a relevant directory change cannot be classified")
                continue
            if interpreter(base):
                for index, word in enumerate(arguments):
                    previous = arguments[index - 1].text if index else ""
                    code_operand = (base in {"eval", "source", "."} or previous in {"-c", "-e", "--eval"}
                                    or (base in SHELLS and previous.startswith("-") and not previous.startswith("--") and "c" in previous))
                    if code_operand and any(capture.placeholder in word.text and re.search(r"\b(?:curl|wget)\b", capture.inner)
                                            for capture in captures):
                        raise Denied("downloaded command substitution is being executed by a host interpreter")
            if base in SHELLS:
                for index, word in enumerate(arguments):
                    if word.text == "-c" or (word.text.startswith("-") and not word.text.startswith("--") and "c" in word.text):
                        if index + 1 >= len(arguments):
                            raise Denied("a nested shell has no command")
                        classify(arguments[index + 1].text, invocation_dir, depth + 1)
                        break
            if base == "eval":
                classify(" ".join(word.text for word in arguments), invocation_dir, depth + 1)
            staged_args = any(staged(word.text, invocation_dir) for word in arguments)
            staged_redirect = any(redirect.op.startswith("<") and staged(redirect.target.strip("\"'"), invocation_dir)
                                  for redirect in simple.redirects)
            at_staging = staged(str(invocation_dir), invocation_dir)
            safe_reader = base in READERS and not any(word.text == "--pre" or word.text.startswith("--pre=") for word in arguments)
            explicit_executable = "/" in head.text or head.text.startswith(".")
            if ((explicit_executable and staged(head.text, invocation_dir))
                    or ((at_staging or staged_args) and not safe_reader)):
                raise Denied("host execution involving the untrusted staging directory requires isolation")
            downloaded = downloaded or base in {"curl", "wget"}
            staged_input = staged_input or staged_args or staged_redirect or (at_staging and bool(arguments))
            if (downloaded or staged_input) and consumes_code(base, arguments):
                raise Denied("downloaded or staged input is being executed by a host interpreter")


def main():
    try:
        contexts = json.load(sys.stdin)
        if not isinstance(contexts, list):
            raise ValueError("contexts")
        for context in contexts:
            if not isinstance(context, dict) or context.get("ambiguous"):
                raise Denied("a nested execution context cannot be classified")
            command, cwd = context.get("cmd"), context.get("workdir")
            if not isinstance(command, str) or not isinstance(cwd, str) or not cwd:
                raise ValueError("context fields")
            classify(command, Path(cwd).resolve(strict=False))
    except Denied as error:
        print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse", "permissionDecision": "deny",
              "permissionDecisionReason": "BLOCKED: " + str(error) + ". Use the canonical dotfiles bin/untrusted-run with explicit minimal inputs and a fresh output directory. If the required VM or runtime is unavailable, stop this execution; do not retry it on the host."}}))
        return 0
    except (ValueError, TypeError, KeyError, OSError):
        print("Security hook failed; tool execution denied.", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
