#!/usr/bin/env python3
"""Resolve explicit git commit selection without changing the real index.

Ordinary index commits retain the existing hook path. Explicit path commits use
a disposable index seeded from HEAD, as git commit --only does. No shell text
is evaluated. Unsupported selection syntax fails closed rather than guessing.
"""

import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile


def git(*args, env=None, input_data=b""):
    directory = os.environ.get("COMMIT_FILE_CWD", os.getcwd())
    return subprocess.check_output(["git", "-C", directory, *args], env=env, input=input_data, stderr=subprocess.PIPE).decode()


def select(command):
    # Heredoc message bodies are data, never commit arguments.
    lines = iter(command.splitlines())
    kept = []
    for line in lines:
        kept.append(line)
        for match in re.finditer(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1", line):
            for body in lines:
                if body.strip() == match.group(2):
                    break
    lexer = shlex.shlex("\n".join(kept), posix=True, punctuation_chars=";&|\n<")
    lexer.whitespace_split = True
    tokens = list(lexer)
    starts = [i + 2 for i in range(len(tokens) - 1) if tokens[i:i + 2] == ["git", "commit"]]
    record = json.loads(os.environ.get("COMMIT_FILE_RECORD") or "null")
    if record is not None:
        args = record["args"]
    elif len(starts) != 1:
        # Regex callers can match inert quoted text (for example an rg
        # pattern). Only the shared invocation classifier can prove this is
        # not a commit; a parser failure or actual/ambiguous commit still denies.
        library = Path(__file__).resolve().parents[2] / "codex/hooks/lib-codex-hook-json.sh"
        classified = subprocess.check_output(
            ["bash", "-c", "set -euo pipefail; source \"$1\"; codex_git_invocations \"$2\"",
             "commit-classifier", str(library), os.environ.get("COMMIT_FILE_CWD", os.getcwd())],
            input=command.encode(), stderr=subprocess.PIPE,
        )
        records = [json.loads(item) for item in classified.split(b"\0") if item]
        if any(not isinstance(item, dict) or not isinstance(item.get("subcommand"), str) for item in records):
            raise ValueError("invalid invocation-classifier response")
        if not any(item["subcommand"] == "commit" for item in records):
            return {"mode": "inspection", "staged": "", "status": "", "diffs": {}}
        raise ValueError("cannot resolve multiple or indirect commit commands")
    else:
        args = []
        for token in tokens[starts[0]:]:
            if token and all(c in ";&|\n" for c in token):
                break
            args.append(token)
    paths, only, include, all_files, amend = [], False, False, False, False
    values = {"-m", "-F", "-C", "-c", "--message", "--file", "--reuse-message",
              "--reedit-message", "--author", "--date", "--cleanup", "--trailer",
              "--fixup", "--squash", "--template", "-t"}
    flags = {"--no-edit", "--edit", "--quiet", "--verbose", "--signoff", "--no-verify",
             "--allow-empty", "--allow-empty-message", "--reset-author", "--no-post-rewrite",
             "--gpg-sign", "--no-gpg-sign", "--status", "--no-status"}
    i = 0
    while i < len(args):
        arg = args[i]
        i += 1
        if arg == "--":
            paths.extend(args[i:])
            break
        if arg == "<<":
            # A stdin heredoc belongs to shell input, not Git's pathspecs.
            # Its body was removed above; consume the remaining delimiter.
            if i == len(args):
                raise ValueError("missing heredoc delimiter")
            i += 1
            continue
        if arg in {"--help", "-h", "--dry-run"}:
            return {"mode": "inspection", "staged": "", "status": "", "diffs": {}}
        if arg == "--only":
            only = True
        elif arg == "--include":
            include = True
        elif arg == "--all":
            all_files = True
        elif arg == "--amend":
            amend = True
        elif arg in values:
            if i == len(args):
                raise ValueError(f"missing argument for {arg}")
            i += 1
        elif any(arg.startswith(value + "=") for value in values if value.startswith("--")):
            pass
        elif arg in flags or arg.startswith("--gpg-sign="):
            pass
        elif arg.startswith("-") and not arg.startswith("--"):
            # Short flag clusters such as -qam MESSAGE and -omMESSAGE.
            for offset, flag in enumerate(arg[1:], 1):
                if flag in "mFCctS":
                    if offset == len(arg) - 1:
                        if i == len(args):
                            raise ValueError(f"missing argument for -{flag}")
                        i += 1
                    break
                if flag == "o":
                    only = True
                elif flag == "i":
                    include = True
                elif flag == "a":
                    all_files = True
                elif flag not in "qvsen":
                    raise ValueError(f"unsupported commit option -{flag}")
        elif arg.startswith("-"):
            raise ValueError(f"unsupported commit option {arg}")
        else:
            paths.append(arg)
    if not paths and not only and not all_files and not include:
        return {"mode": "index"}
    if record and record.get("global_args"):
        raise ValueError("explicit selection with Git global options requires a command in the target directory")
    if only and (include or all_files):
        raise ValueError("conflicting commit selection options")
    if (include or all_files) and any(tokens[i:i + 2] == ["git", "add"] for i in range(len(tokens) - 1)):
        raise ValueError("stage changes separately before an --all or --include commit")
    if any(any(c in path for c in "$`\n") for path in paths):
        raise ValueError("dynamic commit paths cannot be resolved safely")
    selected_files = None
    if paths and not (all_files or include):
        # --only uses paths already known to the real index or HEAD. An
        # untracked child of a selected directory must not supply documentation
        # which the actual commit will omit.
        selected_files = sorted(set(git("ls-files", "-z", "--cached", "--with-tree=HEAD",
                                       "--", *paths).rstrip("\0").split("\0")) - {""})
        untracked = git("ls-files", "-z", "--others", "--", *paths)
        if untracked and any(tokens[i:i + 2] == ["git", "add"] for i in range(len(tokens) - 1)):
            raise ValueError("stage new selected files separately before an --only commit")
        if not selected_files:
            raise ValueError("no tracked files match the commit selection; stage new files first")

    ancestry = git("rev-list", "--parents", "-n", "1", "HEAD").split()
    base = ancestry[0]
    if amend:
        # Root commits have no parent. Other Git errors remain errors.
        base = ancestry[1] if len(ancestry) > 1 else git("hash-object", "-t", "tree", "--stdin").strip()
    with tempfile.TemporaryDirectory(prefix="commit-selection-") as temp:
        index = Path(temp) / "index"
        objects = Path(temp) / "objects"
        objects.mkdir()
        env = {**os.environ, "GIT_INDEX_FILE": str(index),
               "GIT_OBJECT_DIRECTORY": str(objects),
               "GIT_ALTERNATE_OBJECT_DIRECTORIES": str((Path(os.environ.get("COMMIT_FILE_CWD", os.getcwd())) / git("rev-parse", "--git-path", "objects").strip()).resolve())}
        if all_files or include:
            real_index = Path(os.environ.get("COMMIT_FILE_CWD", os.getcwd())) / git("rev-parse", "--git-path", "index").strip()
            if real_index.exists():
                # Git uses the index timestamp to detect racy cached stat
                # entries. A newer copy can hide same-size worktree edits.
                shutil.copy2(real_index, index)
            else:
                git("read-tree", "--empty", env=env)
        else:
            git("read-tree", "HEAD", env=env)
        # Use the same candidate index for attributes and staging: when an
        # attributes file is absent from the worktree, Git reads the index.
        match_args = ["ls-files", "-z", "--cached"]
        if not all_files:
            match_args.extend(["--others", "--exclude-standard"])
        if selected_files is not None:
            matched = "\0".join(selected_files) + "\0"
        else:
            matched = git(*match_args, "--", *paths, env=env) if paths or all_files else ""
        if matched:
            attributes = git("check-attr", "-z", "filter", "--stdin", env=env,
                             input_data=matched.encode()).split("\0")
            for offset in range(0, len(attributes) - 1, 3):
                if attributes[offset + 2] not in {"unspecified", "unset"}:
                    raise ValueError(f"candidate path uses a Git clean filter: {attributes[offset]}")
        if all_files:
            git("add", "--update", env=env)
        if paths:
            if selected_files is not None:
                # A forced addition already present in the real index remains
                # tracked even if HEAD's candidate index considers it ignored.
                git("add", "--all", "--force", "--",
                    *[":(literal)" + path for path in selected_files], env=env)
            else:
                git("add", "--all", "--", *paths, env=env)
        names = git("diff", "--cached", "--name-only", base, env=env).rstrip("\n")
        status = git("diff", "--cached", "--name-status", "-M", base, env=env).rstrip("\n")
        diffs = {name: git("diff", "--no-ext-diff", "--no-textconv", "--cached", "--unified=0", base, "--", name, env=env)
                 for name in names.splitlines() if name.endswith(".el")}
        # Keep manual bytes from this exact candidate before its private index
        # disappears. Reading the working tree later can inspect another version.
        manual_contents = {}
        for name in names.splitlines():
            if not (name == "README.org" or
                    (name.endswith(".org") and (name.startswith("doc/") or "/doc/" in name))):
                continue
            entries = git("ls-files", "--stage", "-z", "--", ":(literal)" + name, env=env).rstrip("\0")
            if not entries:
                manual_contents[name] = None  # Deleted in this candidate.
                continue
            metadata, actual = entries.split("\t", 1)
            mode, oid, stage = metadata.split()
            if actual != name or stage != "0" or "\0" in entries:
                raise ValueError("cannot resolve candidate manual contents")
            manual_contents[name] = git("cat-file", "blob", oid, env=env)
        return {"mode": "selection", "staged": names, "status": status, "diffs": diffs,
                "manual_contents": manual_contents,
                "paths": paths, "amend": amend, "only": not (all_files or include)}


if __name__ == "__main__":
    try:
        print(json.dumps(select(sys.stdin.read())))
    except subprocess.CalledProcessError as error:
        detail = error.stderr.decode(errors="replace").strip() if error.stderr else str(error)
        directory = os.environ.get("COMMIT_FILE_CWD", os.getcwd())
        print(json.dumps({"error": f"Cannot determine proposed commit files in {directory}: {detail}"}))
    except (ValueError, OSError) as error:
        print(json.dumps({"error": f"Cannot determine proposed commit files: {error}"}))
