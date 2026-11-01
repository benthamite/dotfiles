#!/usr/bin/env python3
"""List recent Forethought closeouts with their log entries and transcripts.

Reads ph-ps history only; writes nothing. Prints a JSON list, newest first, one
object per closeout commit:

  {"sha", "date", "subject", "log_files", "sessions": [
     {"id", "person", "exported_md", "exported_jsonl", "raw_jsonl"}]}

A closeout commit is one whose subject contains "close out". Its sessions are
the "session <id>" headings that the commit added to projects/*/logs/*.md.
Transcript paths are null when the file does not exist on this machine.
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import re
import subprocess
import sys
from pathlib import Path

HEADING = re.compile(r"^\+## .*· session ([0-9a-f-]{8,})\s*$")
LOG_PATH = re.compile(r"^projects/[^/]+/logs/\d{4}-\d{2}-\d{2}--([a-z]+)\.md$")


def git(repo: Path, *args: str) -> str:
    return subprocess.run(["git", "-C", str(repo), *args], check=True,
                          capture_output=True, text=True).stdout


def closeout_commits(repo: Path, since: str) -> list[dict]:
    out = git(repo, "log", "--regexp-ignore-case", "--grep=close out",
              f"--since={since}", "--format=%x1e%H%x1f%cI%x1f%s", "--name-only")
    commits = []
    for record in out.split("\x1e")[1:]:
        header, _, names = record.partition("\n")
        sha, date, subject = header.split("\x1f")
        if "close out" not in subject.lower():
            continue  # The grep also matches bodies; only subjects count.
        logs = [n for n in names.split("\n") if LOG_PATH.match(n)]
        commits.append({"sha": sha[:8], "date": date, "subject": subject,
                        "log_files": logs})
    return commits


def sessions_added(repo: Path, sha: str, log_file: str) -> list[str]:
    diff = git(repo, "show", "--format=", "--unified=0", sha, "--", log_file)
    return [m.group(1) for line in diff.splitlines() if (m := HEADING.match(line))]


def find_transcripts(session: str, person: str, exports: Path,
                     home: Path) -> dict:
    def first(pattern: str) -> str | None:
        hits = sorted(glob.glob(pattern, recursive=True))
        return hits[0] if hits else None

    base = exports / person
    return {
        "exported_md": first(str(base / "*" / f"*--{session}.md")),
        "exported_jsonl": first(str(base / "*" / f"*--{session}.jsonl")),
        "raw_jsonl": (first(str(home / ".claude*" / "projects" / "*" / f"{session}.jsonl"))
                      or first(str(home / ".codex*" / "sessions" / "**"
                                   / f"*{session}*.jsonl"))),
    }


def sample(repo: Path, since: str, limit: int, exports: Path, home: Path) -> list[dict]:
    result = []
    for commit in closeout_commits(repo, since):
        sessions = []
        for log_file in commit["log_files"]:
            person = LOG_PATH.match(log_file).group(1)
            for session in sessions_added(repo, commit["sha"], log_file):
                if any(s["id"] == session for s in sessions):
                    continue
                sessions.append({"id": session, "person": person,
                                 **find_transcripts(session, person, exports, home)})
        result.append({**commit, "sessions": sessions})
        if len(result) >= limit:
            break
    return result


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--repo", default="~/repos/forethought/ph-ps")
    parser.add_argument("--since", required=True, help="git date, e.g. 2026-10-01")
    parser.add_argument("--limit", type=int, default=15)
    parser.add_argument("--exports", default="~/.local/share/ph-ps--local-agent-sessions")
    args = parser.parse_args(argv)
    try:
        rows = sample(Path(os.path.expanduser(args.repo)), args.since, args.limit,
                      Path(os.path.expanduser(args.exports)), Path.home())
    except subprocess.CalledProcessError as error:
        print(json.dumps({"error": error.stderr.strip()}))
        return 2
    print(json.dumps(rows, indent=2))
    return 0


if __name__ == "__main__":
    sys.exit(main())
