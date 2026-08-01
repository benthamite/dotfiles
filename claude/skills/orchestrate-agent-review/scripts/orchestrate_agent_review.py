#!/usr/bin/env python3
"""Helpers for supervising live Emacs agent.el planner/reviewer loops."""

from __future__ import annotations

import argparse
import json
import ast
import subprocess
import time
from pathlib import Path
from typing import Any


def run_emacs_eval(expr: str) -> str:
    wrapped_expr = f"""
(let ((inhibit-message t)
      (message-log-max nil))
  {expr})
"""
    proc = subprocess.run(
        ["emacsclient", "--eval", wrapped_expr],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    stdout = proc.stdout.decode("utf-8", "replace")
    stderr = proc.stderr.decode("utf-8", "replace")
    if proc.returncode != 0:
        raise SystemExit(f"emacsclient failed ({proc.returncode}): {stderr.strip()}")
    value = stdout.strip()
    if value.startswith('"') and value.endswith('"'):
        try:
            return ast.literal_eval(value)
        except Exception:
            return value
    return value


def elisp_string(value: str) -> str:
    return json.dumps(value)


def without_tails(value: Any) -> Any:
    """Return VALUE with base64 buffer tails removed from nested structures."""
    if isinstance(value, dict):
        return {k: without_tails(v) for k, v in value.items() if k != "tail_b64"}
    if isinstance(value, list):
        return [without_tails(item) for item in value]
    return value


def json_for_display(value: Any, include_tail: bool = False) -> str:
    if not include_tail:
        value = without_tails(value)
    return json.dumps(value, ensure_ascii=False, indent=2)


def buffers(args: argparse.Namespace) -> None:
    expr = r'''
(progn
  (require 'json)
  (let (items)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (let ((name (buffer-name b)))
          (when (string-match-p "^\\*\\(claude\\|codex\\):" name)
            (push `((buffer . ,name)
                    (state . ,(if (boundp 'agent--session-state)
                                  (format "%s" agent--session-state)
                                "unknown"))
                    (directory . ,(or default-directory ""))
                    (tail_b64 . ,(base64-encode-string
                                  (encode-coding-string
                                   (buffer-substring-no-properties
                                    (max (point-min) (- (point-max) 1200))
                                    (point-max))
                                   'utf-8)
                                  t)))
                  items)))))
    (princ (json-encode (nreverse items)))))
'''
    items = json.loads(run_emacs_eval(expr))
    if args.json:
        print(json_for_display(items, args.include_tail))
        return
    for item in items:
        print(f"{item['state']:15} {item['buffer']} [{item['directory']}]")


def buffer_state(buffer: str) -> dict[str, Any]:
    expr = f'''
(progn
  (require 'json)
  (with-current-buffer {elisp_string(buffer)}
    (princ
     (json-encode
      `((buffer . ,(buffer-name))
        (state . ,(if (boundp 'agent--session-state)
                      (format "%s" agent--session-state)
                    "unknown"))
        (directory . ,(or default-directory ""))
        (tail_b64 . ,(base64-encode-string
                      (encode-coding-string
                       (buffer-substring-no-properties
                        (max (point-min) (- (point-max) 1200))
                        (point-max))
                       'utf-8)
                      t)))))))
'''
    return json.loads(run_emacs_eval(expr))


def state_cmd(args: argparse.Namespace) -> None:
    state = buffer_state(args.buffer)
    if args.json:
        print(json_for_display(state, args.include_tail))
    else:
        print(f"{state['state']:15} {state['buffer']} [{state['directory']}]")


def submit(args: argparse.Namespace) -> None:
    prompt_path = Path(args.prompt_file)
    if not prompt_path.exists():
        raise SystemExit(f"prompt file does not exist: {prompt_path}")
    fn = {
        "claude": "agent-claude-submit-command",
        "claude-code": "agent-claude-submit-command",
        "codex": "agent-codex-submit-command",
    }.get(args.backend)
    if fn is None:
        raise SystemExit("--backend must be claude, claude-code, or codex")
    expr = f'''
(with-current-buffer {elisp_string(args.buffer)}
  (with-temp-buffer
    (insert-file-contents {elisp_string(str(prompt_path))})
    ({fn}
     (buffer-string)
     (get-buffer {elisp_string(args.buffer)}))))
'''
    print(run_emacs_eval(expr))


def transcript_messages(path: Path, since: str | None = None) -> list[dict[str, str]]:
    if not path.exists():
        return []
    out: list[dict[str, str]] = []
    for line in path.read_text(errors="replace").splitlines():
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
        except Exception:
            continue
        ts = str(obj.get("timestamp", ""))
        if since and ts < since:
            continue
        payload = obj.get("payload") or {}
        text = ""
        kind = ""
        if obj.get("type") == "event_msg" and payload.get("type") == "task_complete":
            kind = "complete"
            text = payload.get("last_agent_message") or ""
        elif obj.get("type") == "response_item" and payload.get("type") == "message":
            parts = [
                c.get("text", "")
                for c in (payload.get("content") or [])
                if isinstance(c, dict) and c.get("type") == "output_text"
            ]
            if parts:
                kind = "message"
                text = "\n".join(parts)
        else:
            message = obj.get("message") or {}
            content = message.get("content")
            if isinstance(content, list):
                parts = []
                for item in content:
                    if isinstance(item, dict) and item.get("type") == "text":
                        parts.append(item.get("text", ""))
                if parts:
                    kind = message.get("role") or "message"
                    text = "\n".join(parts)
            elif isinstance(content, str):
                kind = message.get("role") or "message"
                text = content
        if text:
            out.append({"timestamp": ts, "kind": kind, "text": text})
    return out


def transcript_cmd(args: argparse.Namespace) -> None:
    messages = transcript_messages(Path(args.path), args.since)
    if args.last:
        messages = messages[-args.last :]
    print(json.dumps(messages, ensure_ascii=False, indent=2))


def git_status(repo: Path) -> dict[str, str]:
    def git(*argv: str) -> str:
        proc = subprocess.run(
            ["git", "-C", str(repo), *argv],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if proc.returncode != 0:
            return proc.stderr.strip()
        return proc.stdout.strip()

    return {
        "status": git("status", "--short", "--branch"),
        "head": git("log", "--oneline", "-1"),
    }


def status(args: argparse.Namespace) -> dict[str, Any]:
    result: dict[str, Any] = {"repo": git_status(Path(args.repo))}
    if args.planner_buffer:
        result["planner"] = buffer_state(args.planner_buffer)
    if args.reviewer_buffer:
        result["reviewer"] = buffer_state(args.reviewer_buffer)
    if args.planner_transcript:
        p = Path(args.planner_transcript)
        result["planner_transcript"] = {
            "path": str(p),
            "mtime": p.stat().st_mtime if p.exists() else None,
            "latest": transcript_messages(p, args.since)[-1:] if p.exists() else [],
        }
    if args.reviewer_transcript:
        p = Path(args.reviewer_transcript)
        result["reviewer_transcript"] = {
            "path": str(p),
            "mtime": p.stat().st_mtime if p.exists() else None,
            "latest": transcript_messages(p, args.since)[-1:] if p.exists() else [],
        }
    return result


def status_cmd(args: argparse.Namespace) -> None:
    current = status(args)
    if args.json:
        print(json_for_display(current, args.include_tail))
        return
    print("Repo:")
    print(current["repo"]["status"])
    print(f"HEAD {current['repo']['head']}")
    for role in ("planner", "reviewer"):
        if role in current:
            item = current[role]
            print(f"{role.title()}: {item['state']} — {item['buffer']}")
    for key in ("planner_transcript", "reviewer_transcript"):
        if key in current:
            item = current[key]
            latest = item["latest"][0] if item["latest"] else None
            if latest:
                text = latest["text"].replace("\n", " ")
                if len(text) > 180:
                    text = text[:177] + "..."
                print(f"{key}: mtime={item['mtime']} latest={latest['kind']} {text}")
            else:
                print(f"{key}: mtime={item['mtime']} latest=<none>")


def watch(args: argparse.Namespace) -> None:
    last_rendered = ""
    while True:
        current = status(args)
        if args.json:
            rendered = json_for_display(current, args.include_tail)
        else:
            parts = [current["repo"]["head"]]
            for role in ("planner", "reviewer"):
                if role in current:
                    parts.append(f"{role}={current[role]['state']}")
            for key in ("planner_transcript", "reviewer_transcript"):
                if key in current:
                    latest = current[key]["latest"][0] if current[key]["latest"] else None
                    if latest:
                        verdict = ""
                        text = latest["text"]
                        if "IMPLEMENTATION-READY" in text:
                            verdict = " IMPLEMENTATION-READY"
                        elif "NOT READY" in text:
                            verdict = " NOT READY"
                        parts.append(f"{key}_mtime={current[key]['mtime']}{verdict}")
            rendered = " | ".join(parts)
        if rendered != last_rendered:
            print(rendered, flush=True)
            last_rendered = rendered
        time.sleep(args.interval)


def add_status_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--repo", required=True)
    parser.add_argument("--planner-buffer")
    parser.add_argument("--reviewer-buffer")
    parser.add_argument("--planner-transcript")
    parser.add_argument("--reviewer-transcript")
    parser.add_argument("--since")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("buffers", help="List live Emacs agent buffers")
    p.add_argument("--json", action="store_true", help="Emit structured JSON without buffer tails")
    p.add_argument("--include-tail", action="store_true", help="Include base64 buffer tails in JSON output")
    p.set_defaults(func=buffers)

    p = sub.add_parser("state", help="Inspect one live Emacs agent buffer")
    p.add_argument("--buffer", required=True)
    p.add_argument("--json", action="store_true", help="Emit structured JSON without buffer tail")
    p.add_argument("--include-tail", action="store_true", help="Include base64 buffer tail in JSON output")
    p.set_defaults(func=state_cmd)

    p = sub.add_parser("submit", help="Submit a prompt file to a live agent buffer")
    p.add_argument("--buffer", required=True)
    p.add_argument("--backend", required=True, choices=["claude", "claude-code", "codex"])
    p.add_argument("--prompt-file", required=True)
    p.set_defaults(func=submit)

    p = sub.add_parser("transcript", help="Extract assistant messages from a JSONL transcript")
    p.add_argument("--path", required=True)
    p.add_argument("--since")
    p.add_argument("--last", type=int, default=5)
    p.set_defaults(func=transcript_cmd)

    p = sub.add_parser("status", help="Collect repo, buffer, and transcript status once")
    add_status_args(p)
    p.add_argument("--json", action="store_true", help="Emit structured JSON without buffer tails")
    p.add_argument("--include-tail", action="store_true", help="Include base64 buffer tails in JSON output")
    p.set_defaults(func=status_cmd)

    p = sub.add_parser("watch", help="Poll repo, buffer, and transcript status")
    add_status_args(p)
    p.add_argument("--interval", type=float, default=20.0)
    p.add_argument("--json", action="store_true", help="Emit structured JSON without buffer tails on each changed poll")
    p.add_argument("--include-tail", action="store_true", help="Include base64 buffer tails in JSON output")
    p.set_defaults(func=watch)

    args = parser.parse_args(argv)
    args.func(args)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
