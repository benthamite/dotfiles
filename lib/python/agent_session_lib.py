"""Single-actor Emacs agent.el session primitives shared by agent skills.

Extracted from orchestrate-review's helper so that other skills
(currently request-review) can reuse guarded prompt delivery,
transcript-boundary acknowledgment, completion-marker transcript reads, and
buffer lifecycle inspection without duplicating them. Everything here is
single-actor and knows nothing about any skill's run-file state machine.

Consumers load this module by resolved file path (the live skill directories
are symlinks into dotfiles) and call every function through the module object
(``session.<name>``), giving tests exactly one patch point per primitive.
"""

from __future__ import annotations

import ast
import json
import os
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any

VALID_BACKENDS = {"claude-code", "codex"}
DELIVERY_INITIAL_WAIT_SECONDS = 2.0
DELIVERY_RETRY_WAIT_SECONDS = 8.0
DELIVERY_POLL_SECONDS = 0.1


class EmacsClientError(RuntimeError):
    """An emacsclient request failed while the Emacs server may be transiently unavailable."""


def _write_all(fd: int, data: bytes) -> None:
    offset = 0
    while offset < len(data):
        offset += os.write(fd, data[offset:])


def run_emacs_eval(expr: str) -> str:
    proc = subprocess.run(
        ["emacsclient", "--eval", expr],
        capture_output=True,
        check=False,
    )
    stdout = proc.stdout.decode("utf-8", "replace")
    stderr = proc.stderr.decode("utf-8", "replace")
    if proc.returncode != 0:
        raise EmacsClientError(
            f"emacsclient failed ({proc.returncode}): {stderr.strip()}"
        )
    value = stdout.strip()
    if value.startswith('"') and value.endswith('"'):
        try:
            return ast.literal_eval(value)
        except (SyntaxError, ValueError):
            return value
    return value


def elisp_string(value: str) -> str:
    return json.dumps(value)


def run_emacs_json(value_expr: str) -> Any:
    """Evaluate VALUE_EXPR in Emacs and transfer JSON through a temp file.

    `emacsclient --eval' always prints the evaluated form's return value.  To
    keep structured status out of user-visible command output, Emacs writes the
    JSON payload to a one-shot temp file and returns nil.
    """
    fd, path = tempfile.mkstemp(prefix="agent-orch-", suffix=".json")
    os.fchmod(fd, 0o600)
    os.close(fd)
    output_path = Path(path)
    expr = f'''
(let ((out {elisp_string(path)}))
  (require 'json)
  (with-temp-file out
    (insert (json-encode {value_expr})))
  nil)
'''
    try:
        returned = run_emacs_eval(expr)
        if returned != "nil":
            raise SystemExit(f"unexpected emacsclient return value: {returned!r}")
        return json.loads(output_path.read_text(encoding="utf-8"))
    finally:
        output_path.unlink(missing_ok=True)


def json_for_display(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, indent=2)


def buffer_state(buffer: str) -> dict[str, Any]:
    value_expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let ((display-state
         (when (fboundp 'agent-session-display-state)
           (agent-session-display-state (current-buffer)))))
    `((buffer . ,(buffer-name))
      (state . ,(pcase display-state
                   ((or 'waiting 'background-waiting) "awaiting-input")
                   ('busy "busy")
                   (_ (if (boundp 'agent--session-state)
                          (format "%s" agent--session-state)
                        "unknown"))))
      (directory . ,(or default-directory "")))))
'''
    return run_emacs_json(value_expr)


def _submit_function(backend: str) -> str:
    if backend not in VALID_BACKENDS:
        raise SystemExit("--backend must be claude-code or codex")
    return "agent-submit"


def send_return_to_agent(buffer: str, backend: str) -> None:
    if backend not in VALID_BACKENDS:
        raise SystemExit("--backend must be claude-code or codex")
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let ((target (agent-send-return (get-buffer {elisp_string(buffer)}))))
    (unless (buffer-live-p target)
      (error "agent return dispatch did not resolve a live buffer"))
    (princ "submitted")))
'''
    returned = run_emacs_eval(expr)
    if returned != "submitted":
        raise SystemExit(f"unexpected return-submit result: {returned!r}")


def buffer_backend(buffer: str) -> str | None:
    """Return BUFFER's detected backend symbol, or None when undetected."""
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (princ (format "%s" (or (agent--detect-backend (current-buffer)) "none"))))
'''
    returned = run_emacs_eval(expr)
    return None if returned == "none" else returned


def agent1_process_live(buffer: str) -> bool:
    """Return whether BUFFER still owns a live Claude terminal process."""
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let ((process (get-buffer-process (current-buffer))))
    (princ (if (and process (process-live-p process)) "live" "dead"))))
'''
    returned = run_emacs_eval(expr)
    if returned not in {"live", "dead"}:
        raise SystemExit(f"unexpected Claude process state: {returned!r}")
    return returned == "live"


def reconcile_agent1_waiting(buffer: str) -> None:
    """Restore a reset Claude lifecycle state from a verified blocked stop."""
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (agent-session-event (current-buffer) 'blocked)
  (princ (format "%s" (agent-session-display-state (current-buffer)))))
'''
    returned = run_emacs_eval(expr)
    if returned not in {"waiting", "background-waiting"}:
        raise EmacsClientError(
            f"Claude waiting-state reconciliation reported {returned!r}"
        )


def claude_session_initialized(buffer: str, transcript: str) -> bool:
    """Return whether BUFFER's live Claude status names TRANSCRIPT.

    A fresh Claude terminal has no lifecycle event yet, so agent.el reports
    ``unknown`` even after the CLI has initialized its idle composer.  The
    per-process status file supplies the session identity and transcript path
    needed to distinguish that state from an uninitialized terminal.
    """
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let* ((status (and (fboundp 'agent-claude--parse-status-file)
                      (agent-claude--parse-status-file)))
         (session-id (plist-get status :session_id))
         (status-transcript (plist-get status :transcript_path))
         (expected (expand-file-name {elisp_string(transcript)})))
    (princ
     (if (and (stringp session-id)
              (not (string-empty-p session-id))
              (stringp status-transcript)
              (string= (expand-file-name status-transcript) expected))
         "initialized"
       "uninitialized"))))
'''
    returned = run_emacs_eval(expr)
    if returned not in {"initialized", "uninitialized"}:
        raise SystemExit(f"unexpected Claude initialization state: {returned!r}")
    return returned == "initialized"


def pending_prompt_contains(buffer: str, backend: str, marker: str) -> bool:
    """Return whether BUFFER's last visible composer contains MARKER.

    Only the boolean result crosses the Emacs boundary; prompt text stays in
    the fixed top-level session buffer.
    """
    if backend == "codex":
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let ((input (codex-prompt-input (current-buffer))))
    (if (and (stringp input)
             (string-match-p (regexp-quote {elisp_string(marker)}) input))
        (princ "present")
      (princ "absent"))))
'''
    else:
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (save-excursion
    (goto-char (point-max))
    (if (and (re-search-backward "^[❯>$][[:space:]]" nil t)
             (search-forward {elisp_string(marker)} nil t))
        (princ "present")
      (princ "absent"))))
'''
    returned = run_emacs_eval(expr)
    if returned not in {"present", "absent"}:
        raise SystemExit(f"unexpected pending-prompt result: {returned!r}")
    return returned == "present"


def agent_transcript_path(buffer: str, backend: str) -> str | None:
    """Return BUFFER's current transcript path without enumerating sessions."""
    if backend == "claude-code":
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let* ((status (and (fboundp 'agent-claude--parse-status-file)
                      (agent-claude--parse-status-file)))
         (file (plist-get status :transcript_path)))
    (if file
        (princ (expand-file-name file))
      (princ "none"))))
'''
    elif backend == "codex":
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let* ((identity (codex-session-identity (current-buffer)))
         (session-id (plist-get identity :session-id))
         (file (or (and (boundp 'codex--session-transcript-file)
                        codex--session-transcript-file)
                   (and session-id (codex--find-session-transcript session-id)))))
    (if file
        (princ (expand-file-name file))
      (princ "none"))))
'''
    else:
        raise SystemExit("fresh phase restart requires claude-code or codex")
    returned = run_emacs_eval(expr)
    return None if returned == "none" else returned


def _user_marker_offset(path: Path | str, marker: str) -> int | None:
    transcript = Path(path)
    try:
        stream = transcript.open("rb")
    except FileNotFoundError:
        return None
    except OSError as error:
        raise EmacsClientError(f"cannot inspect fresh transcript {path}: {error}") from None
    with stream:
        offset = 0
        for line in stream:
            try:
                obj = json.loads(line.decode("utf-8", "replace"))
            except json.JSONDecodeError:
                offset += len(line)
                continue
            payload = obj.get("payload") or {}
            if (
                obj.get("type") == "response_item"
                and payload.get("type") == "message"
                and payload.get("role") == "user"
            ):
                text = "\n".join(
                    item.get("text", "")
                    for item in (payload.get("content") or [])
                    if isinstance(item, dict) and item.get("type") == "input_text"
                )
                if marker in text:
                    return offset
            offset += len(line)
    return None


def _wait_for_transcript_path(buffer: str, backend: str, timeout: float) -> str | None:
    deadline = time.monotonic() + timeout
    while True:
        path = agent_transcript_path(buffer, backend)
        if path:
            return path
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            return None
        time.sleep(min(DELIVERY_POLL_SECONDS, remaining))


def _transcript_advanced(transcript: str, offset: int) -> bool:
    try:
        return Path(transcript).stat().st_size > offset
    except FileNotFoundError:
        return False
    except OSError as error:
        raise EmacsClientError(
            f"cannot inspect transcript delivery boundary {transcript}: {error}"
        ) from None


def _delivery_observed(
    buffer: str, transcript: str, offset: int, *, accept_busy: bool = True
) -> bool:
    if _transcript_advanced(transcript, offset):
        return True
    return accept_busy and buffer_state(buffer).get("state") == "busy"


def _wait_for_delivery(
    buffer: str,
    transcript: str,
    offset: int,
    timeout: float,
    *,
    accept_busy: bool = True,
) -> bool:
    deadline = time.monotonic() + timeout
    while True:
        if _delivery_observed(
            buffer, transcript, offset, accept_busy=accept_busy
        ):
            return True
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            return False
        time.sleep(min(DELIVERY_POLL_SECONDS, remaining))


def submit_to_agent(
    buffer: str,
    backend: str,
    prompt: str,
    *,
    transcript: str,
    transcript_offset: int,
    delivery_marker: str,
    one_pass: bool = False,
) -> None:
    fn = _submit_function(backend)
    fd, temporary = tempfile.mkstemp(prefix="agent-orch-prompt-", suffix=".txt")
    prompt_path = Path(temporary)
    try:
        os.fchmod(fd, 0o600)
        _write_all(fd, prompt.encode("utf-8"))
        os.close(fd)
        fd = -1
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (with-temp-buffer
    (insert-file-contents {elisp_string(str(prompt_path))})
    (let ((target
           ({fn}
            (buffer-string)
            (get-buffer {elisp_string(buffer)}))))
      (unless (buffer-live-p target)
        (error "agent submit dispatch did not resolve a live buffer"))
      (princ "submitted"))))
'''
        returned = run_emacs_eval(expr)
        if returned != "submitted":
            raise SystemExit(f"unexpected submit result: {returned!r}")
    finally:
        if fd >= 0:
            os.close(fd)
        prompt_path.unlink(missing_ok=True)

    if _wait_for_delivery(
        buffer,
        transcript,
        transcript_offset,
        DELIVERY_INITIAL_WAIT_SECONDS,
        accept_busy=not one_pass,
    ):
        return
    if one_pass:
        raise EmacsClientError(
            "implementation delivery was not independently acknowledged; "
            "the one-pass run remains pending and no Return retry was sent"
        )
    if not pending_prompt_contains(buffer, backend, delivery_marker):
        raise EmacsClientError(
            "submission returned without delivery acknowledgement and the exact "
            "pending prompt could not be proved in the composer"
        )
    send_return_to_agent(buffer, backend)
    if not _wait_for_delivery(
        buffer,
        transcript,
        transcript_offset,
        DELIVERY_RETRY_WAIT_SECONDS,
    ):
        raise EmacsClientError(
            "submission delivery was not acknowledged after retrying only the "
            "submit keystroke"
        )


def _transcript_offset(state: dict[str, Any], actor: str) -> int:
    path = Path(state[actor]["transcript"])
    try:
        return path.stat().st_size
    except FileNotFoundError:
        return 0
    except OSError as error:
        raise SystemExit(f"cannot inspect transcript {path}: {error}") from None


def _bootstrap_fresh_claude_waiting(
    state: dict[str, Any], actor_name: str, live: dict[str, Any]
) -> dict[str, Any]:
    """Reconcile an initialized, untouched Claude session before first use.

    Do not generalize an ``unknown`` lifecycle state to waiting.  Bootstrap
    only a Claude process whose configured transcript has no history, whose
    terminal process is live, and whose per-process status file names that
    exact transcript.
    """
    actor = state[actor_name]
    if (
        live.get("state") == "unknown"
        and actor["backend"] == "claude-code"
        and _transcript_offset(state, actor_name) == 0
        and agent1_process_live(actor["buffer"])
        and claude_session_initialized(actor["buffer"], actor["transcript"])
    ):
        reconcile_agent1_waiting(actor["buffer"])
        return buffer_state(actor["buffer"])
    return live


def transcript_messages(
    path: Path, since: str | None = None, offset: int = 0
) -> list[dict[str, str]]:
    if not path.exists():
        return []
    try:
        size = path.stat().st_size
        if offset > size:
            return []
        with path.open("rb") as stream:
            stream.seek(offset)
            content = stream.read().decode("utf-8", "replace")
    except OSError:
        return []
    out: list[dict[str, str]] = []
    for line in content.splitlines():
        if not line.strip():
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
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
            if message.get("role") != "assistant":
                continue
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
