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
import hashlib
import json
import os
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any

VALID_BACKENDS = {"claude-code", "codex"}
# A busy actor queues the prompt; Claude Code records the queued user message
# in the transcript within seconds, so the marker check covers that case too.
DELIVERY_INITIAL_WAIT_SECONDS = 20.0
DELIVERY_RETRY_WAIT_SECONDS = 8.0
DELIVERY_POLL_SECONDS = 0.1


class EmacsClientError(RuntimeError):
    """An emacsclient request failed while the Emacs server may be transiently unavailable."""


def _write_all(fd: int, data: bytes) -> None:
    offset = 0
    while offset < len(data):
        offset += os.write(fd, data[offset:])


def run_emacs_eval(expr: str) -> str:
    try:
        proc = subprocess.run(
            ["emacsclient", "--eval", expr],
            capture_output=True,
            check=False,
            timeout=15,
        )
    except subprocess.TimeoutExpired:
        # Only the client is stopped. Emacs may still evaluate the request,
        # so callers must preserve pending delivery for reconciliation.
        raise EmacsClientError(
            "emacsclient timed out after 15 seconds; evaluation outcome is unknown"
        ) from None
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


def _actor_identity_expr() -> str:
    """Sample identity and lifecycle together in the selected Emacs buffer."""
    return '''
(let* ((backend (agent--detect-backend (current-buffer)))
       (identity (pcase backend
                   ('claude-code (agent-claude--parse-status-file))
                   ('codex (codex-session-identity (current-buffer)))))
       (session-id (plist-get identity (if (eq backend 'codex)
                                          :session-id :session_id)))
       (file (pcase backend
               ('claude-code (plist-get identity :transcript_path))
               ('codex (or (and (boundp 'codex--session-transcript-file)
                                codex--session-transcript-file)
                           (and session-id
                                (codex--find-session-transcript session-id))))))
       (display-state (when (fboundp 'agent-session-display-state)
                        (agent-session-display-state (current-buffer)))))
  `((buffer . ,(buffer-name))
    (backend . ,(and backend (symbol-name backend)))
    (directory . ,(directory-file-name (file-truename default-directory)))
    (transcript . ,(and file (file-truename (expand-file-name file))))
    (session_id . ,session-id)
    (state . ,(pcase display-state
                ((or 'waiting 'background-waiting) "awaiting-input")
                ('busy "busy")
                (_ (if (boundp 'agent--session-state)
                       (format "%s" agent--session-state) "unknown"))))))
'''


def actor_identity(buffer: str) -> dict[str, Any]:
    """Return the selected actor's canonical identity and state in one sample."""
    identity = run_emacs_json(
        f"(with-current-buffer {elisp_string(buffer)} {_actor_identity_expr()})"
    )
    if not isinstance(identity, dict) or identity.get("buffer") != buffer:
        raise EmacsClientError("actor identity did not resolve the selected buffer")
    if identity.get("backend") not in VALID_BACKENDS:
        raise EmacsClientError("actor identity has no supported backend")
    for key in ("directory", "transcript"):
        value = identity.get(key)
        if value is not None:
            if not isinstance(value, str) or not value:
                raise EmacsClientError("actor identity has an invalid path")
            identity[key] = str(Path(value).resolve())
    return identity


def _identity_guard(buffer: str, backend: str,
                    expected: dict[str, Any] | None) -> str:
    """Build a same-evaluation guard, before any dispatch or buffer switch."""
    if expected is None:
        return ""
    if expected.get("buffer") != buffer or expected.get("backend") != backend:
        raise EmacsClientError("expected actor identity does not match dispatch target")
    comparisons = []
    for key in ("buffer", "backend", "directory", "session_id", "transcript", "state"):
        # A new session can gain its first transcript, but not change identity.
        if key == "transcript" and expected.get(key) is None:
            continue
        value = expected.get(key)
        if value is not None and not isinstance(value, str):
            raise EmacsClientError("invalid expected actor identity")
        literal = "nil" if value is None else elisp_string(value)
        comparisons.append(f"(equal (alist-get '{key} live) {literal})")
    return f'''
  (let ((live {_actor_identity_expr()}))
    (unless (and {' '.join(comparisons)})
      (error "actor identity or lifecycle changed before dispatch")))
'''


def _same_actor(expected: dict[str, Any], actual: dict[str, Any]) -> bool:
    return all(
        actual.get(key) == expected.get(key)
        for key in ("buffer", "backend", "directory", "session_id", "transcript")
        if key != "transcript" or expected.get(key) is not None
    )


def send_return_to_agent(
    buffer: str, backend: str, *,
    expected_identity: dict[str, Any] | None = None,
    expected_prompt_sha256: str | None = None,
) -> None:
    if backend not in VALID_BACKENDS:
        raise SystemExit("--backend must be claude-code or codex")
    prompt_guard = ""
    if expected_prompt_sha256 is not None:
        if backend != "codex":
            raise EmacsClientError("this backend cannot prove an exact pending composer")
        prompt_guard = f'''
  (let ((input (codex-prompt-input (current-buffer))))
    (unless (and (stringp input)
                 (equal (secure-hash 'sha256 (encode-coding-string input 'utf-8-unix))
                        {elisp_string(expected_prompt_sha256)}))
      (error "pending composer changed before Return")))
'''
    expr = f'''
(with-current-buffer {elisp_string(buffer)}
  {_identity_guard(buffer, backend, expected_identity)}
  {prompt_guard}
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


def pending_prompt_contains(
    buffer: str, backend: str, marker: str, *,
    expected_prompt_sha256: str | None = None,
) -> bool:
    """Return whether BUFFER's last visible composer contains MARKER.

    Only the boolean result crosses the Emacs boundary; prompt text stays in
    the fixed top-level session buffer.
    """
    if backend not in VALID_BACKENDS:
        raise SystemExit("--backend must be claude-code or codex")
    if backend == "claude-code" and expected_prompt_sha256 is not None:
        # Terminal rendering is not an exact composer API. The installed
        # agent-claude predicate matches only a prefix; it cannot prove a hash.
        return False
    if backend == "codex":
        exact = (f"(equal (secure-hash 'sha256 (encode-coding-string input 'utf-8-unix)) "
                 f"{elisp_string(expected_prompt_sha256)})"
                 if expected_prompt_sha256 is not None else "t")
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  (let ((input (codex-prompt-input (current-buffer))))
    (if (and (stringp input)
             {exact}
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


def _user_message_text(obj: dict[str, Any]) -> str | None:
    """Return the text of OBJ when it is a user message, else None.

    Handles the Codex app-server shape (``response_item`` / ``message`` /
    ``role: user`` with ``input_text`` parts) and the Claude Code shape
    (``type: user`` with a string or ``text``-part list under ``message``).
    """
    if not isinstance(obj, dict):
        return None
    payload = obj.get("payload")
    payload = payload if isinstance(payload, dict) else {}
    if (
        obj.get("type") == "response_item"
        and payload.get("type") == "message"
        and payload.get("role") == "user"
    ):
        content = payload.get("content")
        return _text_parts(content, "input_text")
    message = obj.get("message")
    message = message if isinstance(message, dict) else {}
    if obj.get("type") == "user" and message.get("role") == "user":
        content = message.get("content")
        if isinstance(content, str):
            return content
        if isinstance(content, list):
            return _text_parts(content, "text")
    return None


def _text_parts(content: Any, kind: str) -> str | None:
    if not isinstance(content, list):
        return None
    parts = [item["text"] for item in content
             if isinstance(item, dict) and item.get("type") == kind
             and isinstance(item.get("text"), str)]
    return "\n".join(parts) if parts else None


def _prompt_matches(text: str | None, marker: str,
                    expected_prompt_sha256: str | None) -> bool:
    return (text is not None and marker in text
            and (expected_prompt_sha256 is None
                 or hashlib.sha256(text.encode("utf-8")).hexdigest()
                 == expected_prompt_sha256))


def _user_marker_offset(
    path: Path | str, marker: str, *,
    expected_prompt_sha256: str | None = None,
) -> int | None:
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
            text = _user_message_text(obj)
            if _prompt_matches(text, marker, expected_prompt_sha256):
                return offset
            offset += len(line)
    return None


def _marker_delivered(
    transcript: str, offset: int, marker: str, *,
    expected_prompt_sha256: str | None = None,
) -> bool:
    """Return whether a user message containing MARKER was appended past OFFSET.

    This is the only acknowledgement of a delivery: the delivered text itself,
    recorded in the actor's own transcript after the boundary captured before
    the submit.  Transcript growth from bookkeeping records and a busy
    terminal (a session still starting up, compacting, or mid-turn) are not
    evidence that the prompt was received.
    """
    path = Path(transcript)
    try:
        if path.stat().st_size <= offset:
            return False
        with path.open("rb") as stream:
            stream.seek(offset)
            data = stream.read()
    except FileNotFoundError:
        return False
    except OSError as error:
        raise EmacsClientError(
            f"cannot inspect transcript delivery boundary {transcript}: {error}"
        ) from None
    for line in data.split(b"\n"):
        if not line.strip():
            continue
        try:
            obj = json.loads(line.decode("utf-8", "replace"))
        except json.JSONDecodeError:
            continue
        text = _user_message_text(obj)
        if _prompt_matches(text, marker, expected_prompt_sha256):
            return True
    return False


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


def _wait_for_delivery(
    transcript: str,
    offset: int,
    marker: str,
    timeout: float,
    *,
    expected_prompt_sha256: str | None = None,
) -> bool:
    deadline = time.monotonic() + timeout
    while True:
        if _marker_delivered(transcript, offset, marker,
                             expected_prompt_sha256=expected_prompt_sha256):
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
    transcript: str | None,
    transcript_offset: int,
    delivery_marker: str,
    one_pass: bool = False,
    expected_identity: dict[str, Any] | None = None,
) -> str:
    fn = _submit_function(backend)
    if transcript is None and (
        expected_identity is None or not expected_identity.get("session_id")
        or transcript_offset != 0
    ):
        raise EmacsClientError("a fresh transcript requires a stable session identity and zero boundary")
    if (expected_identity is not None and transcript is not None
            and expected_identity.get("transcript") is not None
            and str(Path(transcript).resolve()) != expected_identity["transcript"]):
        raise EmacsClientError("dispatch transcript does not match expected actor identity")
    prompt_sha256 = hashlib.sha256(prompt.encode("utf-8")).hexdigest()
    fd, temporary = tempfile.mkstemp(prefix="agent-orch-prompt-", suffix=".txt")
    prompt_path = Path(temporary)
    try:
        os.fchmod(fd, 0o600)
        _write_all(fd, prompt.encode("utf-8"))
        os.close(fd)
        fd = -1
        expr = f'''
(with-current-buffer {elisp_string(buffer)}
  {_identity_guard(buffer, backend, expected_identity)}
  (let ((agent-claude-submit-retries 0))
  (with-temp-buffer
    (insert-file-contents {elisp_string(str(prompt_path))})
    (let ((target
           ({fn}
            (buffer-string)
            (get-buffer {elisp_string(buffer)}))))
      (unless (buffer-live-p target)
        (error "agent submit dispatch did not resolve a live buffer"))
      (princ "submitted")))))
'''
        returned = run_emacs_eval(expr)
        if returned != "submitted":
            raise SystemExit(f"unexpected submit result: {returned!r}")
    finally:
        if fd >= 0:
            os.close(fd)
        prompt_path.unlink(missing_ok=True)

    # The first transcript may be created only after a fresh actor receives
    # its prompt. Never discover it by scanning other sessions or accepting
    # a buffer that has switched identity while the delivery was in flight.
    if expected_identity is not None:
        deadline = time.monotonic() + DELIVERY_INITIAL_WAIT_SECONDS
        while True:
            actual = actor_identity(buffer)
            if not _same_actor(expected_identity, actual):
                raise EmacsClientError("actor identity changed during delivery")
            if actual.get("transcript"):
                transcript = actual["transcript"]
                expected_identity = {**expected_identity, "transcript": transcript}
                break
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise EmacsClientError("the selected session has not published its transcript")
            time.sleep(min(DELIVERY_POLL_SECONDS, remaining))
    assert transcript is not None
    if _wait_for_delivery(
        transcript,
        transcript_offset,
        delivery_marker,
        DELIVERY_INITIAL_WAIT_SECONDS,
        expected_prompt_sha256=prompt_sha256,
    ):
        if expected_identity is not None and not _same_actor(
                expected_identity, actor_identity(buffer)):
            raise EmacsClientError("actor identity changed during acknowledgement")
        return str(Path(transcript).resolve())
    if one_pass:
        raise EmacsClientError(
            "implementation delivery was not independently acknowledged; "
            "the one-pass run remains pending and no Return retry was sent"
        )
    if not pending_prompt_contains(buffer, backend, delivery_marker,
                                   expected_prompt_sha256=prompt_sha256):
        raise EmacsClientError(
            "submission returned without delivery acknowledgement and the exact "
            "pending prompt could not be proved in the composer"
        )
    send_return_to_agent(buffer, backend, expected_identity=expected_identity,
                         expected_prompt_sha256=prompt_sha256)
    if not _wait_for_delivery(
        transcript,
        transcript_offset,
        delivery_marker,
        DELIVERY_RETRY_WAIT_SECONDS,
        expected_prompt_sha256=prompt_sha256,
    ):
        raise EmacsClientError(
            "submission delivery was not acknowledged after retrying only the "
            "submit keystroke"
        )
    if expected_identity is not None and not _same_actor(
            expected_identity, actor_identity(buffer)):
        raise EmacsClientError("actor identity changed during acknowledgement")
    return str(Path(transcript).resolve())


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


def _evidence_records(path: Path | str, offset: int) -> list[dict[str, Any]]:
    """Read a complete JSONL suffix or refuse to draw an evidence conclusion."""
    if isinstance(offset, bool) or not isinstance(offset, int) or offset < 0:
        raise EmacsClientError("invalid transcript evidence boundary")
    try:
        with Path(path).open("rb") as stream:
            size = os.fstat(stream.fileno()).st_size
            if offset > size:
                raise EmacsClientError("transcript was truncated past its evidence boundary")
            if offset:
                stream.seek(offset - 1)
                if stream.read(1) != b"\n":
                    raise EmacsClientError("transcript evidence boundary is not a JSONL boundary")
            stream.seek(offset)
            data = stream.read()
    except OSError:
        raise EmacsClientError("transcript evidence is unavailable") from None
    if data and not data.endswith(b"\n"):
        raise EmacsClientError("transcript evidence contains an incomplete record")
    records = []
    for line in data.split(b"\n"):
        if not line.strip(b" \t\r"):
            continue
        try:
            obj = json.loads(line.decode("utf-8"))
        except (UnicodeError, json.JSONDecodeError):
            raise EmacsClientError("transcript evidence contains an invalid record") from None
        if not isinstance(obj, dict):
            raise EmacsClientError("transcript evidence contains an unsupported record")
        records.append(obj)
    return records


def _record_output(obj: dict[str, Any]) -> bool:
    """Conservatively recognize actor/tool activity, including no-text output."""
    payload = obj.get("payload")
    message = obj.get("message")
    if isinstance(message, dict) and message.get("role") == "assistant":
        return True
    if obj.get("type") == "assistant":
        return True
    if isinstance(payload, dict):
        if obj.get("type") == "response_item":
            return payload.get("type") != "message" or payload.get("role") != "user"
        if obj.get("type") == "event_msg":
            return payload.get("type") in {
                "agent_message", "agent_reasoning", "task_complete", "turn_aborted",
                "item_started", "item_completed", "error",
            }
    # Claude tool results have user role, but they prove a tool was dispatched.
    if isinstance(message, dict) and isinstance(message.get("content"), list):
        return any(isinstance(part, dict) and part.get("type") == "tool_result"
                   for part in message["content"])
    return False


def transcript_has_output(path: Path | str, offset: int = 0) -> bool:
    """Prove absence of actor output only from a readable, complete suffix."""
    return any(_record_output(obj) for obj in _evidence_records(path, offset))


def transcript_is_startup_only(
    path: Path | str, *, expected_session_id: str | None = None,
    expected_directory: str | None = None, backend: str | None = None,
) -> bool:
    """Recognize an empty transcript or only identity-matched Codex headers.

    A nonempty transcript with no assistant text is not necessarily fresh:
    user prompts, tool calls, turn contexts and unknown history all refuse.
    No Claude initialization-only persisted record is assumed here.
    """
    for obj in _evidence_records(path, 0):
        payload = obj.get("payload")
        if (backend not in (None, "codex") or obj.get("type") != "session_meta"
                or not isinstance(payload, dict)):
            return False
        session_id = payload.get("id")
        directory = payload.get("cwd")
        if (not isinstance(session_id, str) or not session_id
                or not isinstance(directory, str) or not directory
                or not Path(directory).is_absolute()):
            return False
        if expected_session_id is not None and session_id != expected_session_id:
            return False
        if (expected_directory is not None
                and Path(directory).resolve() != Path(expected_directory).resolve()):
            return False
    return True


def latest_transcript_return(
    path: Path | str, offset: int = 0, *,
    expected_prompt_sha256: str | None = None,
) -> dict[str, str] | None:
    """Return the latest terminal candidate in the requested user turn.

    This is transcript evidence, not a lifecycle check: the caller must also
    require authoritative waiting state. Claude records can have a null stop
    reason; text-only returns are eligible, but tool use, reasoning-only
    output, explicit non-final phases and later activity invalidate them.
    """
    candidate = None
    current_turn = expected_prompt_sha256 is None
    for obj in _evidence_records(path, offset):
        payload = obj.get("payload")
        payload = payload if isinstance(payload, dict) else {}
        message = obj.get("message")
        message = message if isinstance(message, dict) else {}
        user = _user_message_text(obj)
        if (user is None and obj.get("type") == "event_msg"
                and payload.get("type") == "user_message"
                and isinstance(payload.get("message"), str)):
            user = payload["message"]
        if user is not None:
            candidate = None
            current_turn = (expected_prompt_sha256 is None
                            or hashlib.sha256(user.encode("utf-8")).hexdigest()
                            == expected_prompt_sha256)
            continue
        if not _record_output(obj):
            continue
        candidate = None
        text = None
        kind = ""
        if obj.get("type") == "event_msg" and payload.get("type") == "task_complete":
            text = payload.get("last_agent_message")
            kind = "complete"
        elif obj.get("type") == "response_item" and payload.get("type") == "message":
            if (payload.get("role") == "assistant"
                    and payload.get("phase") in (None, "final_answer")
                    and payload.get("channel") in (None, "final")):
                text = _text_parts(payload.get("content"), "output_text")
                kind = "message"
        elif message.get("role") == "assistant":
            content = message.get("content")
            text_only = isinstance(content, str) or (
                isinstance(content, list) and bool(content)
                and all(isinstance(part, dict) and part.get("type") == "text"
                        for part in content)
            )
            terminal_blocks = (isinstance(content, list) and bool(content)
                               and all(isinstance(part, dict) and part.get("type") in {
                                   "text", "thinking", "redacted_thinking"}
                                   for part in content))
            stop_reason = message.get("stop_reason")
            if ((text_only and stop_reason in (None, "end_turn", "stop_sequence"))
                    or (terminal_blocks and stop_reason in ("end_turn", "stop_sequence"))):
                text = content if isinstance(content, str) else _text_parts(content, "text")
                kind = "assistant"
        if current_turn and isinstance(text, str) and text.strip():
            candidate = {"timestamp": str(obj.get("timestamp", "")),
                         "kind": kind, "text": text}
    return candidate


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
