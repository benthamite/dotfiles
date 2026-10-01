"""The opaque-token heuristic, kept as a log-only signal.

The secret guards used to deny a network command carrying a long mixed-class
token (claude/hooks/lib-public-url-scan.py), with a registry of verified
public routes that exempted known URLs. No recorded exposure came through it;
known secret values and prefixed token shapes are caught by their own checks.
So it no longer denies (docs/plans/2026-10-01-guard-engine.md, intended
difference 3): where it would have been the secret guard's only reason to
deny, it appends one line to LOG naming the runtime, tool and a fixed label,
never the value or the command.

`finding` is the former classifier without its route registry, which existed
only to exempt public URLs from the denial. Its other precision rules are
kept, so the log records what the guard would have denied rather than every
long identifier: quote-aware curl/wget argv roles (output files, local
directories, closed pdftotext reads), Wayback envelopes, loopback API
prefixes, and validated ports. The caller applies the local-read-path
projection of lib-inert-mentions.py first, as the hook did.

If the log ever shows a real secret, docs/secret-guard.md says how to restore
the denial.
"""

from __future__ import annotations

import os
import re
import shlex
import time
from pathlib import Path
from urllib.parse import unquote, unquote_plus, urlsplit

from .core import HOME

LOG = HOME / ".local/state/agent-guard/heuristic.log"
LABEL = "network command with inline secret-like string (opaque token)"

_OPAQUE = re.compile(r"[A-Za-z0-9/+=_-]{30,}")
# Public Ethereum addresses are long hex runs, not keys.
_WALLET = re.compile(r"0x[a-fA-F0-9]{40}(?![a-fA-F0-9])")
_CLASSES = tuple(re.compile(pattern) for pattern in (r"[A-Z]", r"[a-z]", r"[/+=_-]", r"[0-9]"))


def opaque(text: str) -> bool:
    """A 30+ character run with a digit and at least three character classes."""
    for match in _OPAQUE.finditer(_WALLET.sub("", text)):
        value = match.group()
        if re.search(r"[0-9]", value) and sum(bool(c.search(value)) for c in _CLASSES) >= 3:
            return True
    return False


def suspect(text: str) -> bool:
    """`opaque` on TEXT and up to three rounds of percent-decoding; a value still
    encoded after that counts as opaque."""
    for _ in range(4):
        if opaque(text):
            return True
        decoded = unquote(text)
        if decoded == text:
            return False
        text = decoded
    return bool(re.search(r"%[0-9a-fA-F]{2}", text)) or opaque(text)


def url_finding(url: str, *, allow_wayback: bool = True) -> str | None:
    # urlsplit strips controls and is not a validator. Do not classify those.
    if re.search(r"[\x00-\x20\x7f\\]", url):
        return "URL syntax: unclassified opaque token" if suspect(url) else None
    try:
        parts = urlsplit(url)
        parts.port  # Validate bracket/port syntax without weakening authority matching.
    except ValueError:
        return "URL syntax: unclassified opaque token" if suspect(url) else None
    path, query, fragment = parts.path, parts.query, parts.fragment
    if allow_wayback and parts.scheme == "https" and parts.netloc == "web.archive.org":
        # Documented replay and CDX envelopes contain another URL. Scan that
        # target with the ordinary URL rules, without recursively projecting
        # another archive envelope. The host itself is never an exemption.
        replay = re.fullmatch(r"/web/[0-9]{14}(?:id_)?/(https?://.+)", path)
        if replay:
            target = replay.group(1) + ("?" + query if query else "") + ("#" + fragment if fragment else "")
            issue = url_finding(target, allow_wayback=False)
            if issue:
                return "Wayback target " + issue
            path, query, fragment = "/web/ARCHIVED_URL", "", ""
        elif path == "/cdx/search/cdx":
            fields = query.split("&")
            try:
                targets = [(index, unquote_plus(value, errors="strict"))
                           for index, field in enumerate(fields)
                           for key, equals, value in [field.partition("=")]
                           if unquote_plus(key, errors="strict") == "url" and equals]
            except UnicodeDecodeError:
                targets = []
            if len(targets) == 1:
                index, target = targets[0]
                # CDX explicitly accepts targets with no scheme. Do not turn
                # an encoded scheme or userinfo into routing accidentally.
                if re.match(r"[A-Za-z0-9.-]+(?::[0-9]+)?(?:/|$)", target):
                    target = "https://" + target
                # Do not project malformed decoded targets. In particular,
                # form decoding turns literal '+' into spaces, which can
                # split an opaque raw query value into harmless-looking words.
                try:
                    target_parts = urlsplit(target)
                    target_parts.port
                    valid_target = (target_parts.scheme in {"http", "https"}
                                    and bool(target_parts.netloc)
                                    and not re.search(r"[\x00-\x20\x7f\\]", target))
                except ValueError:
                    valid_target = False
                if valid_target:
                    issue = url_finding(target, allow_wayback=False)
                    if issue:
                        return "Wayback target " + issue
                    fields[index] = "url=ARCHIVED_URL"
                    query = "&".join(fields)
    # Preserve existing loopback API prefix policy on actual URL operands only.
    authority = parts.netloc
    if (parts.scheme in {"http", "https"}
            and re.fullmatch(r"(?:127\.0\.0\.1|localhost|\[::1\])(?::[0-9]+)?", authority)):
        match = re.match(r"/api/v[0-9]+/", path)
        if match:
            authority, path = "localhost", "/api/" + path[match.end():]
    for field, value in (("authority", authority), ("path", path),
                         ("query", query), ("fragment", fragment)):
        if suspect(value):
            return f"URL {field}: opaque token"
    # Retain the old cross-component/slash-run detection after projection too.
    # Drop a validated numeric port first: urlsplit has already proved it is
    # digits, the authority field above has already scanned it, and leaving it
    # here lets its digits join an ordinary path into one run.
    residual = (f"{parts.scheme}://{re.sub(r':[0-9]*$', '', authority)}"
                f"{path}?{query}#{fragment}")
    if suspect(residual):
        return "URL retained routing/payload: opaque token"
    return None


def literal_tokens(command: str) -> list[tuple[str, bool]] | None:
    """Keep quoted punctuation as data; do not interpret expansions or redirects."""
    if any(char in command for char in "$`"):
        return None
    tokens, word = [], []
    quote_char = None
    active = False
    for char in command:
        if quote_char:
            if char == "\n" or (char == "\\" and quote_char != "'"):
                return None
            if char == quote_char:
                quote_char = None
            else:
                word.append(char)
        elif char in "'\"":
            quote_char, active = char, True
        elif char in "()<>\\":
            return None
        elif char.isspace() or char in ";&|":
            if active:
                tokens.append(("".join(word), False))
                word, active = [], False
            if char in ";&|\n":
                tokens.append((char, True))
        else:
            word.append(char)
            active = True
    if quote_char:
        return None
    if active:
        tokens.append(("".join(word), False))
    return tokens


def arguments(tokens: list[str]) -> list[tuple[str, str]] | None:
    """Recognize literal download/setup argv, preserving option-value roles."""
    if tokens and tokens[0] == "pdftotext":
        # Closed local-reader form: page bounds, one absolute PDF, stdout.
        # Passwords, unknown options, destinations and wrappers stay scanned.
        index, seen = 1, set()
        while index < len(tokens) and tokens[index] in {"-f", "-l"}:
            option = tokens[index]
            if (option in seen or index + 1 >= len(tokens)
                    or not re.fullmatch(r"[1-9][0-9]{0,5}", tokens[index + 1])):
                return None
            seen.add(option)
            index += 2
        if (len(tokens[index:]) == 2 and tokens[-1] == "-"
                and re.fullmatch(r"/[A-Za-z0-9_./ -]+\.pdf", tokens[index])):
            return [("local-read-path", tokens[index])]
        return None
    # An isolated, literal mkdir -p often prepares a download's destination.
    # These absolute directory names are local, even in a network command list.
    if (len(tokens) >= 3 and tokens[:2] == ["mkdir", "-p"]
            and all(re.fullmatch(r"/[A-Za-z0-9_./ -]+", value) for value in tokens[2:])):
        return [("local-directory", value) for value in tokens[2:]]
    if not tokens or tokens[0] not in {"curl", "wget"}:
        return None
    curl = tokens[0] == "curl"
    value_options = ({"-o", "--output", "-T", "--upload-file", "-K", "--config",
                      "-H", "--header", "-d", "--data", "--data-ascii", "--data-binary",
                      "--data-raw", "--data-urlencode", "--json", "-F", "--form", "--form-string",
                      "-X", "--request", "--url", "--max-time", "--connect-timeout", "--retry",
                      "-A", "--user-agent", "-e", "--referer", "-u", "--user", "-w", "--write-out"}
                     if curl else {"-O", "--output-document", "-o", "--output-file", "--post-file",
                                   "--body-file", "-i", "--input-file", "--header", "--post-data",
                                   "--body-data", "--method", "--timeout", "--tries"})
    switches = ({"--silent", "--show-error", "--location", "--fail", "--head", "--include",
                 "--verbose", "--insecure", "--no-buffer", "--get"} if curl
                else {"--quiet", "--no-verbose", "--verbose"})
    output_options = ({"-o", "--output"} if curl else
                      {"-O", "--output-document", "-o", "--output-file"})
    result = []
    index = 1
    while index < len(tokens):
        token = tokens[index]
        if token.startswith(("https://", "http://")):
            result.append(("URL", token))
        elif token in switches or re.fullmatch(r"-[sSLfqIivkN]+" if curl else r"-(?:q|nv|v|qO-)", token):
            pass
        else:
            option, equals, value = token.partition("=")
            if option not in value_options:
                return None
            if not (token.startswith("--") and equals):
                index += 1
                if index == len(tokens):
                    return None
                value = tokens[index]
            role = ("output-file" if option in output_options
                    and not re.match(r"[A-Za-z][A-Za-z0-9+.-]*:", value)
                    else "URL" if option == "--url" and value.startswith(("https://", "http://"))
                    else "header" if option in {"-H", "--header"}
                    else "body" if option in {"-d", "--data", "--data-ascii", "--data-binary",
                                               "--data-raw", "--data-urlencode", "--json", "-F",
                                               "--form", "--form-string", "--post-data", "--body-data"}
                    else "argument")
            result.append((role, value))
        index += 1
    return result


def finding(command: str) -> str | None:
    """The field and rule of the first opaque token in COMMAND, or None."""
    tokens = literal_tokens(command)
    if tokens is None:
        return "unclassified command: opaque token" if suspect(command) else None
    piped = any(operator and token == "|" for token, operator in tokens)
    segments, current = [], []
    for token, operator in tokens:
        if operator:
            if current:
                segments.append(current)
                current = []
        else:
            current.append(token)
    if current:
        segments.append(current)
    for segment in segments:
        values = arguments(segment)
        if values is None:
            if suspect(shlex.join(segment)):
                return "unclassified command: opaque token"
            continue
        for role, value in values:
            # A proven literal output destination is not sent to the server.
            # mkdir diagnostics may name directories, so pipes retain them.
            # Classify it here, after quote-aware argv parsing: a quoted URL's
            # '&' may prevent the earlier shell-wide file-path projection.
            if role == "output-file" or (role in {"local-directory", "local-read-path"} and not piped):
                continue
            if role == "URL":
                issue = url_finding(value)
                if issue:
                    return issue
            elif suspect(value):
                return f"network {role}: opaque token"
    return None


def record(runtime: str, tool: str, log: Path | None = None) -> None:
    """Append `TIMESTAMP RUNTIME TOOL LABEL` to LOG (directory 0700, file 0600).

    Best effort by design: a log that cannot be written must never turn into a
    denial or a crash of the guard."""
    fields = [time.strftime("%Y-%m-%dT%H:%M:%S%z"), runtime, tool, LABEL]
    log = log or LOG
    line = "\t".join(re.sub(r"[\x00-\x1f\x7f]", "?", field) for field in fields) + "\n"
    try:
        log.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
        descriptor = os.open(log, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o600)
        try:
            os.write(descriptor, line.encode("utf-8", "replace"))
        finally:
            os.close(descriptor)
    except OSError:
        pass
