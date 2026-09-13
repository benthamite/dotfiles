#!/usr/bin/env python3
"""Classify verified public URL routing without suppressing request payloads.

Print only a fixed diagnostic label when the legacy opaque-token heuristic
matches. This is not a universal public-URL detector. The registry records
previously verified routes; unknown syntax receives no routing exemption.
"""
from __future__ import annotations

import re
import shlex
import sys
from urllib.parse import unquote, urlsplit

UUID = r"[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"
MONTH = r"[0-9]{4}/(?:0[1-9]|1[0-2])"
ENTITY = r"(?:area|artist|collection|event|genre|instrument|label|place|recording|release|release-group|series|url|work)"
# Exact authority, path prefix, whether the complete path must match, schemes.
# Sources are the public routes pinned by test_secret_guard_parity.py.
ROUTES = (
    ("www.brown.edu", r"/Departments/Philosophy/bears/", False, ("https",)),
    ("ruj.uj.edu.pl", rf"/entities/publication/{UUID}", True, ("https",)),
    ("www.jesp.org", rf"/pdf/{UUID}", True, ("https",)),
    ("blogs.kent.ac.uk", rf"/futureofnormativity/files/{MONTH}/", False, ("https",)),
    ("myweb.sabanciuniv.edu", rf"/ozgurkibris/files/{MONTH}/", False, ("https",)),
    ("www.happierlivesinstitute.org", rf"/wp-content/uploads/{MONTH}/", False, ("http", "https")),
    ("digital.library.adelaide.edu.au", rf"/(?:bitstreams/{UUID}/download|server/api/core/bitstreams/{UUID}/content)", True, ("http", "https")),
    ("www.zora.uzh.ch", r"/id/eprint/[0-9]+/[0-9]+/", False, ("https",)),
    ("discovery.ucl.ac.uk", r"/id/eprint/[0-9]+/[0-9]+/", False, ("https",)),
    ("www.jacobbarrett.org", r"/uploads/1/2/3/6/123631127/", False, ("https",)),
    ("www.bobbeddor.com", r"/uploads/3/2/0/3/32037343/", False, ("https",)),
    ("eprints.lse.ac.uk", r"/[0-9]+/[0-9]+/", False, ("https",)),
    ("jesp.org", r"/index\.php/jesp/article/download/[0-9]+/[0-9]+", True, ("https",)),
    ("www.frontiersin.org", r"/journals/artificial-intelligence/articles/10\.3389/frai\.[0-9]{4}\.[0-9]+/pdf", True, ("https",)),
    ("80000hours.org", rf"/wp-content/uploads/{MONTH}/", False, ("https",)),
    ("www.cambridge.org", r"/core/services/aop-cambridge-core/content/view/[A-F0-9]{32}/S[0-9]{16}a\.pdf/", False, ("https",)),
    ("journals.publishing.umich.edu", r"/ergo/article/[0-9]+/galley/[0-9]+/download/", True, ("https",)),
    ("files.znu.edu.ua", r"/files/Bibliobooks/Inshi[0-9]+/[0-9]+\.pdf", True, ("http", "https")),
    ("ejpe.org", r"/journal/article/download/[0-9]+/[0-9]+/[0-9]+", True, ("http", "https")),
    ("uplopen.com", rf"/en/books/[0-9]+/files/{UUID}\.pdf", True, ("http", "https")),
    ("ruj.uj.edu.pl", rf"/(?:bitstreams/{UUID}/download|server/api/core/bitstreams/{UUID}/content)", True, ("http", "https")),
    ("proceedings.mlr.press", r"/v[0-9]+/", False, ("http", "https")),
    ("adp.library.ucsb.edu", r"/index\.php/matrix/detail/[0-9]+/", False, ("http", "https")),
    ("musicbrainz.org", rf"/(?:ws/2/)?{ENTITY}/{UUID}", True, ("http", "https")),
)
OPAQUE = re.compile(r"[A-Za-z0-9/+=_-]{30,}")
WALLET = re.compile(r"0x[a-fA-F0-9]{40}(?![a-fA-F0-9])")


def opaque(text: str) -> bool:
    # Preserve the existing public Ethereum-address classification, not keys.
    text = WALLET.sub("", text)
    for match in OPAQUE.finditer(text):
        value = match.group()
        classes = sum(bool(re.search(pattern, value))
                      for pattern in (r"[A-Z]", r"[a-z]", r"[/+=_-]", r"[0-9]"))
        if re.search(r"[0-9]", value) and classes >= 3:
            return True
    return False


def suspect(text: str) -> bool:
    """Scan raw bytes and bounded percent-decoding; never decode into routing."""
    for _ in range(4):
        if opaque(text):
            return True
        decoded = unquote(text)
        if decoded == text:
            return False
        text = decoded
    # Ambiguous deeply encoded values receive no speculative exemption.
    return bool(re.search(r"%[0-9a-fA-F]{2}", text)) or opaque(text)


def url_finding(url: str) -> str | None:
    # urlsplit strips controls and is not a validator. Do not classify those.
    if re.search(r"[\x00-\x20\x7f\\]", url):
        return "URL syntax: unclassified opaque token" if suspect(url) else None
    try:
        parts = urlsplit(url)
        parts.port  # Validate bracket/port syntax without weakening authority matching.
    except ValueError:
        return "URL syntax: unclassified opaque token" if suspect(url) else None
    path = parts.path
    for host, pattern, complete, schemes in ROUTES:
        if parts.netloc != host or parts.scheme not in schemes:
            continue
        match = re.fullmatch(pattern, path) if complete else re.match(pattern, path)
        if match:
            path = "/" + path[match.end():]
            break
    # Preserve existing loopback API prefix policy on actual URL operands only.
    authority = parts.netloc
    if (parts.scheme in {"http", "https"}
            and re.fullmatch(r"(?:127\.0\.0\.1|localhost|\[::1\])(?::[0-9]+)?", authority)):
        match = re.match(r"/api/v[0-9]+/", path)
        if match:
            authority, path = "localhost", "/api/" + path[match.end():]
    for field, value in (("authority", authority), ("path", path),
                         ("query", parts.query), ("fragment", parts.fragment)):
        if suspect(value):
            return f"URL {field}: opaque token"
    # Retain the old cross-component/slash-run detection after projection too.
    residual = f"{parts.scheme}://{authority}{path}?{parts.query}#{parts.fragment}"
    if suspect(residual):
        return "URL retained routing/payload: opaque token"
    return None


def literal_tokens(command: str) -> list[tuple[str, bool]] | None:
    """Keep quoted punctuation as data; do not interpret expansions or redirects."""
    if any(char in command for char in "$`\\\n"):
        return None
    tokens, word = [], []
    quote_char = None
    active = False
    for char in command:
        if quote_char:
            if char == quote_char:
                quote_char = None
            else:
                word.append(char)
        elif char in "'\"":
            quote_char, active = char, True
        elif char in "()<>":
            return None
        elif char.isspace() or char in ";&|":
            if active:
                tokens.append(("".join(word), False))
                word, active = [], False
            if char in ";&|":
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
    """Recognize literal curl/wget argv, preserving option-value roles."""
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
                 "--verbose", "--insecure", "--no-buffer"} if curl
                else {"--quiet", "--no-verbose", "--verbose"})
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
            role = ("URL" if option == "--url" and value.startswith(("https://", "http://"))
                    else "header" if option in {"-H", "--header"}
                    else "body" if option in {"-d", "--data", "--data-ascii", "--data-binary",
                                               "--data-raw", "--data-urlencode", "--json", "-F",
                                               "--form", "--form-string", "--post-data", "--body-data"}
                    else "argument")
            result.append((role, value))
        index += 1
    return result


def finding(command: str) -> str | None:
    tokens = literal_tokens(command)
    if tokens is None:
        return "unclassified command: opaque token" if suspect(command) else None
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
            if role == "URL":
                issue = url_finding(value)
                if issue:
                    return issue
            elif suspect(value):
                return f"network {role}: opaque token"
    return None


if __name__ == "__main__":
    issue = finding(sys.stdin.read())
    if issue:
        print(issue)
