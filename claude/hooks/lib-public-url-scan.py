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
from urllib.parse import unquote, unquote_plus, urlsplit

UUID = r"[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"
MONTH = r"[0-9]{4}/(?:0[1-9]|1[0-2])"
BSB_OBJECT = r"bsb[0-9]{8}(?:_[0-9]{5}_u[0-9]{3})?"
CARETAS_ITEM = (r"Caras_y_Caretas_Buenos_Aires_(?:18|19)[0-9]{2}-"
                r"(?:0[1-9]|1[0-2])-(?:0[1-9]|[12][0-9]|3[01])_N_[0-9]{1,5}")
ENTITY = r"(?:area|artist|collection|event|genre|instrument|label|place|recording|release|release-group|series|url|work)"
# Exact authority, path prefix, whether the complete path must match, schemes.
# Sources are the public routes pinned by test_secret_guard_parity.py.
ROUTES = (
    ("www.utorpheus.com", r"/file/catalog/pdf_musiche/lb018\.pdf", True, ("https",)),
    ("prensahistorica.galiciana.gal",
     rf"/recurso/caras-y-caretas-semanario-festivo-literario/{UUID}", True, ("https",)),
    ("hemerotecadigital.bn.gob.ar",
     r"/collection/001181802/critica(?:/year/19(?:1[4-69]|[2-4][0-9]|5[0-7]))?",
     True, ("https",)),
    ("archive.org", rf"/metadata/{CARETAS_ITEM}", True, ("https",)),
    ("archive.org", rf"/download/(?P<caretas>{CARETAS_ITEM})/(?P=caretas)(?:\.pdf|_djvu\.txt)", True, ("https",)),
    ("archive.org", r"/metadata/", False, ("https",)),
    ("api.digitale-sammlungen.de", rf"/iiif/presentation/v2/{BSB_OBJECT}/manifest", True, ("https",)),
    ("api.digitale-sammlungen.de", r"/iiif/image/v2/bsb[0-9]{8}_[0-9]{5}/full/full/0/default\.jpg", True, ("https",)),
    ("collecties.kb.nl", r"/en/collections/collection-anny-antoine-louis-koopman/1951-1960/cahiers", True, ("https",)),
    ("notes.andymatuschak.org",
     r"/(?:z28QkpK3vRKQTacjFDfGYBhCXHqHuVWJzny9|zVFGpprS64TzmKGNzGxq9FiCDnAnCPwRU5T)",
     True, ("https",)),
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
    ("www.sec.gov",
     r"/Archives/edgar/data/[0-9]+/[0-9]{18}/(?:[A-Za-z0-9_-]{1,29}/)?"
     r"[A-Za-z0-9_.-]{1,29}\.(?:html?|xml|txt|pdf|json|csv)",
     True, ("https",)),
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
    if (parts.scheme == "https" and parts.netloc == "hemerotecadigital.bn.gob.ar"
            and path == "/render.php"):
        # Exact public issue linked by the Crítica 1947 holdings page. Project
        # only its file locator; retain every other field and reject ambiguous
        # repeated selector keys, including percent-encoded spellings.
        fields = query.split("&")
        keys = [unquote_plus(field.partition("=")[0]) for field in fields]
        locator = "url=001181802/1947/BNA_S001181802_19470908N11872.pdf"
        if (keys.count("url") == keys.count("system") == 1
                and locator in fields and "system=001181802" in fields):
            fields[fields.index(locator)] = "url=PUBLIC_ISSUE"
            query = "&".join(fields)
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
            # Known-secret checks still inspect the full original command.
            if role == "output-file" or (role in {"local-directory", "local-read-path"} and not piped):
                continue
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
