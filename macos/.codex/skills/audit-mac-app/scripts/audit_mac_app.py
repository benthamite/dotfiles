#!/usr/bin/env python3
"""Bounded static inventory, not a verdict about an application's safety."""
import argparse
import collections
import hashlib
import json
import os
from pathlib import Path
import plistlib
import re
import selectors
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time
from urllib.parse import urlsplit
from xml.parsers.expat import ExpatError

MAX_FILES = 10000
MAX_ENTRIES = 20000
MAX_DEPTH = 32
MAX_FILE_BYTES = 2 * 1024 * 1024
MAX_SCAN_BYTES = 64 * 1024 * 1024
MAX_TOOL_BYTES = 128 * 1024
MAX_SIGNALS = 200
MAX_HOSTS = 64
MAX_BINARIES = 8
TEXT_SUFFIXES = {".js", ".mjs", ".cjs", ".json", ".html", ".htm", ".css", ".ts", ".plist", ".xml", ".txt"}
MACH_MAGIC = {b"\xfe\xed\xfa\xce", b"\xce\xfa\xed\xfe", b"\xfe\xed\xfa\xcf", b"\xcf\xfa\xed\xfe", b"\xca\xfe\xba\xbe", b"\xbe\xba\xfe\xca", b"\xca\xfe\xba\xbf", b"\xbf\xba\xfe\xca"}
ENTITLEMENTS = (
    "com.apple.security.app-sandbox", "com.apple.security.cs.allow-jit",
    "com.apple.security.cs.allow-unsigned-executable-memory",
    "com.apple.security.cs.disable-library-validation",
    "com.apple.security.cs.allow-dyld-environment-variables",
    "com.apple.security.cs.disable-executable-page-protection",
    "com.apple.security.device.camera", "com.apple.security.device.microphone",
    "com.apple.security.device.audio-input", "com.apple.security.automation.apple-events",
    "com.apple.security.network.client", "com.apple.security.network.server",
    "com.apple.security.files.user-selected.read-only",
    "com.apple.security.files.user-selected.read-write",
)
USAGE_KEYS = (
    "NSCameraUsageDescription", "NSMicrophoneUsageDescription",
    "NSAppleEventsUsageDescription", "NSLocationUsageDescription",
    "NSLocationWhenInUseUsageDescription", "NSContactsUsageDescription",
    "NSCalendarsUsageDescription", "NSPhotoLibraryUsageDescription",
    "NSBluetoothAlwaysUsageDescription", "NSSpeechRecognitionUsageDescription",
)
PATTERNS = {
    "node_integration_true_literal": rb"[\"']?nodeIntegration[\"']?\s*:\s*true\b",
    "context_isolation_false_literal": rb"[\"']?contextIsolation[\"']?\s*:\s*false\b",
    "sandbox_false_literal": rb"[\"']?sandbox[\"']?\s*:\s*false\b",
    "web_security_false_literal": rb"[\"']?webSecurity[\"']?\s*:\s*false\b",
    "insecure_content_true_literal": rb"[\"']?allowRunningInsecureContent[\"']?\s*:\s*true\b",
    "remote_module_true_literal": rb"[\"']?enableRemoteModule[\"']?\s*:\s*true\b",
    "eval_literal": rb"\beval\s*\(", "function_constructor_literal": rb"\bnew\s+Function\s*\(",
    "child_process_literal": rb"\bchild_process\b|\bexecSync\s*\(|\bspawn\s*\(",
    "electron_shell_literal": rb"\bshell\.(?:openExternal|openPath)\b",
    "csp_literal": rb"Content-Security-Policy",
    "aws_key_candidate": rb"\bAKIA[0-9A-Z]{16}\b",
    "github_token_candidate": rb"\bgh[pousr]_[A-Za-z0-9]{36}\b",
    "stripe_key_candidate": rb"\bsk_live_[0-9a-zA-Z]{24,}\b",
    "private_key_marker": rb"BEGIN (?:RSA |DSA |EC |OPENSSH |ENCRYPTED )?PRIVATE KEY",
    "persistence_literal": rb"LaunchAgent|LaunchDaemon|LoginItem|LSSharedFileList|SMLoginItem",
    "suspicious_label_literal": rb"(?i)keylog|password.?stealer|backdoor",
}
PATTERNS = {kind: re.compile(pattern) for kind, pattern in PATTERNS.items()}
URL_PATTERN = re.compile(rb"https?://[^\s\"'<>\\]{1,2048}")


class InputError(Exception):
    """An unsupported or unstable input; messages never include artifact text."""


def root_directory(value):
    """Open every absolute component without following links."""
    if ".." in Path(value).parts:
        raise InputError("Input roots must not contain '..' path components")
    path = Path(os.path.abspath(value))
    if (sys.platform == "darwin" and len(path.parts) > 1
            and path.parts[1] in ("tmp", "var")):
        alias = Path("/") / path.parts[1]
        expected = "/private/" + path.parts[1]
        if alias.is_symlink() and os.readlink(alias) in (expected, expected.lstrip("/")):
            path = Path("/private") / path.relative_to("/")
    descriptor = os.open(path.anchor, os.O_RDONLY | os.O_DIRECTORY)
    try:
        for component in path.parts[1:]:
            following = os.open(component, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                                dir_fd=descriptor)
            os.close(descriptor)
            descriptor = following
        return path, descriptor
    except OSError:
        os.close(descriptor)
        raise InputError("Input roots and parents must be accessible real directories, not symlinks") from None


def tool(argv, seconds=20, pass_fds=(), grace=0):
    """Bound literal argv, retained output and the lifetime of an owned child group."""
    result = {"status": "unavailable", "returncode": None, "stdout": b"", "stderr": b""}
    try:
        child = subprocess.Popen(argv, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE, start_new_session=True, pass_fds=pass_fds)
    except OSError:
        return result
    streams = {child.stdout: "stdout", child.stderr: "stderr"}
    deadline = time.monotonic() + seconds
    total = 0
    selector = selectors.DefaultSelector()
    stopped = None
    try:
        for stream in streams:
            os.set_blocking(stream.fileno(), False)
            selector.register(stream, selectors.EVENT_READ)
        while selector.get_map() or child.poll() is None:
            if time.monotonic() >= deadline:
                stopped = "timeout"
                break
            for key, _events in selector.select(min(0.05, max(0, deadline - time.monotonic()))):
                data = os.read(key.fileobj.fileno(), 16384)
                if not data:
                    selector.unregister(key.fileobj)
                    continue
                remaining = MAX_TOOL_BYTES - total
                result[streams[key.fileobj]] += data[:remaining]
                total += len(data)
                if total > MAX_TOOL_BYTES:
                    stopped = "output_limit"
                    break
            if stopped:
                break
        if stopped:
            if grace:
                try:
                    child.terminate()
                    child.wait(timeout=grace)
                except ProcessLookupError:
                    pass
                except subprocess.TimeoutExpired:
                    stopped = "cleanup_unconfirmed"
            if not grace or child.poll() is None:
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
        try:
            result["returncode"] = child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            stopped = "cleanup_unconfirmed"
        result["status"] = stopped or ("ok" if result["returncode"] == 0 else "failed")
        return result
    except BaseException:
        if grace and child.poll() is None:
            try:
                child.terminate()
                child.wait(timeout=grace)
            except (ProcessLookupError, subprocess.TimeoutExpired):
                pass
        try:
            if not grace or child.poll() is None:
                os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        child.wait(timeout=5)
        raise
    finally:
        selector.close()
        for stream in streams:
            stream.close()


def tool_status(result):
    return {key: result[key] for key in ("status", "returncode")}


def signature_checks(app, runner, assess):
    checks = {}
    details = runner(["/usr/bin/codesign", "--display", "--verbose=4", str(app)])
    checks["signature_details"] = tool_status(details)
    if details["status"] == "ok":
        displayed = details["stdout"] + details["stderr"]
        directories = re.findall(rb"^CodeDirectory v=\d+ size=\d+ flags=0x([0-9a-fA-F]+)(?:\(([^)\r\n]*)\))?(?=[ \r\n]|$)",
                                 displayed, re.MULTILINE)
        flags = directories[0] if len(directories) == 1 else None
        signatures = re.findall(rb"^Signature=([^\r\n]*)$", displayed, re.MULTILINE)
        checks["signature_details"].update({
            "signature_kind": "ad_hoc" if flags and b"adhoc" in flags[1].split(b",") and signatures == [b"adhoc"] else "displayed_not_identity_verified",
            "hardened_runtime_flag": bool(int(flags[0], 16) & 0x10000) if flags else "unknown",
        })
    verification = runner(["/usr/bin/codesign", "--verify", "--deep", "--strict", str(app)])
    checks["signature_verification"] = tool_status(verification)
    checks["signature_verification"]["scope"] = "codesign deep/strict static verification; not a malware or publisher trust verdict"
    entitlements = runner(["/usr/bin/codesign", "--display", "--entitlements", "-", "--xml", str(app)])
    checks["entitlements"] = tool_status(entitlements)
    if entitlements["status"] == "ok":
        try:
            data = plistlib.loads(entitlements["stdout"]) if entitlements["stdout"].strip() else {}
            if not isinstance(data, dict):
                raise ValueError()
            checks["entitlements"].update({
                "scope": "main code signature only; not actual TCC grants or nested helper entitlements",
                "values": {key: (data[key] if type(data[key]) is bool else "non_boolean")
                           for key in ENTITLEMENTS if key in data},
                "other_key_count": len(set(data) - set(ENTITLEMENTS)),
            })
        except (ValueError, TypeError, OverflowError, RecursionError, ExpatError, plistlib.InvalidFileException):
            checks["entitlements"]["status"] = "parse_error"
    checks["gatekeeper_assessment"] = {"status": "not_checked", "reason": "optional host policy assessment may contact Apple services"}
    if assess:
        result = runner(["/usr/sbin/spctl", "--assess", "--verbose", "--type", "execute", str(app)])
        checks["gatekeeper_assessment"] = tool_status(result)
        checks["gatekeeper_assessment"]["assessment"] = "accepted" if result["status"] == "ok" else "not_established"
    checks["notarization"] = {"status": "not_checked", "reason": "Gatekeeper acceptance and signature display do not independently establish notarization"}
    checks["tcc_grants"] = {"status": "not_checked", "reason": "entitlements and usage descriptions are declarations, not permission grants"}
    return checks


def endpoint_host(raw):
    """Return only a syntactically valid host; never URL userinfo/path/query."""
    try:
        parsed = urlsplit(raw.decode("ascii"))
        host = parsed.hostname
        if (parsed.scheme not in ("http", "https") or not host or len(host) > 253
                or not re.fullmatch(r"[a-zA-Z0-9.:-]+", host)):
            return None
        host = host.lower().rstrip(".")
        if any(re.search(pattern.pattern, host.encode("ascii"), re.IGNORECASE)
               for kind, pattern in PATTERNS.items() if kind.endswith("_candidate")):
            return None
        if host == "localhost" or host in ("127.0.0.1", "::1"):
            category = "loopback_literal"
        elif any(host == domain or host.endswith("." + domain)
                 for domain in ("openai.com", "anthropic.com", "googleapis.com")):
            category = "named_service_host_literal"
        else:
            category = "unclassified_host_literal"
        return host, category
    except (UnicodeError, ValueError):
        return None


class Scanner:
    def __init__(self, runner=tool):
        self.runner = runner
        self.counts = collections.Counter()
        self.gaps = collections.Counter()
        self.signals = []
        self.hosts = {}
        self.native = []
        self.info = {"status": "not_found"}
        self.asar = False
        self.asar_unpacked = False
        self.started = time.monotonic()

    def walk(self, descriptor, origin, relative="", depth=0):
        if depth > MAX_DEPTH:
            self.gaps["depth_limit"] += 1
            return
        try:
            with os.scandir(descriptor) as entries:
                for entry in entries:
                    self.counts["entries_seen"] += 1
                    if self.counts["entries_seen"] > MAX_ENTRIES or time.monotonic() - self.started > 180:
                        self.gaps["inventory_limit"] += 1
                        return
                    name = (relative + "/" if relative else "") + entry.name
                    try:
                        before = entry.stat(follow_symlinks=False)
                        if stat.S_ISLNK(before.st_mode):
                            self.gaps["symlink_skipped"] += 1
                            continue
                        if stat.S_ISDIR(before.st_mode):
                            if origin == "bundle" and name == "Contents/Resources/app.asar.unpacked":
                                self.asar_unpacked = True
                            nested = os.open(entry.name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=descriptor)
                            try:
                                self.walk(nested, origin, name, depth + 1)
                            finally:
                                os.close(nested)
                        elif stat.S_ISREG(before.st_mode):
                            self.counts["regular_files_seen"] += 1
                            if self.counts["regular_files_seen"] > MAX_FILES:
                                self.gaps["file_limit"] += 1
                                return
                            opened = os.open(entry.name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=descriptor)
                            try:
                                current = os.fstat(opened)
                                if not stat.S_ISREG(current.st_mode) or (current.st_dev, current.st_ino) != (before.st_dev, before.st_ino):
                                    raise InputError("file changed")
                                self.scan_file(opened, current, name, origin)
                            finally:
                                os.close(opened)
                        else:
                            self.gaps["special_file_skipped"] += 1
                    except (OSError, InputError):
                        self.gaps["unreadable_or_changed_entry"] += 1
        except OSError:
            self.gaps["unreadable_directory"] += 1

    def scan_file(self, descriptor, info, name, origin):
        if origin == "bundle" and name == "Contents/Resources/app.asar":
            self.asar = True
            return
        remaining = MAX_SCAN_BYTES - self.counts["bytes_read"]
        if remaining <= 0:
            self.gaps["byte_limit"] += 1
            return
        data = os.read(descriptor, min(MAX_FILE_BYTES, remaining))
        self.counts["bytes_read"] += len(data)
        after = os.fstat(descriptor)
        if (info.st_size, info.st_mtime_ns, info.st_ctime_ns) != (after.st_size, after.st_mtime_ns, after.st_ctime_ns):
            raise InputError("file changed during read")
        if len(data) < info.st_size:
            self.gaps["file_prefix_only"] += 1
        file_id = hashlib.sha256(os.fsencode(origin + ":" + name)).hexdigest()[:20]
        if origin == "bundle" and name == "Contents/Info.plist":
            try:
                parsed = plistlib.loads(data)
                if not isinstance(parsed, dict) or len(data) < info.st_size:
                    raise ValueError()
                self.info = {"status": "ok", "sha256": hashlib.sha256(data).hexdigest(),
                             "usage_descriptions": {key: "string" if isinstance(parsed[key], str) else "non_string"
                                                    for key in USAGE_KEYS if key in parsed}}
            except (ValueError, TypeError, OverflowError, RecursionError, ExpatError, plistlib.InvalidFileException):
                self.info = {"status": "parse_error"}
        native = data[:4] in MACH_MAGIC
        content = {"content_prefix_sha256": hashlib.sha256(data).hexdigest(),
                   "prefix_bytes": len(data), "whole_file_read": len(data) == info.st_size}
        if native:
            self.counts["native_candidates"] += 1
            if len(self.native) < MAX_BINARIES:
                result = self.runner(["/usr/bin/otool", "-L", "/dev/fd/" + str(descriptor)], pass_fds=(descriptor,))
                item = dict(tool_status(result), file_id=file_id, source=origin, **content)
                if result["status"] == "ok":
                    item["private_framework_reference"] = b"/PrivateFrameworks/" in result["stdout"]
                self.native.append(item)
            else:
                self.gaps["native_tool_limit"] += 1
        if native or Path(name).suffix.lower() in TEXT_SUFFIXES:
            self.counts["pattern_files_scanned"] += 1
            for kind, pattern in PATTERNS.items():
                count = sum(1 for _match in pattern.finditer(data))
                if count:
                    if len(self.signals) < MAX_SIGNALS:
                        self.signals.append(dict(kind=kind, file_id=file_id, source=origin, count=count, **content))
                    else:
                        self.gaps["signal_output_limit"] += 1
            for match in URL_PATTERN.finditer(data):
                candidate = endpoint_host(match[0])
                if candidate and candidate[0] not in self.hosts:
                    if len(self.hosts) < MAX_HOSTS:
                        self.hosts[candidate[0]] = candidate[1]
                    else:
                        self.gaps["endpoint_output_limit"] += 1


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("app")
    parser.add_argument("--assess-gatekeeper", action="store_true")
    source = parser.add_mutually_exclusive_group()
    source.add_argument("--extract-asar", action="store_true")
    source.add_argument("--extracted-root")
    return parser.parse_args(argv)


def main(argv=None, runner=tool):
    args = parse_args(argv)
    descriptors = []
    temporary = None
    try:
        app, root = root_directory(args.app)
        descriptors.append(root)
        if app.suffix.lower() != ".app":
            raise InputError("The target must be an explicit .app directory")
        try:
            contents = os.open("Contents", os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=root)
            try:
                info = os.stat("Info.plist", dir_fd=contents, follow_symlinks=False)
                if not stat.S_ISREG(info.st_mode):
                    raise InputError("Unsupported bundle: Contents/Info.plist must be a regular file")
            finally:
                os.close(contents)
        except OSError:
            raise InputError("Unsupported bundle: accessible Contents/Info.plist is required") from None
        scanner = Scanner(runner)
        scanner.walk(root, "bundle")
        checks = signature_checks(app, runner, args.assess_gatekeeper)
        checks["bundle_info"] = scanner.info
        extraction = {"status": "not_checked", "reason": "archived code requires explicit isolated extraction"} if scanner.asar else {"status": "not_found", "reason": "canonical app.asar was not observed; other code layouts may exist"}
        if args.extracted_root:
            source, source_fd = root_directory(args.extracted_root)
            descriptors.append(source_fd)
            if source == app or app in source.parents or source in app.parents:
                raise InputError("Supplied extraction must be independent of the app directory")
            scanner.started = time.monotonic()
            scanner.walk(source_fd, "supplied_source")
            extraction = {"status": "supplied_unbound", "reason": "caller-supplied source was not independently bound to this archive/app version"}
        elif args.extract_asar and scanner.asar:
            staging_parent, staging_fd = root_directory("/private/tmp" if sys.platform == "darwin" else "/tmp")
            descriptors.append(staging_fd)
            temporary = Path(tempfile.mkdtemp(prefix="audit-mac-app-", dir=str(staging_parent)))
            destination = temporary / "extracted"
            result = runner([str(Path(__file__).with_name("extract-asar.sh")),
                             str(app / "Contents/Resources/app.asar"), str(destination)], seconds=360, grace=45)
            extraction = tool_status(result)
            extraction["provenance"] = "isolated helper used the selected archive path; immutable whole-bundle version binding was not established"
            if result["status"] == "ok":
                try:
                    _source, source_fd = root_directory(destination)
                    descriptors.append(source_fd)
                    scanner.started = time.monotonic()
                    scanner.walk(source_fd, "isolated_extraction")
                except InputError:
                    extraction["status"] = "invalid_output"
            if result["status"] != "ok":
                extraction["cleanup"] = "unconfirmed; isolated runner may still own work; do not retry blindly"
                extraction["retained_directory"] = str(temporary)
                temporary = None
        checks["asar_extraction"] = extraction
        checks["native_link_inventory"] = {"status": "partial" if scanner.native else "not_checked", "files": scanner.native,
                                             "reason": "bounded Mach-O/link/string inspection does not establish native runtime behavior"}
        if scanner.asar and extraction["status"] != "ok":
            scanner.gaps["archived_code_not_bound_or_scanned"] += 1
        if not scanner.counts["pattern_files_scanned"]:
            scanner.gaps["no_supported_pattern_inputs"] += 1
        if temporary is not None:
            trash = shutil.which("trash")
            cleanup = runner([trash, str(temporary)]) if trash else {"status": "unavailable", "returncode": None}
            if cleanup["status"] == "ok":
                try:
                    os.lstat(temporary)
                except FileNotFoundError:
                    pass
                except OSError:
                    cleanup["status"] = "cleanup_unconfirmed"
                else:
                    cleanup["status"] = "cleanup_unconfirmed"
            checks["temporary_cleanup"] = tool_status(cleanup)
            if cleanup["status"] != "ok":
                checks["temporary_cleanup"]["retained_directory"] = str(temporary)
            temporary = None
        report = {
            "schema_version": 1,
            "target": {"path_id": hashlib.sha256(os.fsencode(str(app))).hexdigest()[:20],
                       "device": os.fstat(root).st_dev, "inode": os.fstat(root).st_ino},
            "checks": checks,
            "coverage": {"counts": dict(scanner.counts), "gaps": dict(scanner.gaps),
                         "limits": {"files": MAX_FILES, "entries": MAX_ENTRIES, "depth": MAX_DEPTH,
                                    "bytes_per_file": MAX_FILE_BYTES, "bytes_total": MAX_SCAN_BYTES,
                                    "signal_rows": MAX_SIGNALS, "endpoint_hosts": MAX_HOSTS,
                                    "native_tool_files": MAX_BINARIES},
                         "dependencies_and_unpacked": "included when readable; no node_modules/vendor filename exclusions",
                         "app_asar_unpacked_observed": scanner.asar_unpacked},
            "signals": scanner.signals,
            "endpoint_hosts": [{"host": host, "category": scanner.hosts[host]} for host in sorted(scanner.hosts)],
            "limitations": ["Static inventory only: no application code was intentionally executed and no permissions were granted.",
                            "No safety verdict. No match does not establish absence, enabled protections or effective runtime settings.",
                            "Literal matches include comments, dependencies and unused code; reachability and provenance require review.",
                            "No raw matched lines, token values, filenames or full URLs are emitted; file_id identifies a source-relative path hash.",
                            "Per-file bounded reads are not an immutable whole-bundle snapshot; concurrent writers can invalidate conclusions.",
                            "System signature tools inspect bundle paths with their own traversal semantics; they are not a parser sandbox.",
                            "Native coverage, dynamic loading, generated code, server behavior and TCC state remain limited or unmeasured."],
        }
        print(json.dumps(report, ensure_ascii=True, sort_keys=True))
        return 0
    except (InputError, OSError, KeyboardInterrupt) as error:
        reason = str(error) if isinstance(error, InputError) else "Input unavailable or filesystem operation failed"
        error_report = {"schema_version": 1, "status": "input_error", "reason": reason}
        if temporary is not None:
            error_report["retained_directory"] = str(temporary)
            error_report["cleanup"] = "unconfirmed after interrupted extraction; no automatic deletion"
        if isinstance(error, KeyboardInterrupt):
            error_report.update(status="interrupted", reason="Operation interrupted; no success claimed")
        print(json.dumps(error_report, ensure_ascii=True))
        return 130 if isinstance(error, KeyboardInterrupt) else 2
    finally:
        for descriptor in descriptors:
            os.close(descriptor)


if __name__ == "__main__":
    def interrupted(_signum, _frame):
        raise KeyboardInterrupt

    for interruption in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(interruption, interrupted)
    raise SystemExit(main())
