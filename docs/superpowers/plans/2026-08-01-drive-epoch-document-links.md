# Drive-Compatible Epoch Document Links Implementation Plan

> **Superseded:** Do not execute this plan. Its Drive baseline and sequencing
> rely on the invalid assumption that directory-symlink failures are not
> user-visible. Rewrite it from the revised workspace design before use.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace 36 invalid cross-account `.gdoc` pointers with normal `.url` files that open the same documents through the configured Epoch Chrome profile without changing permissions.

**Architecture:** A standard-library Python helper creates and validates a private `epoch-doc:` URL and delegates opening to the fail-closed `chrome-profile-open epoch` alias. A signed macOS LaunchServices app owns the scheme, while the meeting workflow creates ignored `.url` files through the helper.

**Tech Stack:** Python 3 standard library, Swift/AppKit, LaunchServices, `unittest`, Chrome profile aliases, Google Drive v3, Git, Google Drive for desktop.

---

This is plan 1 of 4. Execute in the real dotfiles and Epoch working trees, not isolated worktrees, because the generated `.url` files and the native Drive count must be verified at their exact live paths. Before Google API checks, read `claude/context/google-services.md` and `claude/context/secrets.md`; never print OAuth tokens.

Before execution, set and export `EPOCH_ACCOUNT_EMAIL` and
`PERSONAL_ACCOUNT_EMAIL` from the owning account configuration. Keep
the real document ID only in `EPOCH_PILOT_DOC_ID`, not in this document.
Meeting paths below are synthetic examples; substitute a privately selected
pilot path before execution and stop if example values remain.

## File map

Dotfiles:

- Create: `bin/epoch-doc-link`
- Create: `bin/install-epoch-doc-handler`
- Create: `macos/epoch-doc-handler/main.swift`
- Create: `macos/epoch-doc-handler/Info.plist`
- Create: `tests/test_epoch_doc_link.py`
- Create: `tests/test_install_epoch_doc_handler.py`
- Modify: `macos/README.org`

Epoch:

- Modify: `.claude/skills/meeting-debrief/SKILL.md`
- Modify: `.codex/skills/meeting-debrief/SKILL.md`
- Modify: `.gitignore`
- Modify: `README.org`
- Replace live ignored state: 36 Epoch-account `.gdoc` files become `.url` files.

Preserve the eight personal-account `.gdoc` files and all unrelated dirty state.

### Task 1: Add failing epoch-doc-link tests

**Files:**

- Create: `tests/test_epoch_doc_link.py`

- [ ] **Step 1: Create the complete test file**

~~~python
from __future__ import annotations

import importlib.machinery
import importlib.util
import stat
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "epoch-doc-link"


def load_module():
    loader = importlib.machinery.SourceFileLoader("epoch_doc_link", str(SCRIPT))
    spec = importlib.util.spec_from_loader("epoch_doc_link", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["epoch_doc_link"] = module
    loader.exec_module(module)
    return module


class EpochDocLinkTests(unittest.TestCase):
    def setUp(self):
        self.module = load_module()

    def test_shortcut_text_is_exact_and_preserves_case(self):
        self.assertEqual(
            "[InternetShortcut]\nURL=epoch-doc:///document/AbC_12-xYz\n",
            self.module.shortcut_text("AbC_12-xYz"),
        )

    def test_create_is_atomic_idempotent_and_mode_0644(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            destination = root / "meeting.url"
            self.module.create_shortcut("First_ID", destination)
            self.module.create_shortcut("Second-ID", destination)
            self.assertEqual(
                self.module.shortcut_text("Second-ID"),
                destination.read_text(),
            )
            self.assertEqual(0o644, stat.S_IMODE(destination.stat().st_mode))
            self.assertEqual([], list(root.glob(".meeting.url.*")))

    def test_create_refuses_symlink_and_non_file_destinations(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            target = root / "target"
            target.write_text("keep", encoding="utf-8")
            symlink = root / "link.url"
            symlink.symlink_to(target)
            directory = root / "directory.url"
            directory.mkdir()
            with self.assertRaisesRegex(
                self.module.LinkError,
                "Refusing to replace symlink",
            ):
                self.module.create_shortcut("Doc123", symlink)
            with self.assertRaisesRegex(
                self.module.LinkError,
                "Refusing to replace non-file",
            ):
                self.module.create_shortcut("Doc123", directory)
            self.assertEqual("keep", target.read_text())

    def test_create_requires_existing_parent_and_url_suffix(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaisesRegex(self.module.LinkError, "must end in .url"):
                self.module.create_shortcut("Doc123", root / "meeting.gdoc")
            with self.assertRaisesRegex(self.module.LinkError, "does not exist"):
                self.module.create_shortcut(
                    "Doc123",
                    root / "missing" / "meeting.url",
                )

    def test_parser_rejects_every_noncanonical_form(self):
        invalid = (
            "https://docs.google.com/document/d/Doc123/edit",
            "epoch-doc://Doc123",
            "epoch-doc:///document/",
            "epoch-doc:///document/Doc123/extra",
            "epoch-doc:///document/Doc123?x=1",
            "epoch-doc:///document/Doc123#fragment",
            "epoch-doc:///document/Doc%31",
            "epoch-doc:///document/Doc 123",
            "epoch-doc:///document/Doc.123",
        )
        for value in invalid:
            with self.subTest(value=value):
                with self.assertRaises(self.module.LinkError):
                    self.module.parse_link_url(value)

    def test_open_uses_exact_profile_argument_vector(self):
        opener = Path("/tmp/chrome-profile-open")
        completed = mock.Mock(returncode=0)
        with mock.patch.object(
            self.module.subprocess,
            "run",
            return_value=completed,
        ) as run:
            status = self.module.open_shortcut(
                "epoch-doc:///document/AbC_12-xYz",
                opener,
            )
        self.assertEqual(0, status)
        run.assert_called_once_with(
            [
                str(opener),
                "epoch",
                "https://docs.google.com/document/d/AbC_12-xYz/edit",
            ],
            check=False,
        )

    def test_open_propagates_profile_opener_failure(self):
        with mock.patch.object(
            self.module.subprocess,
            "run",
            return_value=mock.Mock(returncode=19),
        ):
            self.assertEqual(
                19,
                self.module.open_shortcut(
                    "epoch-doc:///document/Doc123",
                    Path("/tmp/opener"),
                ),
            )

    def test_invalid_open_never_invokes_any_opener(self):
        with mock.patch.object(self.module.subprocess, "run") as run:
            with self.assertRaises(self.module.LinkError):
                self.module.open_shortcut(
                    "epoch-doc:///document/invalid/value",
                    Path("/tmp/opener"),
                )
        run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
~~~

- [ ] **Step 2: Run the focused suite and verify failure**

Run:

~~~bash
python3 -m unittest tests/test_epoch_doc_link.py -v
~~~

Expected: import errors because `bin/epoch-doc-link` does not exist.

### Task 2: Implement epoch-doc-link

**Files:**

- Create: `bin/epoch-doc-link`
- Test: `tests/test_epoch_doc_link.py`

- [ ] **Step 1: Create the complete helper**

~~~python
#!/usr/bin/env python3
"""Create and open fail-closed Epoch Google document links."""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path


DOC_ID_RE = re.compile(r"^[A-Za-z0-9_-]+$")
URL_PREFIX = "epoch-doc:///document/"
PROFILE_ALIAS = "epoch"
PROFILE_OPENER = Path.home() / "bin" / "chrome-profile-open"


class LinkError(Exception):
    """Raised when a shortcut is malformed or unsafe."""


def validate_doc_id(value: str) -> str:
    if not DOC_ID_RE.fullmatch(value):
        raise LinkError(f"Invalid Google document ID: {value!r}")
    return value


def link_url(document_id: str) -> str:
    return URL_PREFIX + validate_doc_id(document_id)


def document_url(document_id: str) -> str:
    validated = validate_doc_id(document_id)
    return f"https://docs.google.com/document/d/{validated}/edit"


def shortcut_text(document_id: str) -> str:
    return f"[InternetShortcut]\nURL={link_url(document_id)}\n"


def parse_link_url(value: str) -> str:
    if not value.startswith(URL_PREFIX):
        raise LinkError("Expected epoch-doc:///document/DOC_ID")
    document_id = validate_doc_id(value[len(URL_PREFIX):])
    if value != link_url(document_id):
        raise LinkError("Epoch document URL is not in canonical form")
    return document_id


def create_shortcut(document_id: str, destination: Path) -> None:
    if destination.suffix != ".url":
        raise LinkError("Shortcut destination must end in .url")
    if not destination.parent.is_dir():
        raise LinkError(
            f"Destination directory does not exist: {destination.parent}"
        )
    if destination.is_symlink():
        raise LinkError(f"Refusing to replace symlink: {destination}")
    if destination.exists() and not destination.is_file():
        raise LinkError(f"Refusing to replace non-file: {destination}")

    descriptor, temporary = tempfile.mkstemp(
        prefix=f".{destination.name}.",
        dir=destination.parent,
        text=True,
    )
    temporary_path = Path(temporary)
    try:
        with os.fdopen(
            descriptor,
            "w",
            encoding="utf-8",
            newline="\n",
        ) as handle:
            handle.write(shortcut_text(document_id))
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary_path, 0o644)
        os.replace(temporary_path, destination)
    finally:
        temporary_path.unlink(missing_ok=True)


def open_shortcut(
    value: str,
    opener: Path = PROFILE_OPENER,
) -> int:
    document_id = parse_link_url(value)
    command = [
        str(opener),
        PROFILE_ALIAS,
        document_url(document_id),
    ]
    try:
        return subprocess.run(command, check=False).returncode
    except OSError as error:
        raise LinkError(f"Cannot run profile opener: {error}") from error


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Create or open an Epoch Google document link."
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    create = subparsers.add_parser("create")
    create.add_argument("document_id")
    create.add_argument("destination", type=Path)
    open_parser = subparsers.add_parser("open")
    open_parser.add_argument("url")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(
        sys.argv[1:] if argv is None else argv
    )
    try:
        if args.command == "create":
            create_shortcut(args.document_id, args.destination)
            return 0
        return open_shortcut(args.url)
    except LinkError as error:
        print(f"epoch-doc-link: {error}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
~~~

- [ ] **Step 2: Set the executable bit and run focused tests**

Run:

~~~bash
chmod 755 bin/epoch-doc-link
python3 -m py_compile bin/epoch-doc-link tests/test_epoch_doc_link.py
python3 -m unittest tests/test_epoch_doc_link.py -v
~~~

Expected: eight tests pass.

### Task 3: Add the native handler source

**Files:**

- Create: `macos/epoch-doc-handler/main.swift`
- Create: `macos/epoch-doc-handler/Info.plist`

- [ ] **Step 1: Create the Swift application**

~~~swift
import AppKit
import Carbon.HIToolbox

enum HandlerError: LocalizedError {
    case missingURL
    case helperFailed(String)
    case launchFailed(String)

    var errorDescription: String? {
        switch self {
        case .missingURL:
            return "LaunchServices did not provide a document URL."
        case .helperFailed(let message), .launchFailed(let message):
            return message
        }
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var handlingRequest = false

    func applicationWillFinishLaunching(_ notification: Notification) {
        NSAppleEventManager.shared().setEventHandler(
            self,
            andSelector: #selector(handleGetURL(_:withReplyEvent:)),
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL)
        )
    }

    @objc private func handleGetURL(
        _ event: NSAppleEventDescriptor,
        withReplyEvent replyEvent: NSAppleEventDescriptor
    ) {
        guard !handlingRequest else { return }
        handlingRequest = true

        guard let rawURL = event
            .paramDescriptor(forKeyword: AEKeyword(keyDirectObject))?
            .stringValue
        else {
            showError(HandlerError.missingURL.localizedDescription)
            return
        }

        do {
            try runHelper(rawURL)
            NSApp.terminate(nil)
        } catch {
            showError(error.localizedDescription)
        }
    }

    private func runHelper(_ rawURL: String) throws {
        let helper = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("bin/epoch-doc-link")
        let process = Process()
        let stderr = Pipe()
        process.executableURL = helper
        process.arguments = ["open", rawURL]
        process.standardError = stderr

        do {
            try process.run()
        } catch {
            throw HandlerError.launchFailed(
                "Could not launch \(helper.path): \(error.localizedDescription)"
            )
        }

        process.waitUntilExit()
        guard process.terminationStatus == 0 else {
            let data = stderr.fileHandleForReading.readDataToEndOfFile()
            let detail = String(data: data, encoding: .utf8)?
                .trimmingCharacters(in: .whitespacesAndNewlines)
            throw HandlerError.helperFailed(
                detail?.isEmpty == false
                    ? detail!
                    : "epoch-doc-link exited with status \(process.terminationStatus)."
            )
        }
    }

    private func showError(_ message: String) {
        NSApp.activate(ignoringOtherApps: true)
        let alert = NSAlert()
        alert.alertStyle = .critical
        alert.messageText = "Epoch document link could not be opened"
        alert.informativeText = message
        alert.runModal()
        NSApp.terminate(nil)
    }

    deinit {
        NSAppleEventManager.shared().removeEventHandler(
            forEventClass: AEEventClass(kInternetEventClass),
            andEventID: AEEventID(kAEGetURL)
        )
    }
}

@main
struct EpochDocumentHandler {
    static func main() {
        let application = NSApplication.shared
        let delegate = AppDelegate()
        application.setActivationPolicy(.accessory)
        application.delegate = delegate
        application.run()
    }
}
~~~

- [ ] **Step 2: Create the bundle metadata**

~~~xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key><string>en</string>
  <key>CFBundleExecutable</key><string>epoch-doc-handler</string>
  <key>CFBundleIdentifier</key><string>com.stafforini.epoch-doc-handler</string>
  <key>CFBundleInfoDictionaryVersion</key><string>6.0</string>
  <key>CFBundleName</key><string>Epoch Document Link</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleURLTypes</key>
  <array>
    <dict>
      <key>CFBundleTypeRole</key><string>Viewer</string>
      <key>CFBundleURLName</key><string>Epoch Document Link</string>
      <key>CFBundleURLSchemes</key>
      <array><string>epoch-doc</string></array>
    </dict>
  </array>
  <key>LSMinimumSystemVersion</key><string>13.0</string>
  <key>LSUIElement</key><true/>
  <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
~~~

- [ ] **Step 3: Compile the Swift source before building installer tests**

Run:

~~~bash
swiftc -parse-as-library -framework AppKit \
  macos/epoch-doc-handler/main.swift \
  -o /tmp/epoch-doc-handler
/usr/bin/plutil -lint macos/epoch-doc-handler/Info.plist
trash /tmp/epoch-doc-handler
~~~

Expected: Swift compilation and plist lint succeed. Diagnose compiler API errors before continuing.

### Task 4: Define the transactional installer target

**Files:**

- Implement after the failing test in Task 5: `bin/install-epoch-doc-handler`

- [ ] **Step 1: Review the complete implementation target**

Do not create the installer yet. The following is the exact implementation to
apply in Task 5 only after its regression test has failed:

~~~python
#!/usr/bin/env python3
"""Build, sign, install, and register the Epoch document URL handler."""

from __future__ import annotations

import argparse
import os
import plistlib
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "macos" / "epoch-doc-handler"
APP_NAME = "Epoch Document Link.app"
BUNDLE_ID = "com.stafforini.epoch-doc-handler"
DEFAULT_APPLICATIONS_ROOT = Path.home() / "Applications"
DEFAULT_DRIVE_ROOT = Path.home() / "My Drive"
DEFAULT_LSREGISTER = Path(
    "/System/Library/Frameworks/CoreServices.framework/"
    "Frameworks/LaunchServices.framework/Support/lsregister"
)


class InstallError(RuntimeError):
    """Raised when installation cannot complete safely."""


@dataclass(frozen=True)
class Toolchain:
    swiftc: Path
    codesign: Path
    plutil: Path
    lsregister: Path
    trash: Path


def lexical_absolute(path: Path) -> Path:
    return Path(os.path.abspath(os.path.expanduser(os.fspath(path))))


def default_toolchain() -> Toolchain:
    trash = shutil.which("trash")
    if trash is None:
        raise InstallError("trash command not found")
    return Toolchain(
        swiftc=Path(os.environ.get("EPOCH_DOC_SWIFTC", "/usr/bin/swiftc")),
        codesign=Path(
            os.environ.get("EPOCH_DOC_CODESIGN", "/usr/bin/codesign")
        ),
        plutil=Path(os.environ.get("EPOCH_DOC_PLUTIL", "/usr/bin/plutil")),
        lsregister=Path(
            os.environ.get("EPOCH_DOC_LSREGISTER", DEFAULT_LSREGISTER)
        ),
        trash=Path(os.environ.get("EPOCH_DOC_TRASH", trash)),
    )


def run_command(command: list[str]) -> None:
    process = subprocess.run(
        command,
        capture_output=True,
        text=True,
    )
    if process.returncode:
        detail = process.stderr.strip() or process.stdout.strip()
        raise InstallError(
            f"command failed ({process.returncode}): "
            f"{' '.join(command)}: {detail}"
        )


def bundle_identifier(application: Path) -> str:
    plist_path = application / "Contents" / "Info.plist"
    try:
        with plist_path.open("rb") as handle:
            payload = plistlib.load(handle)
    except (OSError, plistlib.InvalidFileException) as error:
        raise InstallError(
            f"cannot read application metadata: {plist_path}: {error}"
        ) from error
    return str(payload.get("CFBundleIdentifier") or "")


def is_within(path: Path, root: Path) -> bool:
    return path == root or root in path.parents


def validate_destination(
    applications_root: Path,
    drive_root: Path,
) -> None:
    lexical_applications = lexical_absolute(applications_root)
    lexical_drive = lexical_absolute(drive_root)
    resolved_applications = lexical_applications.resolve(strict=False)
    resolved_drive = lexical_drive.resolve(strict=False)
    if (
        is_within(lexical_applications, lexical_drive)
        or is_within(resolved_applications, resolved_drive)
    ):
        raise InstallError(
            f"refusing to install inside Google Drive: {applications_root}"
        )


def trash_path(path: Path, toolchain: Toolchain) -> None:
    run_command([str(toolchain.trash), str(path)])


def build_bundle(
    application: Path,
    toolchain: Toolchain,
    runner=run_command,
) -> None:
    macos_directory = application / "Contents" / "MacOS"
    macos_directory.mkdir(parents=True)
    info = application / "Contents" / "Info.plist"
    executable = macos_directory / "epoch-doc-handler"
    shutil.copyfile(SOURCE / "Info.plist", info)
    runner(
        [
            str(toolchain.swiftc),
            "-parse-as-library",
            "-framework",
            "AppKit",
            str(SOURCE / "main.swift"),
            "-o",
            str(executable),
        ]
    )
    runner([str(toolchain.plutil), "-lint", str(info)])
    if bundle_identifier(application) != BUNDLE_ID:
        raise InstallError("staged application has the wrong bundle identifier")
    runner(
        [
            str(toolchain.codesign),
            "--force",
            "--deep",
            "--sign",
            "-",
            str(application),
        ]
    )
    runner(
        [
            str(toolchain.codesign),
            "--verify",
            "--deep",
            "--strict",
            str(application),
        ]
    )


def install(
    applications_root: Path,
    drive_root: Path,
    toolchain: Toolchain,
    runner=run_command,
    trash_func=trash_path,
) -> Path:
    applications_root = lexical_absolute(applications_root)
    validate_destination(applications_root, drive_root)
    applications_root.mkdir(parents=True, exist_ok=True)
    installed = applications_root / APP_NAME
    if os.path.lexists(installed):
        if installed.is_symlink() or not installed.is_dir():
            raise InstallError(
                f"refusing to replace non-application path: {installed}"
            )
        if bundle_identifier(installed) != BUNDLE_ID:
            raise InstallError(
                "refusing to replace application with another bundle ID: "
                f"{installed}"
            )

    build_root = Path(
        tempfile.mkdtemp(
            prefix=".epoch-doc-handler-",
            dir=applications_root,
        )
    )
    staged = build_root / "staged.app"
    previous = build_root / "previous.app"
    failed = build_root / "failed.app"
    had_previous = os.path.lexists(installed)
    try:
        build_bundle(staged, toolchain, runner)
        if had_previous:
            os.replace(installed, previous)
        os.replace(staged, installed)
        try:
            runner([str(toolchain.lsregister), "-f", str(installed)])
        except Exception as registration_error:
            if os.path.lexists(installed):
                os.replace(installed, failed)
            if had_previous and previous.exists():
                os.replace(previous, installed)
                try:
                    runner(
                        [str(toolchain.lsregister), "-f", str(installed)]
                    )
                except Exception as restore_error:
                    raise InstallError(
                        f"{registration_error}; previous application was "
                        "restored but registration also failed: "
                        f"{restore_error}"
                    ) from registration_error
            raise
        return installed
    finally:
        if build_root.exists():
            trash_func(build_root, toolchain)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Install the Epoch document URL handler."
    )
    parser.add_argument(
        "--applications-root",
        type=Path,
        default=DEFAULT_APPLICATIONS_ROOT,
    )
    parser.add_argument(
        "--drive-root",
        type=Path,
        default=DEFAULT_DRIVE_ROOT,
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    try:
        installed = install(
            args.applications_root,
            args.drive_root,
            default_toolchain(),
        )
    except (InstallError, OSError) as error:
        print(f"install-epoch-doc-handler: {error}", file=sys.stderr)
        return 1
    print(f"Installed and registered epoch-doc: {installed}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
~~~

- [ ] **Step 2: Keep the implementation unapplied**

Proceed directly to the failing installer test. This ordering is deliberate:
the Swift source has already been compiler-checked, but the installer behavior
must still be developed test-first.

### Task 5: Test the installer transaction and real bundle

**Files:**

- Create: `tests/test_install_epoch_doc_handler.py`
- Test: `bin/install-epoch-doc-handler`

- [ ] **Step 1: Create the complete test file**

~~~python
from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import plistlib
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "install-epoch-doc-handler"


def load_module():
    loader = importlib.machinery.SourceFileLoader(
        "install_epoch_doc_handler",
        str(SCRIPT),
    )
    spec = importlib.util.spec_from_loader(
        "install_epoch_doc_handler",
        loader,
    )
    module = importlib.util.module_from_spec(spec)
    sys.modules["install_epoch_doc_handler"] = module
    loader.exec_module(module)
    return module


class InstallEpochDocHandlerTests(unittest.TestCase):
    def setUp(self):
        self.module = load_module()
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.applications = self.root / "Applications"
        self.drive = self.root / "My Drive"
        self.drive.mkdir()
        self.toolchain = self.module.Toolchain(
            swiftc=Path("/fake/swiftc"),
            codesign=Path("/fake/codesign"),
            plutil=Path("/fake/plutil"),
            lsregister=Path("/fake/lsregister"),
            trash=Path("/fake/trash"),
        )
        self.calls = []
        self.registration_failures = 0

    def runner(self, command):
        self.calls.append(command)
        if command[0] == str(self.toolchain.swiftc):
            output = Path(command[command.index("-o") + 1])
            output.write_text("binary", encoding="utf-8")
            output.chmod(0o755)
        if (
            command[0] == str(self.toolchain.lsregister)
            and self.registration_failures
        ):
            self.registration_failures -= 1
            raise self.module.InstallError("registration failed")

    def trash(self, path, _toolchain):
        count = len(list(self.root.glob("trashed-*")))
        os.replace(path, self.root / f"trashed-{count}")

    def install(self):
        return self.module.install(
            self.applications,
            self.drive,
            self.toolchain,
            runner=self.runner,
            trash_func=self.trash,
        )

    def test_first_install_builds_expected_bundle_and_registers(self):
        installed = self.install()
        info = installed / "Contents" / "Info.plist"
        executable = installed / "Contents" / "MacOS" / "epoch-doc-handler"
        with info.open("rb") as handle:
            payload = plistlib.load(handle)
        self.assertEqual(
            "com.stafforini.epoch-doc-handler",
            payload["CFBundleIdentifier"],
        )
        self.assertEqual(
            ["epoch-doc"],
            payload["CFBundleURLTypes"][0]["CFBundleURLSchemes"],
        )
        self.assertTrue(executable.is_file())
        self.assertTrue(executable.stat().st_mode & stat.S_IXUSR)
        self.assertEqual(
            [str(self.toolchain.lsregister), "-f", str(installed)],
            self.calls[-1],
        )

    def test_reinstall_replaces_only_matching_generated_bundle(self):
        first = self.install()
        (first / "marker").write_text("old", encoding="utf-8")
        second = self.install()
        self.assertEqual(first, second)
        self.assertFalse((second / "marker").exists())

    def test_refuses_existing_application_with_another_bundle_id(self):
        installed = self.applications / self.module.APP_NAME
        contents = installed / "Contents"
        contents.mkdir(parents=True)
        with (contents / "Info.plist").open("wb") as handle:
            plistlib.dump(
                {"CFBundleIdentifier": "com.example.other"},
                handle,
            )
        with self.assertRaisesRegex(
            self.module.InstallError,
            "another bundle ID",
        ):
            self.install()
        self.assertEqual([], self.calls)

    def test_refuses_destination_inside_drive(self):
        with self.assertRaisesRegex(
            self.module.InstallError,
            "inside Google Drive",
        ):
            self.module.install(
                self.drive / "Applications",
                self.drive,
                self.toolchain,
                runner=self.runner,
                trash_func=self.trash,
            )
        self.assertEqual([], self.calls)

    def test_registration_failure_restores_previous_bundle(self):
        installed = self.install()
        (installed / "marker").write_text("preserve", encoding="utf-8")
        self.registration_failures = 1
        with self.assertRaisesRegex(
            self.module.InstallError,
            "registration failed",
        ):
            self.install()
        restored = self.applications / self.module.APP_NAME
        self.assertEqual(
            "preserve",
            (restored / "marker").read_text(),
        )


@unittest.skipUnless(sys.platform == "darwin", "requires macOS")
class EpochDocHandlerCompileTests(unittest.TestCase):
    def test_real_source_compiles_and_signs(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            applications = root / "Applications"
            drive = root / "My Drive"
            drive.mkdir()
            environment = dict(os.environ)
            environment["EPOCH_DOC_LSREGISTER"] = "/usr/bin/true"
            environment["EPOCH_DOC_TRASH"] = "/usr/bin/true"
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--applications-root",
                    str(applications),
                    "--drive-root",
                    str(drive),
                ],
                env=environment,
                capture_output=True,
                text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            application = applications / "Epoch Document Link.app"
            self.assertTrue(
                application.joinpath(
                    "Contents",
                    "MacOS",
                    "epoch-doc-handler",
                ).is_file()
            )
            verify = subprocess.run(
                [
                    "/usr/bin/codesign",
                    "--verify",
                    "--deep",
                    "--strict",
                    str(application),
                ],
                capture_output=True,
                text=True,
            )
            self.assertEqual(verify.returncode, 0, verify.stderr)


if __name__ == "__main__":
    unittest.main()
~~~

- [ ] **Step 2: Run the installer tests and verify failure**

Run:

~~~bash
python3 -m unittest tests/test_install_epoch_doc_handler.py -v
~~~

Expected: import errors because `bin/install-epoch-doc-handler` does not exist.

- [ ] **Step 3: Create the installer and set its executable bit**

Create `bin/install-epoch-doc-handler` with the complete code in Task 4 Step 1,
then run:

~~~bash
chmod 755 bin/install-epoch-doc-handler
~~~

- [ ] **Step 4: Run installer tests**

Run:

~~~bash
python3 -m py_compile \
  bin/install-epoch-doc-handler \
  tests/test_install_epoch_doc_handler.py
python3 -m unittest tests/test_install_epoch_doc_handler.py -v
~~~

Expected: six tests pass, including real Swift compilation and ad-hoc signing on this Mac.

### Task 6: Document, verify, and commit the dotfiles handler

**Files:**

- Modify: `macos/README.org`
- Create: the six implementation and test paths above

- [ ] **Step 1: Add the handler documentation**

~~~org
* Epoch document links

=bin/epoch-doc-link= creates ordinary Internet shortcuts whose URL has the
form =epoch-doc:///document/DOC_ID=.  The local handler validates that form and
delegates to =chrome-profile-open epoch=, so links fail visibly instead of
falling back to another Chrome profile.

Install or repair the generated application outside Google Drive with:

#+begin_src sh
~/bin/install-epoch-doc-handler
#+end_src

The installer compiles and ad-hoc signs =Epoch Document Link.app= under
=~/Applications=, replaces only a bundle with identifier
=com.stafforini.epoch-doc-handler=, rolls back failed registration, and
registers the =epoch-doc= scheme with LaunchServices.
~~~

- [ ] **Step 2: Run complete dotfiles verification**

Run:

~~~bash
python3 -m unittest tests/test_epoch_doc_link.py -v
python3 -m unittest tests/test_install_epoch_doc_handler.py -v
python3 -m unittest tests/test_chrome_profile_open.py -v
python3 -m unittest tests/test_trajectory_open.py -v
/usr/bin/plutil -lint macos/epoch-doc-handler/Info.plist
bin/ai-config-sync audit
git diff --check
~~~

Expected: all focused, Chrome-routing, compilation, signing, plist, and configuration checks pass.

- [ ] **Step 3: Stage and commit only the handler category**

Run:

~~~bash
git add bin/epoch-doc-link
git add bin/install-epoch-doc-handler
git add macos/epoch-doc-handler/main.swift
git add macos/epoch-doc-handler/Info.plist
git add tests/test_epoch_doc_link.py
git add tests/test_install_epoch_doc_handler.py
git add macos/README.org
git diff --cached --name-only
git diff --cached --check
git commit -m "macos: add Epoch document link handler"
~~~

Expected: one local dotfiles commit containing exactly those seven paths. Preserve unrelated concurrent changes and do not push.

### Task 7: Install and fail-close the live handler

**Files:**

- Install generated state: `~/Applications/Epoch Document Link.app`

- [ ] **Step 1: Install and validate the app**

Run:

~~~bash
~/bin/install-epoch-doc-handler
/usr/bin/codesign --verify --deep --strict \
  "$HOME/Applications/Epoch Document Link.app"
/usr/bin/plutil -extract CFBundleIdentifier raw \
  "$HOME/Applications/Epoch Document Link.app/Contents/Info.plist"
~/bin/chrome-profile-open --list-aliases
~~~

Expected bundle ID: `com.stafforini.epoch-doc-handler`. Require the `epoch` alias to resolve to the intended owning-account profile.
Keep the local profile directory and account identity in private configuration.

- [ ] **Step 2: Prove malformed links and invalid account routing do not fall back**

Run malformed CLI input and require status 2 with no Chrome launch:

~~~bash
~/bin/epoch-doc-link open 'epoch-doc:///document/invalid/value'
~~~

Set `CHROME_PROFILE_OPEN_CONFIG` to a nonexistent mode-`0600` temporary path and run a valid link through the CLI. Require nonzero status and no default/personal-profile tab. Move the temporary path to Trash. Finally open a malformed `epoch-doc:` URL through LaunchServices and confirm the native app displays its critical error alert.

### Task 8: Update the meeting-debrief workflow before migration

**Files:**

- Modify: `.claude/skills/meeting-debrief/SKILL.md`
- Modify: `.codex/skills/meeting-debrief/SKILL.md`
- Modify: `.gitignore`
- Modify: `README.org`

- [ ] **Step 1: Invoke the required skill-writing guidance**

Read and follow `superpowers:writing-skills` before changing the paired workflow skills.

- [ ] **Step 2: Replace Step 8 in both skills**

Use this exact section:

~~~~markdown
## Step 8: Create a local Epoch document link

Create a normal `.url` file next to the meeting org file:

~~~text
/Users/pablostafforini/My Drive/Epoch/meetings/<label>/YYYY-MM-DD.url
~~~

Use the validated helper:

~~~bash
epoch-doc-link create "<DOC_ID>" \
  "/Users/pablostafforini/My Drive/Epoch/meetings/<label>/YYYY-MM-DD.url"
~~~

The file must contain exactly:

~~~ini
[InternetShortcut]
URL=epoch-doc:///document/<DOC_ID>
~~~

Do not share the document with the personal account and do not create a Drive
shortcut. This is an ignored local convenience file. Verify the `.url` exists,
has the expected document ID, and opens through the Epoch Chrome profile. Do
not force-add it unless the repository intentionally chooses to track that
exact shortcut.
~~~~

Change review item 6 to verify the `.url` file and exact `epoch-doc:` value. Change both Step 12 `.gdoc` staging references to `.url`.

- [ ] **Step 3: Update ignore and README guidance**

Keep the existing global `*.gdoc` and `*.gsheet` rules, then add:

~~~gitignore
# Local Epoch meeting links
/meetings/**/.url
~~~

Replace the README's `.gdoc` shortcut description with `epoch-doc-link create`, exact Epoch-profile routing, ignored `.url` state, and the prohibition on sharing with the personal account.

- [ ] **Step 4: Verify and commit only the workflow paths**

Run:

~~~bash
git check-ignore -v --no-index meetings/example-meeting/YYYY-MM-DD.url
rg -n '\.gdoc|\.url|epoch-doc-link' \
  .claude/skills/meeting-debrief/SKILL.md \
  .codex/skills/meeting-debrief/SKILL.md \
  README.org \
  .gitignore
/Users/pablostafforini/My\ Drive/dotfiles/bin/ai-config-sync audit
git diff --check
git add .claude/skills/meeting-debrief/SKILL.md
git add .codex/skills/meeting-debrief/SKILL.md
git add .gitignore
git add README.org
git diff --cached --name-only
git diff --cached --check
git commit -m "meeting-debrief: use Drive-compatible document links"
~~~

Expected: one local Epoch commit with exactly four paths. Preserve unrelated changes and do not push.

### Task 9: Pilot one link with permission and cloud-routing checks

**Files:**

- Create ignored live file: `meetings/example-meeting/YYYY-MM-DD.url`
- Move to Trash after success: `meetings/example-meeting/YYYY-MM-DD.gdoc`

- [ ] **Step 1: Reconfirm the native baseline is 56**

Restart Drive, wait for settling, and inspect the native error panel. Require exactly 56 errors before mutation: 36 `INVALID_GOOGLE_DOCUMENT` and 20 `UNSUPPORTED`. If this has changed, re-triage rather than reusing old counts.

- [ ] **Step 2: Create a mode-0600 read-only Drive probe**

Create `/tmp/drive-link-probe.py` with `apply_patch`, set mode `0600`, and use:

~~~python
#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import pathlib
import urllib.error
import urllib.parse
import urllib.request


def access_token(account: str) -> str:
    token_path = (
        pathlib.Path.home()
        / ".config"
        / "gdoc"
        / "accounts"
        / account
        / "token.json"
    )
    token = json.loads(token_path.read_text())
    body = urllib.parse.urlencode(
        {
            "client_id": token["client_id"],
            "client_secret": token["client_secret"],
            "refresh_token": token["refresh_token"],
            "grant_type": "refresh_token",
        }
    ).encode()
    request = urllib.request.Request(token["token_uri"], data=body)
    response = json.loads(urllib.request.urlopen(request).read())
    return response["access_token"]


def api_json(account: str, url: str) -> dict:
    request = urllib.request.Request(
        url,
        headers={"Authorization": f"Bearer {access_token(account)}"},
    )
    return json.loads(urllib.request.urlopen(request).read())


def permissions(account: str, document_id: str) -> None:
    fields = (
        "permissions("
        "id,type,emailAddress,domain,role,"
        "allowFileDiscovery,deleted,pendingOwner"
        ")"
    )
    url = (
        "https://www.googleapis.com/drive/v3/files/"
        f"{urllib.parse.quote(document_id, safe='')}/permissions"
        f"?supportsAllDrives=true&fields={urllib.parse.quote(fields)}"
    )
    payload = api_json(account, url)
    normalized = sorted(
        payload.get("permissions", []),
        key=lambda item: (
            item.get("type", ""),
            item.get("emailAddress", ""),
            item.get("domain", ""),
            item.get("id", ""),
        ),
    )
    print(json.dumps(normalized, indent=2, sort_keys=True))


def file_metadata(account: str, file_id: str) -> dict:
    fields = urllib.parse.quote("id,name,parents")
    url = (
        "https://www.googleapis.com/drive/v3/files/"
        f"{urllib.parse.quote(file_id, safe='')}?fields={fields}"
    )
    return api_json(account, url)


def paths_for_name(account: str, name: str) -> None:
    escaped = name.replace("\\", "\\\\").replace("'", "\\'")
    query = urllib.parse.quote(
        f"name = '{escaped}' and trashed = false"
    )
    url = (
        "https://www.googleapis.com/drive/v3/files"
        f"?q={query}&pageSize=100&fields=files(id,name,parents)"
    )
    payload = api_json(account, url)
    files = payload.get("files", [])
    if not files:
        raise SystemExit(f"no live cloud item named {name}")
    for item in files:
        chain = [item["name"]]
        current = item
        for _ in range(12):
            parents = current.get("parents") or []
            if not parents:
                break
            current = file_metadata(account, parents[0])
            chain.append(current["name"])
        print(" <- ".join(chain))


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    permission_parser = subparsers.add_parser("permissions")
    permission_parser.add_argument("account")
    permission_parser.add_argument("document_id")
    path_parser = subparsers.add_parser("paths")
    path_parser.add_argument("account")
    path_parser.add_argument("name")
    args = parser.parse_args()
    try:
        if args.command == "permissions":
            permissions(args.account, args.document_id)
        else:
            paths_for_name(args.account, args.name)
    except urllib.error.HTTPError as error:
        raise SystemExit(f"Google Drive API HTTP {error.code}") from error


if __name__ == "__main__":
    main()
~~~

The script must never print token data or HTTP response bodies.

- [ ] **Step 3: Snapshot permissions and create the pilot**

Pick any Epoch-only document for the pilot and export its id as
`EPOCH_PILOT_DOC_ID`; it is deliberately not written down here, because this
plan is published and a document id is a direct handle to internal notes.
Run:

~~~bash
umask 077
python3 /tmp/drive-link-probe.py permissions \
  epoch \
  $EPOCH_PILOT_DOC_ID \
  > /tmp/epoch-permissions-before.json
if python3 /tmp/drive-link-probe.py permissions \
  personal \
  $EPOCH_PILOT_DOC_ID; then
  exit 1
fi
~~~

Require the personal query to print only `Google Drive API HTTP 404` and exit nonzero.

Create:

~~~bash
~/bin/epoch-doc-link create \
  $EPOCH_PILOT_DOC_ID \
  meetings/example-meeting/YYYY-MM-DD.url
~~~

Require exact bytes:

~~~ini
[InternetShortcut]
URL=epoch-doc:///document/$EPOCH_PILOT_DOC_ID
~~~

- [ ] **Step 4: Open through LaunchServices and verify the real browser**

Open `meetings/example-meeting/YYYY-MM-DD.url` through Finder/LaunchServices. Use authenticated Chrome control to verify the tab is in the configured Epoch profile, the final URL contains the exact document ID, the document title loads, and no account chooser or permission error appears.

- [ ] **Step 5: Prove permissions did not change**

Run:

~~~bash
umask 077
python3 /tmp/drive-link-probe.py permissions \
  epoch \
  $EPOCH_PILOT_DOC_ID \
  > /tmp/epoch-permissions-after.json
cmp /tmp/epoch-permissions-before.json /tmp/epoch-permissions-after.json
if rg -F "${PERSONAL_ACCOUNT_EMAIL:?Set the personal account identity privately}" \
  /tmp/epoch-permissions-after.json; then
  exit 1
fi
~~~

Any permission difference or personal-account entry stops the migration.

- [ ] **Step 6: Trash the pilot `.gdoc` and require `56 → 55`**

Move only `meetings/example-meeting/YYYY-MM-DD.gdoc` to Trash. Restart Drive, wait for settling, and require exactly 55 native errors with no error for the new `.url`.

- [ ] **Step 7: Verify the uploaded cloud path**

Run the probe's `paths personal YYYY-MM-DD.url` command. Require:

~~~text
YYYY-MM-DD.url <- example-meeting <- meetings <- Epoch <- My Drive
~~~

If there are same-name links elsewhere, identify the exact matching chain rather than assuming the first result.

### Task 10: Migrate and verify the remaining 35 links

**Files:**

- Replace ignored live state under `meetings/`

- [ ] **Step 1: Inventory by parsed account field**

Use `find -print0` plus `jq` to select only JSON pointers whose `.email` equals `$EPOCH_ACCOUNT_EMAIL`. Require 35 remaining after the pilot. Record the eight personal pointers privately before migration and require
that each recorded path remains unchanged. Do not copy the inventory
into this public plan.

- [ ] **Step 2: Create every replacement before removing a source pointer**

Run this from the Epoch root:

~~~bash
bash -euo pipefail -c '
manifest=/tmp/epoch-gdocs-to-trash.nul
test ! -e "$manifest"
umask 077
: > "$manifest"
while IFS= read -r -d "" gdoc_file; do
  account=$(jq -er .email "$gdoc_file")
  [ "$account" = "${EPOCH_ACCOUNT_EMAIL:?Set the work account identity privately}" ] || continue
  document_id=$(jq -er .doc_id "$gdoc_file")
  url_file="${gdoc_file%.gdoc}.url"
  "$HOME/bin/epoch-doc-link" create "$document_id" "$url_file"
  cmp -s "$url_file" <(
    printf "[InternetShortcut]\\nURL=epoch-doc:///document/%s\\n" \
      "$document_id"
  )
  git check-ignore -q --no-index "$url_file"
  printf "%s\\0" "$gdoc_file" >> "$manifest"
done < <(find meetings -type f -name "*.gdoc" -print0)
count=$(python3 -c \
  "from pathlib import Path; print(Path(\"$manifest\").read_bytes().count(b\"\\0\"))")
[ "$count" -eq 35 ]
'
~~~

The script stops on the first malformed JSON, creation failure, byte mismatch,
or ignore-rule failure, before removing any `.gdoc` source.

- [ ] **Step 3: Verify all 36 real opening paths**

For each Epoch `.url`, open it through LaunchServices and use Chrome control to require the final loaded Google Docs URL contains the same case-preserved ID stored in that file and uses the Epoch profile without an account chooser. Close each verified tab before the next to keep the check bounded.

- [ ] **Step 4: Move only the 35 remaining Epoch `.gdoc` files to Trash**

After every real opening check passes, run:

~~~bash
xargs -0 trash < /tmp/epoch-gdocs-to-trash.nul
~~~

Do not use a broad `*.gdoc` deletion. Re-run the inventory and require:

- zero `.gdoc` files with `.email == "$EPOCH_ACCOUNT_EMAIL"`;
- all eight privately recorded personal `.gdoc` files still exist;
- exactly 36 `.url` files contain canonical `epoch-doc:` values.

- [ ] **Step 5: Require `55 → 20`**

Restart Drive and wait for settling. Require exactly 20 native errors, all in the previously identified `UNSUPPORTED` class. Require no new `INVALID_GOOGLE_DOCUMENT` or `.url` error after the restart timestamp.

- [ ] **Step 6: Recheck cloud routing and clean temporary probes**

Resolve samples from at least three different meeting directories through the personal Drive API and require their intended `meeting file ← label ← meetings ← Epoch ← My Drive` chains. Move `/tmp/drive-link-probe.py` and both permission snapshots to Trash.

The instruction-bridge plan starts only after this exact 20-error gate is met.
