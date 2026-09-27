"""Inspect named-profile skill availability without exposing prompt contents."""

import json
from contextlib import contextmanager
import os
from pathlib import Path
import re
import selectors
import subprocess
import time


class ProfilePluginsError(Exception):
    """The effective profile's Chrome availability could not be established."""


class _PreferenceServer:
    """One private app-server connection for the plugin preference transaction."""

    def __init__(self, executable, home):
        self.process = None
        self.selector = None
        self.pending = b""
        self.sequence = 0
        try:
            self.process = subprocess.Popen(
                [str(executable), "app-server", "--listen", "stdio://"],
                stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                env={**os.environ, "CODEX_HOME": str(home)},
            )
            self.selector = selectors.DefaultSelector()
            self.selector.register(self.process.stdout, selectors.EVENT_READ)
            self.request("initialize", {
                "clientInfo": {"name": "chrome_preference_preservation", "version": "1"},
                "capabilities": {"experimentalApi": True},
            })
            self.process.stdin.write(b'{"method":"initialized","params":{}}\n')
            self.process.stdin.flush()
        except BaseException:
            self.close()
            raise ProfilePluginsError("Chrome preference service could not initialize") from None

    def request(self, method, params):
        self.sequence += 1
        try:
            request = {"id": self.sequence, "method": method, "params": params}
            self.process.stdin.write(json.dumps(request).encode() + b"\n")
            self.process.stdin.flush()
            deadline = time.monotonic() + 30
            while True:
                while b"\n" in self.pending:
                    line, self.pending = self.pending.split(b"\n", 1)
                    response = json.loads(line)
                    if not isinstance(response, dict):
                        raise ValueError
                    if response.get("id") != self.sequence:
                        continue
                    if "error" in response or "result" not in response:
                        raise ValueError
                    return response["result"]
                remaining = deadline - time.monotonic()
                if remaining <= 0 or not self.selector.select(remaining):
                    raise TimeoutError
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if not chunk or len(self.pending) + len(chunk) > 16 * 1024 * 1024:
                    raise ValueError
                self.pending += chunk
        except (OSError, ValueError, TypeError, UnicodeError):
            raise ProfilePluginsError("Chrome preference request failed or timed out") from None

    def close(self):
        if self.selector is not None:
            self.selector.close()
        if self.process is not None:
            if self.process.poll() is None:
                self.process.terminate()
                try:
                    self.process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    self.process.kill()
                    self.process.wait()
            for stream in (self.process.stdin, self.process.stdout):
                if stream is not None:
                    stream.close()


def _read_chrome_preference(server, home):
    """Extract only the exact account user layer, excluding project overrides."""
    result = server.request("config/read", {"includeLayers": True})
    expected = Path(home) / "config.toml"
    try:
        layers = result["layers"]
        if not isinstance(layers, list):
            raise ValueError
        users = [layer for layer in layers if layer["name"].get("type") == "user"]
        if not users and not expected.exists():
            return {"value": None, "file": str(expected), "version": None}
        if len(users) != 1:
            raise ValueError
        layer = users[0]
        if Path(layer["name"]["file"]) != expected or layer["name"].get("profile") is not None:
            raise ValueError
        version = layer["version"]
        if not isinstance(version, str) or not version:
            raise ValueError
        plugin = layer["config"].get("plugins", {}).get("chrome@openai-bundled", {})
        value = plugin.get("enabled")
        if value is not None and not isinstance(value, bool):
            raise ValueError
        return {"value": value, "file": str(expected), "version": version}
    except (KeyError, TypeError, ValueError, AttributeError):
        raise ProfilePluginsError("Chrome preference user layer could not be identified") from None


@contextmanager
def preserve_chrome_preference(executable, home, config_args=()):
    """Preserve the user's enabled leaf across an installer that enables plugins.

    Profile and CLI overrides intentionally do not enter this private service:
    restoration concerns only the persisted account user layer.  Codex's API
    owns TOML edits.  A null value removes a previously absent enabled leaf.
    """
    server = _PreferenceServer(executable, home)
    try:
        before = _read_chrome_preference(server, home)
        try:
            yield
        finally:
            after = _read_chrome_preference(server, home)
            if after["value"] != before["value"]:
                # The installer only changes this preference to true.  Do not
                # overwrite a different concurrent preference edit.
                if after["value"] is not True:
                    raise ProfilePluginsError("Chrome preference changed independently during installation")
                result = server.request("config/batchWrite", {
                    "edits": [{"keyPath": "plugins.chrome@openai-bundled.enabled",
                               "mergeStrategy": "upsert", "value": before["value"]}],
                    "filePath": before["file"],
                    "expectedVersion": after["version"],
                })
                if not isinstance(result, dict) or result.get("status") not in ("ok", "okOverridden"):
                    raise ProfilePluginsError("Chrome preference restoration was not accepted")
                if _read_chrome_preference(server, home)["value"] != before["value"]:
                    raise ProfilePluginsError("Chrome preference restoration could not be verified")
    finally:
        server.close()


def split_profile_args(config_args):
    """Remove profile selectors for commands that do not accept them.

    Other option values are opaque: a config value named ``-p`` is not itself
    a profile selector.  The original list remains suitable for runtime probes.
    """
    result = []
    found = False
    index = 0
    value_options = {"-c", "--config", "--enable", "--disable", "-C", "--cd"}
    while index < len(config_args):
        arg = config_args[index]
        if arg in ("-p", "--profile"):
            if index + 1 >= len(config_args):
                raise ProfilePluginsError("Codex profile selector has no value")
            found = True
            index += 2
            continue
        if arg.startswith("--profile=") or (arg.startswith("-p") and not arg.startswith("--")):
            found = True
            index += 1
            continue
        result.append(arg)
        if arg in value_options:
            index += 1
            if index >= len(config_args):
                raise ProfilePluginsError("Codex configuration option has no value")
            result.append(config_args[index])
        index += 1
    return result, found


def split_working_directory(config_args):
    """Extract runtime cwd for diagnostic commands that ignore top-level -C.

    Resolve relative paths once against this launcher's working directory.
    Other option values are opaque and must not be mistaken for cwd selectors.
    """
    result = []
    directory = None
    index = 0
    opaque_values = {"-c", "--config", "-p", "--profile", "--enable", "--disable"}
    while index < len(config_args):
        arg = config_args[index]
        if arg in ("-C", "--cd"):
            index += 1
            if index >= len(config_args):
                raise ProfilePluginsError("Codex working-directory selector has no value")
            directory = config_args[index]
        elif arg.startswith("--cd="):
            directory = arg[len("--cd="):]
        elif arg.startswith("-C") and arg != "-C":
            directory = arg[2:]
            if directory.startswith("="):
                directory = directory[1:]
        else:
            result.append(arg)
            if arg in opaque_values:
                index += 1
                if index >= len(config_args):
                    raise ProfilePluginsError("Codex configuration option has no value")
                result.append(config_args[index])
        index += 1
    if directory is not None:
        if not directory or "\x00" in directory:
            raise ProfilePluginsError("Codex working directory is invalid")
        try:
            directory = os.path.abspath(directory)
        except (OSError, ValueError):
            raise ProfilePluginsError("Codex working directory could not be resolved") from None
    return result, directory


def _chrome_in_prompt(prompt, chrome_root):
    """Read only Codex's dedicated developer skill catalogue, never user text."""
    if not isinstance(prompt, list):
        raise ProfilePluginsError("Codex profile diagnostic has an unsupported structure")
    catalogues = []
    for message in prompt:
        if not isinstance(message, dict) or message.get("type") != "message" or message.get("role") != "developer":
            continue
        content = message.get("content")
        if not isinstance(content, list):
            continue
        for item in content:
            if not isinstance(item, dict) or item.get("type") != "input_text":
                continue
            text = item.get("text")
            if isinstance(text, str) and text.startswith("<skills_instructions>\n## Skills\n"):
                text = text.rstrip("\r\n")
                if not text.endswith("</skills_instructions>"):
                    raise ProfilePluginsError("Codex profile skill catalogue is incomplete")
                catalogues.append(text)
    if len(catalogues) != 1:
        raise ProfilePluginsError("Codex profile diagnostic lacks one unambiguous skill catalogue")
    text = catalogues[0]
    if text.count("### Skill roots\n") != 1 or text.count("### Available skills\n") != 1:
        raise ProfilePluginsError("Codex profile skill catalogue has an unsupported format")
    roots_text, entries_text = text.split("### Available skills\n")
    roots_text = roots_text.split("### Skill roots\n")[1]
    roots = {}
    for line in roots_text.splitlines():
        if not line:
            continue
        match = re.fullmatch(r"- `(r[0-9]+)` = `([^`]+)`", line)
        if not match or match[1] in roots or not Path(match[2]).is_absolute():
            raise ProfilePluginsError("Codex profile skill roots cannot be resolved")
        roots[match[1]] = Path(match[2])
    expected = Path(chrome_root) / "skills/control-chrome/SKILL.md"
    found = []
    for line in entries_text.removesuffix("</skills_instructions>").splitlines():
        if not line:
            continue
        match = re.fullmatch(r"- ([^\s:]+(?::[^\s:]+)?): .+ \(file: (.+)\)", line)
        if not match:
            raise ProfilePluginsError("Codex profile skill entries cannot be resolved")
        name, source = match.groups()
        path = Path(source)
        if not path.is_absolute():
            pieces = source.split("/", 1)
            if len(pieces) != 2 or pieces[0] not in roots or ".." in Path(pieces[1]).parts:
                raise ProfilePluginsError("Codex profile skill entry has an unknown root")
            path = roots[pieces[0]] / pieces[1]
        if name == "chrome:control-chrome":
            if path != expected:
                raise ProfilePluginsError("Codex profile selected a different Chrome installation")
            found.append(path)
    if len(found) > 1:
        raise ProfilePluginsError("Codex profile has ambiguous Chrome skill entries")
    return bool(found)


def chrome_enabled(executable, home, config_args, chrome_root):
    """Return effective Chrome availability using Codex's profile-aware runtime.

    The diagnostic runs without a model request.  Its output may contain private
    instructions, so capture it in memory and report only the exact skill match.
    """
    config_args, directory = split_working_directory(config_args)
    try:
        result = subprocess.run(
            [str(executable), *config_args, "debug", "prompt-input"],
            capture_output=True, text=True, timeout=120,
            env={**os.environ, "CODEX_HOME": str(home)},
            cwd=directory,
        )
    except subprocess.TimeoutExpired:
        raise ProfilePluginsError("Codex profile availability check timed out") from None
    except (OSError, UnicodeError):
        raise ProfilePluginsError("Codex profile availability check could not complete") from None
    if result.returncode:
        raise ProfilePluginsError("Codex profile availability check failed")
    try:
        prompt = json.loads(result.stdout)
    except (TypeError, ValueError):
        raise ProfilePluginsError("Codex profile availability check returned invalid JSON") from None
    return _chrome_in_prompt(prompt, chrome_root)
