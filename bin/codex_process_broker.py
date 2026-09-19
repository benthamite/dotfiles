#!/usr/bin/env python3
"""Pass stdio descriptors to a private, launchd-owned Codex process broker."""

import argparse
import array
import concurrent.futures
import fcntl
import json
import os
from pathlib import Path
import select
import signal
import socket
import stat
import struct
import subprocess
import sys
import threading
import time


MAX_REQUEST = 8 * 1024 * 1024
DEFAULT_RUNTIME = Path.home() / ".local/state/codex-process-broker"


def private_directory(path, create=False):
    """Reject symlink ancestors and require a private, UID-owned endpoint dir."""
    path = Path(os.path.abspath(path))
    for part in reversed((path, *path.parents)):
        try:
            info = part.lstat()
        except FileNotFoundError:
            if not create:
                raise RuntimeError("broker runtime directory is absent")
            part.mkdir(mode=0o700)
            info = part.lstat()
        if not stat.S_ISDIR(info.st_mode):
            raise RuntimeError("broker runtime path contains a symlink or non-directory")
    info = path.stat()
    if info.st_uid != os.getuid() or stat.S_IMODE(info.st_mode) != 0o700:
        raise RuntimeError("broker runtime directory must be UID-owned mode 0700")
    return path


def peer_uid(connection):
    if hasattr(connection, "getpeereid"):
        return connection.getpeereid()[0]
    if sys.platform == "darwin":
        # Darwin SOL_LOCAL/LOCAL_PEERCRED returns struct xucred.
        credentials = connection.getsockopt(0, 1, 76)
        return struct.unpack_from("=I", credentials, 4)[0]
    if hasattr(socket, "SO_PEERCRED"):
        return struct.unpack("3i", connection.getsockopt(
            socket.SOL_SOCKET, socket.SO_PEERCRED, 12))[1]
    raise RuntimeError("peer credential checks unavailable on this platform")


def read_exact(connection, size):
    result = bytearray()
    while len(result) < size:
        chunk = connection.recv(size - len(result))
        if not chunk:
            raise RuntimeError("broker connection closed unexpectedly")
        result.extend(chunk)
    return bytes(result)


def send_record(connection, record):
    payload = json.dumps(record).encode()
    connection.sendall(struct.pack("!I", len(payload)) + payload)


def receive_request(connection):
    descriptors = []
    try:
        header, ancillary, flags, _ = connection.recvmsg(
            4, socket.CMSG_SPACE(3 * array.array("i").itemsize))
        for level, kind, data in ancillary:
            if level == socket.SOL_SOCKET and kind == socket.SCM_RIGHTS:
                received = array.array("i")
                received.frombytes(data[:len(data) - len(data) % received.itemsize])
                descriptors.extend(received)
        if flags & socket.MSG_CTRUNC or len(descriptors) != 3:
            raise RuntimeError("request must provide exactly three stdio descriptors")
        header += read_exact(connection, 4 - len(header))
        size = struct.unpack("!I", header)[0]
        if not 0 < size <= MAX_REQUEST:
            raise RuntimeError("invalid request size")
        request = json.loads(read_exact(connection, size))
        if (not isinstance(request, dict)
                or set(request) != {"argv", "cwd", "env"}
                or not isinstance(request["argv"], list)
                or not all(isinstance(item, str) and "\0" not in item
                           for item in request["argv"])
                or not isinstance(request["cwd"], str)
                or not os.path.isabs(request["cwd"])
                or "\0" in request["cwd"]
                or not isinstance(request["env"], dict)
                or not all(isinstance(key, str) and key and "=" not in key
                           and "\0" not in key and isinstance(value, str)
                           and "\0" not in value
                           for key, value in request["env"].items())):
            raise RuntimeError("invalid broker request")
        return request, descriptors
    except BaseException:
        for descriptor in descriptors:
            os.close(descriptor)
        raise


def terminate_group(process):
    """Bound cleanup to this worker's separately created process group."""
    for sig in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(process.pid, sig)
        except ProcessLookupError:
            break
        if sig == signal.SIGTERM:
            # Descendants can outlive their group leader; always finish cleanup.
            time.sleep(0.2)
    process.wait()


def serve_connection(connection, binary, stopping):
    process = None
    try:
        connection.settimeout(10)
        if peer_uid(connection) != os.getuid():
            raise RuntimeError("broker peer UID mismatch")
        request, descriptors = receive_request(connection)
        try:
            process = subprocess.Popen(
                [binary, *request["argv"]], cwd=request["cwd"],
                env=request["env"], stdin=descriptors[0], stdout=descriptors[1],
                stderr=descriptors[2], start_new_session=True, close_fds=True)
        finally:
            for descriptor in descriptors:
                os.close(descriptor)
        connection.settimeout(None)
        while process.poll() is None:
            if stopping.is_set():
                return
            readable, _, _ = select.select([connection], [], [], 0.1)
            if readable:
                # No post-request client messages are part of this protocol.
                return
        code = process.returncode
        send_record(connection, {"exit_code": code if code >= 0 else 128 - code})
    except (OSError, RuntimeError, ValueError, TypeError):
        # Do not reflect requests, paths or environment into logs/errors.
        try:
            send_record(connection, {"error": "broker request failed; check service availability"})
        except OSError:
            pass
    finally:
        if process is not None:
            terminate_group(process)
        connection.close()


def serve(runtime, binary):
    runtime = private_directory(runtime, create=True)
    if not os.path.isabs(binary) or not os.access(binary, os.X_OK):
        raise RuntimeError("broker executable must be an executable absolute path")
    lock_fd = os.open(runtime / "broker.lock", os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    lock_info = os.fstat(lock_fd)
    if (not stat.S_ISREG(lock_info.st_mode) or lock_info.st_uid != os.getuid()
            or stat.S_IMODE(lock_info.st_mode) != 0o600):
        os.close(lock_fd)
        raise RuntimeError("unsafe broker lock file")
    fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    endpoint = runtime / "broker.sock"
    try:
        info = endpoint.lstat()
    except FileNotFoundError:
        pass
    else:
        if not stat.S_ISSOCK(info.st_mode) or info.st_uid != os.getuid():
            raise RuntimeError("unsafe broker socket path")
        endpoint.unlink()
    stopping = threading.Event()
    for sig in (signal.SIGTERM, signal.SIGINT):
        signal.signal(sig, lambda *_: stopping.set())
    listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        listener.bind(str(endpoint))
        endpoint.chmod(0o600)
        listener.listen(32)
        listener.settimeout(0.2)
        with concurrent.futures.ThreadPoolExecutor(max_workers=32) as workers:
            while not stopping.is_set():
                try:
                    connection, _ = listener.accept()
                except socket.timeout:
                    continue
                workers.submit(serve_connection, connection, binary, stopping)
    finally:
        stopping.set()
        listener.close()
        endpoint.unlink(missing_ok=True)
        os.close(lock_fd)


def client(runtime, argv):
    runtime = private_directory(runtime)
    endpoint = runtime / "broker.sock"
    info = endpoint.lstat()
    if (not stat.S_ISSOCK(info.st_mode) or info.st_uid != os.getuid()
            or stat.S_IMODE(info.st_mode) != 0o600):
        raise RuntimeError("unsafe broker socket")
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as connection:
        connection.settimeout(10)
        connection.connect(str(endpoint))
        if peer_uid(connection) != os.getuid():
            raise RuntimeError("broker peer UID mismatch")
        payload = json.dumps({"argv": argv, "cwd": os.getcwd(),
                              "env": dict(os.environ)}).encode()
        if len(payload) > MAX_REQUEST:
            raise RuntimeError("broker request exceeds size limit")
        header = struct.pack("!I", len(payload))
        sent = connection.sendmsg([header], [(socket.SOL_SOCKET, socket.SCM_RIGHTS,
                                             array.array("i", [0, 1, 2]))])
        connection.sendall(header[sent:] + payload)
        connection.settimeout(None)
        size = struct.unpack("!I", read_exact(connection, 4))[0]
        if not 0 < size <= 4096:
            raise RuntimeError("invalid broker response")
        response = json.loads(read_exact(connection, size))
        if "error" in response:
            raise RuntimeError(response["error"])
        return response["exit_code"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime", type=Path, default=DEFAULT_RUNTIME)
    commands = parser.add_subparsers(dest="mode", required=True)
    server = commands.add_parser("serve")
    server.add_argument("--binary", default="/opt/homebrew/bin/codex")
    caller = commands.add_parser("client")
    caller.add_argument("argv", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    try:
        if args.mode == "serve":
            serve(args.runtime, args.binary)
            return 0
        argv = args.argv[1:] if args.argv[:1] == ["--"] else args.argv
        return client(args.runtime, argv)
    except (OSError, RuntimeError, ValueError) as error:
        # Errors from OS paths contain no request/environment data.
        print(f"codex-isolated: {error}; refusing local execution", file=sys.stderr)
        return 125


if __name__ == "__main__":
    sys.exit(main())
