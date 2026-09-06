#!/usr/bin/env python3
"""Wait for one owned client, not its server or process group.

Usage: python3 run-client.py SECONDS COMMAND [ARG ...]
Client output is inherited; redirect it to reviewed private diagnostic files.
A client timeout or interruption never proves cancellation of server work.
"""
import math
import subprocess
import sys

MAX_SECONDS = 300.0
CLEANUP_SECONDS = 5.0


def stop_client(client, status, reason):
    """Kill and reap only this runner's child; report uncertain cleanup."""
    try:
        client.kill()
        client.wait(timeout=CLEANUP_SECONDS)
    except (OSError, subprocess.TimeoutExpired, KeyboardInterrupt):
        print(f"Client cleanup unconfirmed for owned PID {client.pid}; "
              "stop further requests.", file=sys.stderr)
        return 125
    print(f"{reason}; server outcome remains unknown.", file=sys.stderr)
    return status


def main(argv=None):
    """Run literal argv with a finite diagnostic deadline and no shell."""
    args = sys.argv[1:] if argv is None else argv
    try:
        seconds = float(args[0])
        if not math.isfinite(seconds) or not 0 < seconds <= MAX_SECONDS or not args[1:]:
            raise ValueError
    except (IndexError, ValueError):
        print("Usage: run-client.py SECONDS COMMAND [ARG ...]; "
              "SECONDS must be finite and in (0, 300].", file=sys.stderr)
        return 2

    try:
        client = subprocess.Popen(args[1:], stdin=subprocess.DEVNULL)
    except OSError:
        print("Cannot launch client.", file=sys.stderr)
        return 127

    try:
        result = client.wait(timeout=seconds)
    except subprocess.TimeoutExpired:
        return stop_client(client, 124, "Client deadline reached")
    except KeyboardInterrupt:
        return stop_client(client, 130, "Client wait interrupted")
    # Native client statuses are propagated, so a number alone is not proof
    # of a helper classification; the helper's fixed stderr explains its own.
    return result if result >= 0 else 128 - result


if __name__ == "__main__":
    raise SystemExit(main())
