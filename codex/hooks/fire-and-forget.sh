#!/bin/bash
# Run a hook command in the background (fire-and-forget).
# Reads stdin first so Claude Code doesn't block on a broken pipe,
# then passes it to the real command in a detached background process.
# Use for hooks that don't need to return data (Stop, Notification).
input=$(cat)
(echo "$input" | "$@" &>/dev/null &)
