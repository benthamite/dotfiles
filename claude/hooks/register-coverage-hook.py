#!/usr/bin/env python3
"""One-shot: register regenerate-coverage-map.sh as a PostToolUse(Edit|Write) hook in
~/.claude/settings.json. Idempotent. Run once: `python3 <this file>`."""

import json
import os

p = os.path.expanduser("~/.claude/settings.json")
cmd = "~/My\\ Drive/dotfiles/claude/hooks/regenerate-coverage-map.sh"
d = json.load(open(p))
ptu = d.setdefault("hooks", {}).setdefault("PostToolUse", [])
blk = next((b for b in ptu if b.get("matcher") == "Edit|Write"), None)
if blk is None:
    blk = {"matcher": "Edit|Write", "hooks": []}
    ptu.append(blk)
if any(h.get("command") == cmd for h in blk["hooks"]):
    print("already registered — nothing to do")
else:
    blk["hooks"].append({"type": "command", "command": cmd, "timeout": 60})
    json.dump(d, open(p, "w"), indent=2)
    open(p, "a").write("\n")
    print("registered regenerate-coverage-map.sh")
json.load(open(p))
print("settings.json parses OK")
