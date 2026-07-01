#!/bin/bash
# Garbage-collect stale transient CR worktrees under ~/Trajectory/agent-c/.cr-tmp/.
#
# Why: CR QA work occasionally needs a throwaway worktree (a taigaLink repin, a
# corpus add on someone else's branch). Historically these were created ad-hoc
# (qa-13296-pr, bp-repin, corpus-req, subagent reqa-wt) with inconsistent names
# and were often left behind. The convention now is: ALL transient QA worktrees
# live under ~/Trajectory/agent-c/.cr-tmp/, and this GC removes the stale ones at
# every session start (wired into sync-agent-c-worktree.sh) plus on demand.
#
# Safety: a worktree is removed ONLY when it is BOTH clean (no uncommitted tracked
# changes) AND fully pushed (its HEAD is reachable from some origin branch). Any
# worktree with uncommitted or unpushed work is KEPT and reported — the GC can
# never destroy in-progress edits. Real task worktrees never live under .cr-tmp/,
# so they are never touched.
set -uo pipefail

base="$HOME/Trajectory/agent-c"
tmp="$base/.cr-tmp"
anchor="$base/agent-c-cr-studio"   # a stable, never-deleted worktree to run git from

[ -d "$tmp" ] || exit 0
[ -e "$anchor/.git" ] || exit 0

removed=0 kept=0
for d in "$tmp"/*/; do
  [ -d "$d" ] || continue
  name="$(basename "${d%/}")"

  # Keep if there are uncommitted tracked changes.
  if [ -n "$(git -C "$d" status --porcelain --untracked-files=no 2>/dev/null)" ]; then
    echo "[cr-worktree-gc] kept .cr-tmp/$name (uncommitted changes)"
    kept=$((kept + 1))
    continue
  fi

  # Keep if HEAD is not yet on any origin branch (would lose unpushed commits).
  sha="$(git -C "$d" rev-parse HEAD 2>/dev/null || true)"
  if [ -n "$sha" ] && [ -z "$(git -C "$anchor" branch -r --contains "$sha" 2>/dev/null)" ]; then
    echo "[cr-worktree-gc] kept .cr-tmp/$name (unpushed commits)"
    kept=$((kept + 1))
    continue
  fi

  if git -C "$anchor" worktree remove --force "${d%/}" 2>/dev/null; then
    removed=$((removed + 1))
  fi
done

git -C "$anchor" worktree prune 2>/dev/null || true
# Drop the .cr-tmp dir itself if it is now empty.
rmdir "$tmp" 2>/dev/null || true

[ "$removed" -gt 0 ] && echo "[cr-worktree-gc] removed $removed stale transient worktree(s); kept $kept."
exit 0
