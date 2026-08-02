#!/bin/bash
# Garbage-collect stale transient CR QA worktrees.
#
# Why: CR QA work occasionally needs a throwaway worktree (a taigaLink repin, a
# corpus add on someone else's branch). Historically these were created ad-hoc
# (qa-13296-pr, bp-repin, corpus-req, subagent reqa-wt) with inconsistent names
# and were often left behind. The convention now is: ALL transient QA worktrees
# live under the shared external worktrees root
# ~/repos/.worktrees/reasoning-tasks/qa-<issue|slug> — never inside the
# repository checkout or ~/My Drive — and this GC removes the stale ones at
# every session start (wired into sync-reasoning-tasks-worktree.sh) plus on
# demand. Leftovers in the legacy in-repo .cr-tmp/ location are swept too
# until none remain.
#
# Safety: a worktree is removed ONLY when it is BOTH clean (no uncommitted tracked
# changes AND no meaningful untracked files) AND fully pushed (its HEAD is
# reachable from some origin branch). Untracked new files (e.g. a hand-written
# hardening-impact.md) count as in-progress work and KEEP the worktree; only the
# ephemeral uv virtualenv (.venv/, not gitignored) is disregarded, and taiga pull
# artifacts are already gitignored. Any worktree with uncommitted, untracked, or
# unpushed work is KEPT and reported — the GC can never destroy in-progress edits.
# Real task worktrees never match the qa-* glob (they live under pablo/<slug>
# or the legacy sibling layout), so they are never touched.
set -uo pipefail

base="$HOME/Trajectory/reasoning-tasks"
worktrees="$HOME/repos/.worktrees/reasoning-tasks"
legacy_tmp="$base/.cr-tmp"
anchor="$base/reasoning-tasks-cr-studio"   # a stable, never-deleted worktree to run git from

[ -e "$anchor/.git" ] || exit 0

removed=0 kept=0
for d in "$worktrees"/qa-*/ "$legacy_tmp"/*/; do
  [ -d "$d" ] || continue
  name="$(basename "${d%/}")"

  # Keep if there is any uncommitted work — tracked changes OR new untracked
  # files. Disregard ONLY an untracked ephemeral uv virtualenv (.venv/, which is
  # NOT gitignored so it would otherwise flag every worktree); taiga pull
  # artifacts are already gitignored and never appear here. The filter matches
  # only an untracked ('?? ') .venv/ *directory* entry (git collapses untracked
  # dirs to a single trailing-slash line, quoted or not), so a tracked change or
  # a real file that merely lives under a .venv path is still kept.
  if [ -n "$(git -C "$d" status --porcelain --untracked-files=normal 2>/dev/null | grep -vE '^\?\? "?([^"]*/)?\.venv/"?$')" ]; then
    echo "[cr-worktree-gc] kept $name (uncommitted or untracked changes)"
    kept=$((kept + 1))
    continue
  fi

  # Keep if HEAD is not yet on any origin branch (would lose unpushed commits).
  sha="$(git -C "$d" rev-parse HEAD 2>/dev/null || true)"
  if [ -n "$sha" ] && [ -z "$(git -C "$anchor" branch -r --contains "$sha" 2>/dev/null)" ]; then
    echo "[cr-worktree-gc] kept $name (unpushed commits)"
    kept=$((kept + 1))
    continue
  fi

  if git -C "$anchor" worktree remove --force "${d%/}" 2>/dev/null; then
    removed=$((removed + 1))
  fi
done

git -C "$anchor" worktree prune 2>/dev/null || true
# Drop the legacy in-repo transient dir once it is empty.
rmdir "$legacy_tmp" 2>/dev/null || true

[ "$removed" -gt 0 ] && echo "[cr-worktree-gc] removed $removed stale transient worktree(s); kept $kept."
exit 0
