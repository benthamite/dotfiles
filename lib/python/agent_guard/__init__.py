"""One engine for every PreToolUse guard of Claude Code and Codex.

Replaces the shell guards in claude/hooks and codex/hooks
(docs/plans/2026-10-01-guard-engine.md). Each former hook is a policy module in
``agent_guard.policies`` exposing ``decide(call) -> Result``; ``engine`` runs
the policies a runtime registers for a tool, in the same order the shell hooks
ran, and renders one hook response.

Contract shared with the old hooks:
- a policy that denies returns ``Result.deny(reason)``;
- a policy that rewrites the command (Claude's output wrapping) returns
  ``Result.rewrite(updated_input)``;
- any exception in a policy denies the call ("Security hook failed"), as the
  shell guards' EXIT traps did.
  Exception: a crash (not a HookFailure) on Claude's Read, Grep or Glob allows
  the call with a warning (``engine.crash_warning``).
"""
