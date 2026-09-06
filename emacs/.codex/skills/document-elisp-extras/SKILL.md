---
name: document-elisp-extras
description: Audit, create, or refresh documentation for all Emacs extras packages in the dotfiles repo. Use when the user asks to document every extras package, fill missing extras docs, refresh outdated extras docs, check extras documentation coverage, or bring emacs/extras/doc up to date with emacs/extras sources.
---

# Project-local extras documentation entry

Load the maintained global workflow before doing any documentation work.
This entry preserves project-local discovery without duplicating its procedures.

Resolve the dotfiles root relative to **this SKILL.md file**, not the current
working directory: it is four parent directories above this skill directory.
Then read the complete file for the active runtime:

- Codex: `codex/skills/document-elisp-extras/SKILL.md` under that root.
- Claude: `claude/skills/document-elisp-extras/SKILL.md` under that root.

Use that exact file path. A name-only lookup may select this local entry again;
an `@path` string is not a portable automatic import. Follow the global workflow
and its required documentation instructions, preserving the user's requested
package scope, audit-only versus editing mode, and processing order.

If the matching global file is missing, unreadable or resolves back to this
entry, report the unavailable prerequisite. Do not proceed from an old duplicate
or silently load a different runtime's workflow.
