# Emacs handoff consumer contract

Observed in the configured active-profile checkout of `benthamite/agent`,
commit `7a42b1deff28ba74a287909b10bb543c7758d6f6`, on 2026-09-05:
`agent.el` (handoff definitions and session/start contracts),
`agent-codex.el` / `agent-claude.el` aliases, and its README.

The live registry was unavailable during that inspection. This is source
evidence, not proof that the same code is loaded or that a particular session
can be replaced. Resolve the live source using `dotfiles-context` before any
package edit; do not treat a configured checkout as canonical edit authority.

## Interface and effects

- `agent-handoff` accepts optional `buffer-name` and `target-directory`.
  Pass the exact verified source buffer; without one it can derive a backend
  and directory from unrelated current context or prompt for a backend.
- `agent-codex-handoff` and `agent-claude-handoff` are obsolete aliases to the
  same unified function. Their names do not constrain the selected backend.
- `agent-handoff-from-emacsclient` obtains source buffer and optional target
  from `server-eval-args-left`. A bare `emacsclient -e` call does not inherently
  run in the requesting session's buffer.
- `agent-handoff-files` maps backend symbols to filenames. Defaults are
  `codex` → `/tmp/codex-handoff.md` and
  `claude-code` → `/tmp/claude-code-handoff.md`. It can be dynamically bound for
  one invocation to select a private artifact without overwriting a shared
  slot or permanently changing configuration.
- The consumer reads prompt contents before killing the source. The file itself
  does not bind a project/session/account/digest, and reading it once during
  preparation does not prove which bytes a later invocation will consume.
  Recheck the artifact and source identity at the user-triggered invocation.
- Session identity includes backend, account, directory, instance and CLI ID
  (which may not yet be known). Lazy legacy session capture may derive missing
  fields from current configuration. Require actual source provenance rather
  than assuming a derived account was the account used to launch it.

## Prompt parsing and destination

The reader trims surrounding whitespace and rejects an empty prompt.
It recognizes leading `---` front matter only if a `target-directory:` line is
present, then strips that front matter from the prompt. Its parser extracts a
raw line; it is not a general YAML parser, and quoted YAML paths/comments may
be treated as literal path text.

Explicit target argument takes precedence over file metadata, then source
directory. Relative targets resolve against the source directory; a requested
target that does not exist is rejected before source closure. Prefer validated
explicit directory arguments over inserting control metadata into user-supplied
prose. Check that
parsing preserves intended text; exact-verbatim requests may be incompatible
with trimming or front-matter stripping.

## Failure boundaries

The consumer checks pending captured prompts, closes the source buffer, and
then calls `agent-start-session`. Startup checks (including account availability)
and account synchronization occur in the start path. A startup failure can
therefore occur after closure; do not claim transactional replacement or assume
the old session survives. Check available prerequisites before the user-triggered
close and preserve the prompt independently.

The command's source-level contract says it supplies the prompt to the new
session. Only observing actual launch/delivery establishes that this occurred.
An offline fixture or saved prompt does not. Keep closure user-triggered, and
do not change runtime/package code as an implicit part of preparing a handoff.
