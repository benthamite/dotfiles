# Repository guidance

Use `$dotfiles-context` when working in this repository. Use
`$elisp-conventions` when writing, reviewing, or testing Emacs Lisp.

## Repository expectations

- Treat `~/My Drive/dotfiles/` as the source of truth. Do not edit the
  mirrored elpaca clone under
  `~/.config/emacs-profiles/.../elpaca/sources/dotfiles/`.
- When editing `emacs/config.org`, tangle with
  `emacsclient -e '(init-build-profile (file-name-directory user-init-file))'`
  instead of `org-babel-tangle-file`.
- Before committing Elisp changes, run
  `~/My\ Drive/dotfiles/claude/bin/batch-test.sh <package>` or an equivalent
  `emacs --batch` check that exercises the changed code path.
- If you change files under `claude/`, update `claude/README.org`.
- If you change Codex integration files such as `AGENTS.md`, `.codex/`,
  or `codex/`, update `codex/README.org`.
- Prefer commit messages in the form `<scope>: <description>`.
