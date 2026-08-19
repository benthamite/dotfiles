# Editing `emacs/config.org`

Use these rules only when the task changes `emacs/config.org`.

- Keep one active tangled Elisp block per package section. Do not collapse
  prose-oriented literate sections such as early-init only because they contain
  more than one source block.
- In an Elpaca-backed `use-package` recipe, `:defer t` does not neutralize
  recipe-level `:wait t`. Add `:wait t` only for a specific bootstrap or ordering
  need, and verify the startup effect.
- Do not edit a tangled `init.el` directly.
- After the edit, run the profile-aware tangle command from `SKILL.md` and check
  that it completed before testing the affected configuration.
