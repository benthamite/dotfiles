# Editing `emacs/config.org`

Use these rules only when the task changes `emacs/config.org`.

Before tangling, identify the intended running server, `init-current-profile`,
`user-init-file`, `paths-file-config` and any `init-user-config-file`. Resolve
symlinks to confirm the input is the canonical edited config and the outputs
belong to that profile, not another profile selected by a filesystem alias.
Inspect relevant visited buffers, including an extra user config: the current
`init-tangle` implementation saves its buffers. Reconcile modified or stale
buffers without overwriting or silently saving another session's work.

Inspect the loaded profile builder's relevant behavior before using it after
configuration drift. `init-build-profile` sets output paths, tangles the extra
user config, loads the excluded-package file, tangles the main config, tangles
the extra config again, and runs `init-post-build-hook`. This is not a pure
text conversion or a sandbox for unreviewed Babel/header/local-hook evaluation.
If the server or source identity is unavailable, do not bypass this preflight
with guessed paths, a bare tangle call or a fresh Emacs process with different
profile state. State the activation gap while preserving the source change.

- Keep one active tangled Elisp block per package section. Do not collapse
  prose-oriented literate sections such as early-init only because they contain
  more than one source block.
- In an Elpaca-backed `use-package` recipe, `:defer t` does not neutralize
  recipe-level `:wait t`. Add `:wait t` only for a specific bootstrap or ordering
  need, and verify the startup effect.
- Do not edit generated `init.el`, `early-init.el` or `late-init.el` directly.
- After the edit, run the profile-aware tangle command from `SKILL.md` and check
  that it completed before testing the affected configuration. Inspect the
  intended generated files for the changed forms and expected profile exclusions.
  A normal return is not necessarily the value `t`; use errors and actual output
  evidence, not a guessed return-value convention. If a client times out, the
  server may still be executing: observe completion before retrying.
- Tangling alone does not evaluate the generated configuration. Do not evaluate
  the whole init file, switch profiles or restart a live session as an incidental
  check. Use the separately authorized narrow activation path and verify the
  exact behavior when the task includes activation.
