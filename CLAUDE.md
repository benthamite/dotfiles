# dotfiles conventions

## Testing

To test loading Emacs extras in batch mode with all dependencies:

```bash
ELPACA=/Users/pablostafforini/.config/emacs-profiles/7.1.29-target/elpaca
emacs --batch \
  -L "$ELPACA/repos/dotfiles/emacs/extras" \
  -L "$ELPACA/builds/claude-code" \
  -L "$ELPACA/builds/doom-modeline" \
  -L "$ELPACA/builds/compat" \
  -L "$ELPACA/builds/nerd-icons" \
  -L "$ELPACA/builds/shrink-path" \
  -L "$ELPACA/builds/paths" \
  -L "$ELPACA/builds/eat" \
  -L "$ELPACA/builds/inheritenv" \
  -L "$ELPACA/builds/dash" \
  -L "$ELPACA/builds/s" \
  -L "$ELPACA/builds/f" \
  -L "$ELPACA/builds/transient" \
  --eval "(setq load-prefer-newer t)" \
  --eval "(require 'claude-code-extras)" \
  --eval "(require 'doom-modeline-extras)" \
  --eval "(message \"Result: %S\" (YOUR-TEST-EXPRESSION-HERE))" \
  2>&1
```

The `load-prefer-newer t` setting is important because elpaca may have outdated `.elc` files that don't reflect recent source changes. Source files in `extras/` are loaded from the repos directory; all other dependencies use the builds directory to avoid chasing transitive dependencies.

If a dependency is missing, find it with `ls $ELPACA/builds/ | grep NAME` and add another `-L` flag.

For interactive testing against the running Emacs session, use `emacsclient -e '(EXPRESSION)'`.
