# Basic PATH setup
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Homebrew supplies the default Node runtime; nvm remains available for projects.
export NVM_DIR="$HOME/.nvm"

# Tool paths
export PATH="/opt/homebrew/opt/postgresql@17/bin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"
export PATH="$HOME/.gem/bin:$PATH"
export PATH="$HOME/source/emms/src:$PATH"
export PATH="$HOME/source/gdcv:$PATH"
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"
export GEM_HOME="$HOME/.gem"

# Root of this repository, derived from this file's own real location so the
# tracked shell files work wherever the checkout lives (~/My Drive/dotfiles on
# this machine, elsewhere on others that symlink ~/.zshenv into their clone).
# %N is the file zsh is currently reading ($0 stays "zsh" in startup files) and
# :A resolves the ~/.zshenv symlink to the file inside the repository.
export DOTFILES="${${(%):-%N}:A:h:h}"

# Custom shims (must stay ahead of /opt/homebrew/bin). The emacsclient shim
# resolves the server socket via getconf DARWIN_USER_TEMP_DIR so it works when
# $TMPDIR is overridden (e.g. Claude Code pins TMPDIR=/tmp/claude-$UID).
export PATH="$DOTFILES/shell/shims:$PATH"

# Re-assert that precedence whenever something later prepends /opt/homebrew/bin
# (`brew shellenv` in .zprofile does). Idempotent: the entry is moved to the
# front rather than duplicated, so this is safe to call from .zprofile and .zshrc
# too. Defined here because .zshenv is the one file every zsh reads.
dotfiles_prefer_shims() {
	local shims="$DOTFILES/shell/shims"
	path=("$shims" ${path:#"$shims"})
}
dotfiles_prefer_shims

# Route every `op` invocation through the routing shim, whatever PATH says.
#
# PATH order alone is not enough in agent shells. The Claude Code and Codex Bash
# tools source a snapshot whose last act is a frozen `export PATH=...` that puts
# /opt/homebrew/bin ahead of shell/shims, so the shim loses there even though it
# wins in every ordinary shell. A function is immune: the snapshot rewrites PATH,
# not function definitions.
#
# Without this, bare `op` reaches the real binary with no controlling terminal and
# triggers a Touch ID prompt per invocation -- the thing op-desktop exists to
# prevent. See shell/shims/op for the routing rules and escape hatches.
op() {
	"$DOTFILES/shell/shims/op" "$@"
}

# Essential environment variables
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# Prevent Python from writing __pycache__ bytecode to disk
export PYTHONDONTWRITEBYTECODE=1

# Compiler flags
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"

# Local variables and secrets (keep if needed in non-interactive shells)
source ~/.zvars
source "$DOTFILES/shell/.zshenv-secrets"

# Strip the poison empty ANTHROPIC_AUTH_TOKEN that the agent harness exports.
# An empty string (not unset) makes the Anthropic SDK emit an illegal "Bearer "
# header -> APIConnectionError on every `tl grantmaking` call. .zshenv is sourced
# by EVERY zsh (interactive, login, and the non-interactive shells agent tools
# spawn), and by both Claude Code and Codex regardless of launch path -- so this
# is the one place that neutralizes the footgun universally. Only strips when the
# var is set-and-empty; a real token (deliberate API-key session) is preserved.
# This replaces the old per-command `env -u ANTHROPIC_AUTH_TOKEN` workaround.
if [ "${ANTHROPIC_AUTH_TOKEN+set}" = set ] && [ -z "$ANTHROPIC_AUTH_TOKEN" ]; then
	unset ANTHROPIC_AUTH_TOKEN
fi

# Make terminal Codex use the account selected in Emacs.
#
# `agent-codex-select-account' writes the account name to ~/.codex-current-account
# and sets CODEX_HOME for the sessions it starts. It never writes ~/.codex/auth.json,
# so a bare terminal `codex` kept using whatever identity ~/.codex was last logged
# in to -- a different account from the selected one, with its own quota and
# billing. That split is invisible until one of the two hits a usage limit.
#
# Each ~/.codex-<account> home is a thin overlay: auth.json is the only real file
# and everything else symlinks back to ~/.codex, so pointing CODEX_HOME at one
# changes the identity and nothing else.
#
# An explicit CODEX_HOME always wins, so a pinned account (an isolated benchmark,
# a one-off `CODEX_HOME=... codex`) is never overridden. Unknown or stale account
# names fall through to the ordinary ~/.codex default. `$(<file)` is a zsh builtin
# read, so this costs no subprocess.
if [ -z "${CODEX_HOME:-}" ] && [ -r "$HOME/.codex-current-account" ]; then
	_codex_account="${$(<"$HOME/.codex-current-account")//[[:space:]]/}"
	if [ -n "$_codex_account" ] && [ -d "$HOME/.codex-$_codex_account" ]; then
		export CODEX_HOME="$HOME/.codex-$_codex_account"
	fi
	unset _codex_account
fi
