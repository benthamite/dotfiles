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

# Epoch workspace split (2026-10-06): shared project notes live in the
# epoch-research/ai-enablement checkout; Pablo's private Epoch notes stay in
# ~/My Drive/Epoch. Epoch tooling reads these instead of assuming one tree.
export EPOCH_WORKSPACE="$HOME/repos/epoch/ai-enablement"
export EPOCH_WORKSPACE_REPO="epoch-research/ai-enablement"
export EPOCH_PRIVATE_ROOT="$HOME/My Drive/Epoch"

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
# Each account's home is a thin overlay: auth.json is the only real file and
# everything else symlinks back to ~/.codex, so pointing CODEX_HOME at one
# changes the identity and nothing else. The account's home comes from the
# shared account list, emacs/agent-accounts.json (the one Emacs reads into
# `agent-codex-accounts'), not from a naming convention.
#
# An explicit CODEX_HOME always wins, so a pinned account (an isolated benchmark,
# a one-off `CODEX_HOME=... codex`) is never overridden. Unknown or stale account
# names, and pool names (only Emacs can route a pool), fall through to the
# ordinary ~/.codex default. Every shell reads this file, so the lookup uses only
# zsh builtins: `$(<file)' reads without a subprocess and the parse is pattern
# matching. tests/test_zshenv_codex_home.py checks it against bin/agent-account-home.
if [ -z "${CODEX_HOME:-}" ] && [ -r "$HOME/.codex-current-account" ]; then
	() {
		local account="${$(<"$HOME/.codex-current-account")//[[:space:]]/}"
		local file="$DOTFILES/emacs/agent-accounts.json"
		[[ -n "$account" && -r "$file" ]] || return
		local text="$(<"$file")" entry home
		text="${text#*\"codex\"*\[}"
		text="${text%%\]*}"
		while [[ "$text" =~ '[{]([^}]*)[}]' ]]; do
			entry="$match[1]"
			text="${text[MEND+1,-1]}"
			[[ "$entry" =~ '"name"[[:space:]]*:[[:space:]]*"([^"]*)"' && "$match[1]" == "$account" ]] || continue
			[[ "$entry" =~ '"home"[[:space:]]*:[[:space:]]*"([^"]*)"' ]] || return
			home="${match[1]/#\~/$HOME}"
			[[ -d "$home" ]] && export CODEX_HOME="$home"
			return
		done
	}
fi
