# Basic PATH setup
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# nvm node (before /opt/homebrew/bin to override Homebrew node)
export NVM_DIR="$HOME/.nvm"
export PATH="$NVM_DIR/versions/node/v20.18.2/bin:$PATH"

# Tool paths
export PATH="/opt/homebrew/opt/postgresql@17/bin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"
export PATH="$HOME/.gem/bin:$PATH"
export PATH="$HOME/source/emms/src:$PATH"
export PATH="$HOME/source/gdcv:$PATH"
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"
export GEM_HOME="$HOME/.gem"

# Custom shims (must stay ahead of /opt/homebrew/bin). The emacsclient shim
# resolves the server socket via getconf DARWIN_USER_TEMP_DIR so it works when
# $TMPDIR is overridden (e.g. Claude Code pins TMPDIR=/tmp/claude-$UID).
export PATH="$HOME/My Drive/dotfiles/shell/shims:$PATH"

# Essential environment variables
export DOTFILES="$HOME/My Drive/dotfiles"
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# Prevent Python from writing __pycache__ bytecode to disk
export PYTHONDONTWRITEBYTECODE=1

# Anna's Archive CLI (annas-mcp binary used as a CLI, not as an MCP server)
export ANNAS_BASE_URL="annas-archive.gl"
export ANNAS_DOWNLOAD_PATH="$HOME/My Drive/repos/consensus-trader/papers"

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
