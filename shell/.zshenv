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

# Essential environment variables
export DOTFILES="$HOME/My Drive/dotfiles"
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# Prevent Python from writing __pycache__ bytecode to disk
export PYTHONDONTWRITEBYTECODE=1

# Claude Code: persist high reasoning effort across sessions
export CLAUDE_CODE_EFFORT_LEVEL=high

# Anna's Archive CLI (annas-mcp binary used as a CLI, not as an MCP server)
export ANNAS_BASE_URL="annas-archive.gl"
export ANNAS_DOWNLOAD_PATH="$HOME/My Drive/repos/consensus-trader/papers"

# Compiler flags
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"

# Local variables and secrets (keep if needed in non-interactive shells)
source ~/.zvars
source "$DOTFILES/shell/.zshenv-secrets"
