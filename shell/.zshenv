# Basic PATH setup
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export PATH="/opt/homebrew/opt/node/bin:$PATH"

# Essential environment variables
export DOTFILES="$HOME/Library/CloudStorage/Dropbox/dotfiles"
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# Compiler flags
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"

# Local variables and secrets (keep if needed in non-interactive shells)
source ~/.zvars
source "$DOTFILES/shell/.zshenv-secrets"
