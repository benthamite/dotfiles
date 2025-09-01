# Local override file
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

export NVM_DIR="$HOME/.nvm"
export PATH="$NVM_DIR/versions/node/v20.18.2/bin:$PATH"

# Lazy-load nvm
nvm() {
    unset nvm
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
    nvm "$@"
}

# Lazy-load pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$PYENV_ROOT/shims:$PATH"    # for python/pip shims

pyenv() {
    unset -f pyenv
    eval "$(command pyenv init -)"
    eval "$(command pyenv init --path)"
    pyenv "$@"
}

# Python aliases and functions
alias python="python3"
mkvenv() {
    CURR_VENV=$(basename "$(pwd)")
    echo "Creating venv for $CURR_VENV at $(pwd)/.venv"
    python3 -m venv .venv --prompt "$CURR_VENV"
    source .venv/bin/activate
    python -m pip install --upgrade pip
    pip install --upgrade setuptools wheel
}

# GPG setup
GPG_TTY=$(tty)
export GPG_TTY

# Tool configurations
export GOKU_EDN_CONFIG_FILE="$DOTFILES/karabiner/karabiner.edn"
export LIBBY_OUTPUT_DIR="$HOME/Downloads/"

# Path additions for specific tools
export PATH="$HOME/source/emms/src:$PATH"        # emms
export PATH="$HOME/.gem/bin:$PATH"               # ruby
export PATH="/Library/TeX/texbin:$PATH"          # pdflatex
export PATH="$PATH:$HOME/source/gdcv"            # gdcv
export PATH="$GOPATH/bin:$PATH"                  # go
export GEM_HOME="$HOME/.gem"                     # ruby

# mdfind wrapper
function mdfind() {
    /usr/bin/mdfind "$@" 2>&1 | grep -v '\[UserQueryParser\]'
}

# mu alias
alias muinit="cd ~; mu init --maildir=$HOME/Mail --personal-address=$PERSONAL_EMAIL --personal-address=$PERSONAL_GMAIL --personal-address=$WORK_EMAIL --personal-address=$UNI_EMAIL; mu index"

# Emacs aliases
alias emacsk="pkill -SIGUSR2 Emacs"
alias emacsK="while true; do pkill -SIGUSR2 Emacs; done"
alias emacsicon="osascript -e 'tell application \"Finder\" to make alias file to posix file \"/opt/homebrew/opt/emacs-plus@30/Emacs.app\" at POSIX file \"/Applications\" with properties {name:\"Emacs.app\"}'"

# make node use local certs
export NODE_EXTRA_CA_CERTS="$HOME/Library/Application Support/mkcert/rootCA.pem"

# EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
    source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Docker
export DOCKER_BUILDKIT=1
export COMPOSE_BAKE=1

export PATH="/opt/homebrew/opt/postgresql@17/bin:$PATH"
