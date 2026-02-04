# nvm lazy loading (NVM_DIR and default node path are in .zshenv)
load_nvm() {
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
}

nvm() {
    unset -f nvm node npm npx
    load_nvm
    nvm "$@"
}

node() {
    unset -f nvm node npm npx
    load_nvm
    node "$@"
}

npm() {
    unset -f nvm node npm npx
    load_nvm
    npm "$@"
}

npx() {
    unset -f nvm node npm npx
    load_nvm
    npx "$@"
}

# pyenv setup with lazy loading
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$PYENV_ROOT/shims:$PATH"

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

# mdfind wrapper
function mdfind() {
    /usr/bin/mdfind "$@" 2>&1 | grep -v '\[UserQueryParser\]'
}

# mu alias
alias muinit="cd ~; mu init --maildir=$HOME/Mail --personal-address=$PERSONAL_EMAIL --personal-address=$PERSONAL_GMAIL --personal-address=$WORK_EMAIL --personal-address=$UNI_EMAIL; mu index"

# Emacs aliases
alias emacsk="pkill -SIGUSR2 Emacs"
alias emacsK="while true; do pkill -SIGUSR2 Emacs; done"

# make node use local certs
export NODE_EXTRA_CA_CERTS="$HOME/Library/Application Support/mkcert/rootCA.pem"

# EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
    source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Docker
export DOCKER_BUILDKIT=1
export COMPOSE_BAKE=1

# Local override file (at end so it can override anything above)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
