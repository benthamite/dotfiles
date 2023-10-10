# node
source $(brew --prefix nvm)/nvm.sh

export NVM_DIR="$HOME/.nvm"
# This loads nvm
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"
# This loads nvm bash_completion
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"

# python
alias python="python3"

mkvenv() {
    CURR_VENV=$(basename "$(pwd)")
    echo "Creating venv for $CURR_VENV at $(pwd)/.venv"
    python3 -m venv .venv --prompt "$CURR_VENV"
    source .venv/bin/activate
    python -m pip install --upgrade pip
    pip install --upgrade setuptools wheel
}

# Enable pyenv (github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv)
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# gpg
GPG_TTY=$(tty)
export GPG_TTY

# goku
export GOKU_EDN_CONFIG_FILE="$DOTFILES/karabiner/karabiner.edn"

# emms
export PATH="$HOME/source/emms/src:$PATH" # emms-print-metadata binary

# aliases
alias muinit="cd ~; mu init --maildir=$HOME/Mail --my-address=$PERSONAL_EMAIL --my-address=$PERSONAL_GMAIL; mu index"

# homebrew
export HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1

# libby
export LIBBY_OUTPUT_DIR="$HOME/Downloads/"
