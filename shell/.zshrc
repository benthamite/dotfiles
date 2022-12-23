export PS1="%~ $ "

# Enable NVM
# export NVM_DIR=/usr/share/nvm
# [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
# source /usr/share/nvm/init-nvm.sh
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

alias emacsgui='/opt/homebrew/Cellar/emacs-mac/emacs-28.2-mac-9.1/Emacs.app/Contents/MacOS/Emacs'

mkvenv() {
    CURR_VENV=$(basename "$(pwd)")
    echo "Creating venv for $CURR_VENV at $(pwd)/.venv"
    python3 -m venv .venv --prompt "$CURR_VENV"
    source .venv/bin/activate
    python -m pip install --upgrade pip
    pip install --upgrade setuptools wheel
}

GPG_TTY=$(tty)
export GPG_TTY
