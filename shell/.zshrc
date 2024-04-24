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
# emms-print-metadata binary
export PATH="$HOME/source/emms/src:$PATH"

# mu
alias muinit="cd ~; mu init --maildir=$HOME/Mail --my-address=$PERSONAL_EMAIL --my-address=$PERSONAL_GMAIL --my-address=$WORK_EMAIL; mu index"

# emacs
alias emacsk="pkill -USR2 Emacs"
alias emacsK="while true; do pkill -USR2 Emacs; done"
alias emacsicon="osascript -e 'tell application \"Finder\" to make alias file to posix file \"/opt/homebrew/opt/emacs-plus@29/Emacs.app\" at POSIX file \"/Applications\" with properties {name:\"Emacs.app\"}'"

# libby
export LIBBY_OUTPUT_DIR="$HOME/Downloads/"

# ruby
export PATH="$HOME/.gem/bin:$PATH"
export GEM_HOME="$HOME/.gem"

# mdfind
# silence debugging output
function mdfind() {
    { /usr/bin/mdfind "$@" 2> >(grep --invert-match ' \[UserQueryParser\] ' >&2); } 2>/dev/null
}
