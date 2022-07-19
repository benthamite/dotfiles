export PATH="/usr/local/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"
# export PIPENV_VENV_IN_PROJECT=1 # https://www.reddit.com/r/spacemacs/comments/aer220/help_setting_up_python_integration/edsj5es/?utm_source=reddit&utm_medium=web2x&context=3
export PYENV_ROOT="$HOME/.pyenv" # https://github.com/pyenv/pyenv/issues/1906#issuecomment-834751771
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi
# alias brew='env PATH="${PATH//$(pyenv root)\/shims:/}" brew' # https://github.com/pyenv/pyenv#homebrew-in-macos
alias ptop='tput cup $((LINES/4)) 0'  # Clear quarter
alias pmid='tput cup $((LINES/2)) 0'  # Clear half
alias pdown='tput cup $((3*LINES/4)) 0' # Clear 3/4th
export GOKU_EDN_CONFIG_FILE="/Users/pablostafforini/Dropbox/org/karabiner/karabiner.edn"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
export COIN_MARKET_CAP_API_KEY="58d45be2-5e34-4ed0-a588-7f5d395f2128"
export OBJECT_STORAGE_ACCESS_KEY_ID="AKIATCKDUHFSN45GLIG4"
export OBJECT_STORAGE_SECRET="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"
export OBJECT_STORAGE_REGION_NAME="sa-east-1"
# useful only for Mac OS Silicon M1, 
# still working but useless for the other platforms
docker() {
 if [[ `uname -m` == "arm64" ]] && [[ "$1" == "run" || "$1" == "build" ]]; then
    /usr/local/bin/docker "$1" --platform linux/amd64 "${@:2}"
  else
     /usr/local/bin/docker "$@"
  fi
}
