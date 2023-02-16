export PATH="/usr/local/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"
export PYENV_ROOT="$HOME/.pyenv" # https://github.com/pyenv/pyenv/issues/1906#issuecomment-834751771
export PATH="$PYENV_ROOT/bin:$PATH"
# alias brew='env PATH="${PATH//$(pyenv root)\/shims:/}" brew' # https://github.com/pyenv/pyenv#homebrew-in-macos
export GOKU_EDN_CONFIG_FILE="/Users/pablostafforini/Dropbox/dotfiles/karabiner/karabiner.edn"
export EDITOR="emacsclient -nw"
# export VISUAL="emacsclient"   # $VISUAL opens in GUI mode
source ".zshenv-secrets"
export OBJECT_STORAGE_REGION_NAME="sa-east-1"
export PATH="/opt/homebrew/opt/node@16/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="/Users/pablostafforini/Library/Python/3.9/bin:$PATH"

# https://stackoverflow.com/a/49711594/4479455
export PATH="/opt/homebrew/opt/python@3.10/libexec/bin:$PATH"

alias python="python3"
export NVM_DIR="$HOME/.nvm"
  [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
export GOPATH=/Users/$USER/go
export PATH=$GOPATH/bin:$PATH
# Leftovers from failed attempt to make DBUS work
# export DBUS_LAUNCHD_SESSION_BUS_SOCKET=`launchctl getenv DBUS_LAUNCHD_SESSION_BUS_SOCKET`
# export DBUS_LAUNCHD_SESSION_BUS_SOCKET=unix:path=/private/tmp/com.apple.launchd.RMRy2cvIPn/unix_domain_listener
# export DBUS_SESSION_BUS_ADDRESS="launchd:env=DBUS_LAUNCHD_SESSION_BUS_SOCKET"
. "$HOME/.cargo/env"
