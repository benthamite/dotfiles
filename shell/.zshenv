# export DBUS_LAUNCHD_SESSION_BUS_SOCKET=`launchctl getenv DBUS_LAUNCHD_SESSION_BUS_SOCKET`
# export DBUS_LAUNCHD_SESSION_BUS_SOCKET=unix:path=/private/tmp/com.apple.launchd.RMRy2cvIPn/unix_domain_listener
# export DBUS_SESSION_BUS_ADDRESS="launchd:env=DBUS_LAUNCHD_SESSION_BUS_SOCKET"
export PATH="/usr/local/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"
# export PIPENV_VENV_IN_PROJECT=1 # https://www.reddit.com/r/spacemacs/comments/aer220/help_setting_up_python_integration/edsj5es/?utm_source=reddit&utm_medium=web2x&context=3
export PYENV_ROOT="$HOME/.pyenv" # https://github.com/pyenv/pyenv/issues/1906#issuecomment-834751771
export PATH="$PYENV_ROOT/bin:$PATH"
# alias brew='env PATH="${PATH//$(pyenv root)\/shims:/}" brew' # https://github.com/pyenv/pyenv#homebrew-in-macos
export GOKU_EDN_CONFIG_FILE="/Users/pablostafforini/Dropbox/org/karabiner/karabiner.edn"
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c -n -a ''"         # $VISUAL opens in GUI mode
export COIN_MARKET_CAP_API_KEY="58d45be2-5e34-4ed0-a588-7f5d395f2128"
export OBJECT_STORAGE_ACCESS_KEY_ID="AKIATCKDUHFSN45GLIG4"
export OBJECT_STORAGE_SECRET="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"
export OBJECT_STORAGE_REGION_NAME="sa-east-1"
export PATH="/opt/homebrew/opt/node@16/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="/Users/pablostafforini/Library/Python/3.9/bin:$PATH"
alias python="python3"
export NVM_DIR="$HOME/.nvm"
  [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
export GOPATH=/Users/$USER/go
export PATH=$GOPATH/bin:$PATH
export ENCHANT_CONFIG_DIR="/Users/pablostafforini/.config/enchant"
