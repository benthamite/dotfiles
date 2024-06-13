export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"

# local vars
source ~/.zvars

# secrets
source "$DOTFILES/shell/.zshenv-secrets"

export PATH="/opt/homebrew/opt/node/bin:$PATH"

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# https://stackoverflow.com/a/49711594/4479455
export PATH="/opt/homebrew/opt/python@3.11/libexec/bin:$PATH"

# go
export GOPATH=/Users/$USER/go
export PATH=$GOPATH/bin:$PATH

# rust
# . "$HOME/.cargo/env"

# emacs
export EDITOR="emacsclient -nw"
export VISUAL="$EDITOR"

# gdcv
export PATH="$PATH:$HOME/source/gdcv"

# pdflatex
# to allow Emacs to find pdflatex
export PATH="/Library/TeX/texbin:$PATH"

# make node use local certs
export NODE_EXTRA_CA_CERTS="$HOME/Library/Application Support/mkcert/rootCA.pem"
