# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH="$PATH:/Users/pablostafforini/.dotnet/tools"

# Enable pyenv also in noninteractive shells
# This was added to fix the Emacs problem but it didn’t work
# so consider deleting
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
