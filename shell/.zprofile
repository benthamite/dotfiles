# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# Restore shim precedence after Homebrew's path_helper.
path=("$HOME/My Drive/dotfiles/shell/shims" ${path:#"$HOME/My Drive/dotfiles/shell/shims"})

# .NET tools
export PATH="$PATH:/Users/pablostafforini/.dotnet/tools"

# GPG
GPG_TTY=$(tty)
export GPG_TTY
