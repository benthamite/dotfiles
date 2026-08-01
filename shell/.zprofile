# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# Restore shim precedence after Homebrew's path_helper.
dotfiles_prefer_shims

# .NET tools
export PATH="$PATH:/Users/pablostafforini/.dotnet/tools"

# GPG
GPG_TTY=$(tty)
export GPG_TTY
