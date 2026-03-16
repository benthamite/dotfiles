# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# .NET tools
export PATH="$PATH:/Users/pablostafforini/.dotnet/tools"

# GPG
GPG_TTY=$(tty)
export GPG_TTY
