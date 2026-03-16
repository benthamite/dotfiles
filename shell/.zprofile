# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# .NET tools
export PATH="$PATH:/Users/pablostafforini/.dotnet/tools"

# GPG-encrypted secrets (needs pinentry, so must run in a login shell with GUI context)
GPG_TTY=$(tty)
export GPG_TTY
export MBSYNC_PASSWORD=$(gpg --quiet --decrypt "$HOME/.password-store/auth-sources/mbsync/password.gpg")
