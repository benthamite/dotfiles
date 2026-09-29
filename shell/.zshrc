# Prevent high-confidence credential patterns from reaching persistent history.
# Located relative to this file's real path so it works from any checkout.
source "${${(%):-%N}:A:h}/zsh-history-security.zsh"

# nvm lazy loading (NVM_DIR is in .zshenv). The default alias is `system`,
# so loading nvm does not silently restore an end-of-life project runtime.

# No post-install npm wrapper here: the old node_modules relocation hook
# (move node_modules to a cache dir and symlink it back into Drive) was the
# root cause of recreated Drive symlinks. Dependency state is now kept out
# of ~/My Drive entirely — the agent-side guard denies npm installs under
# Drive, and repositories are migrated to external workspaces.
load_nvm() {
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
}

nvm() {
    unset -f nvm node npm npx
    load_nvm
    nvm "$@"
}

node() {
    unset -f nvm node npm npx
    load_nvm
    node "$@"
}

npm() {
    unset -f nvm node npm npx
    load_nvm
    npm "$@"
}

npx() {
    unset -f nvm node npm npx
    load_nvm
    npx "$@"
}

# pyenv setup with lazy loading
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$PYENV_ROOT/shims:$PATH"

pyenv() {
    unset -f pyenv
    eval "$(command pyenv init -)"
    eval "$(command pyenv init --path)"
    pyenv "$@"
}

# Python aliases and functions
alias python="python3"
mkvenv() {
    if [[ "$PWD" == "$HOME/My Drive"* ]]; then
        echo "mkvenv: refusing to create a virtualenv under ~/My Drive; migrate the repository or run the workflow from its approved external workspace." >&2
        return 1
    fi
    CURR_VENV=$(basename "$(pwd)")
    echo "Creating venv for $CURR_VENV at $(pwd)/.venv"
    python3 -m venv .venv --prompt "$CURR_VENV"
    source .venv/bin/activate
    python -m pip install --upgrade pip
    pip install --upgrade setuptools wheel
}

# GPG setup
GPG_TTY=$(tty)
export GPG_TTY

# Tool configurations
export GOKU_EDN_CONFIG_FILE="$DOTFILES/karabiner/karabiner.edn"
export LIBBY_OUTPUT_DIR="$HOME/Downloads/"

# mdfind wrapper
function mdfind() {
    /usr/bin/mdfind "$@" 2>&1 | grep -v '\[UserQueryParser\]'
}

# mu alias
alias muinit="cd ~; mu init --maildir=$HOME/Mail --personal-address=$PERSONAL_EMAIL --personal-address=$PERSONAL_GMAIL --personal-address=$WORK_EMAIL --personal-address=$UNI_EMAIL; mu index"

# Emacs aliases
# Break a busy interactive Emacs into the Lisp debugger (debug-on-event
# defaults to sigusr2).  Signal only top-level Emacs processes: a plain
# `pkill -SIGUSR2 Emacs` also hits batch children spawned by Emacs
# (package retrieval workers, elpaca builds, test subprocesses), whose
# armed debugger exits them with status 255 at their next activity.
# Batch processes not spawned by Emacs (launchd jobs such as vara-refresh
# run `emacs --batch` under bash) are skipped by command line for the same
# reason: on 2026-08-27 a broadcast killed a launchd refresh mid-run.
emacsk() {
  local pid ppid
  for pid in $(pgrep -x Emacs); do
    case "$(ps -o args= -p "$pid" 2>/dev/null)" in
      *--batch*|*-batch*) continue ;;
    esac
    ppid=$(ps -o ppid= -p "$pid" | tr -d ' ')
    case "$(ps -o comm= -p "$ppid" 2>/dev/null)" in
      *Emacs*) ;;
      *) kill -USR2 "$pid" ;;
    esac
  done
}
emacsK() { while true; do emacsk; done }

# Claude Code multi-account (separate OAuth sessions via config dir)
alias claude-personal='CLAUDE_CONFIG_DIR=~/.claude-personal claude'
alias claude-tlon='CLAUDE_CONFIG_DIR=~/.claude-tlon claude'
alias claude-epoch='CLAUDE_CONFIG_DIR=~/.claude-epoch claude'
# make node use local certs
export NODE_EXTRA_CA_CERTS="$HOME/Library/Application Support/mkcert/rootCA.pem"

# EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
    source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Docker
export DOCKER_BUILDKIT=1
export COMPOSE_BAKE=1

# Local override file (at end so it can override anything above)
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# Last word on PATH: keep shell/shims ahead of /opt/homebrew/bin after every
# prepend above (pyenv, nvm, brew shellenv in .zprofile). Defined in .zshenv.
# This is also the PATH the agent harness captures into its shell snapshot, so
# getting the order right here is what makes the snapshot correct.
dotfiles_prefer_shims
