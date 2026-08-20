# Prevent high-confidence credential patterns from reaching persistent history.
source "$HOME/My Drive/dotfiles/shell/zsh-history-security.zsh"

# nvm lazy loading (NVM_DIR is in .zshenv; path must also be set here
# because macOS path_helper in /etc/zprofile reorders PATH after .zshenv)
export PATH="$NVM_DIR/versions/node/v20.18.2/bin:$PATH"

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
emacsk() {
  local pid ppid
  for pid in $(pgrep -x Emacs); do
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
# Trajectory org disabled claude.ai sign-in, so this account must auth via its
# provisioned API key. Inject the key (single source of truth: the CR studio
# .env) and flip the shim's allow-flag for THIS process only -- never exported,
# so plain `claude` and the other accounts stay key-free and unaffected.
claude-trajectory() {
  local keyfile=~/Trajectory/reasoning-tasks/reasoning-tasks-cr-studio/.claude/.env
  local key; key=$(grep -m1 '^ANTHROPIC_API_KEY=' "$keyfile" | cut -d= -f2-)
  if [ -z "$key" ]; then echo "claude-trajectory: no ANTHROPIC_API_KEY in $keyfile" >&2; return 1; fi
  CLAUDE_CONFIG_DIR=~/.claude-trajectory CLAUDE_CODE_ALLOW_API_KEY_AUTH=1 ANTHROPIC_API_KEY="$key" claude "$@"
}

# Trajectory reasoning-tasks: create a new task worktree + wire its API-key symlink in one step.
# Worktrees live OUTSIDE the repository checkout, under the shared external
# root ~/repos/.worktrees/<repository>/<branch> — never inside ~/My Drive or
# the repo tree itself.
# Usage: newtask <task-slug>   (e.g. newtask compensate-misaligned-ais)
# Then: cd ~/repos/.worktrees/reasoning-tasks/pablo/<task-slug> && claude-trajectory
newtask() {
  if [ -z "$1" ]; then echo "usage: newtask <task-slug>"; return 1; fi
  local root=~/Trajectory/reasoning-tasks
  local wt="$HOME/repos/.worktrees/reasoning-tasks/pablo/$1"
  git -C "$root/main" fetch origin main &&
    mkdir -p "${wt%/*}" &&
    git -C "$root/main" worktree add "$wt" -b "pablo/$1" origin/main &&
    mkdir -p "$wt/.claude" &&
    ln -s "$root/reasoning-tasks-cr-studio/.claude/.env" "$wt/.claude/.env" &&
    echo "ready: cd $wt && claude-trajectory"
}

# Manually merge origin/main into the current reasoning-tasks worktree (skills/docs/etc).
# Runs the same conflict-gated sync that fires automatically at session start.
syncreasoningtasks() {
  SYNC_REASONING_TASKS_VERBOSE=1 ~/My\ Drive/dotfiles/claude/hooks/sync-reasoning-tasks-worktree.sh </dev/null
}

# Legacy name kept during the agent-c -> reasoning-tasks migration.
syncagentc() {
  syncreasoningtasks "$@"
}

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
