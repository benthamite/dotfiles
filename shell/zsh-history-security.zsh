# Keep credential-shaped commands out of persistent interactive-shell history.
# zsh still executes a command when a zshaddhistory hook returns 1.

autoload -Uz add-zsh-hook

_dotfiles_history_contains_credential() {
    local entry="$1"
    local pattern
    local -a patterns=(
        'AKIA[0-9A-Z]{16}'
        'gh[ps]_[A-Za-z0-9_]{36,}'
        'github_pat_[A-Za-z0-9_]{22,}'
        'xox[bporca]-[A-Za-z0-9-]{10,}'
        'sk-ant-[A-Za-z0-9_-]{40,}'
        '(^|[^A-Za-z0-9])sk-[A-Za-z0-9_-]{20,}'
        '(sk|pk|rk)_(live|test)_[A-Za-z0-9]{20,}'
        'lin_api_[A-Za-z0-9]{40,}'
        'glpat-[A-Za-z0-9_-]{20,}'
        'AIza[0-9A-Za-z_-]{35}'
    )

    for pattern in "${patterns[@]}"; do
        [[ "$entry" =~ $pattern ]] && return 0
    done
    return 1
}

_dotfiles_omit_credential_history() {
    if _dotfiles_history_contains_credential "$1"; then
        print -u2 -- "Warning: credential-shaped command omitted from history."
        return 1
    fi
    return 0
}

add-zsh-hook zshaddhistory _dotfiles_omit_credential_history
