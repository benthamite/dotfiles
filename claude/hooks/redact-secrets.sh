#!/bin/bash
# Stdin → stdout filter that masks well-formed secret literals.
#
# Used as the sink of `wrap-bash-output.sh` so any tokens that escape
# into a Bash command's stdout/stderr are replaced before the model
# sees them. Patterns are anchored to recognizable prefixes / shapes —
# the false-positive cost is one `[…_REDACTED]` token in the rare line
# that happens to match a prefix unintentionally.
#
# Local additions belong in `redact-secrets.local.sed` (optional).

set -u

USER_SED="$HOME/.claude/redact-secrets.local.sed"

# `LC_ALL=C` + `--unbuffered` so sed processes byte-streams predictably
# and large outputs don't sit in pipe buffers.
EXTRA_FILE=()
[ -r "$USER_SED" ] && EXTRA_FILE=(-f "$USER_SED")

# GNU sed required for `\b` word boundaries + `--unbuffered`. macOS BSD sed
# doesn't support either. Homebrew installs GNU sed as `gsed`.
SED_BIN=$(command -v gsed || command -v sed)

# PEM blocks span many lines, so we slurp lines into the pattern space
# with a `:a / N / ba` loop bounded by the END marker, then collapse the
# whole block on one substitution. Performed FIRST so subsequent line-
# oriented rules don't see the base64 body. Single-line substitutions
# follow on each line independently.
LC_ALL=C exec "$SED_BIN" -E --unbuffered \
  ${EXTRA_FILE[@]+"${EXTRA_FILE[@]}"} \
  -e '/-----BEGIN [A-Z ]*PRIVATE KEY-----/{:a; /-----END [A-Z ]*PRIVATE KEY-----/!{$!{N;ba}}; s/-----BEGIN [A-Z ]*PRIVATE KEY-----[^-]*-----END [A-Z ]*PRIVATE KEY-----/[PRIVATE_KEY_REDACTED]/g}' \
  -e 's/gg_pat_[A-Za-z0-9_-]{40,80}/[GG_PAT_REDACTED]/g' \
  -e 's/AIza[0-9A-Za-z_-]{35}/[GOOGLE_API_KEY_REDACTED]/g' \
  -e 's/ghp_[A-Za-z0-9]{36,40}/[GITHUB_PAT_REDACTED]/g' \
  -e 's/ghs_[A-Za-z0-9]{36,40}/[GITHUB_PAT_REDACTED]/g' \
  -e 's/gho_[A-Za-z0-9]{36,40}/[GITHUB_OAUTH_REDACTED]/g' \
  -e 's/github_pat_[A-Za-z0-9_]{40,90}/[GITHUB_FINEGRAINED_PAT_REDACTED]/g' \
  -e 's/sk-ant-[a-z0-9_-]{3,}-[A-Za-z0-9_-]{50,}/[ANTHROPIC_KEY_REDACTED]/g' \
  -e 's/sk-proj-[A-Za-z0-9_-]{40,}/[OPENAI_PROJECT_KEY_REDACTED]/g' \
  -e 's/sk-[A-Za-z0-9]{20,}/[OPENAI_KEY_REDACTED]/g' \
  -e 's/xox[bporasct]-[A-Za-z0-9-]{20,}/[SLACK_TOKEN_REDACTED]/g' \
  -e 's/AKIA[0-9A-Z]{16}/[AWS_ACCESS_KEY_REDACTED]/g' \
  -e 's/\b[0-9]{8,10}[:][A-Za-z0-9_-]{35}\b/[TELEGRAM_TOKEN_REDACTED]/g' \
  -e 's#(postgres(ql)?|mysql|mongodb)[:]/+[^[:space:]:@/]+[:][^[:space:]@/]+[@]#\1://[CRED_REDACTED]@#g' \
  -e 's/([Bb]earer|[Tt]oken)[[:space:]]+[A-Za-z0-9._/+=-]{30,}/\1 [REDACTED]/g' \
  -e 's/(Authorization|x-api-key)[[:space:]]*:[[:space:]]*[A-Za-z0-9._/+=-]{20,}/\1: [REDACTED]/gi'
