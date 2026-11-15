#!/bin/bash
# Stdin → stdout filter that masks well-formed secret literals.
#
# Used as the sink of the guard engine's output wrapping
# (agent_guard.policies.dispatcher for Claude, codex_wrap for Codex) so
# any tokens that escape into a shell command's stdout/stderr are
# replaced before the model sees them. Patterns are anchored to recognizable prefixes / shapes —
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
# Second layer: this machine's actual secret values (bin/secret-index), for
# secrets with no recognisable shape. Streams line by line. Callers that run
# the index themselves (redact-tool-output.py) set REDACT_SKIP_INDEX=1.
#
# Credential-bearing URL parameters (the rule right after the PEM block) are
# masked by name, whatever the value's shape: on 2026-11-15 a process listing
# printed a Slack websocket URL whose `token` was masked by its `xoxc-` shape
# but whose `frt` reconnect token, which has no known shape, was not
# (docs/secret-guard.md). A parameter is a name below after `?`, `&`, `;`
# (`&amp;`), their percent or JSON escapes (a URL nested in another), or at
# the start of a line (form-encoded bodies such as OAuth token responses),
# followed by `=` or `%3D` and a value of 12 or more URL characters; the
# value ends at `&`, `%26`, `#`, whitespace, a control character (the output
# hooks join strings with one), a quote or a bracket, so code
# that builds such a URL (`?token={token}`, `"?token=" + t`, `${TOKEN}`) is
# left alone. Names: `token` and its `access_`, `refresh_`, `id_`, `auth_`,
# `session_`, `oauth_`, `api_` and `X-Amz-Security-` forms; `api_key`,
# `apikey`, `api-key`; `client_secret`, `secret` (otpauth TOTP seeds);
# `password`, `passwd`; `sig`, `signature`, `X-Amz-Signature`,
# `X-Goog-Signature` (signed URLs grant access until they expire);
# `ticket` (Slack Socket Mode, CAS); `frt` (Slack); `jwt`. Left out because
# they mostly carry non-secrets: `key` (issue and sort keys; Google API keys
# have their own shape rule), `code` (error and country codes), `pwd` (Zoom
# join links), `state`, and pagination tokens (`pageToken`, `next_token`).
INDEX_PASS=(python3 -B "$(dirname "$0")/../../bin/secret-index" mask --stream)
[ "${REDACT_SKIP_INDEX:-}" = 1 ] && INDEX_PASS=(cat)
set -o pipefail

LC_ALL=C "$SED_BIN" -E --unbuffered \
  ${EXTRA_FILE[@]+"${EXTRA_FILE[@]}"} \
  -e '/-----BEGIN [A-Z ]*PRIVATE KEY-----/{:a; /-----END [A-Z ]*PRIVATE KEY-----/!{$!{N;ba}}; s/-----BEGIN [A-Z ]*PRIVATE KEY-----[^-]*-----END [A-Z ]*PRIVATE KEY-----/[PRIVATE_KEY_REDACTED]/g}' \
  -e 's/(^|[?&;]|%3F|%26|\\u0026|\\u003F)((access_|refresh_|id_|auth_|session_|oauth_|api_|x-amz-security-)?token|api[_-]?key|client_secret|secret|passw(or)?d|(x-amz-|x-goog-|oauth_)?sig(nature)?|ticket|frt|jwt)(=|%3D)(%[013-9A-Fa-f][0-9A-Fa-f]|%2[0-57-9A-Fa-f]|\\\/|[^]%&#;,[:space:][:cntrl:]"'\''`<>(){}[\$|]){12,}/\1\2\7[URL_CREDENTIAL_REDACTED]/gI' \
  -e 's/gg_pat_[A-Za-z0-9_-]{40,80}/[GG_PAT_REDACTED]/g' \
  -e 's/AIza[0-9A-Za-z_-]{35}/[GOOGLE_API_KEY_REDACTED]/g' \
  -e 's/GOCSPX-[A-Za-z0-9_-]{20,}/[GOOGLE_OAUTH_CLIENT_SECRET_REDACTED]/g' \
  -e 's#(^|[^A-Za-z0-9/])1//[A-Za-z0-9_-]{30,}#\1[GOOGLE_REFRESH_TOKEN_REDACTED]#g' \
  -e 's/ya29\.[A-Za-z0-9_-]{20,}/[GOOGLE_ACCESS_TOKEN_REDACTED]/g' \
  -e 's/ghp_[A-Za-z0-9]{36,40}/[GITHUB_PAT_REDACTED]/g' \
  -e 's/ghs_[A-Za-z0-9]{36,40}/[GITHUB_PAT_REDACTED]/g' \
  -e 's/gho_[A-Za-z0-9]{36,40}/[GITHUB_OAUTH_REDACTED]/g' \
  -e 's/github_pat_[A-Za-z0-9_]{40,90}/[GITHUB_FINEGRAINED_PAT_REDACTED]/g' \
  -e 's/sk-ant-[a-z0-9_-]{3,}-[A-Za-z0-9_-]{50,}/[ANTHROPIC_KEY_REDACTED]/g' \
  -e 's/sk-proj-[A-Za-z0-9_-]{40,}/[OPENAI_PROJECT_KEY_REDACTED]/g' \
  -e 's/sk-[A-Za-z0-9]{20,}/[OPENAI_KEY_REDACTED]/g' \
  -e 's/xox[bporasct]-[A-Za-z0-9-]{20,}/[SLACK_TOKEN_REDACTED]/g' \
  -e 's/AKIA[0-9A-Z]{16}/[AWS_ACCESS_KEY_REDACTED]/g' \
  -e 's/cf[a-z]t_[A-Za-z0-9_-]{30,}/[CLOUDFLARE_TOKEN_REDACTED]/g' \
  -e 's/(^|[^A-Za-z])s%3A[A-Za-z0-9._%-]{30,}/\1[SUBSTACK_SESSION_REDACTED]/g' \
  -e 's/\b[0-9]{8,10}[:][A-Za-z0-9_-]{35}\b/[TELEGRAM_TOKEN_REDACTED]/g' \
  -e 's#(postgres(ql)?|mysql|mongodb)[:]/+[^[:space:]:@/]+[:][^[:space:]@/]+[@]#\1://[CRED_REDACTED]@#g' \
  -e 's/([Bb]earer|[Tt]oken)[[:space:]]+[A-Za-z0-9._/+=-]{30,}/\1 [REDACTED]/g' \
  -e 's/(Authorization|x-api-key)[[:space:]]*:[[:space:]]*[A-Za-z0-9._/+=-]{20,}/\1: [REDACTED]/gi' \
  | python3 -B "$(dirname "$0")/../../lib/python/redact_numeric_credentials.py" \
  | "${INDEX_PASS[@]}"
