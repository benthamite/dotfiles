#!/usr/bin/env bash
# twitterapi.sh — thin curl wrapper around api.twitterapi.io.
# Replaces the twitterapi-io MCP server. Used by the twitter, twitter-discover,
# twitter-vet, and twitter-digest skills.
#
# Usage:
#   twitterapi.sh tweet <tweet-id> [<tweet-id>...]
#   twitterapi.sh replies <tweet-id> [--type=Latest|Likes|Relevance] [--cursor=...]
#   twitterapi.sh tweets <username> [--include-replies] [--cursor=...]
#   twitterapi.sh user <username>
#   twitterapi.sh search <query> [--type=Latest|Top] [--cursor=...]
#   twitterapi.sh followers <username> [--page-size=200] [--cursor=...]
#   twitterapi.sh following <username> [--page-size=200] [--cursor=...]
#   twitterapi.sh users <query> [--cursor=...]
#
# Output: raw JSON from the API on stdout. Non-zero exit on error.
# Auth: resolves TWITTERAPI_API_KEY from the active Claude account.

set -euo pipefail

# --- Auth resolution -------------------------------------------------------

if [[ -z "${TWITTERAPI_API_KEY:-}" ]]; then
  config_dir="${CLAUDE_CONFIG_DIR:-}"
  config_dir="${config_dir%/}"
  case "$config_dir" in
    */.claude-epoch)
      TWITTERAPI_API_KEY="${TWITTERAPI_API_KEY_EPOCH:-}"
      ;;
    */.claude-personal|*/.claude-tlon|"")
      TWITTERAPI_API_KEY="${TWITTERAPI_API_KEY_TLON:-}"
      ;;
    *)
      echo "ERROR: unknown CLAUDE_CONFIG_DIR for twitterapi.sh: $config_dir" >&2
      exit 1
      ;;
  esac
fi

if [[ -z "${TWITTERAPI_API_KEY:-}" ]]; then
  echo "ERROR: no Twitter API key set for the active account" >&2
  exit 1
fi

if [[ "$TWITTERAPI_API_KEY" == op://* ]]; then
  TWITTERAPI_API_KEY=$(op read "$TWITTERAPI_API_KEY")
fi

export TWITTERAPI_API_KEY

# --- Helpers ---------------------------------------------------------------

BASE_URL="https://api.twitterapi.io"

# Issue a GET request. Args after the path are passed as `--data-urlencode k=v`
# pairs. Outputs the raw response body. Exits non-zero on HTTP error.
api_get() {
  local path="$1"; shift
  local -a urlencoded=()
  for kv in "$@"; do
    urlencoded+=(--data-urlencode "$kv")
  done
  curl -sf -G "$BASE_URL$path" \
    -H "X-API-Key: $TWITTERAPI_API_KEY" \
    "${urlencoded[@]}"
}

# Parse `--key=value` style args into an associative array.
# Usage: declare -A opts; parse_opts opts "$@"
# Sets each --key=value into opts[key]=value.
parse_opts() {
  local -n _opts=$1; shift
  for arg in "$@"; do
    case "$arg" in
      --*=*)
        local k="${arg%%=*}"; k="${k#--}"
        _opts["$k"]="${arg#*=}"
        ;;
      --*)
        local k="${arg#--}"
        _opts["$k"]=true
        ;;
      *)
        echo "ERROR: unexpected positional arg: $arg" >&2
        return 1
        ;;
    esac
  done
}

usage() {
  sed -n '4,18p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-1}"
}

# --- Subcommands -----------------------------------------------------------

cmd_tweet() {
  [[ $# -ge 1 ]] || { echo "ERROR: tweet requires at least one tweet ID" >&2; exit 1; }
  local ids
  ids=$(IFS=,; echo "$*")
  api_get /twitter/tweets "tweet_ids=$ids"
}

cmd_replies() {
  [[ $# -ge 1 ]] || { echo "ERROR: replies requires a tweet ID" >&2; exit 1; }
  local id="$1"; shift
  declare -A opts
  parse_opts opts "$@"
  local -a params=("tweetId=$id")
  [[ -n "${opts[type]:-}" ]] && params+=("queryType=${opts[type]}")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/tweet/replies/v2 "${params[@]}"
}

cmd_tweets() {
  [[ $# -ge 1 ]] || { echo "ERROR: tweets requires a username" >&2; exit 1; }
  local user="${1#@}"; shift
  declare -A opts
  parse_opts opts "$@"
  local -a params=("userName=$user")
  [[ -n "${opts[include-replies]:-}" ]] && params+=("includeReplies=true")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/user/last_tweets "${params[@]}"
}

cmd_user() {
  [[ $# -ge 1 ]] || { echo "ERROR: user requires a username" >&2; exit 1; }
  local user="${1#@}"
  api_get /twitter/user/info "userName=$user"
}

cmd_search() {
  [[ $# -ge 1 ]] || { echo "ERROR: search requires a query" >&2; exit 1; }
  local query="$1"; shift
  declare -A opts
  parse_opts opts "$@"
  local qtype="${opts[type]:-Top}"
  local -a params=("query=$query" "queryType=$qtype")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/tweet/advanced_search "${params[@]}"
}

cmd_followers() {
  [[ $# -ge 1 ]] || { echo "ERROR: followers requires a username" >&2; exit 1; }
  local user="${1#@}"; shift
  declare -A opts
  parse_opts opts "$@"
  local -a params=("userName=$user")
  [[ -n "${opts[page-size]:-}" ]] && params+=("pageSize=${opts[page-size]}")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/user/followers "${params[@]}"
}

cmd_following() {
  [[ $# -ge 1 ]] || { echo "ERROR: following requires a username" >&2; exit 1; }
  local user="${1#@}"; shift
  declare -A opts
  parse_opts opts "$@"
  local -a params=("userName=$user")
  [[ -n "${opts[page-size]:-}" ]] && params+=("pageSize=${opts[page-size]}")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/user/followings "${params[@]}"
}

cmd_users() {
  [[ $# -ge 1 ]] || { echo "ERROR: users requires a query" >&2; exit 1; }
  local query="$1"; shift
  declare -A opts
  parse_opts opts "$@"
  local -a params=("query=$query")
  [[ -n "${opts[cursor]:-}" ]] && params+=("cursor=${opts[cursor]}")
  api_get /twitter/user/search "${params[@]}"
}

# --- Dispatch --------------------------------------------------------------

[[ $# -ge 1 ]] || usage 1

cmd="$1"; shift
case "$cmd" in
  tweet)     cmd_tweet "$@" ;;
  replies)   cmd_replies "$@" ;;
  tweets)    cmd_tweets "$@" ;;
  user)      cmd_user "$@" ;;
  search)    cmd_search "$@" ;;
  followers) cmd_followers "$@" ;;
  following) cmd_following "$@" ;;
  users)     cmd_users "$@" ;;
  -h|--help|help) usage 0 ;;
  *) echo "ERROR: unknown subcommand: $cmd" >&2; usage 1 ;;
esac
