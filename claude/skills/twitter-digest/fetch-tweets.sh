#!/usr/bin/env bash
# Fetch tweets for twitter-digest skill.
# Reads the list file, checks last-run cutoff, fetches all accounts,
# and outputs everything Claude needs for triage in one shot.
#
# Usage: fetch-tweets.sh <list-name>
#        fetch-tweets.sh <list-name> <override-cutoff>
#
# Output: a self-contained block with metadata header + compact tweet lines.
# Requires: TWITTERAPI_API_KEY (or account-specific variant) env var

set -euo pipefail

# Resolve TWITTERAPI_API_KEY from account-specific env vars if not set directly.
if [[ -z "${TWITTERAPI_API_KEY:-}" ]]; then
  if [[ "${CLAUDE_CONFIG_DIR:-}" == *epoch* ]] && [[ -n "${TWITTERAPI_API_KEY_EPOCH:-}" ]]; then
    TWITTERAPI_API_KEY="$TWITTERAPI_API_KEY_EPOCH"
  elif [[ -n "${TWITTERAPI_API_KEY_TLON:-}" ]]; then
    TWITTERAPI_API_KEY="$TWITTERAPI_API_KEY_TLON"
  fi
fi
# Resolve op:// references via 1Password CLI.
if [[ "${TWITTERAPI_API_KEY:-}" == op://* ]]; then
  TWITTERAPI_API_KEY=$(op read "$TWITTERAPI_API_KEY")
fi
export TWITTERAPI_API_KEY

SKILL_DIR="${SKILL_DIR:-$HOME/.claude/skills/twitter-digest}"
LIST_NAME="$1"
if [[ "$LIST_NAME" =~ [/\\] ]] || [[ "$LIST_NAME" == *..* ]]; then
  echo "ERROR: invalid list name: $LIST_NAME" >&2
  exit 1
fi
LIST_FILE="$SKILL_DIR/lists/$LIST_NAME.md"

if [[ ! -f "$LIST_FILE" ]]; then
  echo "ERROR: list file not found: $LIST_FILE" >&2
  exit 1
fi
if [[ -z "${TWITTERAPI_API_KEY:-}" ]]; then
  echo "ERROR: TWITTERAPI_API_KEY env var not set" >&2
  exit 1
fi

# --- Read list metadata ---
DESCRIPTION=$(python3 -c "
import sys
in_front = False
for line in open(sys.argv[1]):
    line = line.strip()
    if line == '---':
        if in_front: break
        in_front = True; continue
    if in_front and line.startswith('description:'):
        print(line[len('description:'):].strip())
        break
" "$LIST_FILE")
LIST_USERNAMES=$(sed -n 's/^- @\([^ ]*\).*/\1/p' "$LIST_FILE")

if [[ -z "$LIST_USERNAMES" ]]; then
  echo "ERROR: no usernames found in $LIST_FILE" >&2
  exit 1
fi

# --- Resolve cutoff ---
CUTOFF="${2:-}"
if [[ -z "$CUTOFF" ]]; then
  LAST_RUN_FILE="$SKILL_DIR/last-run/$LIST_NAME.txt"
  if [[ -f "$LAST_RUN_FILE" ]]; then
    CUTOFF=$(cat "$LAST_RUN_FILE")
  else
    # Default: 48 hours ago
    CUTOFF=$(date -u -v-48H +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -d '48 hours ago' +%Y-%m-%dT%H:%M:%SZ)
  fi
fi

# --- Output header ---
echo "LIST:$LIST_NAME"
echo "CUTOFF:$CUTOFF"
echo "DESCRIPTION:$DESCRIPTION"
echo "---TWEETS---"

# --- Fetch helpers ---
TMPDIR=$(mktemp -d /tmp/twitter-fetch-XXXXXX)
trap 'rm -rf "$TMPDIR"' EXIT

fetch_one() {
  local user="$1" outfile="$TMPDIR/$user.txt"
  curl -sf "https://api.twitterapi.io/twitter/user/last_tweets?userName=$user" \
    -H "X-API-Key: $TWITTERAPI_API_KEY" \
    -o "$TMPDIR/$user.json" 2>/dev/null || return 0

  python3 -c "
import json, sys, re
from datetime import datetime
cutoff_str = sys.argv[2] if len(sys.argv) > 2 and sys.argv[2] else None
cutoff_dt = None
if cutoff_str:
    try: cutoff_dt = datetime.fromisoformat(cutoff_str.replace('Z', '+00:00'))
    except ValueError: pass
with open(sys.argv[1]) as f: data = json.load(f)
for t in data.get('data', {}).get('tweets', []):
    ds = t.get('createdAt', '')
    if cutoff_dt and ds:
        try:
            if datetime.strptime(ds, '%a %b %d %H:%M:%S %z %Y') <= cutoff_dt: continue
        except ValueError: pass
    text = t.get('text', '')[:300].replace('\n', ' ').replace('|', '/')
    user = t.get('author', {}).get('userName', '?')
    likes, views = t.get('likeCount', 0), t.get('viewCount', 0)
    tid = t.get('id', '')
    rt = t.get('retweetedTweet')
    is_rt, rt_user = bool(rt), ''
    if is_rt and rt:
        rt_user = rt.get('author', {}).get('userName', '')
        text = rt.get('text', text)[:300].replace('\n', ' ').replace('|', '/')
        likes, views = rt.get('likeCount', likes), rt.get('viewCount', views)
    elif text.startswith('RT @'):
        m = re.match(r'RT @(\w+):', text)
        if m: is_rt, rt_user = True, m.group(1)
    print(f'@{user}|{ds[:25]}|{likes}|{views}|{\"RT\" if is_rt else \"OG\"}|{tid}|{rt_user}|{text}')
" "$TMPDIR/$user.json" "$CUTOFF" > "$outfile" 2>/dev/null || true
}

parallel_fetch() {
  local pids=() count=0
  for user in $1; do
    fetch_one "$user" &
    pids+=($!)
    count=$((count + 1))
    if (( count % 8 == 0 )); then
      for p in "${pids[@]}"; do wait "$p" 2>/dev/null || true; done
      pids=()
    fi
  done
  for p in "${pids[@]}"; do wait "$p" 2>/dev/null || true; done
}

# --- Phase 1: Listed accounts ---
parallel_fetch "$LIST_USERNAMES"

LISTED_LOWER=$(echo "$LIST_USERNAMES" | tr '[:upper:]' '[:lower:]' | sort -u)
RT_USERS=""

for f in "$TMPDIR"/*.txt; do
  [[ -f "$f" ]] || continue
  while IFS='|' read -r user date likes views type tid rt_user text; do
    echo "$user|$date|$likes|$views|$type|$tid|$rt_user|$text"
    if [[ "$type" == "RT" && -n "$rt_user" ]]; then
      rt_lower=$(echo "$rt_user" | tr '[:upper:]' '[:lower:]')
      echo "$LISTED_LOWER" | grep -qx "$rt_lower" || RT_USERS="$RT_USERS $rt_user"
    fi
  done < "$f"
done

# --- Phase 2: RT-discovered authors ---
RT_UNIQUE=$(echo "$RT_USERS" | tr ' ' '\n' | sort -u | grep -v '^$' || true)
if [[ -n "$RT_UNIQUE" ]]; then
  rm -f "$TMPDIR"/*.txt "$TMPDIR"/*.json
  parallel_fetch "$RT_UNIQUE"
  echo "---RT_DISCOVERY---"
  for f in "$TMPDIR"/*.txt; do
    [[ -f "$f" ]] || continue
    cat "$f"
  done
  echo "---RT_AUTHORS---"
  for f in "$TMPDIR"/*.json; do
    [[ -f "$f" ]] || continue
    python3 -c "
import json, sys
with open(sys.argv[1]) as f: data = json.load(f)
tweets = data.get('data', {}).get('tweets', [])
if not tweets: sys.exit(0)
author = tweets[0].get('author')
if not author: sys.exit(0)
uname = author.get('userName', '')
followers = author.get('followers', 0)
bio = author.get('description', '')
bio = bio.replace('\n', ' ').replace('|', '/').strip()[:200]
print(f'@{uname}|{followers}|{bio}')
" "$f" 2>/dev/null || true
  done
fi
