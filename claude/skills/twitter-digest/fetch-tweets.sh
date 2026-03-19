#!/usr/bin/env bash
# Fetch tweets for a list of accounts via twitterapi.io REST API,
# extract only the fields needed for triage, filter by cutoff date,
# and optionally fetch RT-discovered authors too.
#
# Usage: fetch-tweets.sh <list-file> [cutoff-iso-timestamp]
#
# Output (stdout), two sections separated by "---RT_DISCOVERY---":
#   Section 1 (listed accounts):  @user|date|likes|views|OG/RT|tweet_id|rt_user|text
#   Section 2 (discovered via RT): same format, only new authors
#
# Requires: TWITTER_API_IO env var

set -euo pipefail

LIST_FILE="$1"
CUTOFF="${2:-}"

if [[ -z "${TWITTER_API_IO:-}" ]]; then
  echo "ERROR: TWITTER_API_IO env var not set" >&2
  exit 1
fi

LIST_USERNAMES=$(sed -n 's/^- @\([^ ]*\).*/\1/p' "$LIST_FILE")
if [[ -z "$LIST_USERNAMES" ]]; then
  echo "ERROR: no usernames found in $LIST_FILE" >&2
  exit 1
fi

TMPDIR=$(mktemp -d /tmp/twitter-fetch-XXXXXX)
trap 'rm -rf "$TMPDIR"' EXIT

# Fetch + extract compact tweet lines for one user
fetch_one() {
  local user="$1" outfile="$TMPDIR/$user.txt"
  curl -sf "https://api.twitterapi.io/twitter/user/last_tweets?userName=$user" \
    -H "X-API-Key: $TWITTER_API_IO" \
    -o "$TMPDIR/$user.json" 2>/dev/null || { return 0; }

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

# Parallel fetch with throttling
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

# --- Phase 1: Fetch listed accounts ---
parallel_fetch "$LIST_USERNAMES"

LISTED_LOWER=$(echo "$LIST_USERNAMES" | tr '[:upper:]' '[:lower:]' | sort -u)

# Collect tweets and find RT discovery candidates
RT_USERS=""
for f in "$TMPDIR"/*.txt; do
  [[ -f "$f" ]] || continue
  while IFS='|' read -r user date likes views type tid rt_user text; do
    echo "$user|$date|$likes|$views|$type|$tid|$rt_user|$text"
    if [[ "$type" == "RT" && -n "$rt_user" ]]; then
      rt_lower=$(echo "$rt_user" | tr '[:upper:]' '[:lower:]')
      if ! echo "$LISTED_LOWER" | grep -qx "$rt_lower"; then
        RT_USERS="$RT_USERS $rt_user"
      fi
    fi
  done < "$f"
done

# --- Phase 2: Fetch RT-discovered authors ---
RT_UNIQUE=$(echo "$RT_USERS" | tr ' ' '\n' | sort -u | grep -v '^$' || true)
if [[ -n "$RT_UNIQUE" ]]; then
  # Clean phase 1 files to avoid mixing
  rm -f "$TMPDIR"/*.txt "$TMPDIR"/*.json
  parallel_fetch "$RT_UNIQUE"

  echo "---RT_DISCOVERY---"
  for f in "$TMPDIR"/*.txt; do
    [[ -f "$f" ]] || continue
    cat "$f"
  done
fi
