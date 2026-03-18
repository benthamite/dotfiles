#!/usr/bin/env bash
# Fetch tweets for a list of accounts via twitterapi.io REST API,
# extract only the fields we need, and filter by cutoff date.
#
# Usage: fetch-tweets.sh <list-file> [cutoff-iso-timestamp] [extra-usernames...]
#
# Output (stdout): pipe-delimited lines:
#   @user|date|likes|views|OG/RT|tweet_id|rt_user|text
#
# Output (stderr): "RT_DISCOVERY:@username" lines for RT authors not in the list.
#
# Requires: TWITTER_API_IO env var (API key)

set -euo pipefail

LIST_FILE="$1"
CUTOFF="${2:-}"
shift 2 2>/dev/null || true
EXTRA_USERS="$*"

if [[ -z "${TWITTER_API_IO:-}" ]]; then
  echo "ERROR: TWITTER_API_IO env var not set" >&2
  exit 1
fi

# Extract usernames from list file (lines matching "- @username")
LIST_USERNAMES=$(sed -n 's/^- @\([^ ]*\).*/\1/p' "$LIST_FILE")

# Combine list usernames + any extra usernames passed as arguments
ALL_USERNAMES="$LIST_USERNAMES"
if [[ -n "$EXTRA_USERS" ]]; then
  for u in $EXTRA_USERS; do
    # Strip leading @ if present
    ALL_USERNAMES="$ALL_USERNAMES
${u#@}"
  done
fi

if [[ -z "$ALL_USERNAMES" ]]; then
  echo "ERROR: no usernames found" >&2
  exit 1
fi

TMPDIR=$(mktemp -d /tmp/twitter-fetch-XXXXXX)
trap 'rm -rf "$TMPDIR"' EXIT

# Fetch tweets for a single account, extract compact format
fetch_one() {
  local user="$1"
  local outfile="$TMPDIR/$user.txt"

  curl -sf "https://api.twitterapi.io/twitter/user/last_tweets?userName=$user" \
    -H "X-API-Key: $TWITTER_API_IO" \
    -o "$TMPDIR/$user.json" 2>/dev/null || {
    echo "WARN: failed to fetch @$user" >&2
    return 0
  }

  python3 -c "
import json, sys
from datetime import datetime

cutoff = sys.argv[2] if len(sys.argv) > 2 and sys.argv[2] else None
if cutoff:
    cutoff = cutoff.replace('Z', '+00:00')
    try:
        cutoff_dt = datetime.fromisoformat(cutoff)
    except ValueError:
        cutoff_dt = None
else:
    cutoff_dt = None

with open(sys.argv[1]) as f:
    data = json.load(f)

tweets = data.get('data', {}).get('tweets', [])
for t in tweets:
    date_str = t.get('createdAt', '')
    if cutoff_dt and date_str:
        try:
            tweet_dt = datetime.strptime(date_str, '%a %b %d %H:%M:%S %z %Y')
            if tweet_dt <= cutoff_dt:
                continue
        except ValueError:
            pass

    text = t.get('text', '')[:300].replace('\n', ' ').replace('|', '/')
    user = t.get('author', {}).get('userName', '?')
    likes = t.get('likeCount', 0)
    views = t.get('viewCount', 0)
    tid = t.get('id', '')
    rt = t.get('retweetedTweet')
    is_rt = bool(rt)
    rt_user = ''
    if is_rt and rt:
        rt_user = rt.get('author', {}).get('userName', '')
        text = rt.get('text', text)[:300].replace('\n', ' ').replace('|', '/')
        likes = rt.get('likeCount', likes)
        views = rt.get('viewCount', views)
    elif text.startswith('RT @'):
        # API sometimes inlines RTs without retweetedTweet object
        import re
        m = re.match(r'RT @(\w+):', text)
        if m:
            is_rt = True
            rt_user = m.group(1)
    print(f'@{user}|{date_str[:25]}|{likes}|{views}|{\"RT\" if is_rt else \"OG\"}|{tid}|{rt_user}|{text}')
" "$TMPDIR/$user.json" "$CUTOFF" > "$outfile" 2>/dev/null || true
}

# Fetch all accounts with controlled parallelism (8 at a time)
PIDS=()
COUNT=0
MAX_PARALLEL=8

for user in $ALL_USERNAMES; do
  fetch_one "$user" &
  PIDS+=($!)
  COUNT=$((COUNT + 1))

  if (( COUNT % MAX_PARALLEL == 0 )); then
    for pid in "${PIDS[@]}"; do
      wait "$pid" 2>/dev/null || true
    done
    PIDS=()
  fi
done

# Wait for remaining
for pid in "${PIDS[@]}"; do
  wait "$pid" 2>/dev/null || true
done

# Collect listed usernames (lowercase) for RT discovery filtering
LISTED_LOWER=$(echo "$LIST_USERNAMES" | tr '[:upper:]' '[:lower:]' | sort -u)

# Output all tweet lines; emit RT discovery candidates on stderr
for f in "$TMPDIR"/*.txt; do
  [[ -f "$f" ]] || continue
  while IFS='|' read -r user date likes views type tid rt_user text; do
    echo "$user|$date|$likes|$views|$type|$tid|$rt_user|$text"
    if [[ "$type" == "RT" && -n "$rt_user" ]]; then
      rt_lower=$(echo "$rt_user" | tr '[:upper:]' '[:lower:]')
      if ! echo "$LISTED_LOWER" | grep -qx "$rt_lower"; then
        echo "RT_DISCOVERY:@$rt_user" >&2
      fi
    fi
  done < "$f"
done
