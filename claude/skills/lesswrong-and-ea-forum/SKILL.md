---
name: lesswrong-and-ea-forum
description: Interact with LessWrong and EA Forum. Use when user says "forum digest", "post to lesswrong", "search ea forum", "create draft", "read post", "search posts", or mentions LessWrong/EA Forum content.
---

# LessWrong and EA Forum

Interact with LessWrong, EA Forum, and Alignment Forum via their GraphQL API.

## Supported forums

| Forum | Key | Aliases | URL |
|-------|-----|---------|-----|
| LessWrong | `lesswrong` | `lw`, `less-wrong` | lesswrong.com |
| EA Forum | `eaforum` | `ea`, `effective-altruism` | forum.effectivealtruism.org |
| Alignment Forum | `alignmentforum` | `af`, `alignment` | alignmentforum.org |

## Capabilities

### Read (no auth required)
- Read individual posts
- Read post comments (with threaded structure)
- Search posts by query
- Generate activity digests
- View user activity and topics

### Write (auth required)
- Create draft posts
- List your drafts

## Prerequisites

1. **Python 3** with `requests` library:
   ```bash
   pip install requests
   ```

2. **Configure subscriptions** (for digests):
   ```bash
   cp ~/.claude/skills/lesswrong-and-ea-forum/config.example.json ~/.claude/skills/lesswrong-and-ea-forum/config.json
   ```

3. **Set up authentication** (for creating drafts):
   See "Authentication setup" below.

## Authentication setup

To create draft posts, you need the `loginToken` cookie from your browser:

1. Log into LessWrong or EA Forum in your browser
2. Open Developer Tools (F12 or Cmd+Option+I)
3. Go to Network tab
4. Perform any action (vote, comment, etc.)
5. Find a request to `/graphql`
6. In request headers, find the `cookie` header
7. Copy the `loginToken` value (the hex string after `loginToken=`)
   - Example: `loginToken=bfb232fd2cc7258bb679bbc5d598a00025795fa3691f7eb4d19c28f3602e2616`
   - Copy just: `bfb232fd2cc7258bb679bbc5d598a00025795fa3691f7eb4d19c28f3602e2616`
8. Save the token:
   ```bash
   python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py set-token --forum lw --token "your-loginToken-value"
   ```

Tokens expire in approximately 5 years (set by Meteor).

**Security note**: The token is stored in `config.json`. Keep this file private and don't commit it to version control.

## Workflows

### Read a post

Fetch and display a post by its slug (from the URL):

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py post graphql-tutorial-for-lesswrong --forum lw
```

### Search posts

Search for posts by keyword:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py search "AI safety" --limit 10 --forum lw
```

### Read post comments

Fetch all comments for a post. When the user asks for post comments, ask whether they want:
1. **A digest** - Summarise the main discussion threads
2. **Full comment contents** - Save the complete threaded comments to a markdown file

```bash
# View comment summary (for digest mode)
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py post-comments POST-SLUG --forum lw

# Save full comments to markdown (for full contents mode)
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py post-comments POST-SLUG --forum lw --save

# Get raw JSON data
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py post-comments POST-SLUG --forum lw --json
```

**Full contents mode** saves to: `~/.claude/skills/lesswrong-and-ea-forum/saved-data/YYYY-MM-DD/<post-slug>-comments.md`

The saved markdown preserves the threaded structure with proper nesting of replies.

### Create a draft post

Create a draft from inline content:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py create-draft \
  --title "My Post Title" \
  --content "# Introduction\n\nThis is my post content in markdown." \
  --forum lw
```

Create a draft from a file:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py create-draft \
  --title "My Post Title" \
  --file /path/to/post.md \
  --forum ea
```

Create a link post:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py create-draft \
  --title "Interesting Article" \
  --content "My commentary on this article." \
  --url "https://example.com/article" \
  --forum lw
```

Create a question post:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py create-draft \
  --title "What's the best approach to X?" \
  --content "Context and details about my question." \
  --question \
  --forum lw
```

### List your drafts

View your unpublished draft posts:

```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py my-drafts --forum lw
```

### Generate a digest

For each subscription in config.json, fetch recent activity:

**User subscriptions:**
```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py user-activity daniel-kokotajlo --forum lw --days 7 --json
```

**Topic subscriptions:**
```bash
python ~/.claude/skills/lesswrong-and-ea-forum/scripts/forum_api.py topic-activity ai-safety --forum ea --days 7 --json
```

When generating a digest:
1. Fetch activity for each subscription
2. Read the full content (`contents.markdown`)
3. Generate 1-2 sentence summaries
4. Detect belief updates (phrases like "I was wrong", "I've updated", "changed my mind")
5. Format as markdown with sections by forum, user/topic, and notable belief updates

## Configuration

Edit `~/.claude/skills/lesswrong-and-ea-forum/config.json`:

```json
{
  "auth": {
    "lesswrong": "your-lw-token-or-null",
    "eaforum": "your-ea-token-or-null"
  },
  "subscriptions": [
    {
      "type": "user",
      "forum": "lesswrong",
      "slug": "daniel-kokotajlo"
    },
    {
      "type": "topic",
      "forum": "eaforum",
      "slug": "ai-governance"
    }
  ],
  "digest_days": 7,
  "output_dir": "digests"
}
```

### Configuration fields

| Field | Description |
|-------|-------------|
| `auth.lesswrong` | Auth token for LessWrong (also used for Alignment Forum) |
| `auth.eaforum` | Auth token for EA Forum |
| `subscriptions` | List of users/topics to track for digests |
| `digest_days` | Number of days to look back (default: 7) |
| `output_dir` | Directory for saved digest files |

### Subscription types

**User subscriptions** track a person's posts and comments:
```json
{
  "type": "user",
  "forum": "lesswrong",
  "slug": "daniel-kokotajlo"
}
```

**Topic subscriptions** track posts tagged with a topic:
```json
{
  "type": "topic",
  "forum": "eaforum",
  "slug": "ai-governance"
}
```

## API commands reference

```bash
# List available forums
python forum_api.py list-forums

# Read a post
python forum_api.py post POST-SLUG --forum lw
python forum_api.py post POST-SLUG --forum lw --json

# Read post comments
python forum_api.py post-comments POST-SLUG --forum lw
python forum_api.py post-comments POST-SLUG --forum lw --json
python forum_api.py post-comments POST-SLUG --forum lw --save

# Search posts
python forum_api.py search "query" --limit 20 --forum lw
python forum_api.py search "query" --forum ea --json

# Create a draft (requires auth)
python forum_api.py create-draft --title "Title" --content "Markdown content"
python forum_api.py create-draft --title "Title" --file path.md
python forum_api.py create-draft --title "Title" --content "..." --url "https://..."
python forum_api.py create-draft --title "Title" --content "..." --question

# List your drafts (requires auth)
python forum_api.py my-drafts --forum lw
python forum_api.py my-drafts --limit 10 --json

# Set auth token
python forum_api.py set-token --forum lw --token "your-token"
python forum_api.py set-token --forum ea --token "your-token"

# User commands
python forum_api.py user USERNAME --forum lw
python forum_api.py user-activity USERNAME --days 7 --forum lw
python forum_api.py posts USERNAME --days 14 --forum ea
python forum_api.py comments USERNAME --days 7 --forum lw

# Topic commands
python forum_api.py topic TOPIC-SLUG --forum lw
python forum_api.py topic-activity TOPIC-SLUG --days 7 --forum lw
python forum_api.py search-topics "query" --limit 10 --forum ea
```

## Finding users and topics

**Users**: Find the username from their profile URL:
- `lesswrong.com/users/USERNAME` → slug is `USERNAME`

**Topics**: Find the topic slug from the tag page URL:
- `lesswrong.com/tag/TOPIC-SLUG` → slug is `TOPIC-SLUG`
- You can also search: `python forum_api.py search-topics "query"`

## Troubleshooting

**"User not found"**: Check the username is correct (case-sensitive, use the URL slug not display name)

**"Post not found"**: Check the post slug from the URL. The slug is the part after `/posts/ID/`

**"No auth token configured"**: Run `set-token` command or add token to config.json

**"GraphQL errors"**: The API may have changed or your token may be invalid. Try refreshing your auth token.

**Network errors**: The forum APIs may rate-limit. Wait and retry.

## Notes

- All three forums use the same GraphQL API structure (ForumMagnum)
- LessWrong and Alignment Forum share the same account/auth token
- EA Forum requires a separate auth token
- Drafts are created with `draft: true` and `submitToFrontpage: true`
- Rate limiting may apply for high-volume requests
