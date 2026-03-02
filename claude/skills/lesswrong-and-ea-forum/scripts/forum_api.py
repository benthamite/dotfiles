#!/usr/bin/env python3
"""
Forum GraphQL API Client

Interact with LessWrong, EA Forum, and Alignment Forum via GraphQL API.

Read operations (no auth required):
- Fetch posts, comments, user activity
- Search posts
- Generate activity digests

Write operations (auth required):
- Create draft posts
- List your drafts

API Documentation: https://www.lesswrong.com/posts/LJiGhpq8w4Badr5KJ/graphql-tutorial-for-lesswrong-and-effective-altruism-forum
"""

import argparse
import json
import sys
from datetime import datetime, timedelta
from pathlib import Path

try:
    import requests
except ImportError:
    print("Error: requests library not installed. Run: pip install requests")
    sys.exit(1)

# Forum configurations
FORUMS = {
    "lesswrong": {
        "name": "LessWrong",
        "url": "https://www.lesswrong.com/graphql",
        "base_url": "https://www.lesswrong.com",
        "aliases": ["lw", "less-wrong"]
    },
    "eaforum": {
        "name": "EA Forum",
        "url": "https://forum.effectivealtruism.org/graphql",
        "base_url": "https://forum.effectivealtruism.org",
        "aliases": ["ea", "effective-altruism", "ea-forum"]
    },
    "alignmentforum": {
        "name": "Alignment Forum",
        "url": "https://www.alignmentforum.org/graphql",
        "base_url": "https://www.alignmentforum.org",
        "aliases": ["af", "alignment", "alignment-forum"]
    }
}

SKILL_DIR = Path(__file__).parent.parent
CONFIG_FILE = SKILL_DIR / "config.json"


def resolve_forum(forum_input):
    """Resolve a forum name or alias to its canonical key."""
    forum_input = forum_input.lower().strip()

    # Direct match
    if forum_input in FORUMS:
        return forum_input

    # Check aliases
    for key, config in FORUMS.items():
        if forum_input in config["aliases"]:
            return key

    raise ValueError(f"Unknown forum: {forum_input}. Valid options: {', '.join(FORUMS.keys())}")


def get_forum_url(forum):
    """Get the GraphQL URL for a forum."""
    forum_key = resolve_forum(forum)
    return FORUMS[forum_key]["url"]


def get_forum_base_url(forum):
    """Get the base URL for a forum."""
    forum_key = resolve_forum(forum)
    return FORUMS[forum_key]["base_url"]


def load_config():
    """Load configuration from config.json."""
    if not CONFIG_FILE.exists():
        return {
            "subscriptions": [],
            "digest_days": 7,
            "output_dir": "digests"
        }
    with open(CONFIG_FILE) as f:
        return json.load(f)


def graphql_query(query, variables=None, forum="lesswrong"):
    """Execute a GraphQL query against a forum's API."""
    url = get_forum_url(forum)

    payload = {"query": query}
    if variables:
        payload["variables"] = variables

    response = requests.post(
        url,
        json=payload,
        headers={"Content-Type": "application/json"}
    )
    response.raise_for_status()

    data = response.json()
    if "errors" in data:
        raise Exception(f"GraphQL errors: {data['errors']}")

    return data.get("data", {})


def get_auth_token(forum="lesswrong"):
    """Get auth token for a forum from config.

    Returns None if no token is configured.
    """
    config = load_config()
    auth = config.get("auth", {})

    forum_key = resolve_forum(forum)
    # Map alignmentforum to lesswrong token (same account)
    if forum_key == "alignmentforum":
        forum_key = "lesswrong"

    return auth.get(forum_key)


def graphql_query_authenticated(query, variables=None, forum="lesswrong"):
    """Execute an authenticated GraphQL query.

    Uses cookie-based authentication with loginToken (Meteor auth).
    Raises an exception if no auth token is configured.
    """
    token = get_auth_token(forum)
    if not token:
        raise Exception(
            f"No auth token configured for {forum}. "
            "See 'set-token' command or add token to config.json."
        )

    url = get_forum_url(forum)

    payload = {"query": query}
    if variables:
        payload["variables"] = variables

    # Use cookie-based auth (Meteor loginToken)
    response = requests.post(
        url,
        json=payload,
        headers={"Content-Type": "application/json"},
        cookies={"loginToken": token}
    )
    response.raise_for_status()

    data = response.json()
    if "errors" in data:
        raise Exception(f"GraphQL errors: {data['errors']}")

    return data.get("data", {})


def save_auth_token(forum, token):
    """Save an auth token to config.json."""
    config = load_config()
    if "auth" not in config:
        config["auth"] = {}

    forum_key = resolve_forum(forum)
    # Map alignmentforum to lesswrong (same account)
    if forum_key == "alignmentforum":
        forum_key = "lesswrong"

    config["auth"][forum_key] = token

    with open(CONFIG_FILE, "w") as f:
        json.dump(config, f, indent=2)

    return forum_key


# ============================================================================
# Post reading and searching (no auth required)
# ============================================================================

def get_post_by_slug(identifier, forum="lesswrong"):
    """Fetch a post by ID, slug, or URL.

    Args:
        identifier: Can be:
            - Post ID (e.g., "pT75MFsLJArrBGkaF")
            - Post slug (e.g., "simple-summary-of-ai-safety-laws-1")
            - Full URL (e.g., "https://lesswrong.com/posts/ID/slug")
        forum: Target forum

    Returns:
        Post dict with _id, title, slug, pageUrl, etc.
    """
    import re

    # Extract ID from URL if provided
    url_match = re.search(r'/posts/([a-zA-Z0-9]+)/', identifier)
    if url_match:
        post_id = url_match.group(1)
    # Check if identifier looks like a post ID (alphanumeric, 17 chars)
    elif re.match(r'^[a-zA-Z0-9]{17}$', identifier):
        post_id = identifier
    else:
        post_id = None

    # Query by ID if we have one
    if post_id:
        query = """
        query GetPostById($documentId: String!) {
          post(input: { selector: { documentId: $documentId } }) {
            result {
              _id
              title
              slug
              pageUrl
              postedAt
              baseScore
              voteCount
              commentCount
              user {
                displayName
                slug
              }
              contents {
                markdown
              }
            }
          }
        }
        """
        data = graphql_query(query, {"documentId": post_id}, forum)
        post = data.get("post", {}).get("result")
        if post:
            return post

    # Otherwise, search by slug
    slug = identifier.split('/')[-1]  # Handle both slug and URL-ending-in-slug

    query = """
    query SearchBySlug($limit: Int) {
      posts(input: {
        terms: {
          limit: $limit
        }
      }) {
        results {
          _id
          title
          slug
          pageUrl
          postedAt
          baseScore
          voteCount
          commentCount
          user {
            displayName
            slug
          }
          contents {
            markdown
          }
        }
      }
    }
    """

    data = graphql_query(query, {"limit": 1000}, forum)
    posts = data.get("posts", {}).get("results", [])

    # Find exact slug match
    for post in posts:
        if post.get("slug") == slug:
            return post

    raise Exception(f"Post not found: {identifier}")


def get_post_comments(post_id, limit=500, forum="lesswrong"):
    """Fetch all comments for a post by post ID.

    Args:
        post_id: The post's _id
        limit: Maximum comments to fetch (default 500)
        forum: Target forum

    Returns:
        List of comment dicts with threading info
    """
    query = """
    query GetPostComments($postId: String!, $limit: Int) {
      comments(input: {
        terms: {
          view: "postCommentsTop",
          postId: $postId,
          limit: $limit
        }
      }) {
        results {
          _id
          postedAt
          baseScore
          voteCount
          parentCommentId
          topLevelCommentId
          user {
            displayName
            slug
          }
          contents {
            markdown
            plaintextMainText
          }
        }
      }
    }
    """

    data = graphql_query(query, {"postId": post_id, "limit": limit}, forum)
    return data.get("comments", {}).get("results", [])


def build_comment_tree(comments):
    """Build a nested tree structure from flat comment list.

    Args:
        comments: List of comment dicts with parentCommentId

    Returns:
        List of top-level comments, each with 'replies' containing nested children
    """
    # Index comments by ID
    by_id = {c["_id"]: {**c, "replies": []} for c in comments}

    top_level = []

    for comment in comments:
        comment_with_replies = by_id[comment["_id"]]
        parent_id = comment.get("parentCommentId")

        if parent_id and parent_id in by_id:
            by_id[parent_id]["replies"].append(comment_with_replies)
        else:
            top_level.append(comment_with_replies)

    # Sort top-level by score descending
    top_level.sort(key=lambda c: c.get("baseScore", 0), reverse=True)

    return top_level


def format_comment_tree_markdown(comments, post_title, post_url, indent_level=0):
    """Format a comment tree as markdown with proper nesting.

    Args:
        comments: List of comments (with 'replies' for children)
        post_title: Title of the post
        post_url: URL of the post
        indent_level: Current nesting level (0 = top-level thread)

    Returns:
        Markdown string
    """
    lines = []

    for comment in comments:
        user = comment.get("user", {}) or {}
        username = user.get("displayName", "Anonymous")
        date = format_date(comment.get("postedAt", ""))
        score = comment.get("baseScore", 0)
        contents = comment.get("contents", {}) or {}
        markdown = contents.get("markdown", contents.get("plaintextMainText", "(no content)"))

        # Use header levels based on indent (## for top-level, ### for replies, etc.)
        # Cap at h6
        header_level = min(2 + indent_level, 6)
        header = "#" * header_level

        lines.append(f"{header} {username} · {date} · score: {score}\n")
        lines.append(f"{markdown}\n")

        # Recursively format replies
        if comment.get("replies"):
            lines.append(format_comment_tree_markdown(
                comment["replies"],
                post_title,
                post_url,
                indent_level + 1
            ))

    return "\n".join(lines)


def save_comments_to_markdown(post, comments_tree, forum="lesswrong"):
    """Save comments to a markdown file in saved-data/YYYY-MM-DD/

    Args:
        post: Post dict with title, slug, pageUrl
        comments_tree: Nested comment tree from build_comment_tree()
        forum: Forum key (will be resolved to canonical key)

    Returns:
        Path to the saved file
    """
    from datetime import date as date_module

    today = date_module.today().strftime("%Y-%m-%d")
    save_dir = SKILL_DIR / "saved-data" / today
    save_dir.mkdir(parents=True, exist_ok=True)

    filename = f"{post['slug']}-comments.md"
    filepath = save_dir / filename

    # Count total comments (flatten tree)
    def count_comments(tree):
        total = len(tree)
        for c in tree:
            total += count_comments(c.get("replies", []))
        return total

    total_comments = count_comments(comments_tree)

    # Resolve forum key to get the proper name
    forum_key = resolve_forum(forum)
    forum_name = FORUMS.get(forum_key, {}).get("name", forum)

    # Build markdown
    lines = [
        f"# Comments on: {post['title']}\n",
        f"**Post URL:** {post.get('pageUrl', 'N/A')}",
        f"**Forum:** {forum_name}",
        f"**Fetched:** {datetime.now().strftime('%Y-%m-%d %H:%M')}",
        f"**Total comments:** {total_comments}\n",
        "---\n",
    ]

    # Add each top-level thread
    for i, thread in enumerate(comments_tree, 1):
        lines.append(f"## Thread {i}\n")
        lines.append(format_comment_tree_markdown([thread], post["title"], post.get("pageUrl", ""), indent_level=1))
        lines.append("\n---\n")

    with open(filepath, "w") as f:
        f.write("\n".join(lines))

    return filepath


def search_posts(query_str, limit=20, forum="lesswrong"):
    """Search posts by text query."""
    query = """
    query SearchPosts($searchQuery: String!, $limit: Int) {
      posts(input: {
        terms: {
          query: $searchQuery,
          limit: $limit
        }
      }) {
        results {
          _id
          title
          slug
          pageUrl
          postedAt
          baseScore
          voteCount
          commentCount
          user {
            displayName
            slug
          }
        }
      }
    }
    """

    data = graphql_query(query, {"searchQuery": query_str, "limit": limit}, forum)
    return data.get("posts", {}).get("results", [])


# ============================================================================
# Draft management (auth required)
# ============================================================================

def create_draft_post(title, contents_markdown, forum="lesswrong",
                      url=None, question=False):
    """Create a draft post. Requires authentication.

    Args:
        title: Post title
        contents_markdown: Post body in markdown format
        forum: Target forum (lesswrong, eaforum, alignmentforum)
        url: Optional URL for link posts
        question: Set True to create a question post

    Returns:
        dict with _id, title, slug, pageUrl, draft fields
    """
    mutation = """
    mutation CreatePost($data: CreatePostDataInput!) {
      createPost(data: $data) {
        data {
          _id
          title
          slug
          pageUrl
          draft
        }
      }
    }
    """

    variables = {
        "data": {
            "title": title,
            "contents": {
                "originalContents": {
                    "type": "markdown",
                    "data": contents_markdown
                }
            },
            "draft": True,
            "submitToFrontpage": True
        }
    }

    if url:
        variables["data"]["url"] = url
    if question:
        variables["data"]["question"] = True

    data = graphql_query_authenticated(mutation, variables, forum)
    return data.get("createPost", {}).get("data")


def get_my_drafts(limit=50, forum="lesswrong"):
    """List current user's draft posts. Requires authentication."""
    query = """
    query GetMyDrafts($limit: Int) {
      posts(input: {
        terms: {
          view: "drafts",
          limit: $limit
        }
      }) {
        results {
          _id
          title
          slug
          pageUrl
          createdAt
          modifiedAt
          draft
        }
      }
    }
    """

    data = graphql_query_authenticated(query, {"limit": limit}, forum)
    return data.get("posts", {}).get("results", [])


# ============================================================================
# User-related queries
# ============================================================================

def get_user_by_slug(slug, forum="lesswrong"):
    """Fetch user details by their URL slug/username."""
    query = """
    query GetUser($slug: String!) {
      user(input: { selector: { slug: $slug } }) {
        result {
          _id
          username
          displayName
          slug
          karma
        }
      }
    }
    """

    data = graphql_query(query, {"slug": slug}, forum)
    user = data.get("user", {}).get("result")

    if not user:
        raise Exception(f"User not found: {slug}")

    return user


def get_user_posts(user_id, since_date=None, limit=50, forum="lesswrong"):
    """Fetch posts by a user, optionally filtered by date."""
    query = """
    query GetUserPosts($userId: String!, $limit: Int) {
      posts(input: {
        terms: {
          view: "userPosts",
          userId: $userId,
          limit: $limit
        }
      }) {
        results {
          _id
          title
          slug
          pageUrl
          postedAt
          baseScore
          voteCount
          commentCount
          contents {
            markdown
          }
        }
      }
    }
    """

    data = graphql_query(query, {"userId": user_id, "limit": limit}, forum)
    posts = data.get("posts", {}).get("results", [])

    # Filter by date if specified
    if since_date:
        posts = [
            p for p in posts
            if datetime.fromisoformat(p["postedAt"].replace("Z", "+00:00")) >= since_date
        ]

    return posts


def get_user_comments(user_id, since_date=None, limit=100, forum="lesswrong"):
    """Fetch comments by a user, optionally filtered by date."""
    query = """
    query GetUserComments($userId: String!, $limit: Int) {
      comments(input: {
        terms: {
          view: "profileComments",
          userId: $userId,
          limit: $limit
        }
      }) {
        results {
          _id
          postedAt
          pageUrl
          baseScore
          voteCount
          post {
            _id
            title
            slug
          }
          contents {
            markdown
            plaintextDescription
          }
        }
      }
    }
    """

    data = graphql_query(query, {"userId": user_id, "limit": limit}, forum)
    comments = data.get("comments", {}).get("results", [])

    # Filter by date if specified
    if since_date:
        comments = [
            c for c in comments
            if datetime.fromisoformat(c["postedAt"].replace("Z", "+00:00")) >= since_date
        ]

    return comments


def fetch_user_activity(slug, days=7, forum="lesswrong"):
    """Fetch all recent activity for a user.

    Returns a dict with user info, posts, and comments from the last N days.
    """
    user = get_user_by_slug(slug, forum)
    since_date = datetime.now().astimezone() - timedelta(days=days)

    posts = get_user_posts(user["_id"], since_date, forum=forum)
    comments = get_user_comments(user["_id"], since_date, forum=forum)

    return {
        "forum": forum,
        "user": user,
        "posts": posts,
        "comments": comments,
        "since_date": since_date.isoformat(),
        "fetched_at": datetime.now().isoformat()
    }


# ============================================================================
# Topic/Tag-related queries
# ============================================================================

def get_tag_by_slug(slug, forum="lesswrong"):
    """Fetch tag/topic details by slug.

    The API doesn't support direct slug lookup, so we fetch tags and filter.
    """
    query = """
    query GetTags($limit: Int) {
      tags(input: {
        terms: {
          view: "allTagsAlphabetical",
          limit: $limit
        }
      }) {
        results {
          _id
          name
          slug
          postCount
        }
      }
    }
    """

    data = graphql_query(query, {"limit": 500}, forum)
    tags = data.get("tags", {}).get("results", [])

    # Find the tag by slug
    slug_lower = slug.lower()
    for tag in tags:
        if tag.get("slug", "").lower() == slug_lower:
            return tag

    raise Exception(f"Tag/topic not found: {slug}")


def search_tags(query_str, limit=10, forum="lesswrong"):
    """Search for tags/topics by name."""
    query = """
    query SearchTags($limit: Int) {
      tags(input: {
        terms: {
          view: "allTagsAlphabetical",
          limit: $limit
        }
      }) {
        results {
          _id
          name
          slug
          postCount
        }
      }
    }
    """

    # Note: The forum API doesn't have a proper search, so we fetch and filter
    data = graphql_query(query, {"limit": 200}, forum)
    tags = data.get("tags", {}).get("results", [])

    # Filter by query string (case-insensitive)
    query_lower = query_str.lower()
    matching = [t for t in tags if query_lower in t["name"].lower()]

    return matching[:limit]


def get_posts_by_tag(tag_id, since_date=None, limit=50, forum="lesswrong"):
    """Fetch posts with a specific tag, optionally filtered by date."""
    query = """
    query GetTagPosts($tagId: String!, $limit: Int) {
      posts(input: {
        terms: {
          view: "tagRelevance",
          tagId: $tagId,
          limit: $limit
        }
      }) {
        results {
          _id
          title
          slug
          pageUrl
          postedAt
          baseScore
          voteCount
          commentCount
          user {
            displayName
            slug
          }
          contents {
            markdown
          }
        }
      }
    }
    """

    data = graphql_query(query, {"tagId": tag_id, "limit": limit}, forum)
    posts = data.get("posts", {}).get("results", [])

    # Filter by date if specified
    if since_date:
        posts = [
            p for p in posts
            if datetime.fromisoformat(p["postedAt"].replace("Z", "+00:00")) >= since_date
        ]

    return posts


def fetch_topic_activity(slug, days=7, forum="lesswrong"):
    """Fetch all recent posts for a topic/tag.

    Returns a dict with tag info and posts from the last N days.
    """
    tag = get_tag_by_slug(slug, forum)
    since_date = datetime.now().astimezone() - timedelta(days=days)

    posts = get_posts_by_tag(tag["_id"], since_date, forum=forum)

    return {
        "forum": forum,
        "topic": tag,
        "posts": posts,
        "since_date": since_date.isoformat(),
        "fetched_at": datetime.now().isoformat()
    }


# ============================================================================
# Output formatting
# ============================================================================

def format_date(iso_date):
    """Format ISO date string as readable date."""
    dt = datetime.fromisoformat(iso_date.replace("Z", "+00:00"))
    return dt.strftime("%b %d, %Y")


def print_user_activity(activity):
    """Print a summary of user activity."""
    user = activity["user"]
    posts = activity["posts"]
    comments = activity["comments"]
    forum_name = FORUMS.get(activity.get("forum", "lesswrong"), {}).get("name", "Forum")

    print(f"\n{'='*60}")
    print(f"[{forum_name}] {user.get('displayName', user['slug'])} (@{user['slug']})")
    print(f"Karma: {user.get('karma', 'N/A')}")
    print(f"{'='*60}")

    print(f"\nPosts ({len(posts)}):")
    if posts:
        for post in posts:
            date = format_date(post["postedAt"])
            score = post.get("baseScore", 0)
            print(f"  [{date}] {post['title']} (score: {score})")
            print(f"    {post.get('pageUrl', '')}")
    else:
        print("  No posts in this period.")

    print(f"\nComments ({len(comments)}):")
    if comments:
        for comment in comments[:10]:  # Show first 10
            date = format_date(comment["postedAt"])
            score = comment.get("baseScore", 0)
            post_title = comment.get("post", {}).get("title", "Unknown post")
            contents = comment.get("contents", {}) or {}
            excerpt = contents.get("plaintextDescription", "")[:100]
            print(f"  [{date}] On: {post_title} (score: {score})")
            print(f"    \"{excerpt}...\"")
            print(f"    {comment.get('pageUrl', '')}")
        if len(comments) > 10:
            print(f"  ... and {len(comments) - 10} more comments")
    else:
        print("  No comments in this period.")

    print()


def print_topic_activity(activity):
    """Print a summary of topic activity."""
    topic = activity["topic"]
    posts = activity["posts"]
    forum_name = FORUMS.get(activity.get("forum", "lesswrong"), {}).get("name", "Forum")

    print(f"\n{'='*60}")
    print(f"[{forum_name}] Topic: {topic['name']}")
    print(f"Total posts: {topic.get('postCount', 'N/A')}")
    if topic.get("description", {}).get("plaintextDescription"):
        desc = topic["description"]["plaintextDescription"][:200]
        print(f"Description: {desc}...")
    print(f"{'='*60}")

    print(f"\nRecent posts ({len(posts)}):")
    if posts:
        for post in posts:
            date = format_date(post["postedAt"])
            score = post.get("baseScore", 0)
            author = post.get("user", {}).get("displayName", "Unknown")
            print(f"  [{date}] {post['title']} by {author} (score: {score})")
            print(f"    {post.get('pageUrl', '')}")
    else:
        print("  No posts in this period.")

    print()


# ============================================================================
# CLI
# ============================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Forum API Client for LessWrong, EA Forum, and Alignment Forum",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Forums:
  lesswrong (lw)      - LessWrong.com
  eaforum (ea)        - forum.effectivealtruism.org
  alignmentforum (af) - alignmentforum.org

Examples:
  Read a post:
    python forum_api.py post graphql-tutorial-for-lesswrong

  Search posts:
    python forum_api.py search "AI safety" --limit 10

  Create a draft (requires auth):
    python forum_api.py create-draft --title "My Post" --content "# Hello"
    python forum_api.py create-draft --title "My Post" --file post.md

  List your drafts (requires auth):
    python forum_api.py my-drafts

  Set auth token:
    python forum_api.py set-token --forum lw --token "your-token-here"

  Fetch user activity:
    python forum_api.py user-activity daniel-kokotajlo --days 7

  Fetch topic activity:
    python forum_api.py topic-activity ai-safety --days 14

  Search for topics:
    python forum_api.py search-topics "alignment"
"""
    )

    parser.add_argument("--forum", "-f", default="lesswrong",
                        help="Forum to query: lesswrong (lw), eaforum (ea), alignmentforum (af)")

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # User info
    user_parser = subparsers.add_parser("user", help="Get user info")
    user_parser.add_argument("slug", help="User slug/username")

    # User activity
    user_activity_parser = subparsers.add_parser("user-activity", help="Get user activity")
    user_activity_parser.add_argument("slug", help="User slug/username")
    user_activity_parser.add_argument("--days", "-d", type=int, default=7,
                                       help="Number of days to look back (default: 7)")
    user_activity_parser.add_argument("--json", "-j", action="store_true",
                                       help="Output as JSON")

    # Topic info
    topic_parser = subparsers.add_parser("topic", help="Get topic/tag info")
    topic_parser.add_argument("slug", help="Topic slug")

    # Topic activity
    topic_activity_parser = subparsers.add_parser("topic-activity", help="Get topic activity")
    topic_activity_parser.add_argument("slug", help="Topic slug")
    topic_activity_parser.add_argument("--days", "-d", type=int, default=7,
                                        help="Number of days to look back (default: 7)")
    topic_activity_parser.add_argument("--json", "-j", action="store_true",
                                        help="Output as JSON")

    # Search topics
    search_parser = subparsers.add_parser("search-topics", help="Search for topics/tags")
    search_parser.add_argument("query", help="Search query")
    search_parser.add_argument("--limit", "-l", type=int, default=10,
                                help="Maximum results (default: 10)")

    # Posts by user
    posts_parser = subparsers.add_parser("posts", help="Get user posts")
    posts_parser.add_argument("slug", help="User slug/username")
    posts_parser.add_argument("--days", "-d", type=int, default=7,
                               help="Number of days to look back")
    posts_parser.add_argument("--json", "-j", action="store_true",
                               help="Output as JSON")

    # Comments by user
    comments_parser = subparsers.add_parser("comments", help="Get user comments")
    comments_parser.add_argument("slug", help="User slug/username")
    comments_parser.add_argument("--days", "-d", type=int, default=7,
                                  help="Number of days to look back")
    comments_parser.add_argument("--json", "-j", action="store_true",
                                  help="Output as JSON")

    # List forums
    subparsers.add_parser("list-forums", help="List available forums")

    # Read a single post
    post_parser = subparsers.add_parser("post", help="Read a single post by slug")
    post_parser.add_argument("slug", help="Post slug (from URL)")
    post_parser.add_argument("--json", "-j", action="store_true",
                              help="Output as JSON")

    # Get comments for a post
    post_comments_parser = subparsers.add_parser("post-comments",
                                                  help="Get all comments for a post")
    post_comments_parser.add_argument("identifier", help="Post slug, ID, or URL")
    post_comments_parser.add_argument("--limit", "-l", type=int, default=500,
                                       help="Maximum comments to fetch (default: 500)")
    post_comments_parser.add_argument("--json", "-j", action="store_true",
                                       help="Output as JSON")
    post_comments_parser.add_argument("--save", "-s", action="store_true",
                                       help="Save to markdown file in saved-data/")

    # Search posts
    search_posts_parser = subparsers.add_parser("search", help="Search posts")
    search_posts_parser.add_argument("query", help="Search query")
    search_posts_parser.add_argument("--limit", "-l", type=int, default=20,
                                      help="Maximum results (default: 20)")
    search_posts_parser.add_argument("--json", "-j", action="store_true",
                                      help="Output as JSON")

    # Create draft post (requires auth)
    create_draft_parser = subparsers.add_parser("create-draft",
                                                 help="Create a draft post (requires auth)")
    create_draft_parser.add_argument("--title", "-t", required=True,
                                      help="Post title")
    create_draft_parser.add_argument("--content", "-c",
                                      help="Post content (markdown)")
    create_draft_parser.add_argument("--file",
                                      help="Read content from file")
    create_draft_parser.add_argument("--url", "-u",
                                      help="URL for link posts")
    create_draft_parser.add_argument("--question", "-q", action="store_true",
                                      help="Create as question post")

    # List user's drafts (requires auth)
    drafts_parser = subparsers.add_parser("my-drafts",
                                           help="List your draft posts (requires auth)")
    drafts_parser.add_argument("--limit", "-l", type=int, default=50,
                                help="Maximum results (default: 50)")
    drafts_parser.add_argument("--json", "-j", action="store_true",
                                help="Output as JSON")

    # Set auth token
    set_token_parser = subparsers.add_parser("set-token",
                                              help="Set auth token for a forum")
    set_token_parser.add_argument("--token", "-t", required=True,
                                   help="Auth token (from browser dev tools)")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    try:
        forum = args.forum if hasattr(args, 'forum') else "lesswrong"

        if args.command == "list-forums":
            print("Available forums:")
            for key, config in FORUMS.items():
                aliases = ", ".join(config["aliases"])
                print(f"  {key} ({aliases})")
                print(f"    {config['name']}: {config['base_url']}")

        elif args.command == "user":
            user = get_user_by_slug(args.slug, forum)
            print(json.dumps(user, indent=2))

        elif args.command == "user-activity":
            activity = fetch_user_activity(args.slug, args.days, forum)
            if args.json:
                print(json.dumps(activity, indent=2, default=str))
            else:
                print_user_activity(activity)

        elif args.command == "topic":
            topic = get_tag_by_slug(args.slug, forum)
            print(json.dumps(topic, indent=2))

        elif args.command == "topic-activity":
            activity = fetch_topic_activity(args.slug, args.days, forum)
            if args.json:
                print(json.dumps(activity, indent=2, default=str))
            else:
                print_topic_activity(activity)

        elif args.command == "search-topics":
            topics = search_tags(args.query, args.limit, forum)
            print(f"Topics matching '{args.query}':")
            for topic in topics:
                print(f"  {topic['name']} (slug: {topic['slug']}, posts: {topic.get('postCount', 'N/A')})")

        elif args.command == "posts":
            user = get_user_by_slug(args.slug, forum)
            since_date = datetime.now().astimezone() - timedelta(days=args.days)
            posts = get_user_posts(user["_id"], since_date, forum=forum)
            if args.json:
                print(json.dumps(posts, indent=2))
            else:
                print(f"Posts by {args.slug} (last {args.days} days):")
                for post in posts:
                    print(f"  - {post['title']}")
                    print(f"    {post.get('pageUrl', '')}")

        elif args.command == "comments":
            user = get_user_by_slug(args.slug, forum)
            since_date = datetime.now().astimezone() - timedelta(days=args.days)
            comments = get_user_comments(user["_id"], since_date, forum=forum)
            if args.json:
                print(json.dumps(comments, indent=2))
            else:
                print(f"Comments by {args.slug} (last {args.days} days): {len(comments)}")
                for comment in comments[:10]:
                    post_title = comment.get("post", {}).get("title", "Unknown")
                    print(f"  - On: {post_title}")
                    print(f"    {comment.get('pageUrl', '')}")

        elif args.command == "post":
            post = get_post_by_slug(args.slug, forum)
            if args.json:
                print(json.dumps(post, indent=2))
            else:
                date = format_date(post["postedAt"])
                author = post.get("user", {}).get("displayName", "Unknown")
                print(f"\n{post['title']}")
                print(f"By {author} | {date}")
                print(f"Score: {post.get('baseScore', 0)} | Comments: {post.get('commentCount', 0)}")
                print(f"URL: {post.get('pageUrl', '')}")
                print(f"\n{'='*60}\n")
                contents = post.get("contents", {}) or {}
                markdown = contents.get("markdown", "(No content)")
                print(markdown)

        elif args.command == "post-comments":
            # Get the post first to get its ID
            post = get_post_by_slug(args.identifier, forum)
            comments = get_post_comments(post["_id"], args.limit, forum)
            comments_tree = build_comment_tree(comments)

            if args.json:
                print(json.dumps(comments, indent=2))
            elif args.save:
                filepath = save_comments_to_markdown(post, comments_tree, forum)
                print(f"Comments saved to: {filepath}")
            else:
                # Print summary
                print(f"\nComments on: {post['title']}")
                print(f"Total comments: {len(comments)}")
                print(f"Top-level threads: {len(comments_tree)}")
                print(f"\n{'='*60}\n")

                for i, thread in enumerate(comments_tree[:10], 1):
                    user = thread.get("user", {}) or {}
                    username = user.get("displayName", "Anonymous")
                    score = thread.get("baseScore", 0)
                    reply_count = len(thread.get("replies", []))
                    contents = thread.get("contents", {}) or {}
                    excerpt = contents.get("plaintextMainText", "")[:150]

                    print(f"Thread {i}: {username} (score: {score}, {reply_count} replies)")
                    print(f"  \"{excerpt}...\"")
                    print()

                if len(comments_tree) > 10:
                    print(f"... and {len(comments_tree) - 10} more threads")
                print(f"\nUse --save to save full comments to markdown file")

        elif args.command == "search":
            results = search_posts(args.query, args.limit, forum)
            if args.json:
                print(json.dumps(results, indent=2))
            else:
                print(f"Posts matching '{args.query}' ({len(results)} results):\n")
                for post in results:
                    date = format_date(post["postedAt"])
                    author = post.get("user", {}).get("displayName", "Unknown")
                    score = post.get("baseScore", 0)
                    print(f"  [{date}] {post['title']}")
                    print(f"    By {author} | Score: {score}")
                    print(f"    {post.get('pageUrl', '')}\n")

        elif args.command == "create-draft":
            # Get content from --content or --file
            if args.content:
                contents_markdown = args.content
            elif args.file:
                with open(args.file, "r") as f:
                    contents_markdown = f.read()
            else:
                print("Error: Must specify --content or --file")
                sys.exit(1)

            draft = create_draft_post(
                title=args.title,
                contents_markdown=contents_markdown,
                forum=forum,
                url=args.url,
                question=args.question
            )

            print(f"Draft created successfully!")
            print(f"  Title: {draft['title']}")
            print(f"  ID: {draft['_id']}")
            print(f"  URL: {draft.get('pageUrl', 'N/A')}")

        elif args.command == "my-drafts":
            drafts = get_my_drafts(args.limit, forum)
            if args.json:
                print(json.dumps(drafts, indent=2))
            else:
                print(f"Your drafts ({len(drafts)}):\n")
                for draft in drafts:
                    modified = draft.get("modifiedAt") or draft.get("createdAt", "")
                    if modified:
                        modified = format_date(modified)
                    print(f"  {draft['title']}")
                    print(f"    Modified: {modified}")
                    print(f"    URL: {draft.get('pageUrl', 'N/A')}\n")

        elif args.command == "set-token":
            saved_forum = save_auth_token(forum, args.token)
            print(f"Auth token saved for {saved_forum}")
            print(f"Token stored in: {CONFIG_FILE}")

    except requests.HTTPError as e:
        print(f"HTTP Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
