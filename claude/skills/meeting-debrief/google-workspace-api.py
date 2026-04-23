#!/usr/bin/env python3
"""Gmail helper for meeting-debrief skill.

Archives a Gemini notification email by removing the INBOX label. Handles
OAuth token refresh using credentials from environment variables, keeping
secrets out of the skill's prompt context.

Google Docs / Drive operations are handled by `gdoc` (see SKILL.md); this
helper only covers the Gmail call that the `google-workspace-epoch` MCP
server doesn't expose.

Usage:
  google-workspace-api.py archive-email <email_id>
"""

import json
import os
import sys
import urllib.parse
import urllib.request


def get_workspace_access_token():
    """Get an access token using Google Workspace (Epoch) OAuth credentials."""
    client_id = os.environ["GOOGLE_WORKSPACE_CLIENT_ID"]
    client_secret = os.environ["GOOGLE_WORKSPACE_CLIENT_SECRET"]
    refresh_token = os.environ["GOOGLE_WORKSPACE_REFRESH_TOKEN"]

    data = urllib.parse.urlencode(
        {
            "client_id": client_id,
            "client_secret": client_secret,
            "refresh_token": refresh_token,
            "grant_type": "refresh_token",
        }
    ).encode()
    req = urllib.request.Request("https://oauth2.googleapis.com/token", data=data)
    resp = urllib.request.urlopen(req)
    return json.loads(resp.read())["access_token"]


def archive_email(email_id):
    access_token = get_workspace_access_token()
    archive_data = json.dumps({"removeLabelIds": ["INBOX"]}).encode()
    req = urllib.request.Request(
        f"https://gmail.googleapis.com/gmail/v1/users/me/messages/{email_id}/modify",
        data=archive_data,
        headers={
            "Authorization": f"Bearer {access_token}",
            "Content-Type": "application/json",
        },
    )
    resp = urllib.request.urlopen(req)
    result = json.loads(resp.read())
    print("Archived. Labels now:", result.get("labelIds", []))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    cmd = sys.argv[1]
    if cmd == "archive-email" and len(sys.argv) == 3:
        archive_email(sys.argv[2])
    else:
        print(__doc__)
        sys.exit(1)
