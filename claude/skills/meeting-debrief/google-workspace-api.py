#!/usr/bin/env python3
"""Google Workspace API helper for meeting-debrief skill.

Handles OAuth token refresh and API calls using credentials from environment
variables, keeping secrets out of the skill's prompt context.

Usage:
  google-workspace-api.py archive-email <email_id>
  google-workspace-api.py share-doc <doc_id> <email>
  google-workspace-api.py create-shortcut <doc_id> <name> <parent_folder_id>
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


def get_personal_access_token():
    """Get an access token using gdrive's stored credentials for the personal account."""
    home = os.path.expanduser("~")
    tokens_path = os.path.join(
        home, ".config/gdrive3/pablo.stafforini@gmail.com/tokens.json"
    )
    secret_path = os.path.join(
        home, ".config/gdrive3/pablo.stafforini@gmail.com/secret.json"
    )

    with open(tokens_path) as f:
        tokens = json.load(f)
    with open(secret_path) as f:
        secret = json.load(f)

    token_data = tokens[0]["token"]

    data = urllib.parse.urlencode(
        {
            "client_id": secret["client_id"],
            "client_secret": secret["client_secret"],
            "refresh_token": token_data["refresh_token"],
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


def share_doc(doc_id, email):
    access_token = get_workspace_access_token()
    perm_data = json.dumps(
        {"role": "reader", "type": "user", "emailAddress": email}
    ).encode()
    req = urllib.request.Request(
        f"https://www.googleapis.com/drive/v3/files/{doc_id}/permissions",
        data=perm_data,
        headers={
            "Authorization": f"Bearer {access_token}",
            "Content-Type": "application/json",
        },
    )
    resp = urllib.request.urlopen(req)
    print("Shared:", json.loads(resp.read()))


def create_shortcut(doc_id, name, parent_folder_id):
    access_token = get_personal_access_token()
    shortcut_metadata = {
        "name": name,
        "mimeType": "application/vnd.google-apps.shortcut",
        "shortcutDetails": {"targetId": doc_id},
        "parents": [parent_folder_id],
    }
    req = urllib.request.Request(
        "https://www.googleapis.com/drive/v3/files",
        data=json.dumps(shortcut_metadata).encode(),
        headers={
            "Authorization": f"Bearer {access_token}",
            "Content-Type": "application/json",
        },
    )
    resp = urllib.request.urlopen(req)
    print(json.loads(resp.read()))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    cmd = sys.argv[1]
    if cmd == "archive-email" and len(sys.argv) == 3:
        archive_email(sys.argv[2])
    elif cmd == "share-doc" and len(sys.argv) == 4:
        share_doc(sys.argv[2], sys.argv[3])
    elif cmd == "create-shortcut" and len(sys.argv) == 5:
        create_shortcut(sys.argv[2], sys.argv[3], sys.argv[4])
    else:
        print(__doc__)
        sys.exit(1)
