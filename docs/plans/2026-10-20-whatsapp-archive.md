# Read-only WhatsApp archive for agents

Status: proposed, awaiting review (2026-10-20). Not implemented.

## Problem

Agents have no local path to Pablo's WhatsApp. Today they read it through
WhatsApp Web in Chrome (`claude/context/service-access.md`, "WhatsApp"). That
path is slow (scrolling), virtualizes long chats so page text skips messages,
shows only the history the phone sends to the linked browser, and lets a
misclick open an unrelated private chat. Signal already has the target shape:
sgn stores every message in SQLite and `bin/sgn-read` reads it read-only with
no send path. WhatsApp should follow the same principle.

## Design

1. **Daemon (`wa-archive`)**: a small Go program written for this purpose on
   `go.mau.fi/whatsmeow` (the library behind the mautrix WhatsApp bridge). It
   links to Pablo's account as a companion device (QR code shown once in the
   terminal), receives live messages and the initial history sync, and writes
   them to SQLite. It contains no code that sends messages, reactions, receipts
   beyond what the protocol requires to stay connected, or presence.
   - Source: new local git repository `~/repos/wa-archive` (no remote for now).
   - Data: `~/.local/share/wa-archive/` — whatsmeow's device store
     (`device.db`, the session keys) and `messages.db` (chats, contacts,
     messages). Directory mode 0700, files 0600; outside the Drive sync root.
   - Schema (messages.db): `chats(jid, name, is_group, updated_at)`,
     `messages(chat_jid, id, sender_jid, sender_name, ts, text, quoted_id,
     media_type, file_name, edited, deleted)`, unique on `(chat_jid, id)`;
     upserts make history-sync replays idempotent; edits and revokes update the
     row rather than deleting it.
   - Media: text and attachment metadata only (type, file name, caption). No
     media download in the first version.
2. **Reader (`bin/wa-read`)** in dotfiles, modelled on `bin/sgn-read`: opens
   `messages.db` with `mode=ro`, subcommands `chats`, `show CHAT [--since]
   [--limit]`, `search QUERY [--chat] [--limit]`. No write or send path.
   - Allowlist: `~/.config/wa-archive/allowlist` (one chat JID or exact name
     per line, private, not in dotfiles). `show` and `search` refuse chats not
     on it; `chats` lists only allowlisted chats unless `--all-names`, which
     prints names only. The allowlist is a guardrail against agents wandering
     into unrelated private chats, not a security boundary: the daemon stores
     every chat, as sgn does, because WhatsApp delivers past history only once,
     at link time, so a chat added to the allowlist later would otherwise have
     no history.
3. **Service**: a LaunchAgent (`com.stafforini.wa-archive`) installed through
   `~/repos/launchd/bin/launchd-install.py`, `KeepAlive`, logs under the launchd
   repo's log convention, with a side-effect-free preflight (binary present,
   data directory writable). The daemon reconnects on its own; if WhatsApp
   unlinks the device it exits with a distinct status and logs it, and the
   launchd monitor alerts.
4. **Docs**: `service-access.md` maps WhatsApp (read) to `bin/wa-read`; WhatsApp
   Web stays as the send path, with Pablo's confirmation per message.

## Risks

- whatsmeow is an unofficial client; WhatsApp's terms do not permit it. Bans
  for passive, read-only companions are believed rare but cannot be ruled out.
- The device store holds session keys equivalent to a logged-in WhatsApp Web
  session. Anyone who can read it as Pablo's user can impersonate the device.
- Uses one of WhatsApp's four companion-device slots (two are already used by
  WhatsApp Web in two Chrome profiles).
- History sync depth on link is controlled by the phone/server and is unknown
  until tried.
- Supply chain: whatsmeow and its dependencies come from the Go module proxy,
  pinned in `go.sum`.

## Verification

1. After linking, a message posted to an allowlisted chat appears in
   `wa-read show` within a minute.
2. `wa-read show` on a non-allowlisted chat refuses.
3. Restarting the LaunchAgent reconnects without a new QR scan, and a message
   sent during the downtime arrives after reconnecting.
4. `wa-read` cannot open the database for writing (read-only URI), checked by a
   test.
5. The daemon binary has no call sites of whatsmeow's send APIs
   (`SendMessage`, `SendPresence`, `MarkRead`), checked by a grep-based test.
