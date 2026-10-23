# Read-only WhatsApp archive for agents

Status: implemented 2026-10-20 (`~/repos/wa-archive`, `bin/wa-read`); revised after one Codex review (see "Review changes") and during implementation (see "Implementation notes").

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
     every chat, as sgn does, because WhatsApp delivers the bulk of past history
     at link time; whatsmeow can request more from the phone on demand, but
     availability is untested, so a chat added to the allowlist later could
     otherwise have no history.
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

## Review changes

One Codex review (run of 2026-10-20 on commit `a592c97`) returned seven
findings; all were accepted. They amend the design above as follows and take
precedence over it where they conflict.

1. **Durable before acknowledged.** Enable whatsmeow's synchronous
   acknowledgement (`Client.SynchronousAck`) so a message is acknowledged only
   after its event handler returns. The handler commits the SQLite transaction
   before returning; a write failure is returned as an error, not swallowed,
   the message is not acknowledged, and the daemon exits non-zero. Each history
   sync chunk is committed in one transaction together with a `sync_chunks`
   row, so a crash mid-chunk leaves nothing half-applied and the chunk is
   reprocessed. Tests: a failed write leaves no acknowledgement; a process
   killed mid-chunk, restarted, ends with the same rows as an uninterrupted run.
2. **Merge rules for edits and revokes.** The message row keeps the original
   content; edits and revokes are stored as their own events keyed by target
   message id, with server timestamps. The displayed state is the original plus
   the latest edit, or a tombstone if revoked. An edit or revoke that arrives
   before its target is kept and applied when the target arrives. Replayed or
   older history never overwrites a newer edit or a revoke. Tests: both arrival
   orders, repeated history, restart between events.
3. **Allowlist by stable identity, failing closed.** The allowlist holds chat
   JIDs only (a trailing comment may carry the name). `wa-read resolve NAME`
   lists candidate chats with their JIDs so Pablo can pick one; names are never
   matched at read time. A missing, empty or malformed allowlist makes every
   command refuse. `search` without `--chat` searches allowlisted chats only;
   `--chat` with a non-allowlisted chat refuses; `chats` lists allowlisted chats
   only. Each of these is tested.
4. **Health and unlink alerting that actually fires.** `KeepAlive` is
   `{SuccessfulExit: false}`. On logout or unlink the daemon writes a
   persistent `unlinked` marker in its state directory and exits 0, so launchd
   does not restart it into a loop. While running it updates a heartbeat file
   after each successful connection and each committed write; the job is
   registered with a launchd-repo success heartbeat so a stale heartbeat
   alerts. `wa-read` prints a warning when the marker exists or the heartbeat
   is stale. Verification triggers each path (marker, stale heartbeat) and
   checks that the launchd monitor reports it.
5. **One identity per chat.** Chats and senders are stored under a canonical
   key: the phone-number JID when whatsmeow's LID store maps the LID to one,
   otherwise the LID, with an alias table recording both forms. History, live
   events, edits, quotes and allowlist resolution all go through the same
   normalization. Tests cover a chat seen under both address forms and messages
   Pablo sends from his other devices.
6. **Coverage is recorded and shown.** A `coverage` table records history
   chunks received, whether initial sync completed, and the oldest and newest
   message timestamps per chat. `wa-read show` and `search` print that coverage
   with the results, so missing messages cannot be read as proof that a
   conversation did not happen. Verification includes a message sent before
   linking, multiple history chunks, captions and sender attribution. On-demand
   history requests stay out of v1.
7. **Passive behaviour, defined and checked.** Permitted traffic is what the
   protocol requires to stay linked: delivery and retry receipts, key uploads,
   keepalives. The daemon never sends read receipts, presence, chat state,
   reactions or messages. Before building, review the pinned whatsmeow
   version's automatic sends and its configuration for these. Verify end to
   end that a new incoming message stays unread on Pablo's phone and in
   WhatsApp Web after the daemon ingests it and after `wa-read` shows it, and
   that no presence or typing indicator appears. This replaces verification
   item 5's grep as the evidence; the grep stays as a regression check only.

## Implementation notes

- History chunks use whatsmeow's manual download mode: the handler downloads
  a chunk, commits it with its `sync_chunks` row, and only then sends the
  history receipt and deletes the blob from the server. In automatic mode
  whatsmeow deletes the blob even when the handler fails, which would lose the
  chunk.
- With synchronous acks, whatsmeow asks Pablo's own phone to resend a message
  that failed to decrypt on its first retry, regardless of
  `AutomaticMessageRerequestFromPhone`. It is a peer message to his own device,
  invisible to others, as WhatsApp Web does; it is listed as permitted traffic.
- `show` and `search` also accept a name substring, matched only among
  allowlisted chats, so a name can never admit a chat that is not on the list.
- Direct-chat names come from whatsmeow's contact store, then the contact's
  latest display name; masked phone numbers ("+54∙∙∙90") are discarded.
- Pairing uses a phone-number code (`PairPhone`) instead of a QR image.

## Verification (2026-10-20)

- Compared against WhatsApp Web for the five allowlisted chats (per-day counts
  of text messages, documents and voice notes, then message by message where
  counts differed). Every message Web showed was in the archive, except two
  classes, both fixed: content types the daemon did not recognise were dropped
  (a group invite), and messages deleted before linking were absent. Unknown
  content is now stored under its protobuf field name, and pre-link deletions
  as "[deleted]".
- Removing the device from the phone made the daemon exit 0 with the
  `unlinked` marker; launchd did not restart it and `wa-read` warned.
- A stale heartbeat produced a launchd-audit error; restarting reconnected
  without pairing; a restart during history sync lost nothing.
- Full history: `RequireFullSync` was set and the device relinked, but in the
  first five minutes the phone sent only the recent batch (no FULL chunks).
  Older history may still arrive; otherwise backfilling needs on-demand
  history requests (`BuildHistorySyncRequest`), not implemented.
- Not yet observed: that an incoming message stays unread on the phone after
  the daemon stores it (passive check).

## Retention and backups (2026-10-23)

Pablo's decision: the archive keeps 90 days of messages (`Retention` in `archive.go`; pruned at startup and hourly; a history-sync replay of older messages is pruned again on the next pass), and both `~/.local/share/wa-archive` and `~/.local/share/inbox-monitor` are excluded from Time Machine (`tmutil addexclusion`), since they hold decrypted messages and the linked device's session keys. FileVault protects them at rest.

**Allowlist replaced by a deny-list (2026-10-23)**: Pablo's decision. `wa-read` reads every chat except those on the optional `~/.config/wa-archive/denylist`; a malformed deny-list still makes it refuse. See the inbox monitor plan (`2026-10-23-inbox-monitor.md`, decision 6).
