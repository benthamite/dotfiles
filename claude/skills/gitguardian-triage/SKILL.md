---
name: gitguardian-triage
description: Triage and remediate all open GitGuardian secret incidents in the user's workspace. Fetches incidents via the GG API, classifies each (rotate vs ignore vs close), drives provider-side revocation via the matching Chrome profile, updates pass and .zshenv-secrets consumers, and closes each incident in GG. Use when the user says "gitguardian", "gg triage", "secret audit", "process gg incidents", "rotate leaked secrets", or wants to clear their GitGuardian dashboard.
user-invocable: true
model: opus
---

# GitGuardian triage and remediation

Go through every open incident in the user's GitGuardian workspace and resolve it: rotate real secrets at the provider, update local consumers, and close the incident in GG with the right disposition. Everything is driven via the GitGuardian API — no browser needed for the triage phase.

## Prerequisites (check first, fail loud if missing)

1. **GG API token** at `pass auth-sources/api.gitguardian.com/pablo@stafforini.com` — first line is the `gg_pat_*` token. If missing, walk the user through creating one at `https://dashboard.gitguardian.com/api/personal-access-tokens` with scopes `incidents:read`, `incidents:write`, `sources:read`. Store via `pass insert -m auth-sources/api.gitguardian.com/pablo@stafforini.com`.

2. **Chrome profile pairings** (only if rotation will involve browser flows). Check each of `~/.claude-personal/.claude.json`, `~/.claude-epoch/.claude.json`, `~/.claude-tlon/.claude.json` for a `chromeExtension.pairedDeviceId`. Unpaired accounts are fine for providers that can be rotated via CLI (gcloud, gh) or BotFather in Telegram Desktop. Needed for: OpenAI dashboard, Anthropic console, Google AI Studio.

3. **gcloud authentications** (for Google / Gemini rotations). Check `gcloud auth list`. Personal Google keys live under `pablo.stafforini@gmail.com`; Tlön keys under `pablo@tlon.team`; Epoch under `pablo@epoch.ai`.

## Step 1: Enumerate open incidents

```bash
GG_TOKEN=$(pass auth-sources/api.gitguardian.com/pablo@stafforini.com | head -1)
mkdir -p /tmp/gg
curl -s -H "Authorization: Token $GG_TOKEN" \
  "https://api.gitguardian.com/v1/incidents/secrets?per_page=100&status=TRIGGERED" \
  -o /tmp/gg/triggered.json
jq 'length' /tmp/gg/triggered.json   # count
```

Note pagination: if `length == 100`, also fetch subsequent pages via the `link` header.

Fetch detailed records in parallel (rate limit is 500/min, so a few hundred is fine):

```bash
jq -r '.[].id' /tmp/gg/triggered.json > /tmp/gg/ids.txt
while read id; do
  curl -s -H "Authorization: Token $GG_TOKEN" \
    "https://api.gitguardian.com/v1/incidents/secrets/$id" \
    > "/tmp/gg/incident-$id.json" &
done < /tmp/gg/ids.txt
wait
```

## Step 2: Build a summary table

Present to the user:

```bash
jq -r '[.id, .detector.display_name, .severity, .validity, (.occurrences_count|tostring), (.occurrences[0].source.full_name // "?"), (.occurrences[0].source.visibility // "?"), (.occurrences[0].filepath // "?"), (.date[0:10])] | @tsv' /tmp/gg/incident-*.json \
  | sort -k2,2 -k1,1n | column -t -s $'\t'
```

Columns: id, detector, severity, validity, occurrence_count, source_repo, visibility, filepath, date.

## Step 3: Classify each incident

Bucket every incident into ONE of these dispositions. Follow the order — stop at the first match.

### 3a. Already-revoked → close with `resolve`

Any incident where `validity == "invalid"`. The key no longer authenticates; just mark it resolved.

```bash
curl -s -X POST -H "Authorization: Token $GG_TOKEN" -H "Content-Type: application/json" \
  -d '{"secret_revoked": true}' \
  "https://api.gitguardian.com/v1/incidents/secrets/$id/resolve"
```

### 3b. Known false-positive patterns → close with `ignore` reason=`false_positive`

Do NOT use this reason for anything that was ever a real credential, even if it's now dead. Reserve for things that were never secrets.

Patterns the GG detectors misfire on (confirmed by pulling the flagged byte range — see Step 3e):

- **`Generic High Entropy Secret` matches in data files** — JSON artifacts, timestamps (`2026-04-23T04:44:...`), canonical IDs (`TI-JoseaDecar`, `SRC-foo-12345`), dispute enum strings. Common in `walk-decisions.json`, ML training data, etc.
- **`Generic High Entropy Secret` matches in BibTeX/bibliography** — book titles, DOIs, author names.
- **`Generic Password` or `Username Password` matches in philosophy/literature text** — long English phrases that trip the entropy heuristic.
- **Provider-key detectors flagging code that uses variable references** — e.g., `oauth2-auto.el` where the code has `(client_id . ,oauth2-auto-google-client-id)` with no hardcoded value.
- **`Company Email Password` matches in org-mode property drawers** (e.g., `:password: XXXXXX` next to Zoom / Google Meet links). Meeting passwords are technically secrets but transient; use `low_risk` if the flagged string looks meeting-like, `false_positive` only if clearly misparsed email/URL fragments.

### 3c. Low-risk historical secrets → `ignore` reason=`low_risk`

- Legacy meeting passwords (Zoom, Google Meet, Jitsi) in calendar/meeting notes — the meeting has ended; the password is worthless.
- 3+ year old OAuth client credentials in inactive/archive folders where the OAuth app has almost certainly been deleted.
- Slack app credentials from 4+ year old todo.org entries.

### 3d. Dev-only credentials → `ignore` reason=`test_credential`

- `letmein`, `password123`, `admin`, `test`, `example` in dev docker-compose files or `.env.example` / similar.

### 3e. Extract the flagged substring to confirm the above

GG exposes `matches[0].indice_start`/`indice_end` as byte offsets within the file (not the line). Resolve against the local clone at the commit:

```bash
cd <repo>
start=$(jq -r '.occurrences[0].matches[0].indice_start' /tmp/gg/incident-$id.json)
end=$(jq   -r '.occurrences[0].matches[0].indice_end'   /tmp/gg/incident-$id.json)
sha=$(jq   -r '.occurrences[0].sha'                     /tmp/gg/incident-$id.json)
file=$(jq  -r '.occurrences[0].filepath'                /tmp/gg/incident-$id.json)
git show "$sha:$file" 2>/dev/null | dd bs=1 skip=$start count=$((end - start)) 2>/dev/null
```

If the commit isn't present locally (archived repos, detached history), use the HEAD content at the same byte range, or fetch from the repo's GitHub URL (the incident JSON has `.occurrences[0].url`).

### 3f. Real, still-valid secrets → rotate (Step 4)

Anything where `validity == "valid"` or where a check against the provider succeeds (see Step 4.0). These get rotated end-to-end.

### 3g. Unknown validity + real-looking pattern → inspect + ask user

`validity == "failed_to_check"` or `"no_checker"` with a pattern that looks like a real credential (postgres connection string, API key, RSA private key). Present each to the user with:

- What service it's for (inferred from filename / nearby text)
- Whether the service is still in use (user must answer — you won't know)

Then rotate (if still used) or `ignore` with reason=`low_risk` (if archived).

## Step 4: Rotate real secrets

### 4.0 Verify the key is actually live

Before any rotation, hit the provider with the key and confirm the call succeeds. Never trust GG's `valid` flag alone — sometimes keys have been manually revoked since last scan.

| Provider | Probe |
|---|---|
| GitHub PAT | `curl -s -H "Authorization: Bearer $TOK" https://api.github.com/user` → expect `login` field |
| OpenAI | `curl -s -H "Authorization: Bearer $TOK" https://api.openai.com/v1/me` → expect `email` field |
| Anthropic | `curl -s -H "x-api-key: $TOK" -H "anthropic-version: 2023-06-01" https://api.anthropic.com/v1/messages` with a tiny test message |
| Google Gemini | `curl -s "https://generativelanguage.googleapis.com/v1beta/models?key=$TOK"` → expect `models` array |
| Google generic | `curl -s "https://translation.googleapis.com/language/translate/v2?q=hi&target=es&key=$TOK"` → the error message reveals the owning project number |
| Telegram bot | `curl -s "https://api.telegram.org/bot$TOK/getMe"` → expect bot identity |

### 4.1 Find live consumers of each secret

**The right question is not "where is this exact leaked string used?"** It's "what env var / pass entry / config line uses this service, and does it currently hold the leaked value?" Rotated keys can look identical in git history but the live consumer might already have a newer key.

For each secret:

1. Extract the USE pattern (e.g., `OPENAI_API_KEY`, `GEMINI_API_KEY`, `GH_TOKEN`).
2. In `.zshenv-secrets` (plaintext at edit time; git-crypt encrypted at rest):
   ```bash
   grep -E '^export NAME=' "$HOME/My Drive/dotfiles/shell/.zshenv-secrets" | sed -E 's/.*="([^"]+)".*/\1/'
   ```
3. In `pass`:
   ```bash
   cd ~/.password-store
   find . -name "*.gpg" -not -path "./.git/*" | while read f; do
     rel="${f#./}"; rel="${rel%.gpg}"
     val=$(pass "$rel" 2>/dev/null | head -1)
     [ "$val" = "$LEAKED_VAL" ] && echo "MATCH in pass: $rel"
   done
   ```
4. Other file-level consumers:
   ```bash
   grep -rIl -F "$LEAKED_VAL" \
     "$HOME/My Drive/dotfiles" \
     "$HOME/My Drive/repos" \
     "$HOME/.password-store" \
     --exclude-dir=.git --exclude-dir=node_modules --exclude-dir=elpaca \
     --exclude-dir=__pycache__ --exclude-dir=.venv --exclude-dir=archive
   ```

Files in `repos/archive/` and `notes/claude-logs/` and `notes/gptel/` generally don't count as live consumers — they're historical copies. Focus on `.zshenv-secrets`, `pass` entries, config files in active repos, and running services (gcloud, etc.).

### 4.2 Per-provider rotation procedures

Route each rotation to the Chrome profile that owns the credential. See `dotfiles/claude/README.org` → "Chrome integration and multi-account" for the account ↔ profile table. If the current Claude-Code session's account doesn't match the provider's account, either switch sessions (`CLAUDE_CONFIG_DIR=~/.claude-<acct> claude --chrome`) or have the user handle that provider manually.

#### GitHub PAT (classic)

1. Navigate to `https://github.com/settings/tokens` in the matching Chrome profile.
2. Identify the leaked token by its **scope set** (each PAT lists its scopes on the listing page). Match the scopes from the leaked-value probe (`curl -I` → `x-oauth-scopes` header).
3. Click **Regenerate** on the matching entry. GitHub requires sudo-mode 2FA — ask the user to complete it in the browser.
4. After regenerate, GitHub shows the new value once. Use the "Copy token" button, then extract via:
   ```js
   // javascript_tool
   const v = document.querySelector('input[value^="ghp_"]').value;
   const blob = new Blob([v], {type:'text/plain'});
   const a = document.createElement('a');
   a.href = URL.createObjectURL(blob);
   a.download = 'gg-new-pat.txt';
   document.body.appendChild(a); a.click();
   setTimeout(()=>a.remove(), 1000);
   ```
   Then `cat ~/Downloads/gg-new-pat.txt` to read it. Don't try to read via clipboard — macOS blocks clipboard writes from backgrounded Chrome tabs.
5. Verify new token works, verify old is 401.
6. Update `.zshenv-secrets` (Python edit — never put the token literal in the shell command):
   ```bash
   export FILE="$HOME/My Drive/dotfiles/shell/.zshenv-secrets"
   export OLD="<old_token>"  # via pass/env, never literal in this doc
   python3 <<'PY'
   import os
   new = open(os.path.expanduser('~/Downloads/gg-new-pat.txt')).read().strip()
   p = os.environ['FILE']; c = open(p).read()
   c = c.replace(os.environ['OLD'], new)
   open(p, 'w').write(c)
   PY
   ```
7. Update any matching `pass` entries (iterate; skip entries holding different PATs):
   ```bash
   for entry in auth-sources/api.github.com/benthamite^{code-review,forge,ghub,github-review,magit}; do
     cur=$(pass "$entry"); [ "$(echo "$cur" | head -1)" = "$OLD" ] || continue
     printf '%s\n' "${cur//$OLD/$NEW}" | pass insert -m -f "$entry"
   done
   ```
8. **Refresh `gh`'s macOS-keychain entry (CRITICAL — otherwise launchd jobs break).** `gh` stores credentials in two places: the `GH_TOKEN` environment variable *and* the macOS keychain "keyring". Rotating `GH_TOKEN` in `.zshenv-secrets` only updates the env-var side. Terminal sessions that source `.zshenv-secrets` are fine, but launchd jobs run with a minimal env (no `GH_TOKEN` inherited) and fall back to the keyring — which still holds the revoked PAT, causing `git push` / `git fetch` over HTTPS to fail with exit 128 ("Password authentication is not supported for Git operations"). Impacted: every launchd plist that does HTTPS git ops (e.g., `com.stafforini.sa-lp-refresh`, `com.stafforini.epoch-website-pull`, `com.stafforini.download-pdfs`, any daily refresh script).
   
   `gh auth login --with-token` refuses to write to the keyring while `GH_TOKEN` is set — it sees the env var and no-ops. Use `env -u` to unset it for the single command:
   ```bash
   env -u GH_TOKEN -u GITHUB_TOKEN bash -c "echo '$NEW_TOKEN' | gh auth login --with-token"
   ```
   After running, `gh auth status` should show *both* sources ✓ (env-var and keyring). Verify the keyring path works without the env var via:
   ```bash
   env -u GH_TOKEN -u GITHUB_TOKEN gh auth git-credential get <<< $'protocol=https\nhost=github.com\n\n'
   ```
   Also make sure `gh auth setup-git` has been run once (adds `credential.https://github.com.helper=!gh auth git-credential` to `~/.gitconfig`); without it, git won't even ask `gh` for the token.
9. `shred -u -n 3 ~/Downloads/gg-new-pat.txt` (or `trash` it).
10. Commit `.zshenv-secrets` in the dotfiles repo with a descriptive message.
11. Mark GG incident resolved (Step 5).

#### OpenAI key

1. Navigate to `https://platform.openai.com/api-keys` in the matching profile.
2. Switch org (Personal vs Tlön) if needed — org selector is top-left of sidebar.
3. Legacy user keys (`sk-<51 chars>`) live under the **User API Keys > Legacy** tab. Project keys (`sk-proj-…`) live on the Project API Keys tab. **Legacy user keys are visible only to the user who created them** — org admins can't manage another user's legacy keys; that user must sign in themselves.
4. Click the trash icon in the row, confirm "Revoke key". If clicking via ref doesn't work (OpenAI uses non-standard button structure), use the JS `button.click()` path: find the button inside the row whose SVG path starts with `M10.556 4a1…` (trash icon).
5. Issue a replacement if the key is still needed (Project API Key tab → Create new secret key). Limit scope to what's required.
6. Repeat the consumer-update flow as in GitHub step 6-10.

#### Anthropic key

1. Navigate to `https://console.anthropic.com/settings/keys`.
2. Delete the leaked key, create a replacement, extract via the same download-attribute trick.
3. Update `.zshenv-secrets` `ANTHROPIC_API_KEY=` and relevant `pass` entries, commit.

#### Google Gemini key (API key, not OAuth)

Owning project number comes from probing the key against a non-enabled API:

```bash
curl -s "https://translation.googleapis.com/language/translate/v2?q=hi&target=es&key=$KEY" | jq -r '.error.details[0].metadata.consumer'
```

That returns `projects/<number>`. Then:

```bash
gcloud projects describe <number> --format="value(projectId)"  # need the owning account authed
gcloud alpha services api-keys list --project=<projectId>
gcloud alpha services api-keys get-key-string <uid> --project=<projectId>  # to cross-ref
gcloud alpha services api-keys delete <uid> --project=<projectId>
gcloud alpha services api-keys create --project=<projectId> --display-name="rotated-YYYYMMDD"
gcloud alpha services api-keys get-key-string <new-uid> --project=<projectId>
```

Update `.zshenv-secrets` `GEMINI_API_KEY=`. Note `GOOGLE_GENERATIVE_AI_API_KEY="$GEMINI_API_KEY"` is a reference — no separate update needed.

If you can't authenticate to the owning project's account, ask the user to either run `gcloud auth login <email>` or handle it in the matching Chrome profile at `https://aistudio.google.com/app/apikey` (must import the project first).

#### Telegram bot token

BotFather only works in a logged-in Telegram client. Telegram Web requires QR-code login you can't automate, so use Telegram Desktop.

Ask the user to:
1. Open Telegram Desktop, search `@BotFather`.
2. Send `/revoke`.
3. Pick the bot (menu shows bot username).
4. BotFather returns the new token. User pastes it back to you.

You update `pass` entries (e.g., `tlon/core/Telegram Bot/<botname>`) and any live env consumer.

#### PostgreSQL / generic service credentials

Per-service — ask the user to rotate in Supabase / DigitalOcean / RDS / Kalshi / etc. dashboards, then update the consumer config.

## Step 5: Close the GG incident

After rotation is verified (new key works, old key returns 401/403):

```bash
curl -s -X POST -H "Authorization: Token $GG_TOKEN" -H "Content-Type: application/json" \
  -d '{"secret_revoked": true}' \
  "https://api.gitguardian.com/v1/incidents/secrets/$id/resolve"
```

For false positives / low risk / test credentials (no rotation involved):

```bash
curl -s -X POST -H "Authorization: Token $GG_TOKEN" -H "Content-Type: application/json" \
  -d '{"ignore_reason": "false_positive"}' \
  "https://api.gitguardian.com/v1/incidents/secrets/$id/ignore"
```

Valid `ignore_reason` values: `false_positive`, `low_risk`, `test_credential`. No other strings.

## Step 6: Scrub live-key files from HEAD (optional, only while rotations are pending)

If a currently-live key is captured in a committed file (claude-logs, gptel transcripts, .env leaked to wrong repo), delete that file from the working tree during the rotation window so GG stops re-flagging:

```bash
cd <repo>; trash path/to/leaky-file
git add -u path/to/leaky-file
git commit -m "<path>: scrub file that leaks live API keys"
```

Do NOT rewrite history. The dead value in history is inert once rotated; the rotation + GG resolve is what actually matters. Rewriting history requires force-push and re-seeding every clone, which is invasive and rarely worth it for private repos.

## Step 7: Surface blocked items for user follow-up

At the end, report:

- ✅ Incidents resolved (rotated + closed).
- ✅ Incidents ignored (false positive / low risk / test).
- ⏸ Incidents blocked pending user action. For each: provider, account, what the user needs to do. Group by account so the user can batch (e.g., "Tlön: 3 keys; log in to platform.openai.com in Profile 3 and delete these").

## Gotchas

- **Secret-leak hook**: the `block-secret-leak.sh` PreToolUse hook blocks any Bash command that contains an obvious secret literal. Always move secrets to env vars or extract from `pass` at runtime. Never `grep -F "$literal"` with the literal written in the command.
- **Clipboard does not work from backgrounded Chrome tabs** on macOS. Use the download-attribute trick from Step 4.2 GitHub step 4 to exfil secrets from browser.
- **`gcloud alpha services api-keys get-key-string`** requires the current account to be authenticated on the owning project. Different Google projects may belong to different user accounts.
- **OpenAI legacy user keys** are per-user, not per-org. Org admins can't revoke them on behalf of users.
- **GG incidents stay OPEN across scans** even after rotation, until you explicitly call `/resolve` or `/ignore`.
- **Pagination**: `/v1/incidents/secrets` returns up to 100; follow the `link: <...>; rel="next"` header if present.
- **Repo-scanning scope**: GG scans both public and private repos. Private-repo incidents are still worth resolving; they indicate the key is present, not that it's exposed to the world.

## Use TaskCreate

For any non-trivial batch (>5 incidents), create one Task per bucket at the start (one for valid-rotations, one for invalid-closures, one for false-positive batch, etc.) and mark them in_progress / completed as you go. Individual incidents within a bucket don't need their own Task unless they need user follow-up.
