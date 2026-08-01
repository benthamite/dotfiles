---
name: review-lulu-alert
description: Use when a LuLu firewall alert is open on this Mac and the user asks whether to allow or block it, or asks to inspect, check, audit, or review the connection a LuLu alert is prompting about. Covers explicit $review-lulu-alert requests and questions like "should I allow this?", "what is this process connecting to?", or "is this LuLu prompt safe?".
---

# Review LuLu Alert

Audit the LuLu alert currently on screen and recommend allow or block. LuLu is
a per-connection outbound firewall: an open alert is a decision the user has
not made yet.

**Reviewing an alert is not answering it.** The alert is the user's decision.
Your job ends at a recommendation.

## The mutation boundary

During a review you must never:

- press **Allow** or **Block**
- change the **Rule Scope** pop-up or the **Rule Duration** radio buttons
- edit `/Library/Objective-See/LuLu/rules.plist` or `preferences.plist`
- kill the process, move or delete its executable, or send any file or hash to
  an external service

The helper is read-only by construction. There is no action command in this
skill, and clicking through UI automation is the same violation as clicking by
hand. **Violating the letter of this rule is violating its spirit.**

### Rationalizations that mean stop

| Excuse | Reality |
|---|---|
| "They asked me to recommend allow or block, so clicking *is* the recommendation" | Recommending names the button. Pressing it answers for them. |
| "Block is the safe action, so clicking Block is harmless" | Block writes a rule and forecloses their choice. Safe-direction is still a decision. |
| "Clicking dismisses the alert as fast either way, so the deadline isn't a reason to allow" | Correct reasoning, wrong conclusion. Neither button is yours to press. |
| "I'll just narrow the scope first — that's strictly safer" | Changing scope changes the decision they are about to make. |
| "Their existing rules all use Any Endpoint + Always, so matching them is consistent" | Consistency with past rules is not authorization to widen this one. |
| "They said 'handle it'" | Ambiguous. Review, recommend, and ask which button they want. |
| "The process is obviously malware, so containment can't wait" | Report it and say it's urgent. Containment is still theirs to authorize. |

### Red flags — stop and re-read this section

- You are composing a click, keystroke, or `osascript` aimed at the alert
- You are about to change scope or duration "to make the rule safer"
- Your action list contains a verb other than *read*, *check*, or *report*
- You are planning steps that outlive the alert: killing, quarantining,
  rotating credentials, uploading a hash

## Workflow

**1. Read the live alert.**

```bash
# Set this to the directory containing this SKILL.md.
SKILL_DIR="/path/to/review-lulu-alert"
"$SKILL_DIR/scripts/inspect-lulu-alert"
```

It prints one JSON object. Never audit a pasted transcript — it can disagree
with the window, and the window is what the user will act on.

If `alert_present` is `false`, say so and stop. Do not open LuLu to look.

If `unreadable_fields` is non-empty, consult `raw_label_pairs` before treating
a field as missing, and name what you could not read.

**2. Record the identity.** Copy `process.pid`, `process.path`,
`process.args`, `connection.ip_address`, `connection.port_protocol` and
`alert_timestamp` into your notes. This is the tuple you must re-check before
any later action.

If `alert_windows_open` is greater than 1, more alerts are queued. Audit only
the one you read.

**3. Confirm the alert matches this machine.**

- Path missing from disk → you cannot verify provenance. Say so and do not
  recommend Allow.
- PID gone but path present → normal for a short-lived CLI. Continue on static
  evidence and name which live checks became unavailable.
- Both missing → you cannot inspect the executable at all: no signature, no
  hash, no entitlements, no ancestry, no open sockets. That forecloses Allow.
  Recommend Block or leaving the alert unanswered, and say plainly that the
  verdict rests only on the alert's own fields and LuLu's configuration.
  Do not narrate what the absence implies — a binary gone from disk is
  consistent with self-deletion, with ordinary cleanup, and with the alert
  never having matched this machine. Report the gap; don't fill it.

  Choosing between the two: recommend **Block** when the alert's own fields
  carry independent suspicion — a name imitating a system component, call-home
  arguments, an unattributable destination. Recommend **leaving it unanswered**
  when the process looks routine and its absence is the only oddity.

**4. Trace the process.** Ancestry, working directory, open sockets, and what
user action or automation launched it. Explain the workflow it belongs to, or
say you could not establish one.

**5. Verify the executable in proportion to its form.**

- App bundle → use the `audit-mac-app` skill for signing, notarization,
  entitlements, and static checks.
- Command-line binary → resolve the path (`which -a`, and check for alias or
  function shims that shadow it), then `codesign -dv --verbose=4`, package
  manager receipts and hashes, linked libraries, and signs of replacement.

A Homebrew Go or Rust bottle reports `adhoc, linker-signed` with no Team ID.
That is normal, not a finding.

**6. Verify the destination.** Prefer authoritative DNS, the vendor's own
published address ranges, and the local package source over reverse DNS alone.

Free to run: `whois` (including a second query against the regional registry
when the first returns only a transfer record) and DNS through the system
resolver. Ask first: connecting to the alerted host, fetching its TLS
certificate, and third-party reputation services — the first two can raise
another LuLu alert, and the third is an external request about the user's
machine. If your own audit raises an alert, identify it as yours and dismiss
only that, creating no lasting rule.

## Report contract

Your answer has these parts, in this order:

1. **Verdict** — `Allow`, `Block`, or `Cannot recommend`, on one line.
2. **Alert identity** — process name, pid, path, args, destination, port and
   protocol, as read from the live window.
3. **What decides it** — the two or three findings that carry the verdict, each
   naming the evidence behind it.
4. **Unverified** — what you could not establish, and what that leaves open.
5. **Suggested rule** — the narrowest scope and duration that fits, named
   exactly as LuLu names them, plus the button to press.

Default suggestion for an allow is **Rule Scope: Remote Endpoint** and **Rule
Duration: Process lifetime** — the connection audited, for as long as the
process audited. Recommend wider only when the user asked for persistence, and
say what the wider rule permits.

Blocks invert both defaults. On a **block**, prefer **Rule Scope: Process** and
**Rule Duration: Always**: scope becomes containment, cutting the binary off
from every destination rather than the one endpoint it happened to try, and
there is no reason to let a refusal lapse. Say which direction you mean and why.

`rule.scope_options` lists only the current choice when
`scope_options_complete` is `false` — LuLu draws scope as a pop-up button whose
menu items reach the Accessibility tree only once opened, and opening it is a
click. Report the current scope; do not claim it is the only one available.

When the publisher or the destination cannot be verified, recommend Block or
leaving the alert unanswered. Do not guess.

## Read-only evidence sources

All of these are world-readable; none needs `sudo`.

| Source | What it gives |
|---|---|
| `/Library/Objective-See/LuLu/preferences.plist` | `allowApple`, `allowInstalled`, `passiveMode`. With the first two true, an alert firing at all means the process is neither Apple-signed nor present at install time. |
| `/Library/Objective-See/LuLu/rules.plist` | Existing rules, as an `NSKeyedArchiver` plist. An existing rule for the same path means this alert should not have fired — investigate that rather than clicking through. |
| `~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV2` | Where a downloaded binary came from. No row means it did not arrive via a browser — which says nothing at all when the file is already gone. |
| `<prefix>/INSTALL_RECEIPT.json` | Whether a Homebrew binary was poured from a bottle or built locally. |

To search the rules, decode to stdout and keep the file untouched:

```bash
plutil -convert xml1 -o - /Library/Objective-See/LuLu/rules.plist | grep -i -C 5 'SoftwareUpdater'
```

**Never omit `-o -`.** `plutil -convert xml1 <file>` rewrites the file in place,
and that file is LuLu's rule database. A substring hit also only proves a
string is present somewhere in the archived object graph — read the surrounding
context before concluding a rule exists or what it does.

## Acting on an explicit instruction

An instruction in the current request — "allow it", "block it" — authorizes
that single decision and nothing else.

Before acting: re-run the helper and require an exact match with the audited
pid, path, args, and endpoint. Any difference means this is a different alert;
stop and report it. Use the narrowest scope and duration unless the user asked
for persistence.

After acting: confirm the alert closed, say whether the user's workflow
progressed or failed, and confirm no broader persistent rule was created.
A `Process lifetime` rule leaves nothing in `rules.plist`; an `Always` rule does.

Queued alerts are out of scope unless the request covers them. Stop when the
next alert has a different identity.

## Capturing a fixture

If the parse looks wrong against a real alert, save the raw Accessibility tree
and replay it. Both are reads.

```bash
"$SKILL_DIR/scripts/inspect-lulu-alert" --dump > capture.json
"$SKILL_DIR/scripts/inspect-lulu-alert" --fixture capture.json
```

A capture worth keeping belongs in `tests/fixtures/review-lulu-alert/`, where
`test_review_lulu_alert.py` will hold the parser to it.

A replay checks the parser. It is never the basis for a verdict: a saved
capture cannot tell you what is on screen now, which is the whole reason step 1
reads the live window.
