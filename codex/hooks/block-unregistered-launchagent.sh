#!/bin/bash
# PreToolUse hook: force LaunchAgent changes through ~/repos/launchd.
#
# ~/repos/launchd is the registry of record for this machine's launchd jobs.
# A managed job needs a canonical plist in agents/, a live symlink in
# ~/Library/LaunchAgents, an entry in registry/jobs.json, and a loaded
# launchctl service. Nothing enforced that, and the rule was documented only
# inside the launchd repo — which a session working in some other repo has no
# reason to read.
#
# So every job on this machine arrived the same way: written straight into
# ~/Library/LaunchAgents and discovered up to a day later by the drift audit.
# com.stafforini.vara-refresh on 2026-08-21 was the latest; the 2026-06-07
# batch was a sweep of jobs that had accumulated the same way.
#
# bin/launchd-install.py in that repo does the whole sequence in one call, so
# this hook denies the hand-rolled paths and names the script instead.
#
# Removal is denied too: dropping a live plist that the registry still lists
# turns into an audit ERROR the next morning, and the registry entry should go
# in the same change.
#
# ALLOW_LAUNCHAGENT_CHANGE=1 is the narrow override for a change Pablo has
# explicitly authorized, such as a deliberate temporary bootout while
# debugging. It is not a way around registering a new job.
#
# Reads JSON from stdin and normalizes Bash, exec_command, functions.exec,
# apply_patch, and Edit/Write payloads via lib-codex-paths.sh.
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

# Install before sources or external commands: ordinary nonzero hook exits
# are nonblocking. Keep this bootstrap dependency-free, including when jq fails.
hook_bootstrap_complete=0
trap 'hook_status=$?; if [ "$hook_status" -ne 0 ] || [ "$hook_bootstrap_complete" -ne 1 ]; then
  printf "%s\n" "Security hook failed; tool execution denied." >&2
  exit 2
fi' EXIT

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-paths.sh
source "$SCRIPT_DIR/lib-codex-paths.sh"

hook_bootstrap_complete=1
INPUT=$(cat)

LIVE_DIR="$HOME/Library/LaunchAgents"
INSTALLER="$HOME/repos/launchd/bin/launchd-install.py"

deny() {
  jq -n --arg reason "$1" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
  exit 0
}

INSTALL_GUIDANCE="Install it through the launchd registry instead:

  python3 ~/repos/launchd/bin/launchd-install.py PLIST --purpose 'One sentence.'

That writes the canonical plist to ~/repos/launchd/agents/, symlinks the live
file, adds the registry/jobs.json entry, and runs the audit. Add --adopt for a
plist already sitting in ~/Library/LaunchAgents, --load to bootstrap it, and
--dry-run to see the plan first. Commit the launchd repo afterwards."

# --- apply_patch/Edit/Write payloads -------------------------------------

while IFS= read -r FILE_PATH; do
  [ -n "$FILE_PATH" ] || continue
  case "$FILE_PATH" in
    "$LIVE_DIR"/*)
      deny "BLOCKED: writing a plist directly into ~/Library/LaunchAgents.

$FILE_PATH

Live plists are symlinks to canonical files in ~/repos/launchd/agents/. Authoring
one in place creates a job the drift audit will flag as unregistered.

Write the plist somewhere else (the launchd repo, or a scratch path), then:

$INSTALL_GUIDANCE"
      ;;
  esac
done < <(codex_changed_paths "$INPUT")

# --- Shell payloads ---------------------------------------------------------

COMMAND=$(codex_shell_command "$INPUT")
[ -n "$COMMAND" ] || exit 0

# Cheap pre-filter: almost every command skips the parsing below.
if ! printf '%s' "$COMMAND" | grep -qE 'LaunchAgents|launchctl'; then
  exit 0
fi

# Documented override for an explicitly authorized change.
if printf '%s' "$COMMAND" | grep -qE '(^|[[:space:]])ALLOW_LAUNCHAGENT_CHANGE=1([[:space:]]|$)'; then
  exit 0
fi

VERDICT=$(CMD="$COMMAND" LIVE_DIR="$LIVE_DIR" INSTALLER="$INSTALLER" python3 <<'PY'
import os
import re
import sys

cmd = os.environ["CMD"]
live_dir = os.environ["LIVE_DIR"]
installer = os.environ["INSTALLER"]

# Drop heredoc bodies unless the body itself is being written into the live
# directory — `cat > ~/Library/LaunchAgents/x.plist <<EOF` is exactly the
# pattern this hook exists to stop, so the opener line is kept either way.
kept, marker = [], None
for line in cmd.split("\n"):
    if marker is not None:
        if line.strip() == marker:
            marker = None
        continue
    kept.append(line)
    opener = re.search(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1", line)
    if opener:
        marker = opener.group(2)
stripped = "\n".join(kept)

# The sanctioned path. Anything routed through the installer is fine, including
# the launchctl bootstrap it runs under --load.
if re.search(r"launchd-install(?:\.py)?\b", stripped):
    sys.exit(1)

home = os.path.expanduser("~")
live_patterns = [
    re.escape(live_dir),
    re.escape(live_dir.replace(home, "~")),
    r"~/Library/LaunchAgents",
    r"\$HOME/Library/LaunchAgents",
    r"Library/LaunchAgents",
]
live_re = "(?:" + "|".join(live_patterns) + r")\S*"

# Command position: start of string, or after a newline or shell operator.
POS = r"(?:^|[\n;&|(\x60])\s*(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*"

findings = []

# 1. Writing into the live directory.
write_verbs = r"(?:cp|mv|ln|install|touch|tee|rsync|plutil|defaults|sed|perl|python3?|ruby|awk)"
for match in re.finditer(POS + write_verbs + r"\b[^\n;&|]*", stripped):
    fragment = match.group(0)
    if not re.search(live_re, fragment):
        continue
    # plutil -p and defaults read only inspect.
    if re.search(r"\bplutil\b[^\n]*\s-p\b", fragment):
        continue
    if re.search(r"\bdefaults\s+read\b", fragment):
        continue
    findings.append(("write", fragment.strip()))
    break

# 2. Redirection into the live directory: `... > ~/Library/LaunchAgents/x.plist`
if not findings:
    for match in re.finditer(r">>?\s*(" + live_re + ")", stripped):
        findings.append(("write", match.group(0).strip()))
        break

# 3. Loading or unloading a service.
if not findings:
    load_re = POS + r"launchctl\s+(bootstrap|bootout|load|unload|enable|disable|remove|submit)\b[^\n;&|]*"
    for match in re.finditer(load_re, stripped):
        verb = match.group(1)
        kind = "load" if verb in ("bootstrap", "load", "enable", "submit") else "unload"
        findings.append((kind, match.group(0).strip()))
        break

if not findings:
    sys.exit(1)

kind, fragment = findings[0]
print(kind)
print(fragment)
PY
) || exit 0

KIND=$(printf '%s' "$VERDICT" | sed -n '1p')
FRAGMENT=$(printf '%s' "$VERDICT" | sed -n '2,$p')

case "$KIND" in
  write)
    deny "BLOCKED: installing a plist into ~/Library/LaunchAgents by hand.

  $FRAGMENT

Every job on this machine got here this way and had to be adopted into the
registry afterwards, always after the drift audit emailed about it.

$INSTALL_GUIDANCE"
    ;;
  load)
    deny "BLOCKED: loading a LaunchAgent outside the launchd registry.

  $FRAGMENT

A job loaded without a registry/jobs.json entry runs unmanaged until the next
drift audit finds it.

$INSTALL_GUIDANCE

If the job is already registered and you are only reloading it, rerun with
ALLOW_LAUNCHAGENT_CHANGE=1 once Pablo has confirmed the reload."
    ;;
  unload)
    deny "BLOCKED: unloading or removing a LaunchAgent outside the launchd registry.

  $FRAGMENT

If a job the registry still lists disappears, tomorrow's audit reports it as a
missing managed job. Retire the job and its registry entry together: remove the
managed_jobs entry in ~/repos/launchd/registry/jobs.json, delete the canonical
plist under agents/, and commit that change alongside the bootout.

For a deliberate temporary bootout that Pablo has confirmed, rerun with
ALLOW_LAUNCHAGENT_CHANGE=1."
    ;;
esac

exit 0
