# macOS controls, isolation, and recovery

Record the OS version and the workflows that handle sensitive data or execute
untrusted code. Use non-mutating checks without sudo, permission grants, or
security prompts. Unreadable settings are unknown, not disabled. Output only
needed state; launch arguments, task registrations, and network data may contain
secrets and need local filtering.

## Platform controls

Check pending security updates with `softwareupdate -l` (bounded wait), FileVault
with `fdesetup status`, Gatekeeper with `spctl --status`, SIP with
`csrutil status`, and the application firewall with
`/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate`.
Explain each disabled control's actual exposure, without automatically labeling
every disabled feature critical. Check automatic security updates and supported
OS/browser versions where the available evidence permits.

Inspect password-on-wake/automatic locking and remote-access state: Remote
Login, Screen Sharing, and Remote Management. Distinguish intentional, scoped
access from unexpected exposure; do not enable/disable a service to test it.
Report active outbound-firewall status and relevant policy, not just installation.

Audit LuLu's rules with `scripts/lulu-rules.py`, which reads its rules and
preferences read-only. An allow rule for an interpreter, shell, `osascript` or
`curl` to any endpoint lets every script that program runs reach any host, so
the firewall does not stop exfiltration by such code; rate it High when such
code handles untrusted input (dependencies, agents), and name the workflow
that needs the rule. `allowApple` and `allowInstalled` admit programs without
a rule (Apple-signed `curl` and `osascript` among them); report them as
Medium. Report stale rules as cleanup. An `unreadable` file is an unverified
high-risk item.

List listening sockets with `lsof -nP -iTCP -sTCP:LISTEN` (process name, user,
address, port only). A service bound to a non-loopback address is reachable
from the network; establish its owner and authentication.

For Lockdown Mode, use actual System Settings evidence through an existing
permitted read path, or mark status not checked. An MDM profile identifier is
not Lockdown status. Recommend it for a relevant targeted-attack threat after
considering compatibility. It affects WebKit/JIT and other system services;
avoid blanket claims that another browser is unaffected. Do not enroll in MDM
or change device-management state.

## Actual isolation

Follow a representative untrusted-code workflow and establish its effective
boundary: sandbox/VM type, host-readable/writable mounts, SSH-agent forwarding,
injected credentials, shared clipboard/home integrations, host services,
network egress, Docker/Unix sockets, and privileged/container capabilities.
Installed Docker, OrbStack, or UTM is inventory evidence only.

OrbStack's ordinary machines expose host integrations; its isolated-machine
mode removes several integrations but has documented shared-kernel/escape
limits. Containers and Python virtualenvs are not interchangeable with a
dedicated VM. Cloud agent execution also needs an access assessment: repository
contents, connector credentials, network access, and persistence remain assets.
Do not execute malware or introduce a new mount/network permission to test
isolation. Use harmless fixtures in existing authorized environments if needed;
otherwise keep configuration evidence separate from observed enforcement.

Chrome/Firefox profiles compartmentalize sessions and extension state but do not
protect against a malicious process with the same OS-user access. Multiple
profile directories do not establish that finance/email sessions are separate.
Inspect profile metadata only, never cookies or session databases.

## Privileged applications and persistence

- Review standing privacy grants with `scripts/tcc-grants.py`, which reads the
  system and per-user TCC databases read-only and lists Full Disk Access,
  Accessibility, Automation, synthetic input, Input Monitoring, Screen
  Recording and Developer Tools grants. Do not grant access to complete the
  read; an `unreadable` or `location-unknown` database means unknown grants,
  handled as an unverified high-risk item (SKILL.md, Report). From macOS 27
  the per-user database (Automation and other per-user grants) lives in a
  protected system container that Full Disk Access cannot read, so expect
  `unreadable` there and take those grants from System Settings.
- A grant to a generic executor (an interpreter, shell, `osascript`, `cron`,
  a terminal, an editor, a macro engine or an AI agent app; the helper's
  `generic_executor`, whose list may lag new apps)
  extends to every script that program runs, so any code running as the user
  holds it. Rate Accessibility, synthetic input, Automation of System Events
  or System Settings, Full Disk Access and Developer Tools held that way as
  High: such code can click through confirmations such as firewall alerts,
  read every protected file, or start programs without Gatekeeper checks.
  An app that runs only its own code holding the same grant is a narrower
  exposure; assess its intended use. Explain which workflow relies on each
  flagged grant before recommending its removal, and report `stale` grants on
  deleted paths as cleanup. Route a specific questionable app to `audit-mac-app`.
- Inspect background-task registrations through metadata for `~/Library/LaunchAgents`,
  `/Library/LaunchAgents`, and `/Library/LaunchDaemons`. Include legacy login
  items where relevant. Review executable path, ownership, signature/provenance,
  and the command's access; an unfamiliar name alone is not malware evidence.
  Do not print raw plist environment values or arbitrary program arguments.
  Do not run `sfltool dumpbtm`: it requires administrator authorization and
  raises a password prompt (observed 2026-10-18). For the full registration
  list, ask Pablo for System Settings > General > Login Items & Extensions.
- A launch item whose program or script, or any directory above it, is
  writable by the user or lies under Google Drive runs whatever that path holds
  next: rate a root-run (LaunchDaemon) target High, a user target as a
  persistence path. Check the user crontab (`crontab -l`, entries only),
  `/etc/periodic` and `/etc/*.local`, and login hooks
  (`defaults read com.apple.loginwindow`, LoginHook/LogoutHook keys only).
  The root crontab needs sudo: ask Pablo to run `sudo crontab -l`, or report it
  as an unverified high-risk item when `cron` holds Full Disk Access.
- Startup files are code: shell startup files, the Emacs init and early-init,
  `~/.ssh/config`, git config and hooks, and any file they load. When they
  live in Google Drive, anything that can write the Drive account can run code
  at the next shell or Emacs start. Compare tracked startup files with git
  using `git status --porcelain` and `git diff --stat` (names and counts, never
  content) and report modified or untracked loaded files, and symlinks from
  home into Drive, as unreviewed code. Note whether Drive version history
  could restore a clean copy.
- Check sudo configuration through readable metadata: `/etc/sudoers.d` file
  names and modes, and, when readable, `NOPASSWD`, `timestamp_timeout` and
  `!tty_tickets` entries. A `NOPASSWD` rule for a program that runs arbitrary
  code is High. Unreadable sudoers is not checked, not clean.
- PATH order: a user-writable directory ahead of `/usr/bin` lets any process
  running as the user shadow `sudo`, `ssh` or `git` for later shells. Report
  each writable entry before the system directories.
- Look for quarantine stripping in scripts, aliases and functions
  (`xattr -d`/`-c`/`-r` on `com.apple.quarantine`, `--no-quarantine`,
  `HOMEBREW_CASK_OPTS`): it removes the Gatekeeper check for what follows.
- Inventory clipboard-history apps and their retention. Copied passwords and
  tokens persist there and are readable by any process with the app's data.
- Inventory browser extensions across applicable profiles. Prioritize granted
  host access, cookies/debugger/native-messaging privileges, install/update
  origin, and continued need. Publisher size is supporting context only.
  Keep extension permissions, agent site grants, and OAuth grants distinct.

## Authentication and recovery

- Recognize Apple Passwords, browser managers, and CLI workflows, as
  well as third-party apps. Installed software does not prove use; no detected
  app/extension does not prove absence. Never inspect saved passwords.
- Phone passkeys and hardware security keys both provide phishing resistance.
  Discuss device-bound hardware protection and recovery independence where
  useful; do not describe hardware keys as the first phishing-resistant option.
- Check SSH directory/key-file ownership and permissions, agent forwarding
  configuration, and public authorized-key metadata. Do not run
  `ssh-keygen -y` or otherwise read private keys to infer passphrases. Report
  passphrase status unverified unless established from permitted existing
  evidence. Unrecognized authorized keys need provenance, not an automatic
  malicious label.
- Online-account second factors, recovery paths, and Google Advanced Protection
  belong to [accounts](accounts.md).
- Check backup freshness, encryption, destination access, and available
  independent recovery copies using metadata. Distinguish a completed backup
  from a demonstrated restore. A restore test belongs to explicitly authorized
  remediation/verification in a disposable destination; never overwrite live
  data. Consider whether the same compromised account can delete every copy.
- Assess backup integrity separately from freshness and encryption: available
  historical versions, their provenance, and whether a compromised host/account
  could alter retained copies or contaminate new ones. After an established
  incident, look for a trusted recovery point predating the earliest plausible
  compromise; a timestamp alone does not prove cleanliness. Assess how executable
  files, startup configuration, and restored credentials will be checked before
  reuse. A successful restore or clean malware scan alone does not rule out
  persistence. Unknown compromise timing or unavailable integrity evidence remains
  a recovery gap; do not mount, execute, or restore suspect backups during an audit.

## Reference maintenance

Checked 2026-09-04. Use installed macOS help and current Apple documentation when
commands or Settings paths differ; do not substitute undocumented defaults keys.

- [Apple Lockdown Mode](https://support.apple.com/en-us/105120)
- [Apple background-task management](https://support.apple.com/en-ca/guide/deployment/depdca572563/web)
- [Apple file-access controls](https://support.apple.com/en-ca/guide/security/secddd1d86a6/web)
- [Apple Passwords](https://support.apple.com/guide/passwords/welcome/mac)
- [Time Machine backups](https://support.apple.com/en-us/104984)
- [Chromium's security boundary](https://chromium.googlesource.com/chromium/src/+/main/docs/security/faq.md)
- [OrbStack isolation limits](https://docs.orbstack.dev/machines/isolated)
- [Docker Desktop boundaries](https://docs.docker.com/security/faqs/containers/)
- [OpenSSH key commands](https://man.openbsd.org/ssh-keygen)
- [Extension permissions](https://developer.chrome.com/docs/extensions/develop/concepts/declare-permissions)
- [FIDO passkeys](https://fidoalliance.org/passkeys/)
- [Advanced Protection compatibility](https://support.google.com/accounts/answer/7539956)

Added 2026-09-15: recovery integrity, informed by backup poisoning described in
[Anthropic's September 2026 report](https://www.anthropic.com/threat-intelligence-report-september-2026), p. 35.
