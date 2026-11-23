# macOS controls, browsers, isolation, recovery, and the home network

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

The settings below are read in System Settings. Ask Pablo for a pane the
session cannot read.

- **Lock when idle** (Lock Screen): "Require password after screen saver
  begins or display is turned off" and the two "Turn display off ... when
  inactive" times. A delay before the password is required, or no display
  timeout, is Medium on a laptop that leaves the house.
- **Unlock** (Touch ID & Password). The checklist advises a password instead
  of biometrics. Do not report Touch ID: the Mac still demands the password
  after a restart, after 48 hours without an unlock and after five failed
  attempts. That password is what protects the disk at power-on. Ask whether
  it is used anywhere else (Medium if so) and is long enough not to be
  guessed; never ask for it. Record whether an Apple Watch unlocks the Mac: it then
  also approves password requests.
- **Accessories** (Privacy & Security, "Allow accessories to connect"; Apple
  silicon laptops). "Ask for New Accessories" is the default. It covers new
  USB and Thunderbolt devices and SD cards, not power adapters,
  non-Thunderbolt displays or hubs already approved. "Always" lets a device
  attach to a locked Mac: Medium. "Automatically When Unlocked" is Low.
- **Find My Mac** ([name] > iCloud > See All > Find My Mac; needs Location
  Services). Turning it on turns on Activation Lock, and a lost Mac that
  comes online can be locked or erased. Off is Medium on a laptop.
- **Configuration profiles** (General > Device Management). A profile can
  install a root certificate, route traffic through a proxy or VPN, or grant
  privacy permissions. `profiles list` shows the current user's profiles;
  computer-level profiles need root, so ask Pablo for the pane. A profile or
  an enrolment Pablo does not recognize is High.
- **Voice assistants and dictation** (Siri, or Apple Intelligence & Siri).
  Record whether Siri is on, whether it listens for a phrase, and "Allow Siri
  when locked", which answers anyone near a locked Mac: Low. Siri sends
  audio, contacts and location to Apple under a random identifier. Record
  third-party dictation and meeting-recording apps and where their audio
  goes. The checklist advises turning assistants off; that is a preference.
- **Analytics** (Privacy & Security > Analytics & Improvements): "Share Mac
  Analytics", "Improve Siri & Dictation", "Share with app developers", "Share
  iCloud Analytics". On is optional hardening.
- **Sleep.** The checklist advises shutting down instead of sleeping. Do not
  report sleep. On Apple silicon the FileVault keys stay inside the Secure
  Enclave, and Apple documents no setting that discards them during sleep
  there: `pmset destroyfvkeyonstandby` predates Apple silicon and its effect
  on it is unconfirmed, so do not recommend it. Shutting down before a border
  crossing belongs to [habits](habits.md).

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

## Browsers and mail clients

Every account is signed in to through a browser. For each installed browser,
and each profile in use, record the state below from the browser's settings
pages or profile metadata; when an agent cannot open a settings page, ask
Pablo for it. Settings that only limit tracking are optional hardening, never
findings. Do not change a setting or relaunch a browser during the audit.

- **Version.** Compare the running version with the vendor's current stable
  release. In Chrome, Help > About Google Chrome shows the version and a
  Relaunch button when an update is downloaded but not applied; no button
  means current. A browser left open for weeks keeps running the old version.
  A pending update is Low, and Medium when its release notes list a
  vulnerability exploited in the wild.
- **Safe Browsing** (Chrome: Settings > Privacy and security > Security).
  Standard protection is the default. "No protection" is Medium. Enhanced
  protection sends each site's URL and samples of page content to Google;
  state that cost and do not recommend it by default.
- **HTTPS** (Chrome: same page, "Always use secure connections"). Chrome warns
  before loading a public site over HTTP by default since version 154, Firefox
  upgrades to HTTPS by default since 136, and Brave upgrades by default.
  Warnings turned off is Low.
- **Secure DNS** (Chrome: same page, under Advanced, "Use secure DNS"). On by
  default in automatic mode, which falls back to unencrypted lookups when the
  current provider fails; a named provider does not fall back. Chrome turns
  it off on a managed device. Off is optional hardening; a named provider
  moves who sees the lookups from the network to that provider.
- **Content blocker.** Record which blocker each general-browsing profile
  runs. Ads carry malware and fake sign-in pages, so no blocker at all is
  Low; which blocker is a preference. Chrome no longer runs uBlock Origin
  (Manifest V2 extensions are disabled since Chrome 138 and left the store on
  2026-08-31); it offers uBlock Origin Lite, which its author does not call
  an equal replacement. Firefox runs the full extension and Brave blocks by
  default. A blocker reads every page, so it also goes through the extension
  inventory below.
- **Third-party cookies** (Chrome: Settings > Privacy and security >
  Third-party cookies): Allow or Block. Chrome allows them outside Incognito
  unless changed, Brave blocks them, and Firefox isolates them per site.
  Allowed is optional hardening.
- **Cookies and sessions.** The checklist advises clearing cookies regularly.
  Do not recommend it: it signs him out everywhere and does nothing against a
  process that reads the profile today. The checks that matter are signed-in
  sessions on root accounts ([accounts](accounts.md)) and which profile holds
  which sign-ins (above).
- **Browser, search engine and fingerprint.** Record the browser and default
  search engine. These are tracking preferences. Name the workflows that
  depend on the current browser before mentioning an alternative, and do not
  report a browser's fingerprint as a gap.
- **Remote content in mail.** Loading a message's images tells the sender it
  was opened. Gmail on the web: Settings > See all settings > Images, "Ask
  before displaying external images"; Google says image loading there cannot
  reveal his computer or location or set cookies, but may show that he opened
  the message. Apple Mail: Mail > Settings > Privacy, "Protect Mail
  Activity", or "Block All Remote Content" when that is off. For mail read in
  Emacs or another local client, establish from its configuration whether
  HTML mail fetches remote images when displayed: a local client fetches from
  his own address, which the sender then learns. Automatic loading is
  optional hardening in Gmail on the web and Low in a client that fetches
  directly.

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
- The helper lists only the grants that reach everything. Camera,
  Microphone, Contacts, Calendars, Photos, Bluetooth and Speech Recognition
  sit in the per-user database, and Location Services and Local Network are
  stored elsewhere, so take those lists from System Settings > Privacy &
  Security. A generic executor holding Camera or Microphone is Medium: any
  script it runs can record. Another app holding one with no use for it is
  Low. Do not recommend a camera cover: the indicator light cannot be
  bypassed by software, the microphone is disconnected in hardware when the
  lid is closed, and Apple warns that closing a laptop on a cover can damage
  the display.
- List installed apps (`/Applications`, `~/Applications`) with
  `mdls -name kMDItemLastUsedDate`. The date records only opens through
  LaunchServices, so a missing date is not proof of disuse. Every installed
  app is code that can update itself and keep helpers running. An app unused
  for a year is cleanup; one that also holds a privacy grant or a launch item
  is Low. Uninstalling is remediation.
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
  Check that at least one copy is offline: not mounted, not on a share the
  Mac reaches with stored credentials, and not deletable through a signed-in
  sync client. A network share the Mac mounts on a schedule is reachable by
  malware running as Pablo. If every copy is reachable from the Mac, report
  Medium.
- Assess backup integrity separately from freshness and encryption: available
  historical versions, their provenance, and whether a compromised host/account
  could alter retained copies or contaminate new ones. After an established
  incident, look for a trusted recovery point predating the earliest plausible
  compromise; a timestamp alone does not prove cleanliness. Assess how executable
  files, startup configuration, and restored credentials will be checked before
  reuse. A successful restore or clean malware scan alone does not rule out
  persistence. Unknown compromise timing or unavailable integrity evidence remains
  a recovery gap; do not mount, execute, or restore suspect backups during an audit.

- Ask where identity documents, tax and medical records are kept, by kind of
  place only. Google Drive encrypts files in transit and at rest but not end
  to end, so whoever takes the Google account, or holds a signed-in session,
  reads them: record this under that account's rank in
  [accounts](accounts.md), not as a separate finding. iCloud's Advanced Data
  Protection adds end-to-end encryption to iCloud Drive, backups, Photos and
  Notes, but not Mail, Contacts or Calendars, and needs a recovery contact or
  key; off is optional hardening. Copies left in mail attachments and chat
  threads are readable by whoever reads that mailbox or chat. A synced
  folder is also readable by any code running as Pablo on the Mac.

## Other devices on Pablo's network

The Mac is not the only way in. A router, camera or home hub that no longer
gets updates sits on the same network. Ask Pablo for an inventory of
always-on devices: router and mesh points, NAS or backup target, cameras,
smart-home hub, printers and the like. The second-factor phone belongs to
[accounts](accounts.md). For each device, record:

- make and model, and whether the vendor still ships security updates for it,
  from the vendor's current support or end-of-life page;
- whether updates install automatically;
- whether its admin account still uses the default password or a reused one,
  and whether remote administration from the internet is on;
- for a smart-home device, the vendor account that controls it. That account
  belongs in the accounts domain: a lock or camera whose vendor account has
  no second factor is Medium;
- whether it has a microphone or camera, and a physical mute or shutter.

Do not log in to a device or scan the network during the audit. A router or
other internet-facing device that no longer gets updates is High; a device
reachable only from the home network is Medium. The remedy is to replace or
unplug it.

A device or network name that gives away the make and model, a vendor's
data-sharing settings, and a vendor account in Pablo's real name are
preferences from the checklist: record the first two as optional hardening
and do not report the third.

### Wi-Fi

- **Security mode.** Option-clicking the Wi-Fi menu shows the current
  network's security standard; otherwise ask Pablo. Apple recommends WPA3
  Personal, or WPA2/WPA3 Transitional for older devices, and accepts WPA2
  Personal (AES) where nothing better works. Open and WEP networks are High.
  WPA, TKIP and WPA/WPA2 mixed modes are Medium. WPA2 (AES) with WPA3
  available on the router is optional hardening.
- **Passphrase.** Ask whether it is still the one printed on the router, and
  who outside the household has it. Never ask for it, read it, or capture
  it: do not read Wi-Fi keychain items or router pages, and do not
  screenshot or dump a phone or app screen that has a Wi-Fi password field,
  since such fields are often prefilled.
- **WPS.** Its PIN mode bypasses the passphrase. On is Medium.
- **Separation.** Whether guests and smart-home devices use a network apart
  from the Mac, phone and backup target. Not separated is optional
  hardening, and Medium when one of those devices no longer gets updates.

### Untrusted networks

Ask what Pablo does on hotel, café and conference Wi-Fi. HTTPS protects the
content of what he opens. The network still sees which sites he visits and
can tamper with anything unencrypted. A VPN or a tailnet exit node moves that
view to whoever runs it, and a phone hotspot avoids the network. The
checklist calls a paid VPN essential. Record what he uses; using none is
optional hardening, never a finding.

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

Added 2026-11-23 from the Essential items of the
[Personal Security Checklist](https://digital-defense.io/), vendor pages
checked the same day: browsers and mail clients, lock, unlock, accessories,
Find My Mac, configuration profiles, voice assistants, analytics, sleep,
other privacy grants, installed apps, where personal documents live,
smart-home accounts, Wi-Fi and untrusted networks. Not confirmed there: where
"Start Screen Saver" sits in macOS 27, and whether `profiles status -type
enrollment` runs without root. See [checklist coverage](checklist-coverage.md).

- [Chrome security settings](https://support.google.com/chrome/answer/10468685)
- [Chrome Safe Browsing levels](https://support.google.com/chrome/answer/9890866)
- [Chrome third-party cookies](https://support.google.com/chrome/answer/95647)
- [Chrome updates](https://support.google.com/chrome/answer/95414)
- [Chrome 154 release notes](https://developer.chrome.com/release-notes/154)
- [Manifest V2 timeline](https://developer.chrome.com/docs/extensions/develop/migrate/mv2-deprecation-timeline)
- [uBlock Origin](https://github.com/gorhill/uBlock)
- [Firefox 136 release notes](https://www.firefox.com/en-US/firefox/136.0/releasenotes/)
- [Brave Shields](https://brave.com/shields/)
- [Gmail external images](https://support.google.com/mail/answer/145919)
- [Apple Mail privacy](https://support.apple.com/guide/mail/protect-email-privacy-mlhlp1205/mac)
- [Mac accessory security](https://support.apple.com/en-us/102282)
- [Touch ID on Mac](https://support.apple.com/en-us/105095)
- [Mac lock screen settings](https://support.apple.com/guide/mac-help/mh11784/mac)
- [Find My Mac](https://support.apple.com/guide/findmy-mac/fmm53101237/mac)
- [Activation Lock for Mac](https://support.apple.com/en-us/102541)
- [Mac configuration profiles](https://support.apple.com/guide/mac-help/mh35561/mac)
- [Siri settings on Mac](https://support.apple.com/guide/mac-help/mchl3fd7fc15/mac)
- [Ask Siri, Dictation & Privacy](https://www.apple.com/legal/privacy/data/en/ask-siri-dictation/)
- [Mac Privacy & Security settings](https://support.apple.com/guide/mac-help/mchl211c911f/mac)
- [FileVault on Apple silicon](https://support.apple.com/guide/security/sec4c6dc1b6e/web)
- [Mac camera covers](https://support.apple.com/en-us/102177)
- [Mac microphone disconnect](https://support.apple.com/guide/security/secbbd20b00b/web)
- [kMDItemLastUsedDate](https://developer.apple.com/documentation/coreservices/kmditemlastuseddate)
- [Advanced Data Protection](https://support.apple.com/en-us/102651)
- [Google Drive encryption](https://support.google.com/drive/answer/10375054)
- [Apple's recommended router settings](https://support.apple.com/en-us/102766)

Added 2026-11-15: the offline backup copy and other devices on the network,
informed by [The Hackening](https://thehackening.vercel.app/#advice).

Added 2026-09-15: recovery integrity, informed by backup poisoning described in
[Anthropic's September 2026 report](https://www.anthropic.com/threat-intelligence-report-september-2026), p. 35.
