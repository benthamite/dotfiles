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

- Review existing Full Disk Access, Accessibility, Automation, Input Monitoring,
  and screen-recording grants for terminals, editors, agents, and their helpers.
  Use already permitted settings/metadata access; do not grant access or open
  protected databases to complete the audit. Explain which workflow inherits
  the access. Route a specific questionable app to `audit-mac-app`.
- Inspect modern background-task registrations (Apple's `sfltool dumpbtm`,
  locally filtered) and metadata for `~/Library/LaunchAgents`,
  `/Library/LaunchAgents`, and `/Library/LaunchDaemons`. Include legacy login
  items where relevant. Review executable path, ownership, signature/provenance,
  and the command's access; an unfamiliar name alone is not malware evidence.
  Do not print raw plist environment values or arbitrary program arguments.
- Inventory browser extensions across applicable profiles. Prioritize granted
  host access, cookies/debugger/native-messaging privileges, install/update
  origin, and continued need. Publisher size is supporting context only.
  Keep extension permissions, agent site grants, and OAuth grants distinct.

## Authentication and recovery

- Recognize Apple Passwords, browser managers, `pass`, and CLI workflows, as
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
- Google Advanced Protection is an optional account-hardening recommendation.
  Check existing evidence without assuming enrollment, setup time, or access.
  Mention app-password and third-party compatibility implications when relevant;
  never change enrollment or test recovery during an audit.
- Check backup freshness, encryption, destination access, and available
  independent recovery copies using metadata. Distinguish a completed backup
  from a demonstrated restore. A restore test belongs to explicitly authorized
  remediation/verification in a disposable destination; never overwrite live
  data. Consider whether the same compromised account can delete every copy.

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
