# Online accounts, breach exposure, and recovery

This domain assumes an attacker who never touches the Mac: one who replays
breached passwords, phishes, abuses account recovery, or takes over a phone
number. State which accounts and email addresses are in scope before checking.
Employer-administered settings (Epoch, Forethought) are out of scope; check only
what Pablo's own user pages show, and name any admin-only control as not checked.

Every check here is read-only. Do not change a setting, sign out a session or
device, remove an app grant, edit forwarding or filters, rotate a password, or
complete a security-checkup action. Load `chrome-use` before the first browser
step. Account security pages are sensitive sites: request one-time access, not a
permanent grant. A re-authentication prompt is Pablo's step; never type or
autofill his password. Never reveal, copy, or screenshot a password, recovery
code, or Secret Key, and never submit a password to a breach checker.

## Reset graph and root accounts

- Inventory the email addresses from existing configuration (mail contexts, git
  identities, `service-access.md`, `google-services.md`) rather than guessing.
- Build the reset graph: for each root account (each mailbox, Apple Account, the
  1Password accounts, GitHub, and the registrar/DNS for any domain that receives
  mail), record which mailbox or phone number can reset it and which second
  factors it accepts. The finding is the shortest takeover path: a single phone
  number or mailbox that reaches everything, a cycle in which each account can
  recover the other, or a recovery address on a lapsed or shared domain.
- A domain whose registrar account is taken over means the attacker controls
  mail for that domain and can reset every account that uses it. Check registrar
  2FA and transfer lock where Pablo's own account pages show them. Find the
  registrar account that actually holds a domain from its renewal notices
  before asking Pablo to sign in; one person can have several registrar
  accounts, and an empty domain list means the wrong one.

## Breach exposure

- Search each address with Have I Been Pwned's free website search in the
  browser. The `breachedaccount`, paste, and stealer-log APIs need a paid key; do
  not buy one. Stealer-log and paste coverage are not checked unless an existing
  subscription provides them. Treat stealer-log hits as higher priority than
  ordinary breaches, because they imply malware on some device that held the
  session or password.
- For each breach record its name, date, and data classes. A breach that leaked
  only a phone number, address, or birthday still matters: it supports phishing
  and SIM-swap attempts, which links it to the phone-number checks below.
- Match each breached site to vault items by title only. Whether the password
  changed after the breach comes from Watchtower's compromised-website report,
  not from opening the item. A breached site with no vault item is an unknown:
  the account may be abandoned, or its password may be stored somewhere else.

## Vault hygiene

- For each 1Password account in `op-desktop account list`, read Watchtower's
  counts and item titles for compromised websites, vulnerable passwords, reused
  passwords, weak passwords, and available-but-unused two-factor authentication.
  Watchtower uses Have I Been Pwned data. Do not open an item's details or
  reveal a field to confirm a finding. If Watchtower is disabled in settings,
  report it as not checked instead of enabling it. The 1Password web app ends
  its session when its tab navigates by URL, so once Pablo signs in, reach
  Watchtower through the app's own navigation, never by loading a URL.
- Prioritize reused and compromised passwords on root accounts. A reused password
  on a low-value site is low severity unless that password also protects a root
  account.

## Root-account controls

- **Google** (each personal account; work accounts only where the user pages
  allow it): the Security Checkup, 2-Step Verification methods (passkeys,
  security keys, authenticator, SMS), recovery phone and email, signed-in
  devices, third-party connections with Gmail or Drive access, and app passwords.
  In Gmail settings, check forwarding addresses and filters that forward,
  delete, or skip the inbox. These are how an attacker keeps reading mail after
  losing the password. Advanced Protection is optional hardening. Mention its
  app-password and third-party compatibility costs, and never enroll during an
  audit.
- **Apple Account:** trusted phone numbers, signed-in devices, and whether a
  recovery contact or recovery key is set. iPhone Stolen Device Protection
  cannot be read from the Mac; ask Pablo or mark it not checked.
- **1Password:** account two-factor status, authorized devices, and whether an
  offline Emergency Kit exists. Ask about the Emergency Kit; never locate or
  open it.
- **GitHub:** 2FA methods, SSH and signing keys, tokens, OAuth and GitHub App
  grants, and sessions, read through `gh api` metadata endpoints or the settings
  pages. Never print token values.

## Phone-number exposure

List the root accounts where SMS or a phone call is a second factor or a
recovery path. That number is only as safe as the carrier's port-out and
SIM-replacement protections. Carrier protections cannot be verified from here;
ask Pablo whether a port-out PIN or number lock is set, or mark it not checked.
SMS as a fallback next to a passkey still leaves the SMS path open.

## Severity guidance

- Critical: a forwarding address or filter Pablo does not recognize, an unknown
  signed-in device or app grant on a root account, or a stealer-log hit newer
  than the last password change for that account.
- High: a root account whose only second factor or recovery path is SMS, or a
  compromised-website item for a root account that has not changed since the breach.
- Medium: a reused password shared with a root account, or a breached
  non-root account with an unchanged password and no second factor.
- Low or optional hardening: breach data classes without credentials, weak
  passwords on throwaway accounts, Advanced Protection not enrolled.

## Reference maintenance

Checked 2026-10-17.

- [Have I Been Pwned API v3](https://haveibeenpwned.com/API/v3)
- [1Password Watchtower](https://support.1password.com/watchtower/)
- [Google Security Checkup](https://myaccount.google.com/security-checkup)
- [Gmail forwarding](https://support.google.com/mail/answer/10957)
- [Advanced Protection compatibility](https://support.google.com/accounts/answer/7539956)
- [Apple Account security](https://support.apple.com/en-us/102614)
- [GitHub account security](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure)
