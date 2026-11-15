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

## Account tiers

Rank account findings and their TODOs by what a takeover reaches:

1. **Root:** each mailbox, the Apple Account, the 1Password accounts, GitHub,
   registrars for domains that receive mail, the phone number, and the phone
   that holds second factors.
2. **Money:** banks, payment apps, brokers, crypto exchanges, and cloud or AI
   providers that bill a card or hold API keys that can run up charges.
3. **Identity:** national tax and identity portals and health records.
4. **Communication:** messaging apps and social accounts. A takeover lets an
   attacker impersonate Pablo to his contacts, a common opening for scams
   aimed at them, so do not rank these as low value.
5. **Everything else.**

AI providers also hold work data and conversation history; rank one by the
higher of its billing and data exposure.

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

## Account discovery

Configuration names only the accounts tools use; it misses the rest. Search
each personal mailbox in the inventory above with `gmail.py query` (per
`service-access.md`; it prints sender, date and subject only) for
account-lifecycle mail: welcome or verify-your-address messages, new sign-in
and new-device alerts, security alerts, password resets, and 2FA changes.
Never fetch a message body. Group the results by sender domain, rank the
senders by the tiers above, and compare them with vault item titles. Add any
root account found this way to the reset graph.

- **An account Pablo does not recognize** is an identity-fraud lead: ask him
  first. If he did not open it, report a High finding. The next steps are
  Pablo's, outside the audit: contact the provider, and check credit records
  for products opened in his name. In Argentina (checked 2026-11-15) that
  means the BCRA Central de Deudores lookup by CUIT (24 months, updated
  monthly), the free Mi Nosis app, and Veraz's free access request (once every
  six months). Argentina has no credit freeze or fraud alert; a false record
  is contested with a police or court complaint and a claim to the lender,
  escalated to the BCRA after 10 business days. Veraz's free alerts, which
  email when his report is consulted or changes, are optional hardening.
- **A dormant account** (no sign-in mail for years, or a site Pablo no longer
  uses) is better closed than rotated: a closed account cannot be taken over.
  Recommend closing it; closing is remediation.
- **An account with no vault item** is an unknown: its password may be reused
  or stored elsewhere. Report it; do not ask Pablo to sign in during the audit.

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
  its session when its tab navigates by URL. Its vault and Watchtower views are
  also masked from agent page reads and screenshots by the credential-page
  policy, so Watchtower is Pablo's step: ask him for the counts and the
  root-account titles in each category instead of asking him to sign in.
  He shares counts and item titles only, typed or as a screenshot of the
  Watchtower list. Decline a vault export: it holds every password. Delete
  a screenshot he shares once its counts and titles are recorded.
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
  recovery contact or recovery key is set.
- **1Password:** account two-factor status, authorized devices, whether a
  recovery code is set up, and whether an Emergency Kit exists. Ask Pablo two
  questions, never for the secrets themselves:
  - Is the account password used anywhere else? High if it is.
  - Where are the Emergency Kit copies, by kind of place only? 1Password
    itself suggests a printed copy, a personal cloud copy and one with a
    trusted person. The exposure is a copy with the account password written
    in that sits in a synced, shared or emailed location: it holds both
    secrets together. Never locate or open a kit.

  Key derivation is fixed by 1Password and passkey unlock is a closed beta
  (checked 2026-11-15), so neither is a finding.
- **GitHub:** 2FA methods, SSH and signing keys, tokens, OAuth and GitHub App
  grants, and sessions, read through `gh api` metadata endpoints or the settings
  pages. Never print token values.

## Money, identity and communication accounts

Check these after the root accounts, in tier order, from Pablo's own settings
pages or his answers:

- **Money:** second factor and its method, the recovery email and phone (they
  belong in the reset graph), and whether the password is unique. For cloud and
  AI providers, also API keys by name and age, spend limits or alerts, and
  connected apps.
- **AI providers:** the same, plus who else is in the organization or workspace
  and which connectors reach Pablo's mail, drive or code.
- **Identity:** whether the national tax and identity portals use a second
  factor, and which phone or email recovers them. In Argentina, ARCA's Clave
  Fiscal level 4 adds the Token ARCA app as a second factor; Mi Argentina
  documents none, so its password and recovery address carry it alone.
- **Communication:** second factor and recovery for each social account.
  WhatsApp two-step verification (now a password; older accounts may still
  have a 6-digit PIN) and its recovery email, and Signal's Registration Lock.
  Without them, whoever receives the SMS registration code, for instance after
  a SIM swap, takes over the account and its contacts.

## The second-factor phone

The phone that receives SMS codes, runs the authenticator or holds passkeys is
a root device: whoever unlocks it, or reads its lock screen, receives most
second factors. Its settings cannot be read from the Mac. Ask Pablo for each
item below; one he cannot answer is not checked, and an unchecked phone goes in
the summary as an unverified high-risk item.

- **Screen lock:** a PIN of six or more digits or a password. Google recommends
  either over a pattern, which has fewer combinations and is easier to guess.
- **Theft protection** (Android: Settings > Google > All services > Theft
  protection): Theft Detection Lock, Offline Device Lock, Remote Lock, and
  Identity Check where the device supports it. On an iPhone, Stolen Device
  Protection.
- **Lock-screen notifications:** sensitive content hidden (on a Pixel,
  Settings > Notifications > Notifications on lock screen > Show sensitive
  content off). Otherwise a thief holding the locked phone reads SMS codes and
  can reset any account where SMS is a factor.
- **Authenticator backup:** Google Authenticator signed in to a Google Account
  syncs its codes there, so that account can restore every code it holds and
  belongs in the reset graph above them. Google documents encryption in transit
  and at rest, not end-to-end. Codes kept only on the device are lost with it
  unless each account has backup codes. Record which applies.
- **SIM PIN:** optional hardening against use of a stolen SIM (see below for
  what it does not cover).

## Phone-number exposure

List the root accounts where SMS or a phone call is a second factor or a
recovery path. That number is only as safe as the carrier's port-out and
SIM-replacement protections, which vary by country, carrier and plan.
Carrier protections cannot be verified from here; ask Pablo for each such
number:

- its country, carrier and plan type;
- who holds the account: on a family or corporate plan the account holder,
  not Pablo, can request a replacement SIM or a port, so that person and
  their carrier account become part of the reset graph;
- which port-out or SIM-replacement protections the carrier offers and which
  are on. Check the carrier's current page for what exists; do not assume a
  port-out PIN does. In Argentina (checked 2026-11-15) Claro, Movistar and
  Personal document no lock a customer can set. ENACOM requires carriers to
  re-check identity biometrically before changing a SIM, and a port needs a
  4-digit PIN sent by SMS, which the holder of a postpaid account can direct
  to another of its lines. The account holder (titular) is therefore the
  person who matters.

A SIM PIN stops someone using a stolen SIM card in another phone. It does not
stop the carrier issuing a replacement SIM to an attacker, so do not accept it
as SIM-swap protection. Where the carrier offers no lock, the remedy is to take
SMS off root accounts. SMS as a fallback next to a passkey still leaves the SMS
path open.

## Severity guidance

- Critical: a forwarding address or filter Pablo does not recognize, an unknown
  signed-in device or app grant on a root account, or a stealer-log hit newer
  than the last password change for that account.
- High: a root account whose only second factor or recovery path is SMS, a
  compromised-website item for a root account that has not changed since the
  breach, an account Pablo did not open, the 1Password account password used
  elsewhere, or a second-factor phone with no screen lock.
- Medium: a reused password shared with a root account, a breached non-root
  account with an unchanged password and no second factor, a money account
  without a second factor, lock-screen notifications that show SMS codes, a
  pattern lock or theft protection off on the second-factor phone, or WhatsApp
  or Signal without two-step verification or Registration Lock.
- Low or optional hardening: breach data classes without credentials, weak
  passwords on throwaway accounts, a dormant low-value account left open, no
  SIM PIN, Advanced Protection not enrolled.

## Reference maintenance

Checked 2026-10-17; tiers, discovery, the second-factor phone, other account
tiers and carrier questions checked 2026-11-15.

- [Have I Been Pwned API v3](https://haveibeenpwned.com/API/v3)
- [1Password Watchtower](https://support.1password.com/watchtower/)
- [Google Security Checkup](https://myaccount.google.com/security-checkup)
- [Gmail forwarding](https://support.google.com/mail/answer/10957)
- [Advanced Protection compatibility](https://support.google.com/accounts/answer/7539956)
- [Apple Account security](https://support.apple.com/en-us/102614)
- [GitHub account security](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure)
- [1Password recovery codes](https://support.1password.com/recovery-codes/)
- [1Password Emergency Kit](https://support.1password.com/emergency-kit/)
- [1Password passkey unlock (beta)](https://support.1password.com/cs/passkeys/)
- [1Password PBKDF2](https://support.1password.com/pbkdf2/)
- [Android theft protection](https://support.google.com/android/answer/15146908)
- [Android screen lock advice](https://www.android.com/articles/how-to-secure-android-phone/)
- [Pixel lock-screen notifications](https://support.google.com/pixelphone/answer/16520562)
- [Google Authenticator sync](https://support.google.com/accounts/answer/1066447)
- [WhatsApp two-step verification](https://faq.whatsapp.com/general/26000021/)
- [Signal Registration Lock](https://support.signal.org/hc/en-us/articles/4850133017242)
- [FCC on SIM swapping](https://docs.fcc.gov/public/attachments/DOC-398483A1.txt)
- [BCRA Central de Deudores](https://www.bcra.gob.ar/situacion-crediticia/)
- [Nosis access right](https://www.nosis.com/es/legales/derecho-acceso)
- [Equifax Argentina (Veraz) privacy and access](https://www.soluciones.equifax.com.ar/legal/politica-privacidad/)
- [BCRA claims about financial products](https://www.bcra.gob.ar/reclamo-productos-servicios-financieros/)
- [ENACOM portability](https://www.enacom.gob.ar/portabilidad/)
- [ENACOM Res. 1391/2024, SIM changes](https://www.boletinoficial.gob.ar/detalleAviso/primera/319070/20241231)
- [ARCA Clave Fiscal levels](https://www.argentina.gob.ar/servicio/obtener-la-clave-fiscal)
