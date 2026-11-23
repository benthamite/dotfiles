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
- Ask where each root account's backup codes are kept, by kind of place only.
  Codes in the same vault as the password are no independent way back in if
  the vault is lost or taken. Codes in a file in a synced folder make that
  cloud account a path to every account whose codes it holds: add the edge to
  the graph. Paper kept apart from the phone is the independent copy.
- Record which address signs in to each root and money account. An address
  published on his site, profiles or commits is the one phishing and reset
  attempts target, and the one breaches list. A published address that also
  signs in to and recovers root accounts is optional hardening, not a finding;
  the remedy is an unpublished address or alias used only for those sign-ins.
  Do not recommend withdrawing an address he publishes on purpose.
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
- Ask which accounts Pablo shares with another person, and how the password
  reached them. For shared vaults, ask for vault names and member names only.
  A shared account is as safe as the other person's devices, and a password
  sent by message or email stays in both histories. A root or money account
  shared outside a vault is Medium; a vault member who no longer needs access
  is cleanup.

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
  belong in the reset graph), and whether the password is unique. Also:
  - alerts for sign-ins, transfers, and changes to contact details or payees:
    on, and delivered where Pablo sees them. A mail filter that files them
    away disables them;
  - per-day and per-transfer limits, and how much sits in payment apps that
    move money instantly. Money that moves slowly limits what a takeover can
    drain;
  - every app, aggregator, AI tool or automation connected to the account,
    Pablo's own included, and whether its access can move money or only read.
    A connection that can move money and does not need to is High.

  For cloud and AI providers, also API keys by name and age, spend limits or
  alerts, and connected apps.
- **AI providers:** the same, plus who else is in the organization or workspace
  and which connectors reach Pablo's mail, drive or code.
- **Identity:** whether the national tax and identity portals use a second
  factor, and which phone or email recovers them. In Argentina, ARCA's Clave
  Fiscal level 4 adds the Token ARCA app as a second factor; Mi Argentina
  documents none, so its password and recovery address carry it alone.
- **Credit files:** ask Pablo in which countries he has a credit file. For
  each, check on the regulator's or bureaus' current pages what the country
  offers against credit opened in his name (a freeze, a fraud alert,
  monitoring) and whether it is on. Argentina offers no freeze (see account
  discovery). A free freeze left off is Medium.
- **Communication:** second factor and recovery for each social account.
  WhatsApp two-step verification (now a password; older accounts may still
  have a 6-digit PIN) and its recovery email, and Signal's Registration Lock.
  Telegram's Two-Step Verification (Settings > Privacy & Security) matters
  more: its ordinary chats are kept on Telegram's servers, so a new sign-in
  also gets the history.
  Without them, whoever receives the SMS registration code, for instance after
  a SIM swap, takes over the account and its contacts. As optional hardening,
  ask whether Pablo and his family have agreed how to verify an urgent request
  for money or codes by phone or message, such as calling back on a known
  number or a code word: a cloned voice makes a familiar voice no proof.

## Messaging apps

Registration takeover is covered above. This section covers who else can read
the messages. List the messengers Pablo uses and what each carries, then ask
him for each view below. Never unlink a device or change a setting.

- **Encryption.** Record whether a messenger's ordinary chats are end-to-end
  encrypted. WhatsApp's and Signal's are. On Telegram only Secret Chats are;
  its private and group chats are readable by Telegram's servers. Sensitive
  conversations in a messenger without end-to-end encryption are Low; the
  remedy is to move those conversations. The checklist also asks for
  open-source messengers only. Record which are, and do not report a
  closed-source end-to-end encrypted messenger he relies on as a gap.
- **Linked devices.** WhatsApp: Linked devices, up to four; each receives a
  copy of recent history, works while the phone is offline, and is logged out
  after 30 days unused. Signal: Settings > Linked devices, up to five,
  unlinked after 45 days unused. Telegram: Settings > Devices; its FAQ
  documents no automatic expiry. A linked device Pablo does not recognize is
  Critical, like an unknown signed-in device on a root account. One he no
  longer uses is cleanup. The checklist advises against desktop and web
  companions; do not recommend unlinking one a workflow depends on. Record
  each linked session on the Mac, and any local archive of messages, as an
  asset that code running as Pablo can read.
- **Backups.** WhatsApp: Settings > Chats > Chat backup > End-to-end encrypted
  backup. A backup in Google or iCloud storage is end-to-end encrypted only
  when he has turned that on. Without it, the cloud account can restore the
  whole history: Medium, and an edge in the reset graph. Signal's Secure
  Backups are opt-in and end-to-end encrypted under a recovery key; ask where
  the key is kept, by kind of place only.
- **Security notifications.** WhatsApp: Settings > Account > Security
  notifications, set per device. A notice that a contact's code changed is
  the visible sign that their account moved to another phone, as after a
  takeover. Signal always shows safety-number changes. Off is optional
  hardening.
- **Group adds.** WhatsApp: Settings > Privacy > Groups. The default,
  Everyone, lets a stranger add him to a group, a common opening for scams.
  Optional hardening.
- **Presence.** Read receipts, last seen and typing indicators are
  preferences. Do not report them.

## Social accounts and public exposure

The second factor and recovery of each social account are covered above.
This section covers what else can act through an account, and what the
accounts tell an attacker.

- **Connected apps.** Read the page listing third-party apps with access.
  Google: `myaccount.google.com/linkedapps`. X: Apps and sessions > Connected
  apps. Facebook: Settings > Apps and websites. LinkedIn: Settings & Privacy >
  Data privacy > Other applications > Permitted Services. Instagram: Apps and
  websites, in its settings. An app Pablo no longer uses that can post or
  read messages is Medium: a breach of its vendor acts as him. One that can
  only read his profile is cleanup. An app he does not recognize on a root
  account is Critical (severity guidance).
- **Sign in with another account.** An account opened through "Sign in with
  Google" or another provider is recovered by that provider's account: add
  the edge to the reset graph. Google's linked-apps page has a filter that
  lists them. The checklist advises against such sign-ins. Do not report one
  that hangs off an account with a strong second factor; report Medium when
  the provider account has no second factor or only SMS.
- **Audience settings.** For each social account, record who can see posts,
  the contact list, and his phone and email, and whether the profile can be
  found by phone number or email address. Lookup by phone number confirms to
  a stranger which number is his: Low.
- **Public data.** Read his public profiles and site as a signed-out visitor
  and search his name. List which of these are public: date of birth, phone
  number, home address, identity or tax numbers, family members, employer,
  travel plans. Then compare the list with what his carrier, banks and
  account-recovery flows accept as proof of identity. A public datum that a
  provider on a root path accepts as proof is Medium, linked to the
  phone-number checks below. Anything else public is optional hardening. A
  search sends its terms to the search engine: get Pablo's agreement to
  search his name before the first query, and never search a phone number,
  address or identity number.
- **People-search listings.** Record any listing site the search finds.
  Asking for removal is remediation.

## The second-factor phone

The phone that receives SMS codes, runs the authenticator or holds passkeys is
a root device: whoever unlocks it, or reads its lock screen, receives most
second factors. Its settings cannot be read from the Mac. Ask Pablo for each
item below; one he cannot answer is not checked, and an unchecked phone goes in
the summary as an unverified high-risk item.

- **Updates:** the phone still gets security updates (the maker's current
  support page gives the end date) and installs them; one that no longer does
  is High, as it holds the second factors.
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
- **Encryption:** every Pixel, and every phone that shipped with Android 10
  or later, encrypts its storage by default. The data is protected only by
  the screen lock, so a weak lock is the finding, not encryption. Ask about
  encryption only for an older phone.
- **Finding and erasing:** Android: Settings > Security > Find Hub (formerly
  Find My Device), "Allow device to be located". It locates, locks ("Mark as
  lost") and factory-resets a phone that is on, online and signed in to a
  Google Account. iPhone: Settings > [name] > Find My. Off is Medium: a lost
  phone that holds the second factors cannot be locked or wiped. Erasing it
  means signing in to that account from another device, so check that a
  second factor for the account exists off the phone.
- **App sources:** Google Play Protect (Play Store > profile > Play Protect >
  Settings, "Scan apps with Play Protect") is on by default; off is Medium.
  Settings > Apps > Special app access > Install unknown apps lists which
  apps may install others. An alternative store Pablo chose is recorded with
  what he installs from it. A browser, messenger or file manager allowed
  there is Medium: one tap on a downloaded file installs it.
- **Special access:** ask for the apps listed under Accessibility,
  "Notification read, reply & control", Device admin apps and Display over
  other apps, and those holding SMS in the Permission manager. Google's own
  warning for Accessibility is that the app "can read content on your screen
  and interact with apps on your behalf"; notification and SMS access read
  every code that arrives. An app there that Pablo does not recognize or need
  is High on this phone. Location, camera, microphone and contacts held by
  apps with no use for them are Low.
- **App count:** unused apps are removed, and "Pause app activity if unused"
  is on for the rest. Cleanup.
- **Stalkerware:** required when the habits interview says someone else knows
  the PIN or has had the unlocked phone, optional otherwise. Ask about apps
  he did not install, the special-access lists above, an unknown fingerprint
  or face, cloud backup turned on by someone else, and battery or data use
  with no explanation; some stalkerware shows none of these. On an iPhone,
  also Safety Check (Settings > Privacy & Security) and Settings > General >
  VPN & Device Management for profiles. A suspected case is High. Do not
  remove anything: removal can alert whoever installed it and destroys
  evidence. Pablo decides the next step, from another device.
- **Other phones and tablets** signed in to a root account get the same
  questions.

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
  signed-in device or app grant on a root account, a linked device on a
  messenger that Pablo does not recognize, or a stealer-log hit newer
  than the last password change for that account.
- High: a root account whose only second factor or recovery path is SMS, a
  compromised-website item for a root account that has not changed since the
  breach, an account Pablo did not open, the 1Password account password used
  elsewhere, a second-factor phone with no screen lock or past its last
  security update, a connection that can move money without needing to, an
  unrecognized or unneeded app with Accessibility, notification or SMS access
  on the second-factor phone, or suspected stalkerware.
- Medium: a reused password shared with a root account, a breached non-root
  account with an unchanged password and no second factor, a money account
  without a second factor or with its alerts off or filtered away, root-account
  backup codes in a synced file, a free credit freeze left off, lock-screen
  notifications that show SMS codes, a pattern lock or theft protection off on
  the second-factor phone, or WhatsApp, Signal or Telegram without two-step
  verification or Registration Lock. Also: a root or money account shared
  outside a vault, a WhatsApp cloud backup without end-to-end encryption,
  Find Hub or Find My off on the second-factor phone, Play Protect off, a
  browser or messenger allowed to install apps, an unused connected app that
  can post or read messages, a sign-in provider with no second factor or only
  SMS, or a public datum that a root-path provider accepts as proof of
  identity.
- Low or optional hardening: breach data classes without credentials, weak
  passwords on throwaway accounts, a dormant low-value account left open, no
  SIM PIN, transfer limits or payment-app balances above need, no agreed way
  to verify money requests, Advanced Protection not enrolled, sensitive
  conversations in a messenger without end-to-end encryption, a profile
  findable by phone number, WhatsApp security notifications off or group adds
  open to everyone, a published address that also signs in to root accounts,
  other public personal data.

## Reference maintenance

Checked 2026-10-17; tiers, discovery, the second-factor phone, other account
tiers and carrier questions checked 2026-11-15. Money-account alerts, limits
and connections, backup-code storage, credit files and money-request
verification added 2026-11-15, informed by
[The Hackening](https://thehackening.vercel.app/#advice). Shared accounts,
sign-in addresses, messaging apps, social accounts and public exposure, and
the phone's encryption, finding, app-source, special-access and stalkerware
checks added 2026-11-23 from the Essential items of the
[Personal Security Checklist](https://digital-defense.io/); vendor pages
checked the same day. See [checklist coverage](checklist-coverage.md).

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
- [WhatsApp linked devices](https://faq.whatsapp.com/378279804439436/?locale=en_US)
- [WhatsApp end-to-end encrypted backup](https://faq.whatsapp.com/490592613091019/?locale=en_US)
- [WhatsApp security notifications](https://faq.whatsapp.com/1524220618005378/?locale=en_US)
- [WhatsApp group privacy](https://faq.whatsapp.com/general/security-and-privacy/how-to-change-group-privacy-settings/?locale=en_US)
- [Signal linked devices](https://support.signal.org/hc/en-us/articles/360007320551-Linked-Devices)
- [Signal safety numbers](https://support.signal.org/hc/en-us/articles/360007060632-What-is-a-safety-number-and-why-do-I-see-that-it-changed)
- [Signal Secure Backups](https://support.signal.org/hc/en-us/articles/9708267671322-Signal-Secure-Backups)
- [Telegram FAQ](https://telegram.org/faq)
- [Google linked apps](https://support.google.com/accounts/answer/13533235)
- [X connected apps](https://help.x.com/en/managing-your-account/connect-or-revoke-access-to-third-party-apps)
- [Facebook apps and websites](https://www.facebook.com/help/170585223002660)
- [LinkedIn permitted services](https://www.linkedin.com/help/linkedin/answer/a519947)
- [Android encryption](https://source.android.com/docs/security/features/encryption)
- [Find Hub](https://support.google.com/android/answer/6160491)
- [Google Play Protect](https://support.google.com/googleplay/answer/2812853)
- [Android unknown apps](https://support.google.com/pixelphone/answer/7391672)
- [Android restricted settings](https://support.google.com/android/answer/12623953)
- [Android notification access](https://support.google.com/android/answer/15235441)
- [Android unused apps](https://support.google.com/android/answer/9431959)
- [Coalition Against Stalkerware](https://stopstalkerware.org/stalkerware-detection-removal-and-prevention/)
- [Apple Personal Safety User Guide](https://support.apple.com/guide/personal-safety/welcome/web)
- [BCRA Central de Deudores](https://www.bcra.gob.ar/situacion-crediticia/)
- [Nosis access right](https://www.nosis.com/es/legales/derecho-acceso)
- [Equifax Argentina (Veraz) privacy and access](https://www.soluciones.equifax.com.ar/legal/politica-privacidad/)
- [BCRA claims about financial products](https://www.bcra.gob.ar/reclamo-productos-servicios-financieros/)
- [ENACOM portability](https://www.enacom.gob.ar/portabilidad/)
- [ENACOM Res. 1391/2024, SIM changes](https://www.boletinoficial.gob.ar/detalleAviso/primera/319070/20241231)
- [ARCA Clave Fiscal levels](https://www.argentina.gob.ar/servicio/obtener-la-clave-fiscal)
