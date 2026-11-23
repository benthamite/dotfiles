# Practices and physical security

This domain covers what no setting records: how Pablo handles links, calls,
documents and devices away from the desk. It is an interview. Every state that
can be read (browser settings, linked devices, public profile data, phone
permissions) belongs to [machine](machine.md) or [accounts](accounts.md); do
not ask here what those references tell you to observe.

## How to ask

- Run it for a comprehensive audit or when `--habits` is selected, never as a
  side effect of another domain.
- Put the questions in one numbered list, each answerable in a few words. Join
  it with the questions the other selected domains have for Pablo, so he
  answers once.
- Ask only the practices the profile (SKILL.md, "Profile") lacks or marks
  unknown. Do not ask again about one it records.
- Record an answer as stated, in the profile, with its date. It is
  self-report, not an observed control; say so in the coverage table.
- Never test him: no simulated phishing message, call or pop-up.
- Do not lecture. Give the reason for a practice only where his answer shows
  it is not in place.

## Reporting

A practice not in place is optional hardening. List it separately from
findings, with the attack it would stop, and give it no severity by itself.
It joins a finding only when a fact observed in another domain completes the
chain: reading codes to callers matters more when SMS still recovers a root
mailbox, and an unattended unlocked laptop matters more with no automatic
lock. Such a chain is reported once, under the observed finding. A practice
gets a TODO only when Pablo asks for one.

Some checklist items are preferences, or do not fit Pablo's situation as the
checklist words them. Ask the narrower question given below and do not report
the wider advice as a gap.

## Links, sign-ins and prompts

- **Where sign-ins start.** From a bookmark, a typed address or the password
  manager, not from a link in a message. A padlock or `https` shows the
  connection is encrypted, not that the site is the real one.
- **Autofill as the signal.** The password manager fills only on the domain it
  saved. Ask what he does when it offers nothing: checking the address first
  is the practice; pasting the password regardless defeats the check. Sites
  where autofill is known to fail should say so in the vault item, so a
  refusal elsewhere stays meaningful.
- **Requests for money, codes or changed payment details** that arrive by
  email or message, even from a known sender: confirmed on a second channel he
  already has, such as a number from his contacts. Sender names and addresses
  can be forged.
- **Pop-ups and browser warnings.** A page or notification that asks him to
  install, update, call a number or sign in is closed, and updates come from
  the app's own updater. A browser warning about a site is not clicked past.

## Calls

- **Inbound calls.** A caller who says they are from a bank, carrier or
  vendor gets no personal data, password or code. He hangs up and calls the
  number printed on the card or the official site. A one-time code is never
  read aloud to anyone.

## Sharing and posting

- **Posts are public and permanent.** He posts assuming anyone can read it and
  that it cannot be withdrawn, whatever the audience setting.
- **Location.** Posts that show where he is go up after he has left,
  especially while travelling, since they also show the home is empty.
- **Photos and uploads.** Checked for what the background shows: screens,
  documents, tickets, keys, the street outside.
- **Identity documents sent to third parties.** Fields the recipient does not
  need are covered with opaque blocks (not blur or a highlighter-style
  overlay, which can be reversed), and the copy carries a visible note naming
  the recipient, the purpose and the date, so a leaked copy is traceable and
  harder to reuse.
- **Passwords shared with another person** go through a shared vault or the
  password manager's sharing link, never a message or email. Which accounts
  are shared is an observed check in [accounts](accounts.md).

## Messages

- **Sensitive content** (passwords, identity documents, health, money) goes
  only through an end-to-end encrypted messenger, and only to people whose own
  devices he has reason to trust: a conversation is as exposed as its least
  protected member.
- **Groups.** Before sending something sensitive to a group, he checks who is
  in it. The larger the group, the less it should carry.

## Devices in public and at home

- **Stepping away.** The Mac and phone are locked every time, by shortcut or
  hot corner, and not left unattended in a café, coworking space or
  conference. The automatic lock is the backstop, checked in
  [machine](machine.md).
- **Screens in public.** Sensitive material is not opened where others can
  read the screen. A privacy filter is optional.
- **Unknown USB devices and public charging ports.** A found or gifted drive
  is not plugged in. Charging away from home uses his own charger, or a cable
  that carries power only. The accessory prompt, checked in
  [machine](machine.md), is the backstop.
- **Someone else's computer.** He avoids signing in to his accounts there. If
  he must, he uses a private window and signs out, and treats the password as
  exposed if the machine is not trusted.
- **Radios.** Bluetooth, NFC and Wi-Fi auto-join are off where he does not use
  them, and the phone does not auto-join open networks. Optional.
- **Camera and microphone.** Smart speakers and cameras are muted or
  unplugged for sensitive conversations. Do not recommend a cover on a
  MacBook camera; [machine](machine.md) gives Apple's reasons.
- **Entering a PIN.** Shielded from people and cameras. Card readers and cash
  machines get a look for loose or added parts; paying by phone or contactless
  avoids the reader.
- **Devices out of sight.** Not visible from the street or left in a parked
  car.
- **The home.** Who else holds keys or is let in unsupervised (cleaner,
  landlord, contractors), and whether devices are locked and recovery
  printouts put away when they are.
- **Who knows the unlock codes.** Anyone who knows the phone PIN or Mac
  password, or has had the unlocked phone to themselves, could have installed
  a monitoring app. An answer of yes sends the stalkerware checks in
  [accounts](accounts.md) from optional to required.
- **Travel.** Before a border crossing, or leaving a device in a hotel room,
  he shuts it down instead of sleeping it. A powered-off encrypted device
  needs the password; a sleeping one is unlocked data behind a lock screen.
  The checklist advises shutting down daily; do not report daily sleep as a
  gap.
- **New surroundings.** He notices who can watch or reach his devices before
  settling in. Nothing to record beyond the answer.

## Paper and records

- **Disposal.** Papers with identity, account or health details are shredded,
  including delivery labels and superseded recovery printouts (old backup
  codes, old Emergency Kits).
- **People-search and data-broker listings.** Whether he has looked for and
  removed listings of his address and phone. What is publicly findable is an
  observed check in [accounts](accounts.md); the opt-out itself is
  remediation.

## Reference maintenance

Added 2026-11-23 from the Essential items of the
[Personal Security Checklist](https://digital-defense.io/) (data file last
changed 2026-10-29). [Checklist coverage](checklist-coverage.md) maps every
Essential item to the check that carries it.
