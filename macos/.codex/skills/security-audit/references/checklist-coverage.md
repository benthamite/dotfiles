# Personal Security Checklist coverage

Maintenance aid, not a run-time reference. Read it when the
[Personal Security Checklist](https://digital-defense.io/) changes, or before
moving or removing a check it supplied. It maps each item the checklist
marks Essential to the place that carries it.

Source: `personal-security-checklist.yml` in
`Lissy93/personal-security-checklist`, last changed 2026-10-29, read
2026-11-23: 94 Essential items of 258. To update, list the Essential items of
the current file, compare their titles with the tables below, and place each
new one by these rules.

- **check**: a state the audit reads or asks about, which can be a finding.
- **hardening**: recorded and reported as optional hardening, never with a
  severity. Used for items that only limit tracking.
- **interview**: a practice no setting records, asked in
  [habits](habits.md).
- **narrowed**: the checklist's wording does not fit Pablo's situation or
  contradicts a vendor's guidance, so a narrower check carries it. The
  reference named gives the reason. Do not widen one without his decision.

## Authentication

| Item | Carried by | As |
|---|---|---|
| Use a Strong Password | accounts: Vault hygiene; machine: Unlock | check |
| Don't Reuse Passwords | accounts: Vault hygiene, Root-account controls | check |
| Use a Secure Password Manager | accounts: Vault hygiene, Account discovery | check |
| Avoid Sharing Passwords | accounts: Vault hygiene; habits: Sharing and posting | check, interview |
| Enable 2-Factor Authentication | accounts: Root-account controls, Money, identity and communication accounts | check |
| Keep Backup Codes Safe | accounts: Reset graph | check |

## Web browsing

| Item | Carried by | As |
|---|---|---|
| Block Ads | machine: Browsers, Content blocker | check |
| Ensure Website is Legitimate | habits: Links, sign-ins and prompts | interview |
| Watch out for Browser Malware | machine: Browsers, Safe Browsing; extension inventory; habits: Pop-ups | check, interview |
| Use a Privacy-Respecting Browser | machine: Browsers, Browser, search engine and fingerprint | hardening, narrowed |
| Use a Private Search Engine | machine: Browsers, Browser, search engine and fingerprint | hardening |
| Remove Unnecessary Browser Addons | machine: Privileged applications, extension inventory | check |
| Keep Browser Up-to-date | machine: Browsers, Version | check |
| Check for HTTPS | machine: Browsers, HTTPS; habits: Where sign-ins start | check, interview |
| Use DNS-over-HTTPS | machine: Browsers, Secure DNS | hardening |
| Compartmentalise Your Browsing | machine: Actual isolation (profiles) | check |
| Use Incognito | habits: Someone else's computer | interview |
| Understand Your Browser Fingerprint | machine: Browsers, Browser, search engine and fingerprint | hardening, narrowed |
| Manage Cookies | machine: Browsers, Cookies and sessions | narrowed |
| Block Third-Party Cookies | machine: Browsers, Third-party cookies | hardening |
| Block Third-Party Trackers | machine: Browsers, Content blocker | hardening |

## Email

| Item | Carried by | As |
|---|---|---|
| Have more than one email address | accounts: Reset graph, sign-in addresses | hardening |
| Keep Email Address Private | accounts: Reset graph, sign-in addresses | hardening, narrowed |
| Keep your Account Secure | accounts: Root-account controls | check |
| Disable Automatic Loading of Remote Content | machine: Browsers, Remote content in mail | check |

## Messaging

| Item | Carried by | As |
|---|---|---|
| Only Use Fully End-to-End Encrypted Messengers | accounts: Messaging apps, Encryption | check |
| Use only Open Source Messaging Platforms | accounts: Messaging apps, Encryption | hardening, narrowed |
| Use a "Trustworthy" Messaging Platform | accounts: Messaging apps, Encryption | hardening, narrowed |
| Check Security Settings | accounts: Messaging apps, Security notifications, Group adds, Presence | check |
| Ensure your Recipients Environment is Secure | habits: Messages | interview |
| Disable Cloud Services | accounts: Messaging apps, Linked devices, Backups | check, narrowed |
| Secure Group Chats | accounts: Messaging apps, Group adds; habits: Messages | check, interview |
| Create a Safe Environment for Communication | accounts: Messaging apps; habits: Messages | check, interview |

## Social media

| Item | Carried by | As |
|---|---|---|
| Secure your Account | accounts: Money, identity and communication accounts | check |
| Check Privacy Settings | accounts: Social accounts, Audience settings | check |
| Think of All Interactions as Public | habits: Sharing and posting | interview |
| Think of All Interactions as Permanent | habits: Sharing and posting | interview |
| Don't Reveal too Much | accounts: Social accounts, Public data; habits: Sharing and posting | check, interview |
| Be Careful what you Upload | habits: Sharing and posting | interview |
| Don't Share Email or Phone Number | accounts: Social accounts, Audience settings, Public data | check |
| Don't Grant Unnecessary Permissions | accounts: The second-factor phone, Special access | check |
| Be Careful of 3rd-Party Integrations | accounts: Social accounts, Connected apps, Sign in with another account | check, narrowed |
| Avoid Publishing Geo Data while still Onsite | habits: Sharing and posting | interview |

## Networks

| Item | Carried by | As |
|---|---|---|
| Use a VPN | machine: Untrusted networks | hardening, narrowed |
| Change your Router Password | machine: Other devices on Pablo's network | check |
| Use WPA2, and a strong password | machine: Wi-Fi | check |
| Keep router firmware up-to-date | machine: Other devices on Pablo's network | check |

## Mobile devices

| Item | Carried by | As |
|---|---|---|
| Encrypt your Device | accounts: The second-factor phone, Encryption | check |
| Turn off connectivity features that aren't being used | habits: Radios | interview |
| Keep app count to a minimum | accounts: The second-factor phone, App count | check |
| App Permissions | accounts: The second-factor phone, Special access | check |
| Only install Apps from official source | accounts: The second-factor phone, App sources | check, narrowed |
| Set up a mobile carrier PIN | accounts: Phone-number exposure | check, narrowed |

## Personal computers

| Item | Carried by | As |
|---|---|---|
| Keep your System up-to-date | machine: Platform controls | check |
| Encrypt your Device | machine: Platform controls (FileVault) | check |
| Backup Important Data | machine: Authentication and recovery | check |
| Be Careful Plugging USB Devices into your Computer | machine: Platform controls, Accessories; habits: Unknown USB devices | check, interview |
| Activate Screen-Lock when Idle | machine: Platform controls, Lock when idle | check |
| Disable Cortana or Siri | machine: Platform controls, Voice assistants and dictation | check, narrowed |
| Review your Installed Apps | machine: Privileged applications, installed apps | check |
| Manage Permissions | machine: Privileged applications, privacy grants | check |
| Disallow Usage Data from being sent to the Cloud | machine: Platform controls, Analytics | hardening |
| Avoid Quick Unlock | machine: Platform controls, Unlock | check, narrowed |
| Power Off Computer, instead of Standby | machine: Platform controls, Sleep; habits: Travel | narrowed, interview |

## Smart home

| Item | Carried by | As |
|---|---|---|
| Rename devices to not specify brand/model | machine: Other devices on Pablo's network | hardening |
| Disable microphone and camera when not in use | machine: Other devices on Pablo's network; habits: Camera and microphone | check, interview |
| Understand what data is collected, stored and transmitted | machine: Other devices on Pablo's network | hardening |
| Set privacy settings, and opt out of sharing data with third parties | machine: Other devices on Pablo's network | hardening |
| Don't link your smart home devices to your real identity | machine: Other devices on Pablo's network (vendor account) | narrowed |
| Keep firmware up-to-date | machine: Other devices on Pablo's network | check |
| Protect your Network | machine: Wi-Fi | check |

## Personal finance

| Item | Carried by | As |
|---|---|---|
| Sign up for Fraud Alerts and Credit Monitoring | accounts: Account discovery, Credit files | check |
| Apply a Credit Freeze | accounts: Credit files | check, narrowed |

## Human aspect

| Item | Carried by | As |
|---|---|---|
| Verify Recipients | habits: Requests for money, codes or changed payment details | interview |
| Don't Trust Your Popup Notifications | habits: Pop-ups and browser warnings | interview |
| Never Leave Device Unattended | machine: Lock when idle, Find My Mac; accounts: Finding and erasing; habits: Stepping away | check, interview |
| Prevent Camfecting | machine: Privileged applications, privacy grants; habits: Camera and microphone | check, narrowed |
| Stay protected from shoulder surfers | habits: Screens in public | interview |
| Educate yourself about phishing attacks | habits: Links, sign-ins and prompts | interview |
| Watch out for Stalkerware | accounts: The second-factor phone, Stalkerware; machine: Configuration profiles, privacy grants | check |
| Install Reputable Software from Trusted Sources | supply chain; machine: Platform controls (Gatekeeper); accounts: App sources | check |
| Store personal data securely | machine: Authentication and recovery, where personal documents live | check |
| Obscure Personal Details from Documents | habits: Identity documents sent to third parties | interview |
| Do not assume a site is secure, just because it is `HTTPS` | habits: Where sign-ins start | interview |

## Physical security

| Item | Carried by | As |
|---|---|---|
| Destroy Sensitive Documents | habits: Paper and records | interview |
| Opt-Out of Public Records | accounts: Social accounts, People-search listings; habits: Paper and records | check, interview |
| Watermark Documents | habits: Identity documents sent to third parties | interview |
| Don't Reveal Info on Inbound Calls | habits: Calls | interview |
| Stay Alert | habits: New surroundings | interview |
| Secure Perimeter | habits: The home | interview |
| Physically Secure Devices | habits: Stepping away, Screens in public | interview |
| Keep Devices Out of Direct Sight | habits: Devices out of sight | interview |
| Protect your PIN | habits: Entering a PIN | interview |
| Check for Skimmers | habits: Entering a PIN | interview |

## Narrowed items

Each is a decision about how far the checklist's advice applies. The reason
sits beside the check.

| Checklist advice | What the audit does instead |
|---|---|
| Use only open-source, "trustworthy" messengers | Records which are; reports only missing end-to-end encryption |
| Avoid desktop and web messaging companions | Audits linked devices and local archives |
| Avoid social sign-in | Adds the edge to the reset graph; reports a weak provider account |
| Use a paid VPN | Asks about untrusted networks; none is optional hardening |
| Install apps only from the official store | Records a store Pablo chose; reports other install channels |
| Set a carrier PIN | Checks what the carrier offers; Argentina's carriers offer none |
| Apply a credit freeze | Checks what each country offers; Argentina has none |
| Use a password instead of biometrics | Checks the password behind Touch ID |
| Shut down instead of sleeping | Travel practice only |
| Disable Siri | Checks Siri while locked and where audio goes |
| Use a privacy browser; reduce the fingerprint | Records the browser; no gap reported |
| Clear cookies regularly | Not recommended; checks sessions and profiles |
| Keep the email address private | Checks whether a published address signs in to root accounts |
| Cover the webcam | Not recommended on a MacBook, per Apple |
| Keep smart-home accounts anonymous | Checks the vendor account's second factor |
