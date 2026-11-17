# Account remediation pitfalls

Read before authorized remediation of an online account. Each entry is a step
whose side effect is easy to miss. Entries cite the vendor page that documents
them; an entry marked "observed" comes from a run and has no vendor page.
Before relying on an entry, check that the vendor's page still says it, and
add a pitfall met during remediation here with its source.

## Google

- **A password change revokes refresh tokens that carry Gmail scopes.** Before
  changing a Google account's password, list the local tools holding such a
  token for that account (`google-services.md`, "Auth", plus mail sync). After
  the change, re-grant each one with the command that section names and verify
  it with a real call. Tokens without Gmail scopes survive.
- **The 2-Step Verification phone and the recovery phone are separate
  settings.** Removing SMS from 2-Step Verification leaves the same number as
  the recovery phone, which can still reset the account. Remove or replace it
  in both places, and check the account's other phone numbers too: Google says
  a deleted recovery number may still be used by other Google services.
- **A new session can be locked out of security changes.** After a sign-in
  Google does not trust, "couldn't verify it's you" can block both the
  password change page and turning on 2-Step Verification, even in the
  owner's usual browser. The account-recovery flow, with the code sent to the
  recovery email, may still allow the password change. If 2-Step Verification
  has to wait for the lock to lift, give that wait its own TODO.

## 1Password

- **A password change does not create new encryption keys or change the
  Secret Key.** It re-wraps the existing keys under the new password.
  Regenerating the Secret Key is a separate action, and each signed-in device
  then asks for it. If the account password and Secret Key may both have
  leaked, change both.
- **After changing the password or Secret Key, make a new Emergency Kit** and
  destroy the old copies, including any with the old password written in.
- **The key derivation is not configurable.** 1Password fixes the PBKDF2
  iteration count; an older account moves to the current count only when its
  password or Secret Key changes. There is nothing to tune.

## Amazon

- **Amazon's passwordless sign-in sends a one-time code by SMS, WhatsApp or
  email.** An account with no password is only as safe as that number or
  mailbox. Adding a passkey keeps the password usable, and Amazon allows one
  passkey per passkey provider. Delete a passkey from both Amazon and the
  password manager.

## AWS

- **After adding MFA to the root user, sign out and back in** (observed in a
  friend's run, 2026-11-07; not in AWS's documentation). The console showed
  "You need permissions" until a fresh sign-in. AWS documents the same step
  for IAM users whose policies require MFA: a session started without MFA
  does not carry it.

## Sources

Checked 2026-11-15.

- [Google OAuth refresh token expiration](https://developers.google.com/identity/protocols/oauth2#expiration)
- [Google recovery phone](https://support.google.com/accounts/answer/183723)
- [Google account phone numbers](https://support.google.com/accounts/answer/3463280)
- [1Password change account password](https://support.1password.com/change-account-password/)
- [1Password Emergency Kit](https://support.1password.com/emergency-kit/)
- [1Password PBKDF2](https://support.1password.com/pbkdf2/)
- [1Password security white paper](https://1passwordstatic.com/files/security/1password-white-paper.pdf), §A.5
- [Amazon sign-in without a password](https://www.amazon.com/gp/help/customer/display.html?nodeId=TQ2Hs0FBkC4Y6eaL7A)
- [Amazon passkeys](https://www.amazon.com/gp/help/customer/display.html?nodeId=TPphmhSWBgcI9Ak87p)
- [AWS IAM self-managed MFA tutorial](https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_users-self-manage-mfa-and-creds.html)
