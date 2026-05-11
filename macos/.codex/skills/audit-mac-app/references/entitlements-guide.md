# macOS Entitlements Security Guide

This reference classifies macOS app entitlements by security risk level.

## Risk Levels

| Level | Meaning |
|-------|---------|
| **CRITICAL** | Can enable arbitrary code execution or bypass core security |
| **HIGH** | Grants access to sensitive user data or system control |
| **MEDIUM** | Elevated privileges that could be misused |
| **LOW** | Standard app capabilities with minimal risk |
| **INFO** | Informational, generally positive security indicators |

---

## CRITICAL Entitlements

These entitlements significantly weaken macOS security protections:

### `com.apple.security.cs.disable-library-validation`
**Risk:** Allows loading unsigned or differently-signed dynamic libraries
**Impact:** App can load malicious code not reviewed by Apple
**Legitimate use:** Plugins, extensions from third parties

### `com.apple.security.cs.allow-dyld-environment-variables`
**Risk:** Allows DYLD environment variables to affect the app
**Impact:** Attackers can inject malicious libraries via environment
**Legitimate use:** Rare - debugging, some legacy compatibility

### `com.apple.security.cs.disable-executable-page-protection`
**Risk:** Allows writable and executable memory pages
**Impact:** Easier exploitation of memory corruption bugs
**Legitimate use:** JIT compilers, emulators

---

## HIGH Entitlements

These grant access to sensitive data or system capabilities:

### `com.apple.security.device.screen-capture`
**Risk:** Can record screen contents silently
**Impact:** Can capture passwords, private messages, documents
**Legitimate use:** Screen recorders, video conferencing, accessibility tools

### `com.apple.security.device.accessibility`
**Risk:** Can control other applications, read UI elements
**Impact:** Keylogging, automation of other apps, data extraction
**Legitimate use:** Accessibility tools, automation software

### `com.apple.security.cs.allow-unsigned-executable-memory`
**Risk:** Can map unsigned executable memory
**Impact:** Enables JIT compilation but increases attack surface
**Legitimate use:** JavaScript engines, emulators

### `com.apple.security.device.audio-input`
**Risk:** Can record audio from microphone
**Impact:** Eavesdropping, unauthorized recording
**Legitimate use:** Voice apps, video calls, dictation

### `com.apple.security.personal-information.addressbook`
**Risk:** Can read contacts database
**Impact:** Privacy breach, data exfiltration
**Legitimate use:** Email clients, social apps

### `com.apple.security.personal-information.calendars`
**Risk:** Can read calendar events
**Impact:** Privacy breach, schedule tracking
**Legitimate use:** Calendar apps, scheduling tools

### `com.apple.security.personal-information.location`
**Risk:** Can access device location
**Impact:** Privacy breach, location tracking
**Legitimate use:** Maps, weather, location-based services

### `com.apple.security.personal-information.photos-library`
**Risk:** Can access Photos library
**Impact:** Privacy breach, access to personal photos
**Legitimate use:** Photo editors, backup tools

---

## MEDIUM Entitlements

Elevated privileges that warrant attention:

### `com.apple.security.cs.allow-jit`
**Risk:** Just-In-Time compilation allowed
**Impact:** More flexible code execution
**Legitimate use:** JavaScript engines, VMs (common in Electron)

### `com.apple.security.device.camera`
**Risk:** Can access camera
**Impact:** Unauthorized video recording
**Legitimate use:** Video calls, photo apps

### `com.apple.security.device.microphone`
**Risk:** Can access microphone
**Impact:** Unauthorized audio recording
**Legitimate use:** Voice apps, video calls

### `com.apple.security.device.bluetooth`
**Risk:** Can access Bluetooth
**Impact:** Device scanning, data transfer
**Legitimate use:** Peripheral connectivity

### `com.apple.security.device.usb`
**Risk:** Can access USB devices
**Impact:** Data transfer to/from USB devices
**Legitimate use:** Device management, file transfer

### `com.apple.security.automation.apple-events`
**Risk:** Can send Apple Events to other apps
**Impact:** Automation, potential control of other apps
**Legitimate use:** Automation tools, scripting

### `com.apple.security.files.downloads.read-write`
**Risk:** Read/write access to Downloads folder
**Impact:** Access to downloaded files
**Legitimate use:** Download managers, file utilities

---

## LOW Entitlements

Standard capabilities with limited security impact:

### `com.apple.security.app-sandbox`
**Risk:** None - this is a security feature
**Impact:** POSITIVE - Restricts app to sandbox
**Note:** Apps WITH this entitlement are more secure

### `com.apple.security.network.client`
**Risk:** Can make outbound network connections
**Impact:** Standard networking capability
**Legitimate use:** Almost all networked apps

### `com.apple.security.network.server`
**Risk:** Can accept incoming network connections
**Impact:** App can run a server
**Legitimate use:** Server apps, P2P, local services

### `com.apple.security.files.user-selected.read-only`
**Risk:** Can read files user explicitly selects
**Impact:** Minimal - user controls access
**Legitimate use:** Document editors, file viewers

### `com.apple.security.files.user-selected.read-write`
**Risk:** Can read/write files user explicitly selects
**Impact:** Minimal - user controls access
**Legitimate use:** Document editors, file managers

### `com.apple.security.files.bookmarks.app-scope`
**Risk:** Can create app-scoped bookmarks
**Impact:** Remember file access across launches
**Legitimate use:** Document-based apps

### `com.apple.security.files.bookmarks.document-scope`
**Risk:** Can create document-scoped bookmarks
**Impact:** Remember related file access
**Legitimate use:** Apps working with file references

---

## Entitlement Combinations to Watch

### High-Risk Combinations

| Combination | Risk |
|-------------|------|
| `disable-library-validation` + `network.client` | Can download and load unsigned code |
| `accessibility` + `network.client` | Keylogger with exfiltration capability |
| `screen-capture` + `network.client` | Screen recording with upload |
| `allow-unsigned-executable-memory` + `disable-library-validation` | Maximum code execution flexibility |

### Safer Alternatives

| Instead of | Consider |
|------------|----------|
| `disable-library-validation` | Ship signed plugins or use XPC |
| `accessibility` | Use system accessibility APIs with user consent |
| No sandbox | Sandbox with explicit file access entitlements |

---

## Quick Reference: Command to Extract Entitlements

```bash
# Extract all entitlements
codesign -d --entitlements - /path/to/App.app 2>&1

# Pretty print as XML
codesign -d --entitlements :- /path/to/App.app 2>&1
```

## Interpreting Missing Entitlements

- **No sandbox entitlement**: App runs with full user privileges (older apps or by design)
- **No network entitlements**: App may still use network via frameworks
- **Missing expected entitlements**: May indicate privilege escalation risk

---

## References

- [Apple Entitlement Key Reference](https://developer.apple.com/documentation/bundleresources/entitlements)
- [Hardened Runtime](https://developer.apple.com/documentation/security/hardened_runtime)
- [App Sandbox](https://developer.apple.com/documentation/security/app_sandbox)
