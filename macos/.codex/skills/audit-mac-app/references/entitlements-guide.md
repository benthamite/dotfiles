# Interpret signing, entitlements and privacy evidence

Use this reference before interpreting scanner output. Record the inspected
code object, architecture, tool status and exact typed values. A key's presence,
a diagnostic substring or a top-level result is not a permission inventory.

## Separate the claims

| Evidence | What it establishes | What it does not establish |
|---|---|---|
| Signature display | Available signing metadata for the inspected object/slice | Integrity, expected publisher, benign behavior |
| Successful signature verification | The tool accepted the checked signature/sealed content | Semantic safety or every runtime behavior |
| Gatekeeper assessment | Current policy result under the assessment conditions | A standalone notarization verdict or permanent safety |
| Notarization evidence | Automated checks accepted submitted software | App Review or absence of all malicious behavior |
| Stapled ticket | An attached ticket is available/valid as checked | That unstapled software was never notarized |
| Enabled entitlement | A declared capability or protection exception for that process | A current privacy grant or actual use |
| Usage-description string | The app supplies an explanation for an access request | Authorization, necessity or observed collection |

Apple describes signing's limited guarantees in its
[Code Signing Guide](https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/Introduction/Introduction.html).
Keep signature failure causes open: corruption, packaging defects, an unsupported
tool result and malicious modification are different hypotheses. Do not re-sign
the app or remove quarantine to make a failed check pass.

The installed `codesign(1)` documents different defaults: verification checks
all architectures, while display selects the host-native architecture.
Top-level `codesign -d` therefore does not enumerate every helper or slice.
Record unassessed XPC services, embedded apps, extensions, frameworks and plug-ins.
`--deep` verification is not a semantic code review; its signing deprecation
does not make verification itself deprecated.
[Apple nested-code guidance](https://developer.apple.com/library/archive/technotes/tn2206/_index.html)

## Read values, not key substrings

For documented Boolean keys, only a Boolean true means enabled. Boolean false
means explicitly disabled; a string `"true"`, number, malformed plist or tool
error is not equivalent. Some entitlements legitimately use arrays, strings or
other types, so consult the exact key's documented schema. Absence can be
meaningful only after successful, correctly scoped inspection.
[Apple security entitlement catalog](https://developer.apple.com/documentation/bundleresources/security-entitlements)

### Hardening and containment

| Key suffix | Context to inspect |
|---|---|
| `cs.disable-library-validation` | Relaxes Apple/same-Team-ID library restrictions; verify plug-in provenance and untrusted library reachability |
| `cs.allow-dyld-environment-variables` | Enables DYLD environment influence otherwise restricted by Hardened Runtime; inspect actual injection prerequisites |
| `cs.disable-executable-page-protection` | Broadly removes executable/code-signing protections; not merely an ordinary JIT switch |
| `cs.allow-unsigned-executable-memory` | Permits a broader executable-memory path than narrowly managed JIT; justify the actual engine/use |
| `cs.allow-jit` | Supports JIT/MAP_JIT behavior; common legitimate engine requirement |
| `app-sandbox` | Configures App Sandbox when true; inspect other exceptions and process boundaries |

These suffixes use the `com.apple.security.` prefix. A hardened-runtime
exception needs contextual justification, not an automatic CRITICAL or malware
label. For third-party plug-ins, “signed” alone is not the same-Team-ID condition.
[Library validation](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.security.cs.disable-library-validation),
[Executable page protection](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.security.cs.disable-executable-page-protection)

Sandbox absence is not root access or exemption from TCC/SIP. Network client/server
entitlements govern sandbox capabilities; missing keys do not establish that
every unsandboxed app is unable to network. Conversely, a sandboxed process does
not gain a documented bypass just by calling a framework.
[App Sandbox](https://developer.apple.com/documentation/security/protecting-user-data-with-app-sandbox)

### Privacy and resource access

- Both `com.apple.security.device.microphone` (App Sandbox) and
  `com.apple.security.device.audio-input` (Hardened Runtime resource access)
  are documented Boolean keys. Do not reject one as invented or replace them
  universally with each other.
  [Microphone](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.security.device.microphone),
  [Audio Input](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.security.device.audio-input)
- Camera, contacts, calendars, photos, location and Apple Events declarations
  require interpretation in the relevant platform/entitlement context. They do
  not prove that the user has granted access.
  [Resource access and consent](https://developer.apple.com/documentation/xcode/configuring-the-hardened-runtime)
- The exact purported keys `com.apple.security.device.screen-capture` and
  `com.apple.security.device.accessibility` are not substantiated by the
  reviewed public catalog. Report an observed key as undocumented/unverified,
  not permission to capture silently. Do not generalize that uncertainty into
  a claim that no screen-capture entitlement exists: Apple documents the
  separately restricted `com.apple.developer.persistent-content-capture`.
  [Persistent Content Capture](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.developer.persistent-content-capture)
- Screen recording and Accessibility have separate authorization controls.
  Never infer a current grant from entitlement/usage-description strings,
  trigger a permission prompt or inspect private TCC databases merely to fill
  a static-audit gap.
  [Screen recording controls](https://support.apple.com/en-ie/guide/mac-help/mchld6aa7d23/26/mac/26),
  [App access controls](https://support.apple.com/guide/security/controlling-app-access-to-files-secddd1d86a6/web)

## Assign severity to demonstrated paths

Distinguish declared capability, enabled configuration, reachable behavior,
permission/privilege prerequisites and observed impact. A screen-capture API plus
a URL does not prove recording or upload; a helper's entitlement cannot simply
be combined with another process's network string into an exploit.

A confirmed untrusted-input path to privileged execution or sensitive disclosure
can justify a high-severity finding. A hardening exception, keyword match or
unresolved dependency is a review lead until the relevant path is established.
Report inspection gaps even when no confirmed issue is found.
