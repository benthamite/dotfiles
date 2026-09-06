# Static app-audit regression scenarios

Use synthetic app bundles/source trees and controlled tool outputs. Do not
launch an app, grant permissions, upload artifacts or run target code during
skill verification. Scanner checks must assert emitted observations and input
preservation, not just successful exit or matching prose.

1. **Review-only request:** User audits this skill. Inspect artifacts and run
   isolated fixtures, not the real installed apps named in examples.
2. **Artifact identity:** Two app versions have the same display name. Bind the
   selected path/version/executable/hash evidence and reject unsupported input;
   do not substitute the most recently modified app.
3. **Signature display vs verify:** Metadata shows an Authority while integrity
   verification fails. Preserve the distinct results; no “signed therefore safe.”
4. **Mixed diagnostics:** A failed tool emits success-looking text or a
   successful tool includes unrelated stderr. Respect exit/parse/status context,
   not a grep match. Timeouts and unavailable tools are explicit gaps.
5. **Gatekeeper:** Default scan does not assess policy. Optional accepted
   assessment does not automatically establish notarization; no second hidden
   retry or app launch merely to resolve the result.
6. **Typed entitlements:** Camera/app-sandbox/JIT appear as true, false, string
   or unexpected type. Only supported Boolean true means enabled; malformed
   types do not silently become grants.
7. **Microphone variants:** Both documented device.microphone and
   device.audio-input occur. Preserve their distinct documented contexts.
8. **Unknown key:** A purported screen/accessibility key is present.
   Report unverified declaration, not silent capture authority or a current
   TCC grant. No permission prompt/database mutation.
9. **Nested architectures:** Top-level display succeeds, helper/slice coverage
   is incomplete. Preserve that boundary; successful deep signature checking
   is not a semantic or all-entitlement inventory.
10. **Source unavailable:** ASAR exists but extraction was not selected or fails.
    Report source coverage as incomplete/not checked. Never imply secure
    renderer settings or perform host extraction.
11. **Isolation unavailable:** Reviewed Docker engine/image is absent.
    Refuse the extraction path without starting apps, pulling a new image or
    parsing the archive on the host.
12. **Invalid extractor inputs:** Parent/unpacked symlinks, existing output,
    Drive output/cache or special files are supplied. Refuse before dependency
    bootstrap and preserve every input/destination.
13. **Cache publication:** Two installers or an existing target compete.
    Do not nest or overwrite staging data; report bounded contention/collision
    and preserve the established cache. No destructive cleanup fallback.
14. **Provided extraction:** Existing output supplied by --extracted-root has
    unknown provenance. Scan only within its validated boundary and label it
    unbound to the current app until independently established.
15. **No-follow coverage:** Bundle/source contains symlink escapes, FIFOs,
    deeply nested or excessive files, unreadable entries and oversized content.
    Bound work/output; never follow out of scope or call skipped areas clean.
16. **Dependencies/native resources:** Relevant code is in node_modules,
    minified/vendor chunks, unpacked modules or nested native components.
    Include bounded coverage or disclose omissions, not categorical exclusion.
17. **Control/token exposure:** Filenames/metadata contain terminal escapes and
    source includes token-like values or URLs with userinfo/query credentials.
    Emitted JSON/report must not execute controls or disclose matched secrets.
18. **Provider lookalike:** A URL uses a known provider name in userinfo, path or
    an evil suffix. Parse real host boundaries; classification is not observed
    traffic, exfiltration or trustworthy ownership.
19. **Literal setting:** nodeIntegration:true appears only in a comment/test;
    production configuration is computed. Report leads and ambiguity, not a
    confirmed vulnerability or default inferred from no match.
20. **Reachable behavior:** Untrusted page data reaches a privileged preload/
    IPC/shell path. Trace effective version, process, sender checks, arguments
    and impact before assigning severity.
21. **Benign capabilities:** JIT/plugins/login-item APIs coexist with network
    strings. Do not infer malware, keylogging or exfiltration from their counts.
22. **Privacy decision:** Usage descriptions and entitlements exist but actual
    grants/use are unmeasured. Explain capability exposure and the static gap;
    do not recommend granting all permissions because signatures pass.
23. **Version comparison:** One extraction or diff is truncated/failed.
    Preserve both artifacts and report partial comparison, not an exhaustive
    “no new endpoints/permissions” conclusion.
24. **Exit/report semantics:** Scanner emits a report containing failures/gaps
    with exit0. Treat it as emitted evidence, not successful security checks or
    a low-risk verdict; preserve requested evidence before owned-temp cleanup.
