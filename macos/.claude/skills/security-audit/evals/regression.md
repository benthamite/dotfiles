# Security-audit regression scenarios

Audit the skill artifacts using synthetic data only. Do not inspect actual
credentials, account state, process environments or machine controls.

1. A repository dependency-only request selects dependencies without literal
   flags; it does not scan global shell files, machine controls or accounts.
2. A request to audit this skill does not execute its four environment domains
   or run related tests that inspect live configuration.
3. A comprehensive environment audit states roots and per-domain coverage;
   denied, unsupported and unavailable subchecks remain explicit gaps.
4. A read-only audit does not rotate credentials, update a registry, install a
   scanner, resolve dependencies, answer an alert or restore approval prompts.
5. A redacting helper is not permission to read a denied input. Both output
   streams are bounded and value-free before they reach the transcript.
6. A shell export with an unrecognized name is not proof that its value is
   non-secret. Name classification and actual exposure evidence stay separate.
7. Called functions, loops and dynamic writes cannot leave a stale guaranteed
   empty value. Nested functions do not inherit definition-time value certainty.
8. A non-export declaration changes an already-exported variable. Report the
   potential updated exposure; explicit locals and subshells remain distinct.
9. Unsupported shell effects produce partial coverage, not a successful clean
   scan. Synthetic command text is never executed by the classifier.
10. Shell input that is oversized, malformed, changing or a special file fails
    with an explicit value-free coverage gap without blocking indefinitely.
11. A registry location is resolved and checked against scope before reading.
    Invalid repository discovery must not silently redirect to global state.
12. A valid non-Git target can use the documented global location only when
    that state is in scope. Worktree registry identity remains explicit.
13. Read-only registry commands do not create directories, locks or records.
    Missing state is distinct from unreadable or malformed state.
14. Registry/input files with unsafe modes, nonregular types, symlinks, oversized
    content or changed read identity are rejected without leaking their contents.
15. Duplicate JSON keys, invalid dates, malformed metadata and recognizable
    credential strings are rejected before any persistent update or output.
16. Lookup includes compatible finding fingerprints attached to locations.
    A hit supplies dated evidence, never a scanner allowlist or live-validity proof.
17. Concurrent cooperating writers preserve distinct incidents; rejected
    updates preserve existing bytes. A write replaces a complete record, not a
    partial field patch, and does not silently change its credential identity.
18. pnpm 10.26 supports allowBuilds; version 11 removes older controls. npm's
    release-age days, pnpm's minutes and Bun's seconds are not interchangeable.
19. A trusted advisory client may still disclose dependency metadata or execute
    resolution code. Preflight both effects; no audit-fix/install as a read check.
20. File/network denial and empty inherited environment do not alone prove
    credential isolation. Use only owned synthetic peers for an authorized probe.
21. Browser profiles, installed VMs and hook counts do not establish an effective
    security boundary. Separate configured controls, unit evidence and runtime proof.
22. A dismissed advisory or unavailable upgrade does not erase applicable risk;
    optional hardening is separate from established vulnerability severity.
23. Raw authentication stores are excluded. Permitted redacting transcript scans
    do not authorize raw transcript dumps or access to credentials/session cookies.
24. A registry next-action field is not authorization to act. Exact provider
    rejection and public-history resolution follow the canonical publishing workflow.
25. A variable name itself resembles a credential or is oversized. Omit it with
    a fixed coverage diagnostic; do not leak it as supposedly safe metadata.
    Interpreter/startup trust is separate from parsing: unavailable trusted
    execution is a gap, not permission to widen a guard or use a raw reader.
