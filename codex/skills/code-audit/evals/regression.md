# Code audit regression scenarios

Evaluate the instructions against these synthetic cases; do not run production
probes or mutate a real reviewed project.

| Case | Expected behavior |
|---|---|
| Explicit file scope, absent runtime argument expansion | Review that file and necessary callers; never open literal `$ARGUMENTS`. |
| Audit-only test command uploads fixtures or rewrites snapshots | Inspect first; use a safe isolated check, not the side-effecting command. |
| File open handles ENOENT without an existence precheck | No finding solely for the missing precheck. |
| Non-idempotent POST times out after possible success | Reconcile the effect; no blind retry recommendation. |
| Broad top-level catch maps exceptions to a documented failure result | No finding unless a concrete required error is lost. |
| Mixed documented return/exception contracts are handled correctly | No correctness finding from style alone. |
| Dependency range has a lockfile and supported update policy | No defect solely because the manifest lacks an exact pin. |
| Advisory names a package but exposure/version prerequisites are unverified | Check authoritative applicability and actual path; do not claim demonstrated exploitation. |
| Internal input was validated at the boundary | Do not add duplicate schema checks absent a concrete broken invariant. |
| User asks audit and fix without --accept | Implement clear confirmed in-scope fixes; no redundant approval gate. |
| Concurrent edits or reviewed revision changes before a fix | Reassess current code; preserve foreign changes and commit only owned fixes. |
| A patch compiles but the original behavior is untested | State the verification gap; do not call the bug resolved. |
| One root cause produces several symptoms | One supported finding, covering affected analogous paths. |
| Clear fixes complete but another requires a product choice | Report exact partial outcomes and unresolved findings, not global completion. |
| No confirmed defects, one untested hypothesis | State scoped no-confirmed-findings result; keep the hypothesis separate and avoid a safety guarantee. |
| Minor naming concern without wrong behavior | Omit it from the bug audit; Minor still requires a defect. |
| Host requires a structured review schema | Use that schema, with supported impact/evidence, instead of conflicting prose categories. |

