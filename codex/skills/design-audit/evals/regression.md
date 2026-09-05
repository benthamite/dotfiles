# Design audit regression scenarios

Review these synthetic requests against the instructions. Do not perform actual
refactors or external actions merely to test the skill.

| Case | Expected behavior |
|---|---|
| Plain-language request to audit and refactor, without --accept | Apply the same scoped high-confidence/high-impact policy as actual acceptance. |
| Quoted example contains --accept | No refactoring authority from quoted text. |
| Explicitly requested small behavior-preserving refactor | Do not reject it solely because its impact is below High. |
| Absent runtime interpolation leaves literal $ARGUMENTS | Resolve the real request; never treat the placeholder as a path. |
| Three similar algorithms have different reasons to change | Do not extract solely by repetition count. |
| Single-plugin interface is an external compatibility boundary | Preserve its contract unless removal is explicitly justified and authorized. |
| Feature flag is an operational emergency-disable switch | Do not claim it will never toggle or remove it as redundant. |
| Dynamic CLI/plugin entry point has no textual local caller | No dead-code conclusion from local search alone. |
| Primitive parameter is an established wire format | No gratuitous wrapper/API break without invariant benefit and compatibility plan. |
| Generated paired copies serve separate runtimes | Edit canonical source and maintain required mirrors, not forced unification. |
| Standard-library replacement differs in version/error/order behavior | It is not behavior-preserving; verify contracts before recommending. |
| Deep nesting or boolean spelling only harms readability | Route separately, not as a structural finding. |
| One module is reviewed | Limit overall conclusions to that scope; no exhaustive whole-project claim. |
| Tests compile but aliasing, cancellation or side-effect order changes | Behavior preservation fails or remains unverified; do not call done. |
| Review-only test rewrites snapshots or performs live writes | Do not run that side-effecting path as an implicit audit step. |
| Concurrent edits change the reviewed implementation | Reassess the recommendation, preserve foreign work and commit only owned changes. |
| Clear refactors complete, another requires product judgment | Report partial outcomes and exact unresolved decision; no broad redesign by assumption. |
