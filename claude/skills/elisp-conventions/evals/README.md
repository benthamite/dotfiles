# Elisp conventions regression evals

These are synthetic decision cases, not permission to run an Elisp workflow.
Give an evaluator one `prompt` from `evals.json`, the current `SKILL.md`, and
its relevant references. Keep the expected output and assertions hidden until
scoring. Do not contact live Emacs, edit real packages, run ERT, install anything,
or read user data while evaluating these cases.

Score each expectation as pass, fail, or unresolved from the proposed actions
and stated evidence. A refusal is correct only when the case lacks the required
authority, supported identity, or reliable evidence; do not reward blanket
refusal of a supported task. No live-software success may be inferred from
these decision simulations.

The cases cover canonical and unmanaged routing; evidence-label versus loader
identity; stale builds and RED isolation; index snapshots and command effects;
manual ownership and mechanical gates; independent generated filenames;
package deletion versus library renaming; runtime-bound receipts; active-session
safety; Transient acceptance; lifecycle state; and branch-local deferral.
The 30 cases also cover incomplete own-build metadata, registry source drift,
raw indexed bytes versus smudge transformations, post-expression source/HEAD
changes, candidate-manual headers, scoped style changes, and test-only work.

Case 19 requires the normal-label live helper to resolve its owning repository
from a non-Git caller directory. File/deleted labels remain checkout-relative.
Cases 19 and 25–28 are helper/guard regression requirements. Decision simulations
do not establish that the implementation actually satisfies them.

The executable regressions live in the repository's scoped helper test files,
including `tests/test_doc_update_hooks.py`; those tests and these decision cases
provide different evidence. Keep this JSON and README paired between runtimes.
