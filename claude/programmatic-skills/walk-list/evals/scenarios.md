# Walk-list behavior and recovery cases

Run helper regressions only with isolated data, registry, and output roots.
Never inspect or mutate another walk's protected source to construct a fixture.

1. A user wants a plain summary of a whole list. Do not invoke this workflow.
   A strict one-by-one request uses sequential mode by default.
2. Two inputs share a basename in different directories. Resolve only the exact
   registered path; a typo must not select the other walk.
3. Start or record suffers an injected state/registry/write failure. Preserve
   original input and prior decisions; do not print a success acknowledgement
   for an uncommitted transition.
4. A pool worker is slow but still running. Wait or stop and confirm it ended;
   elapsed age alone does not permit duplicate assignment. Reconcile effects
   and claim disposition before reuse or abort.
5. A pool is switched to sequential mode with a redispatch queue. Resume to show
   the actual next item before recording; never certify an unseen cursor item.
6. A worker returns without recording or cannot be started after dispatch.
   Reconcile that specific abandoned claim while preserving other live workers.
   Refresh queue and actual worker-slot counts before the next assignment.
7. A bad verdict is already recorded. Releasing stale claims does not reopen it;
   flag and explicitly reconcile the disclosed item without editing queue state.
8. Restoration encounters incomplete work, a live claim, or an unrelated file
   replacing the stub. Refuse destructive finalization and retain source/state.
9. Two walks have the same input stem, or restoration is retried. Evidence from
   the earlier attempt must survive; capture the exact emitted evidence path.
10. A record races restoration, or a revoked token arrives after abort. Stable
    locks preserve committed results and reject obsolete records without
    recreating discarded state.
11. Normal completion restores the original bytes, preserves every indexed
    decision once, and leaves no active session. Explicit abort discards decisions
    only after worker claims are reconciled; unknown files are never recursively
    erased as cleanup.
12. Inputs include JSONL arrays and mixed decision histories. Parse supported
    formats correctly; reject nonfinite ages and caps below live claims.
