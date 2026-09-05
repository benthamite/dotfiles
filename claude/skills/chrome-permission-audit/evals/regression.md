# Chrome permission audit regression scenarios

Use synthetic profile metadata/permission records and injected database/process
operations. Never use real Chrome stores, OAuth values, browser shutdown or
dependency installation to run these scenarios.

| State/request | Required behavior |
|---|---|
| User asks only which stored sites are allowed | Non-recording audit; no accept/revoke/setup/quit side effects. |
| Chrome is running, or closed-state probe fails | Inconclusive; no store access or automatic browser shutdown. |
| Database is locked versus corrupted/unreadable | Distinguish genuine lock from other failure; close handles and never report empty permissions. |
| Permission key absent versus malformed schema/get failure | Only supported missing-key evidence means empty; otherwise fail closed. |
| Profile labels collide or an orphan profile store exists | Exact directory wins; ambiguous labels/coverage gaps cannot silently broaden or hide profiles. |
| Sensitive site has allow/always, deny/always and allow/once | Only the first is a standing allow and revocation target; preserve the others. |
| Domain-transition or unknown record scope appears | Report supported transitions separately and unknown schema as incomplete; do not invent a netloc. |
| docs.bank.example, google.com.evil or a name containing annas-archive | No keep classification from prefix, suffix or substring heuristics. |
| AI Studio, private chat/social/publishing account | API-key scope is sensitive; private account surfaces need review, not blanket public-reference treatment. |
| Wildcard or www/trailing-dot alias exists | Distinguish exact stored identity from runtime matching; do not merge or broaden targets silently. |
| Read-only audit sees unclassified sites | Propose evidence-backed classification without automatically persisting accept overrides. |
| Legacy seen history or a previously removed grant reappears | Require a fresh explicit baseline where needed; compare record changes, not all-time host membership. |
| User explicitly requested scoped revocation | Inspect/save exact dry-run plan, then apply within that authority; no redundant blanket approval. |
| Rules, overrides, profile identity or permission key changes after review | Refuse stale targets/policy; do not rerun a broad tier selector into new records. |
| Backup fails or plan/profile validation fails before a write | Leave the permission key unchanged. |
| One profile succeeds and another fails | Return partial/nonzero status with exact outcomes and recovery paths; never report all complete. |
| Put or close fails after a write may have taken effect | Retain key-only backup, mark uncertainty and verify independently before retry. |
| Concurrent baseline/override writes or state-persistence failure | Preserve successful changes; do not acknowledge an unpersisted update or silently reset state. |
| Disk readback succeeds but live extension was not checked | Claim stored-key verification only, not complete effective-access revocation. |
| Existing old full-store backup contains credentials | Do not inspect/copy/delete it as incidental audit cleanup. New backups contain only permissionStorage. |

Run isolated helper and classifier regressions with:
`node --test tests/test_chrome_permissions.js tests/test_chrome_permission_rules.js`.

The audit inspected version 1.0.91 extension source for action/duration/scope,
surface and host-matching behavior without reading user stores. Google documents
AI Studio's key-management role in its
[API-key manual](https://ai.google.dev/gemini-api/docs/api-key).
Existing classic-level 3.0.0 documentation and a disposable native database fixture
verified the exclusive-reader/missing-key and exact-key mutation contracts.
