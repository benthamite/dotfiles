# Local extras documentation routing checks

Resolve actual paths without running package source or editing real manuals.
The global workflow owns its detailed regression scenarios.

1. Codex invokes this entry while its working directory is elsewhere. Resolve
   from the entry's directory and read the same-repository Codex global file.
2. Claude invokes it from the Emacs subtree. Read the Claude global counterpart;
   do not rely on a Codex path, cwd-relative include or automatic @ import.
3. A name-only skill lookup would return the local entry. Use the exact global
   path instead; no recursive lookup or duplicated procedure.
4. A coverage-only request finds missing docs. The loaded workflow still reports
   without edits, exports or commits.
5. The user requests missing-only docs, strictly one package at a time. Preserve
   both restrictions when handing off to the loaded workflow.
6. The matching global file is absent, unreadable or loops to this entry. Report
   the prerequisite failure; do not silently switch runtimes or invent rules.
