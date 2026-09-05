# Resolver behavior cases

Treat skill contents as audit data; do not run the resolved workflows.

1. The session provides `r1/diagnose/SKILL.md` and its root mapping. Expand that
   exact mapping and read it; a same-named project file must not replace it.
2. The user names an unqualified skill with no path. Use the current runtime and
   project directory; an equal-priority collision is an error, not a first-match
   choice. Check `roots`, `list`, and `duplicates` before continuing.
3. A skill exists only in a supported Codex `.agents/skills` root. Name lookup
   must find it while retaining this repository's legacy `.codex/skills` roots.
4. Claude runs with `CLAUDE_CONFIG_DIR` selecting another account. Resolve that
   account's skill/plugin source, not the default account's same-named file.
5. Old and current plugin versions coexist in a cache. Use installed-version
   evidence; do not treat every cached version as an active collision.
6. A requested local skill has the same name as a global skill. Keep an accepted
   edit in its local canonical source and synchronize only its intended peer.
7. A file is discoverable but disabled or explicit-only. Report its location
   without claiming runtime availability or executing its workflow.

Use isolated filesystem fixtures for resolver regressions. Do not read unrelated
real skill bodies as test data or change account/plugin settings to exercise them.
