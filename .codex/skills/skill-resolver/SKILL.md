---
name: skill-resolver
description: Resolve and read Claude/Codex skill files by skill name with deterministic filesystem lookup. Use when a user names a skill without an exact path, when you need to open, inspect, debug, or locate a skill, or before manually expanding a skill root/path.
---

# Skill Resolver

Use the resolver instead of manually expanding skill root aliases or guessing where a skill lives. The resolver is read-only: it locates and prints skill files; it does not install, edit, or synchronize skills.

## Workflow

1. Skip the resolver when the user gave a literal absolute `SKILL.md` path and you only need that file.
2. Choose the lookup scope: use `--tool codex` in Codex, `--tool claude` in Claude Code, and `--tool any` only when comparing paired copies or debugging tool-agnostic ambiguity.
3. Choose the lookup directory. Use the current working directory for the active project; pass `--cwd /path/to/project` when resolving a project-local skill for another workspace.
4. Run `path` to get the file path, `cat` to read the skill, `list` to inventory visible skills, or `roots` to inspect search order.
5. If the resolver reports ambiguity or no match, inspect `list` and `roots` before continuing; do not substitute a likely-looking path.
6. For edits, use the canonical tracked skill path when one exists, such as dotfiles `claude/skills/<name>/` or `codex/skills/<name>/`. Keep paired Claude/Codex copies synchronized except for intentional tool-specific frontmatter or manifest-recorded divergences.

Command:

```bash
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill
```

Common operations:

```bash
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill path slack-emacs --tool codex
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill cat slack-emacs --tool codex
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill path diagnose --tool claude
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill list --tool codex
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill roots --tool codex
/Users/pablostafforini/My\ Drive/dotfiles/bin/agent-skill path triage-issues --tool codex --cwd /Users/pablostafforini/My\ Drive/repos/tlon.el
```

Rules:

- If you need to read a skill and do not already have a literal absolute `SKILL.md` path, resolve it with `agent-skill` first.
- Use `--tool claude` when operating in Claude Code and `--tool codex` when operating in Codex.
- The resolver searches project-local roots from `--cwd` first, then configured workspace/global roots, then system/plugin skill locations.
- If the resolver reports ambiguity or no match, stop and investigate the skill inventory instead of substituting a likely-looking path.
