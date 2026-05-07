---
name: skill-resolver
description: Resolve and read Claude/Codex skill files by skill name with deterministic filesystem lookup. Use whenever you need to open, inspect, debug, or locate a skill, especially before manually expanding a skill root/path or when a skill path is uncertain.
user-invocable: true
allowed-tools: Bash
---

# Skill Resolver

Use the resolver instead of manually expanding skill root aliases or guessing where a skill lives.

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
```

Rules:

- If you need to read a skill and do not already have a literal absolute `SKILL.md` path, resolve it with `agent-skill` first.
- Use `--tool claude` when operating in Claude Code and `--tool codex` when operating in Codex.
- The resolver searches project-local skills first, then global skills, then system/plugin skill locations.
- If the resolver reports ambiguity or no match, stop and investigate the skill inventory instead of substituting a likely-looking path.
