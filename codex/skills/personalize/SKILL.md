---
name: personalize
description: Draft in Pablo's own voice anything he will send or publish as himself — email, Slack, forum/PR comments, tweets, Google Docs/Sheets comments, letters, reviews. Use whenever composing text that will go out under Pablo's name, even a one-line reply or a quick DM. It matches his private voice samples AND avoids AI-writing tells in a single composition pass, so there is no separate cleanup step afterwards. Do NOT use for text Pablo is not personally authoring (use `humanize` for that), nor for internal notes, commit messages, or code.
---

# Personalize

Write it as Pablo would write it, the first time. One pass, no post-hoc scrubbing.

## Why this skill exists

Anything Pablo sends or publishes under his own name has to sound like him, not
like a language model. There are two failure modes, and this skill exists to
close both at once, while writing:

- **Generic AI register** — the "tells": vocabulary like *delve/robust/
  comprehensive*, em-dash overuse, rule-of-three stacks, trailing "-ing" summary
  clauses, copula avoidance, and so on.
- **Correct-but-not-Pablo** — prose that is clean and grammatical yet missing his
  rhythm, his explicit hedging, and his vocabulary. Avoiding AI tells is not the
  same as sounding like him.

Historically this was two steps: draft, then run the `humanize` skill as a
separate audit. That double pass is tedious, and when the draft is already
in-voice it changes almost nothing. So this skill folds both concerns into
composition itself: you hold the tells in mind *and* match his voice as you
write, producing clean, in-voice text directly. There is no separate humanize
invocation for ordinary messages.

## Procedure

1. **Load his voice.** Read the private samples at
   `/Users/pablostafforini/My Drive/dotfiles/claude/context/voice-samples.md`
   (gitignored, so it may be absent on a fresh clone — if missing, rely on the
   style rules below alone). Pattern-match register, rhythm, sentence length, and
   vocabulary. Do not reuse their content.
2. **Load the tells to avoid.** If the `humanize` tells catalogue is not already
   in your context this session, read the *Tells catalogue* section of
   `/Users/pablostafforini/My Drive/dotfiles/claude/skills/humanize/SKILL.md`.
   You are applying it *as you write*, not running humanize's mark/edit/report
   audit — it is simply the reference list of what to steer clear of. Keeping the
   list in one place (humanize) means it can't drift out of sync.
3. **Compose in one pass**, in his voice, with the tells absent by construction.
4. **Present the draft.** No separate cleanup pass for ordinary messages. For a
   long, high-stakes *public* piece — a launch blog post, a widely-read thread —
   where genuine fresh-eyes QA helps, you may still run a separate `humanize`
   audit afterwards, but treat that as a judgment call, not a routine step.

## Pablo's style (distilled from the samples)

- Full, grammatical sentences even in chat; no fragment-style punchiness.
  Contractions are natural and welcome.
- Plain, direct openings: "Hi X," then straight to the point. Closings offer
  further help concretely ("do let me know and I'll try to elaborate"), never
  formulaically.
- Precise epistemic bookkeeping: state confidence explicitly ("I don't have
  strong views", "I wouldn't be surprised if", "with great confidence"), give
  credences or rough numbers where possible, and separate the claim from the
  confidence in it.
- Number the points when replying to several questions or raising several
  considerations; use plain asterisk bullets for casual lists.
- Concrete examples immediately after abstract claims, often introduced by
  "e.g.", "for example", or a bare "Examples:" list.
- Candid self-deprecation where honest ("I know this is all very general and I
  doubt it will be very useful") — but never false modesty about the substance
  of a judgment.
- Willing to state a blunt judgment plainly, then bound its scope ("please don't
  place much weight on them").
- Occasional Latin or philosophical vocabulary where it earns its place (prima
  facie, ex post facto, ceteris paribus); otherwise common words.
- Bare URLs on their own line in chat; normal links in prose.
- Minimal exclamation marks, no emoji, no engagement-bait rhetoric.
- Dashes are used sparingly and deliberately; commas and parentheses carry most
  asides.
- Always smart quotes, never straight or angular ones. Double quotes for quoting
  and “scare quoting”; single quotes for mentioning (e.g. the word ‘pet’ has
  three letters).

## Relationship to humanize

`humanize` means "don't sound like an AI", and applies to any outgoing text.
`personalize` means "sound like Pablo", which already implies not sounding like
an AI. They are **not stacked**: a message in Pablo's voice gets `personalize`
only; text that has to go out but isn't in his personal voice gets `humanize`
only. The tells list lives in one place — humanize — and this skill reuses it.
