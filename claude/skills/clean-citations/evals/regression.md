# Citation cleanup regression acceptance

These cases exercise a fresh agent using the installed workflow. Run them after
changing citation selection, replacement or resumable state. The structural
checker's unit tests do not establish these agent-level behaviors.

## Isolate the fixture

Create separate disposable directories outside Drive for Claude and Codex.
Supply `note.org`, `fixture.bib`, `evidence.md` and an explicit `runtime/` state
directory. State that only these fixture files may change, that evidence describes
fictional works, and that no network, real bibliography, active Emacs, import,
credential handling, external messages or subagents are authorized. Bibliography
entries are immutable fixture input. Do not silently enable acquisition or fill
missing facts. Keep attachment state separate from citation validity.

Inspect native CLI help, authentication mode and startup effects before launching.
Use existing subscription authentication without copying tokens or changing auth
namespaces. Preserve mandatory guards, remove inherited Emacs/session identifiers,
and use native no-persistence and sandbox controls. A temporary cwd alone does not
isolate hooks: unrelated lifecycle cleanup must not run as a side effect of this
test. If isolation excludes user skill discovery, a fixture project skill symlink
to the exact production directory may test native implicit selection; report that
the user-level placement/startup layer was not exercised.

Record each loaded skill, shared policy, converter and invoked helper's content
identity, the actual tool reads/invocations, and the native terminal result. Match
those identities after each run. Preserve failed/aborted trials; repeat only after
a relevant change or a declared isolation correction. Bound each attempt before
launch, and stop on authentication failure, unsafe effects or missing guards.

## Minimal reproducible input

Use this `note.org`:

```org
#+title: Citation cleanup fixture

Ada Example describes a notebook method. Sources: [[https://doi.org/10.5555/citation-cleanup-fixture][A Notebook Method]]; [[https://example.org/notebook-method][publisher copy]].

Source: [[https://example.org/anthology.pdf#page=57][Berta Exampleauthor, Notebooks, in the 1987 anthology]], printed pp. 66–68.

The electronic edition is licensed [[https://creativecommons.org/licenses/by/4.0/][CC BY 4.0]].

Existing citation: [cite:@Preserved2010, p. 7].
```

Create these decoded BibLaTeX records in `fixture.bib`; omit attachments and
abstracts rather than inventing them:

| Key/type | Fields |
|---|---|
| `Example2020` / `article` | author `Example, Ada`; title `A Notebook Method`; journaltitle `Journal of Fictional Methods`; date `2020`; volume `1`; number `1`; pages `1--8`; doi `10.5555/citation-cleanup-fixture`; url `https://example.org/notebook-method`; langid `english` |
| `ExampleAuthor1901` / `book` | author `Exampleauthor, Berta`; title `Hefte`; date `1901`; publisher `Fictional Press`; location `Berlin`; langid `german` |
| `Preserved2010` / `book` | author `Preserved, Carl`; title `Existing Citation`; date `2010`; publisher `Fictional Press`; location `London`; langid `english` |

In `evidence.md`, establish these synthetic facts explicitly:

- The two article URLs identify the first and only published edition of
  `Example2020`; every listed article field is correct. They are one work.
- `ExampleAuthor1901` is the first German edition. The note quotes an English
  translation in a 1987 anthology. Its title/editor/translator/publication details,
  first translation date and page correspondence to the German edition are
  unknown. No supplied artifact resolves them.
- `Preserved2010` and the existing citation's locator are correct.
- The Creative Commons URL supplies licensing information, not a source work.
- No attachment is supplied or obtainable through the permitted local fixture
  inputs. This does not create evidence about external availability.

## Three fresh sessions per client

Do not name `clean-citations` or give its path in the initial task prompt. Use a
natural request such as “Clean up all the citations in note.org using the supplied
bibliography and evidence. Complete only the first unambiguous work, save progress
under runtime/, and stop.” Include the fixture boundaries above.

1. **Convert one work and checkpoint.** The native agent discovers the workflow,
   reads the shared policy and converter, reviews the existing article and runs
   the structural helper. The article title remains as plain text beside
   `[cite:@Example2020]`. Preserve `Sources:` and surrounding prose. A redundant
   access label need not create another citation, but both URLs must be retained
   in bibliography/provenance evidence and one work item. The bibliography stays
   byte-identical. Save metadata, attachment and replacement outcomes separately;
   do not report attachment completion or invent a new entry.
2. **Resume in a new session.** Ask it to continue from `runtime/`. It revalidates
   the note, bibliography, policy and completed source mapping before continuing.
   It does not reimport/reconvert the article. It preserves the anthology link,
   printed locator, licence link and existing citation exactly. The missing
   anthology/translation identity and page mapping remain explicit blockers;
   the first German edition is not a substitute. Completed work remains complete.
3. **Run again unchanged.** Ask to clean all citations again with the same scope.
   The note and bibliography remain byte-identical to session 2. The unresolved
   source remains honestly unresolved. Ledger bookkeeping may be refreshed;
   no duplicate work, import or replacement may appear.

Independently compare the saved note with its preimage and declared source spans,
resolve all inserted keys, inspect per-item ledger states, and parse Org citations
with a clean installed Org parser when syntax/locators are material. A final
agent claim, JSON state flag or citation count alone is insufficient evidence.

## Additional cases and acceptance limits

Exercise these when the corresponding behavior changes:

- Change source text after checkpoint: re-match current spans instead of replaying
  offsets or overwriting the user's edit.
- Change the shared policy or selected bibliography record: revalidate affected
  items before reuse; a stale key/hash must not authorize replacement.
- Supply a verified first-edition locator mapping: accept it only with the
  original span, original/new locators and the exact verification source.
- Put an existing citation beside a raw reference, or a URL in a source/example
  block: preserve the existing citation/literal example while processing only
  the independently resolved reference.
- Supply a malformed or semantically misleading entry: keep structural failure,
  metadata judgment and attachment state distinct; no successful import or
  nonempty abstract may substitute for review.

This fixture proves discovery, existing-entry selection, preservation, scoped
attachment-unavailability reporting, blocked identity/locator handling and restart
idempotency. It does not prove Zotra import, live Ebib persistence/unsaved-buffer
reconciliation, downloads, original book-page inspection, filesize selection or
asynchronous attachment callbacks. Exercise those through their real owning
workflows with separately preflighted owned artifacts before claiming them.
