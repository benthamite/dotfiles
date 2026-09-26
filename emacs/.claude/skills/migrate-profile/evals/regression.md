# Migration regression scenarios

Evaluate against synthetic profiles and stores (see
`tests/test_migrate_profile.py`); never migrate real history while auditing.

1. **No arguments, many retired profiles:** Sessions sit in 1.0, 1.1 and 2.0;
   `active` is 2.0. Every 1.0 and 1.1 session moves to 2.0 without a question.
2. **Consolidation:** 1.0 and 1.1 both have Claude buckets for the same
   package. The first moves as a bucket; the second merges session by session.
   Nothing is overwritten.
3. **Orphan prompt history:** History entries (with or without `sessionId`)
   name a retired directory whose transcripts are gone. They follow to the
   current profile.
4. **No counterpart:** A retired package directory has no match in the current
   profile. Its sessions stay and are listed, not silently skipped.
5. **Live agent:** A Claude or Codex process still runs in a retired
   directory. The script skips that directory and names the process;
   everything else still moves.
   The agent asks before ending the process.
6. **Marker mismatch:** `active` and `.current-profile` disagree. The script
   stops; the agent reports it and edits neither marker.
7. **Dry run:** `--dry-run` writes nothing, including backups.
8. **Idempotence:** A second run after a complete migration moves nothing.
