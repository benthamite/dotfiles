---
name: transcribe-note
description: Transcribe a recent or named audio file from Downloads into a structured org-mode note. Use when the user says "transcribe", "transcribe note", "transcribe audio", "audio to note", "voice note", "voice memo", "dictation", "recording to note", or asks to turn an audio recording into a written note.
argument-hint: [language] [filename]
model: sonnet
---

# Transcribe audio to note

Finds a recent or named audio file in `~/Downloads`, transcribes it with whisperx, then uses the transcript to create a well-organized org-mode note.

## Scope

Use this for the local voice-note workflow: audio input from `~/Downloads`, `whisperx` transcription, and an org note under `/Users/pablostafforini/My Drive/notes/`.

Do not use it when the user wants only a verbatim transcript, subtitles, timestamps, diarization, or another output format. Handle those as ordinary transcription tasks or ask which output they want. Do not silently move or copy files from other directories into `~/Downloads`; if a request names a path outside the Downloads workflow, pause and clarify.

## Arguments

- `$ARGUMENTS` may contain a language code (e.g. `en`, `es`) and/or a specific filename in `~/Downloads`. If a filename is given, use that file instead of the most recent one. If a language is given, pass it to whisperx with `--language`. If no language is given, let whisperx auto-detect.

## Procedure

### 1. Prepare and find the audio file

Verify the local dependencies before starting:

```bash
command -v whisperx ffmpeg emacsclient
```

If any dependency is missing, report the missing command and diagnose the setup instead of inventing a fallback.

If a filename was given in the arguments, look for it in `~/Downloads/`. Otherwise, find the most recent file by modification time matching these extensions: `m4a`, `mp3`, `wav`, `ogg`, `flac`, `aac`, `opus`, `webm`, `mp4`, `wma`.

```bash
find "$HOME/Downloads" -maxdepth 1 -type f \( -iname '*.m4a' -o -iname '*.mp3' -o -iname '*.wav' -o -iname '*.ogg' -o -iname '*.flac' -o -iname '*.aac' -o -iname '*.opus' -o -iname '*.webm' -o -iname '*.mp4' -o -iname '*.wma' \) -print0 | xargs -0 stat -f '%m %N' 2>/dev/null | sort -rn | sed 's/^[0-9]* //' | head -1
```

If no audio file is found, tell the user and stop.

Report the filename and file size before transcribing:

```bash
stat -f '%N (%z bytes)' "$AUDIO_FILE"
```

### 2. Transcribe with whisperx

Run whisperx with the `large-v3` model and `int8` compute type (required on Apple Silicon CPU), outputting plain text to a temporary directory. Use a trap as backup cleanup, and remove the temporary directory explicitly after the transcript is loaded.

```bash
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
whisperx --model large-v3 --compute_type int8 --output_dir "$TMPDIR" --output_format txt [--language LANG] "AUDIO_FILE"
TRANSCRIPT_FILE=$(find "$TMPDIR" -maxdepth 1 -type f -name '*.txt' -print -quit)
test -s "$TRANSCRIPT_FILE"
```

Read the resulting `.txt` file from `$TMPDIR`. Once the transcript is loaded, clean up the temporary directory and clear the trap:

```bash
rm -rf "$TMPDIR"
trap - EXIT
```

If whisperx fails, **do not fall back to whisper** (it runs on CPU without ctranslate2 and is extremely slow). Instead, diagnose the failure: use a subagent or the local debugging workflow when available, inspect stderr, identify the root cause (e.g. compute type incompatibility, broken ffmpeg linkage, missing libraries), and fix it. Then retry whisperx. Only proceed to step 3 once whisperx succeeds.

### 3. Ask the user for a title

Ask the user what the note should be titled, using the agent's user-input mechanism or normal chat. Show them the first ~200 words of the transcript so they have context. Wait for the title; do not invent one silently.

### 4. Create the note

Using the full transcript as source material, write a well-organized org-mode note. This is **not** a verbatim copy of the transcript. Instead:

- **Restructure** the content into logical sections with org headings (`**` level, since the top-level `*` heading is the title).
- **Clean up** verbal artifacts: filler words, false starts, repetitions, digressions.
- **Preserve** all substantive content, ideas, arguments, and nuances. Do not omit points just because they were expressed informally.
- **Improve** the prose so it reads like a well-written document, while keeping the author's voice and intent.
- **Add structure** where the spoken content was stream-of-consciousness: group related points, add section headings, use lists where appropriate.

The note should follow the org-note format:

1. **Slugify the title.** Run `emacsclient -e '(simple-extras-slugify "TITLE")'` to get the slug, replacing `TITLE` with an Elisp-escaped string rather than interpolating raw user text. Use the result (minus surrounding quotes) as the filename, appending `.org`.

2. **Write the file** to `/Users/pablostafforini/My Drive/notes/SLUG.org` with this structure. If the file already exists, ask before overwriting or appending.

   ```org
   #+title: TITLE

   * TITLE
   RESTRUCTURED CONTENT HERE
   ```

3. **Generate an org ID.** Run:

   ```bash
   emacsclient -e '(with-current-buffer (find-file-noselect "FILEPATH") (goto-char (point-min)) (org-next-visible-heading 1) (org-id-get-create) (save-buffer))'
   ```

### 5. Verify and confirm

Before reporting success, verify the note file exists, the title lines are present, the org ID was saved, and the temporary transcript directory was removed:

```bash
test -s "$NOTE_FILE"
rg -n '^#\+title:|^\* |:ID:' "$NOTE_FILE"
test ! -e "$TMPDIR"
```

Tell the user the note has been saved, showing the file path and a brief summary of the sections created.
