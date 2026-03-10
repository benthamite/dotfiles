---
name: transcribe-note
description: Transcribe the most recent audio file in Downloads and create an org-mode note from it. Use when the user says "transcribe", "transcribe note", "transcribe audio", "audio to note", "voice note", or wants to turn a recording into a written note.
argument-hint: [language] [filename]
---

# Transcribe audio to note

Finds the most recent audio file in `~/Downloads`, transcribes it with whisperx, then uses the transcript to create a well-organized org-mode note.

## Arguments

- `$ARGUMENTS` may contain a language code (e.g. `en`, `es`) and/or a specific filename. If a filename is given, use that file instead of the most recent one. If a language is given, pass it to whisperx with `--language`. If no language is given, let whisperx auto-detect.

## Procedure

### 1. Find the audio file

If a filename was given in the arguments, look for it in `~/Downloads/`. Otherwise, find the most recent file by modification time matching these extensions: `m4a`, `mp3`, `wav`, `ogg`, `flac`, `aac`, `opus`, `webm`, `mp4`, `wma`.

```bash
ls -t ~/Downloads/*.{m4a,mp3,wav,ogg,flac,aac,opus,webm,mp4,wma} 2>/dev/null | head -1
```

If no audio file is found, tell the user and stop.

Print the filename and file size so the user can confirm the right file was found.

### 2. Transcribe with whisperx

Run whisperx with the `large-v3` model, outputting plain text to a temporary directory:

```bash
TMPDIR=$(mktemp -d)
whisperx --model large-v3 --output_dir "$TMPDIR" --output_format txt [--language LANG] "AUDIO_FILE"
```

Read the resulting `.txt` file from `$TMPDIR`. Clean up the temporary directory afterwards.

If whisperx fails, fall back to `whisper`:

```bash
whisper --model large-v3 --output_dir "$TMPDIR" --output_format txt [--language LANG] "AUDIO_FILE"
```

### 3. Ask the user for a title

Use AskUserQuestion to ask the user what the note should be titled. Show them the first ~200 words of the transcript so they have context.

### 4. Create the note

Using the full transcript as source material, write a well-organized org-mode note. This is **not** a verbatim copy of the transcript. Instead:

- **Restructure** the content into logical sections with org headings (`**` level, since the top-level `*` heading is the title).
- **Clean up** verbal artifacts: filler words, false starts, repetitions, digressions.
- **Preserve** all substantive content, ideas, arguments, and nuances. Do not omit points just because they were expressed informally.
- **Improve** the prose so it reads like a well-written document, while keeping the author's voice and intent.
- **Add structure** where the spoken content was stream-of-consciousness: group related points, add section headings, use lists where appropriate.

The note should follow the org-note format:

1. **Slugify the title.** Run `emacsclient -e '(simple-extras-slugify "TITLE")'` to get the slug. Use the result (minus surrounding quotes) as the filename, appending `.org`.

2. **Write the file** to `/Users/pablostafforini/My Drive/notes/SLUG.org` with this structure:

   ```org
   #+title: TITLE

   * TITLE
   RESTRUCTURED CONTENT HERE
   ```

3. **Generate an org ID.** Run:

   ```bash
   emacsclient -e '(with-current-buffer (find-file-noselect "FILEPATH") (goto-char (point-min)) (org-next-visible-heading 1) (org-id-get-create))'
   ```

### 5. Confirm

Tell the user the note has been saved, showing the file path and a brief summary of the sections created.
