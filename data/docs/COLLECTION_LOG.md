# Limongi Podcast Audio Collection

Date accessed: 2026-05-09

## Source

Two YouTube episodes of *Fora da Política Não Há Salvação* with Fernando Limongi on *Operação Impeachment* were collected with `yt-dlp`:

- Part 1: `FRCE4DQViGM`, uploaded 2023-05-13, duration 5285 seconds.
- Part 2: `m8j09NLRgSk`, uploaded 2023-05-20, duration 5490 seconds.

The duplicate YouTube IDs found in search (`kTwrEPNKaZI`, `pRDsp3d_nI0`) were not downloaded; the originals above were used.

## Raw Files

Raw audio and YouTube metadata are stored in `data/raw/audio/limongi_impeachment/`:

- `20230513_FRCE4DQViGM_part1.mp4`
- `20230513_FRCE4DQViGM_part1.info.json`
- `20230520_m8j09NLRgSk_part2.mp4`
- `20230520_m8j09NLRgSk_part2.info.json`

## Transcription

Automatic subtitles were attempted with `yt-dlp --write-subs --write-auto-subs`, but YouTube returned HTTP 429 and no usable subtitle file was collected. The audio was therefore transcribed locally with `openai-whisper` using the `small` model, language `pt`, and JSON output.

Derived transcript JSON files are stored in `data/processed/transcripts/limongi_impeachment/`:

- `20230513_FRCE4DQViGM_part1.json`: 684 segments, 88.1 minutes covered, 10,047 words.
- `20230520_m8j09NLRgSk_part2.json`: 1,158 segments, 91.1 minutes covered, 10,618 words.

These are automatic transcripts and should be treated as working evidence, not as a citable verbatim source without checking against the audio.

## Reproduction

Download:

```bash
python3 scripts/python/download_limongi_podcast_audio.py
```

Transcribe after installing Whisper in a local environment:

```bash
python3 scripts/python/transcribe_limongi_whisper.py
```

Checksums for collected and processed files are stored in `data/checksums.sha256`.
