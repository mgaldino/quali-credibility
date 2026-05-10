#!/usr/bin/env python3
"""Transcribe Limongi impeachment podcast audio with local Whisper CLI.

Requires openai-whisper available as `.venv-whisper/bin/whisper` or `whisper`
on PATH. Raw audio is preserved; JSON transcripts are written to processed/.

Last execution: 2026-05-09
"""

from __future__ import annotations

import logging
import shutil
import subprocess
from pathlib import Path


logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

RAW_DIR = Path("data/raw/audio/limongi_impeachment")
OUT_DIR = Path("data/processed/transcripts/limongi_impeachment")

AUDIO_FILES = [
    RAW_DIR / "20230513_FRCE4DQViGM_part1.mp4",
    RAW_DIR / "20230520_m8j09NLRgSk_part2.mp4",
]


def find_whisper() -> str:
    local = Path(".venv-whisper/bin/whisper")
    if local.exists():
        return str(local)
    path = shutil.which("whisper")
    if path:
        return path
    raise RuntimeError("Whisper CLI not found. Install openai-whisper first.")


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    missing = [str(path) for path in AUDIO_FILES if not path.exists()]
    if missing:
        raise FileNotFoundError(f"Missing audio files: {missing}")

    cmd = [
        find_whisper(),
        *map(str, AUDIO_FILES),
        "--model",
        "small",
        "--language",
        "pt",
        "--task",
        "transcribe",
        "--output_format",
        "json",
        "--output_dir",
        str(OUT_DIR),
        "--verbose",
        "False",
        "--threads",
        "8",
    ]
    logger.info("Transcribing %d audio files", len(AUDIO_FILES))
    subprocess.run(cmd, check=True)


if __name__ == "__main__":
    main()
