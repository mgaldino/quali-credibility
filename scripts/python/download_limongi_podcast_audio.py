#!/usr/bin/env python3
"""Download Limongi impeachment podcast audio with yt-dlp.

Sources:
- https://www.youtube.com/watch?v=FRCE4DQViGM
- https://www.youtube.com/watch?v=m8j09NLRgSk

Last execution: 2026-05-09
"""

from __future__ import annotations

import logging
import subprocess
from pathlib import Path


logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s")
logger = logging.getLogger(__name__)

RAW_DIR = Path("data/raw/audio/limongi_impeachment")

EPISODES = [
    {
        "url": "https://www.youtube.com/watch?v=FRCE4DQViGM",
        "format": "234-1",
        "output": "20230513_FRCE4DQViGM_part1.%(ext)s",
    },
    {
        "url": "https://www.youtube.com/watch?v=m8j09NLRgSk",
        "format": "234",
        "output": "20230520_m8j09NLRgSk_part2.%(ext)s",
    },
]


def download_episode(episode: dict[str, str]) -> None:
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    output_template = str(RAW_DIR / episode["output"])
    cmd = [
        "yt-dlp",
        "--no-playlist",
        "-f",
        episode["format"],
        "--write-info-json",
        "--no-write-subs",
        "--no-write-auto-subs",
        "-o",
        output_template,
        episode["url"],
    ]
    logger.info("Downloading %s", episode["url"])
    subprocess.run(cmd, check=True)


def main() -> None:
    for episode in EPISODES:
        download_episode(episode)


if __name__ == "__main__":
    main()
