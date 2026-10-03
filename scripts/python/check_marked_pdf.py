#!/usr/bin/env python3
"""Conferência mecânica do PDF desta revisão; não substitui a leitura visual.

Executar da raiz: python3 scripts/python/check_marked_pdf.py
Requer Poppler, pypdf e Pillow. As imagens de inspeção ficam temporárias.
"""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile
import xml.etree.ElementTree as ET

from PIL import Image
from pypdf import PdfReader


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def inspect_pdf(pdf_path, image_dir):
    prefix = image_dir / "page"
    subprocess.run(
        ["pdftoppm", "-scale-to", "792", "-png", str(pdf_path), str(prefix)],
        check=True,
    )
    bbox_xml = subprocess.check_output(["pdftotext", "-bbox", str(pdf_path), "-"])
    ns = {"x": "http://www.w3.org/1999/xhtml"}
    pages = ET.fromstring(bbox_xml).findall(".//x:page", ns)
    reader = PdfReader(pdf_path)
    assert len(pages) == len(reader.pages)

    observations = []
    for number, page in enumerate(pages, start=1):
        words = page.findall(".//x:word", ns)
        width = float(page.attrib["width"])
        height = float(page.attrib["height"])
        clipped = []
        beyond_horizontal_margin = []
        for word in words:
            x_min, y_min, x_max, y_max = (
                float(word.attrib[key]) for key in ("xMin", "yMin", "xMax", "yMax")
            )
            if x_min < 0 or y_min < 0 or x_max > width or y_max > height:
                clipped.append(word.text)
            # Margens de 2,5 cm, com tolerância para marcas de nota e composição.
            if x_min < 64 or x_max > width - 64:
                beyond_horizontal_margin.append(word.text)

        filename = prefix.with_name(f"page-{number:0{len(str(len(pages)))}d}.png")
        with Image.open(filename) as rendered:
            rgb = rendered.convert("RGB")
            yellow_pixels = sum(pixel == (255, 245, 140) for pixel in rgb.getdata())
        observations.append({
            "page": number,
            "word_count": len(words),
            "yellow_pixels_at_792px_height": yellow_pixels,
            "words_beyond_page_bounds": clipped,
            "words_beyond_horizontal_margin_with_tolerance": beyond_horizontal_margin,
        })

    assert all(p["word_count"] > 0 for p in observations), "Página sem texto"
    assert all(not p["words_beyond_page_bounds"] for p in observations), "Texto cortado"
    assert all(not p["words_beyond_horizontal_margin_with_tolerance"]
               for p in observations), "Texto excede a margem horizontal"
    assert sum(p["yellow_pixels_at_792px_height"] for p in observations) > 0
    return observations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pdf", type=Path, default=Path("paper_dados_format_quali.pdf"))
    parser.add_argument("--output", type=Path,
                        default=Path("quality_reports/2026-10-03_pdf-qa.json"))
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="quali-pdf-qa-") as temporary:
        observations = inspect_pdf(args.pdf, Path(temporary))
    result = {
        "status": "PASS_mechanical",
        "scope": "Renderização, presença de amarelo e limites físicos do texto; "
                 "não verifica argumento, números ou cobertura semântica da marcação.",
        "pdf": str(args.pdf),
        "pdf_sha256": sha256(args.pdf),
        "source_sha256": sha256("paper_dados_format_quali.Rmd"),
        "script_sha256": sha256(__file__),
        "page_count": len(observations),
        "pages_with_yellow": [p["page"] for p in observations
                              if p["yellow_pixels_at_792px_height"] > 0],
        "pages": observations,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n",
                           encoding="utf-8")
    print(f"PASS mecânico: {len(observations)} páginas; {args.output}")


if __name__ == "__main__":
    main()
