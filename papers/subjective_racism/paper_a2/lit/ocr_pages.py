"""OCR for scanned sources without a text layer. Not run by the suites.

    python ocr_pages.py SOURCE.pdf OUT.txt [dpi] [single]

Renders each page with pdftoppm, runs RapidOCR (rapidocr_onnxruntime), reads
the left column top to bottom and then the right column (or, with `single`,
the lines top to bottom), and writes one
page per form feed, as pdftotext does. The output is pinned by sha256 in the
record that uses it, so a later run with another OCR version shows up as a
changed hash rather than a silent change of text.
"""
import os
import subprocess
import sys
import tempfile

from rapidocr_onnxruntime import RapidOCR


def single_column(result):
    lines = sorted(((min(p[1] for p in box), min(p[0] for p in box), text) for box, text, _ in result))
    return "\n".join(t for _, _, t in lines)


def page_text(engine, png, single=False):
    result, _ = engine(png)
    if not result:
        return ""
    if single:
        return single_column(result)
    boxes = []
    width = max(max(p[0] for p in box) for box, _, _ in result)
    left_edge = min(min(p[0] for p in box) for box, _, _ in result)
    mid = (left_edge + width) / 2
    for box, text, _ in result:
        x0 = min(p[0] for p in box)
        x1 = max(p[0] for p in box)
        y0 = min(p[1] for p in box)
        spans = x0 < mid - 0.05 * width and x1 > mid + 0.05 * width
        col = 0 if spans or x0 < mid else 1
        boxes.append((0 if spans and y0 < 0.12 * width else 1, col, y0, x0, text))
    boxes.sort()
    return "\n".join(b[4] for b in boxes)


def main():
    src, out = sys.argv[1], sys.argv[2]
    dpi = sys.argv[3] if len(sys.argv) > 3 else "200"
    single = len(sys.argv) > 4 and sys.argv[4] == "single"
    engine = RapidOCR()
    with tempfile.TemporaryDirectory() as td:
        subprocess.run(["pdftoppm", "-r", dpi, "-png", src, os.path.join(td, "p")], check=True)
        pages = sorted(f for f in os.listdir(td) if f.endswith(".png"))
        texts = [page_text(engine, os.path.join(td, f), single) for f in pages]
    with open(out, "w") as fh:
        fh.write("\f".join(texts))
    print(f"{len(texts)} pages written to {out}")


if __name__ == "__main__":
    main()
