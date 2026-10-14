"""OCR of Fernandez and Gali, NBER Working Paper 5930 (1997), whose PDF holds page images only.

Run with a Python that has rapidocr-onnxruntime (the text pinned by the suite was made with
rapidocr-onnxruntime 1.2.3, in ~/.cache/entry_contest/ocr_venv). The 46 page images (300 dpi,
one per page, 1-bit) are stored flipped, with the PDF's transformation turning them the right
way, so `pdfimages` would hand the OCR mirrored scans. The pages are therefore rendered with
`pdftoppm -r 300 -gray` and converted to RGB; the boxes are grouped
into lines by their vertical centre and read left to right, and the pages are written to
~/.cache/entry_contest/fernandez_gali_1997.txt separated by form feeds.
"""

import pathlib
import subprocess

import numpy as np
from PIL import Image
from rapidocr_onnxruntime import RapidOCR

CACHE = pathlib.Path.home() / ".cache" / "entry_contest"
PDF = CACHE / "fernandez_gali_1997_w5930.pdf"
PAGES = CACHE / "fg_pages"
NPAGES = 46

PAGES.mkdir(exist_ok=True)
subprocess.run(["pdftoppm", "-r", "300", "-gray", "-png", str(PDF), str(PAGES / "pg")], check=True)
eng = RapidOCR()
out = []
for k in range(NPAGES):
    img = Image.open(PAGES / f"pg-{k + 1:02d}.png").convert("RGB")
    res, _ = eng(np.asarray(img))
    boxes = []
    for box, text, score in res or []:
        ys = [p[1] for p in box]
        xs = [p[0] for p in box]
        boxes.append(((min(ys) + max(ys)) / 2, min(xs), max(ys) - min(ys), text))
    boxes.sort()
    lines, cur, cy = [], [], None
    for y, x, h, t in boxes:
        if cy is not None and abs(y - cy) > 0.5 * h:
            lines.append(" ".join(t for _, t in sorted(cur)))
            cur = []
        cur.append((x, t))
        cy = y if not cur[:-1] else (cy + y) / 2
    if cur:
        lines.append(" ".join(t for _, t in sorted(cur)))
    out.append("\n".join(lines))
(CACHE / "fernandez_gali_1997.txt").write_text("\f".join(out) + "\f")
