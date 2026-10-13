"""OCR of Corcoran and Karels (1985), whose PDF holds page images only.

Run with a Python that has rapidocr-onnxruntime (the text pinned by the suite was made with
rapidocr-onnxruntime 1.2.3 and onnxruntime 1.31, in ~/.cache/entry_contest/ocr_venv). The page
images are extracted with `pdfimages -png`; each is upscaled 2.5x (Lanczos) and read, the
boxes are grouped into lines by their vertical centre and read left to right, and the pages
are written to ~/.cache/entry_contest/corcoran_karels_1985.txt separated by form feeds.
"""

import pathlib
import subprocess

from PIL import Image
from rapidocr_onnxruntime import RapidOCR

CACHE = pathlib.Path.home() / ".cache" / "entry_contest"
PDF = CACHE / "corcoran_karels_1985.pdf"
PAGES = CACHE / "ck_pages"

PAGES.mkdir(exist_ok=True)
subprocess.run(["pdfimages", "-png", str(PDF), str(PAGES / "img")], check=True)
eng = RapidOCR()
out = []
for k in range(20):
    img = Image.open(PAGES / f"img-{2 * k:03d}.png").convert("RGB")
    img = img.resize((int(img.width * 2.5), int(img.height * 2.5)), Image.LANCZOS)
    path = PAGES / f"up-{k + 1:02d}.png"
    img.save(path)
    res, _ = eng(str(path))
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
(CACHE / "corcoran_karels_1985.txt").write_text("\f".join(out) + "\f")
