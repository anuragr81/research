"""Our reading of Bohren, Imas and Rosenberg (2019), the working paper of
January 2019, checked against the cached source. Pages are PDF pages. The
printed page is the PDF page minus 1."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "bohren_imas_rosenberg_2019",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "bohren_imas_rosenberg_2019_wp.pdf"),
    "pdf_sha256": "aa697aa3f30eccc4e9b0204ed88c996dc2d4b6742a30a8dee35906cff9a3639a",
    "text": os.path.join(CACHE, "bohren_imas_rosenberg_2019_wp.txt"),
    "text_sha256": "227c28d76240a095495aa8f3434db66c6c43529a6a82a14845d72c9e44d29ba3",
    "pages": (1, 83),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (19, "Then fixing an evaluation history, discrimination decreases across periods and eventually reverses."),
    "controls": ["BohrenEtAl2019.control_taste_persists", "BohrenEtAl2019.control_reversal_misspecified"],
})
