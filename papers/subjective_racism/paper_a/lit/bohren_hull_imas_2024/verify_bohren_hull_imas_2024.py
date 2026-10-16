"""Our reading of Bohren, Hull and Imas, NBER working paper 29820, revised
December 2024, checked against the cached source. Pages are PDF pages. The
printed page is the PDF page minus 2."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "bohren_hull_imas_2024",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "bohren_hull_imas_wp.pdf"),
    "pdf_sha256": "0973ae89c4763ef029902cbf21fae8deb6abffc8a0a375b106d6eac34ef184f7",
    "text": os.path.join(CACHE, "bohren_hull_imas_wp.txt"),
    "text_sha256": "f8c579e063d5014e0e8e789a54c48d0c5c9509987d728d32d5c284b59d8d40c1",
    "pages": (1, 66),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (20, "any total discrimination that arises from a race-neutral action rule is direct, as the systemic component is zero by definition."),
    "controls": ["BohrenEtAl2024.control_pairing_matters", "BohrenEtAl2024.control_audit_misses_systemic"],
})
