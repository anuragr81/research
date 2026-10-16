"""Our reading of Austen-Smith and Fryer (2003), NBER Working Paper 9904,
checked against the cached source. Pages are PDF pages. Printed page = PDF
page - 1 from PDF page 3 on."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "austen_smith_fryer_2003",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "austen_smith_fryer_2005.pdf"),
    "pdf_sha256": "8705cf80b719b7dd0a68814d4200587064633a6eb125d782a8fdf5ce30e3ed9e",
    "text": os.path.join(CACHE, "austen_smith_fryer_2005.txt"),
    "text_sha256": "625275d1bfbf89dee7ffb18c187a0511fc1244c50ef04df103c5b762da11a023",
    "pages": (1, 39),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (5, "firms are assumed to care about each employee's group membership"),
    "controls": ["AustenSmithFryer2003.control_no_assumption7_all_accepted",
                 "AustenSmithFryer2003.control_separation_rejects"],
})
