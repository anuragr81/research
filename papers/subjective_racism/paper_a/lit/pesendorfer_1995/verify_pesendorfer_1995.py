"""Our reading of Pesendorfer (1995), checked against the cached source. Pages
are PDF pages; PDF page n is journal page 770 + n."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "pesendorfer_1995",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "pesendorfer_1995.pdf"),
    "pdf_sha256": "d3519ab9bf286baab1a569a6f4b5560f7b27f0e04a5440c800b7cf63ab7a3f03",
    "text": os.path.join(CACHE, "pesendorfer_1995.txt"),
    "text_sha256": "4299dd24cb88aaa894bdb280b70e01a6fbceb0d50f1c08cd9b50e479b4ad9b26",
    "pages": (1, 22),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (5, "low types prefer to be matched with low types"),
    "controls": ["Pesendorfer1995.control_no_separation_without_complementarity",
                 "Pesendorfer1995.control_quadratic_low_prefers_low"],
})
