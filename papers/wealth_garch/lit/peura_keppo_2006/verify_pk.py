import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

PAGES = {p: p - 2162 for p in range(2163, 2187)}
PAGES[2187] = 24
PAGES.update({p: p - 2163 for p in range(2188, 2202)})

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "PEURA AND KEPPO (2006), JOURNAL OF BUSINESS 79(4)",
    "pdf": "peura_keppo_2006.pdf",
    "sha256": "112de752eccbf5a81642ea1af52484ebe97daf6e6bbad2d4ebcc566f714fe5ab",
    "ocr": "peura_keppo_2006.ocr",
    "pages": (2163, 2201),
    "offset": 2162,
    "pdf_page": PAGES,
    "prefix": "PK",
    "fabricated": "Our main modeling innovation is to impose a fixed cost on capital issues.",
    "wrong_page": 2170,
    "controls": ["PeuraKeppo.control_dplus_needs_positive_rho",
                 "PeuraKeppo.control_paste_needs_distinct_roots"],
})
