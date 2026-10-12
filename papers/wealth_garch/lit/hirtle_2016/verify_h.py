import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "HIRTLE (2016), FRBNY STAFF REPORT 666, REVISED APRIL 2016",
    "pdf": "hirtle_2016.pdf",
    "sha256": "a053fd02846c45b1f3889762fc384346845861634440526257a872825d4efa5e",
    "pages": (1, 35),
    "offset": -2,
    "prefix": "H",
    "fabricated": "Many large U.S. bank holding companies (BHCs) stopped paying dividends early in the recent financial crisis",
    "wrong_page": 8,
    "controls": ["Hirtle.control_rounding_rejects_wrong_percentage",
                 "Hirtle.control_rounding_rejects_wrong_count"],
})
