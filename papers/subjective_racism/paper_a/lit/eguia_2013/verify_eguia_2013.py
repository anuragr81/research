"""Our reading of Eguia (2013), the working paper of 10 September 2013,
checked against the cached source. Pages are PDF pages, equal to printed pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "eguia_2013",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "eguia_2013_wp.pdf"),
    "pdf_sha256": "8a54c300b3d8953cd0c32ae6f94c7219785681d1a7b2b2b1ae42d2f138df1b9e",
    "text": os.path.join(CACHE, "eguia_2013_wp.txt"),
    "text_sha256": "1d3565e6d27d8b48515a474026d1200edef1883af74f7f39b742dc1164781126",
    "pages": (1, 40),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (6, "Individual ability is publicly observed by every agent."),
    "controls": ["Eguia2013.control_no_mobility", "Eguia2013.control_lump_sum_no_screen"],
})
