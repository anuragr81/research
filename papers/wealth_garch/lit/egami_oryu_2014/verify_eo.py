import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "EGAMI AND ORYU, AN EXCURSION-THEORETIC APPROACH TO REGULATOR'S BANK REORGANIZATION PROBLEM",
    "pdf": "egami_oryu_2013_v2.pdf",
    "sha256": "522825d48acfc464aed85efa9b1fc5d5b1d185971716aa0b60adf979210c9e91",
    "pages": (1, 24),
    "offset": 0,
    "prefix": "EO",
    "fabricated": "the bank chooses its own dividend barrier and pays a fixed cost to issue equity",
    "wrong_page": 9,
    "controls": ["EgamiOryu.control_zero_scale_ties"],
})
