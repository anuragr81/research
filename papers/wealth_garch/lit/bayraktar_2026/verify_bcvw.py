import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BAYRAKTAR, CHEVALIER, LY VATH AND WANG (2026), arXiv:2603.14557v2",
    "pdf": "bcvw_2603.14557v2.pdf",
    "sha256": "e080555878991988c1978a51b788be16145e8082f59a2be19682441d721cd935",
    "pages": (1, 39),
    "offset": 0,
    "prefix": "BCVW",
    "fabricated": "the parameter a3 is irrelevant for the asymptotic upper bound of the cap",
    "wrong_page": 3,
    "controls": ["BCVW.control_nondegenerate_needs_abs_c_lt_one", "BCVW.control_limit_needs_a1_le_a3"],
})
