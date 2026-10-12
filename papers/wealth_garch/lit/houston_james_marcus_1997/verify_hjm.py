import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "HOUSTON, JAMES AND MARCUS (1997), JOURNAL OF FINANCIAL ECONOMICS",
    "pdf": "houston_james_marcus_1997.pdf",
    "sha256": "a1b9fabc832d80eeca67d0d069afcba1ca18d31d2aaf12988c165f43345eafb1",
    "pages": (135, 164),
    "offset": 134,
    "prefix": "HJM",
    "fabricated": "Some companies tend to keep the maximum capital levels in their subsidiary banks",
    "wrong_page": 150,
    "controls": ["HoustonJamesMarcus.control_sensitivity_rises_if_b_positive",
                 "HoustonJamesMarcus.control_zero_not_at_13_with_other_sign"],
})
