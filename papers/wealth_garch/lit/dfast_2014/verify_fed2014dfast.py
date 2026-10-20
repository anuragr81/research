import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FEDERAL RESERVE (2014), DODD-FRANK ACT STRESS TEST 2014, SUPERVISORY STRESS TEST METHODOLOGY AND RESULTS",
    "pdf": "dfast_2014_results.pdf",
    "sha256": "fe23ea0adbbd3d270abd42697c0b0e6368f8ded7133021c0c86b7612b05e4195",
    "pages": (1, 162),
    "offset": -6,
    "prefix": "DF14",
    "fabricated": "Zions Bancorporation 10.5 3.6 6.3",
    "wrong_page": 29,
    "controls": ["StressTests.control_horizon_not_quarters", "StressTests.control_decline_needs_order"],
})
