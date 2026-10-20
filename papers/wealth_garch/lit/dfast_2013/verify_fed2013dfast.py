import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FEDERAL RESERVE (2013), DODD-FRANK ACT STRESS TEST 2013, SUPERVISORY STRESS TEST METHODOLOGY AND RESULTS",
    "pdf": "dfast_2013_results.pdf",
    "sha256": "11acc1d8c687e13767a323913e6b5fed0d835e1c463419603ce288d3779161a6",
    "pages": (1, 68),
    "offset": -6,
    "prefix": "DF13",
    "fabricated": "JPMorgan Chase & Co. 10.4 6.8 4.3",
    "wrong_page": 18,
    "controls": ["StressTests.control_horizon_not_quarters", "StressTests.control_decline_needs_order"],
})
