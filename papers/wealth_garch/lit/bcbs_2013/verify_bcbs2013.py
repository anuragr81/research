import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BASEL COMMITTEE (2013), BASEL III LIQUIDITY COVERAGE RATIO",
    "pdf": "bcbs_lcr_2013.pdf",
    "sha256": "cb2d45abd8a04243c750d6eea841129897ad1a2f72296574ea18a394719f9c33",
    "pages": (1, 69),
    "offset": -6,
    "prefix": "LCR",
    "fabricated": "the value of the ratio be no lower than 120%",
    "wrong_page": 5,
    "controls": ["MeasurementMap.control_lcr_needs_positive_runoff"],
})
