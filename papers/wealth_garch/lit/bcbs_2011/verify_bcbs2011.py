import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BASEL COMMITTEE (2011), BASEL III CAPITAL FRAMEWORK, REV. JUNE 2011",
    "pdf": "bcbs_basel3_2011.pdf",
    "sha256": "cbb1d8ea33595d817b18dc5a40c437335c750518d21bb3d063a6e77196eff96e",
    "pages": (1, 69),
    "offset": -8,
    "prefix": "B3",
    "fabricated": "Common Equity Tier 1 must be at least 3.0% of risk-weighted assets at all times.",
    "wrong_page": 13,
    "controls": ["MeasurementMap.control_solvency_needs_positive_exposure",
                 "MeasurementMap.control_rwa_needs_zero_weight_on_riskless",
                 "MeasurementMap.control_leverage_needs_l0_lt_one"],
})
