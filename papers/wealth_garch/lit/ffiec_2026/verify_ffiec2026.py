import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FFIEC (2026), CALL REPORT INSTRUCTIONS, SCHEDULE RC-R PART I, JUNE 2026",
    "pdf": "ffiec_rcr_2026.pdf",
    "sha256": "6c56f2fb5a51d788835da01cfa1bfd9956c0a91595f618946d98b5a9c5f7d944",
    "pages": (1, 73),
    "offset": 0,
    "prefix": "RCR",
    "fabricated": "On the FFIEC 041: Divide Schedule RC-R, Part I, item 26 by item 27.",
    "wrong_page": 47,
    "controls": ["MeasurementMap.control_rwa_needs_zero_weight_on_riskless",
                 "MeasurementMap.control_leverage_needs_l0_lt_one"],
})
