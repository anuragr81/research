import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "OCC (2024), 12 CFR PART 50, LIQUIDITY RISK MEASUREMENT STANDARDS",
    "pdf": "cfr_occ_part50_2024.pdf",
    "sha256": "39d67c38ec26793afe510394c98d663f223e4a62623fd52f040a198fc10034a4",
    "pages": (953, 997),
    "offset": 952,
    "layout": False,
    "prefix": "OCCL",
    "fabricated": "This part establishes a minimum liquidity standard for all national banks",
    "wrong_page": 955,
    "controls": ['MeasurementMap.control_lcr_needs_positive_runoff'],
})
