import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "OCC (2024), 12 CFR PART 6, PROMPT CORRECTIVE ACTION",
    "pdf": "cfr_occ_part6_2024.pdf",
    "sha256": "6dd317516e83f84774c2858b910ad086f356c7e28b1fe4bacd60f5b1641ec70b",
    "pages": (392, 402),
    "offset": 391,
    "layout": False,
    "prefix": "OCC",
    "fabricated": "has a total risk-based capital ratio of 12.0 percent or greater",
    "wrong_page": 395,
    "controls": ['MeasurementMap.control_distribution_needs_positive_rwa'],
})
