import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FDIC (2024), 12 CFR PART 327, ASSESSMENTS",
    "pdf": "cfr_fdic_part327_2024.pdf",
    "sha256": "f44906067b7230ec3a06300fb026bd562eecf486afba71dcf9ae309237e4cbca",
    "pages": (472, 560),
    "offset": 471,
    "layout": False,
    "prefix": "ASMT",
    "fabricated": "each of five financial ratios will be multiplied by a corresponding pricing multiplier.",
    "wrong_page": 510,
    "controls": ['MeasurementMap.control_assessment_needs_negative_multiplier'],
})
