import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BONAPARTE AND COOPER (2009), COSTLY PORTFOLIO ADJUSTMENT",
    "pdf": "bonaparte_cooper_2010.pdf",
    "sha256": "487ed16b1aea7f2f352e5275da069479a5f7325bbc6262bc985c34fd7b4000b2",
    "pages": (1, 18),
    "offset": -2,
    "prefix": "BC",
    "fabricated": "the adjustment cost is identified from the thresholds of the inaction region",
    "wrong_page": 9,
    "controls": ["BonaparteCooper.control_no_cost_at_one"],
})
