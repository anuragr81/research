import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "GORDON AND HIRTLE (2026), FRBNY ECONOMIC POLICY REVIEW 32(1)",
    "pdf": "gordon_hirtle_2026.pdf",
    "sha256": "f1fe4e9c87a9ee8b00248446555a41ed7c339d2c3ff9404382fd408c5510e64e",
    "pages": (1, 49),
    "offset": 0,
    "prefix": "GH",
    "fabricated": "dividend payments are positive in more than 95 percent of bank-year observations in our sample",
    "wrong_page": 20,
    "controls": ["GordonHirtle.control_ratio_needs_positive_earnings",
                 "GordonHirtle.control_lower_persistence_below_half"],
})
