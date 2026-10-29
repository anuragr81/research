import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "PEURA AND JOKIVUOLLE (2003), SIMULATION-BASED STRESS TESTING OF BANKS' REGULATORY CAPITAL ADEQUACY, BANK OF FINLAND DP 4/2003",
    "pdf": "peura_jokivuolle_2003.pdf",
    "sha256": "f5ab78a562a6a9fa622378e66de276e0dfe57abb6a4b512ee12decf8c4c8b0cd",
    "pages": (3, 41),
    "offset": -3,
    "prefix": "PJ",
    "fabricated": "If the average US bank used a ‘recession’ scenario in its capital adequacy analysis, the average (or median) bank capital ratio of 11.9% were consistent with a confidence level of approximately 95.0%.",
    "wrong_page": 10,
    "controls": ["PeuraJokivuolle.control_not_normal_tail", "PeuraJokivuolle.control_breach_order"],
})
