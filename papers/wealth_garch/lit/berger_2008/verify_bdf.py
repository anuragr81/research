import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BERGER, DEYOUNG, FLANNERY, LEE AND OZTEKIN (2008), KANSAS CITY FED RWP 08-01",
    "pdf": "berger_2008.pdf",
    "sha256": "741123eec7a14635d41678537bff7843305d506763c28233694ba4a1b251114a",
    "pages": (1, 37),
    "offset": -2,
    "prefix": "BDF",
    "fabricated": "Since the early 1990s, however, the largest U.S. banking organizations have maintained ratios close to regulatory minimums.",
    "wrong_page": 10,
    "controls": ["BergerEtAl.control_overshoot_without_speed_bound",
                 "BergerEtAl.control_step1_lower_below_half"],
})
