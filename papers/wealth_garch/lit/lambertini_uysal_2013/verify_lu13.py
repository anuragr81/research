import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LAMBERTINI AND UYSAL (2013), BANK CAPITAL REQUIREMENTS",
    "pdf": "lambertini_uysal_2013.pdf",
    "sha256": "4d3439e68a042ef1b5092c2a04b6fd461a68f35c083b8350e997a81c95314c18",
    "pages": (1, 47),
    "offset": 0,
    "prefix": "LU13",
    "fabricated": "the bank pays a fixed cost each time it issues new shares",
    "wrong_page": 5,
    "controls": ["LambertiniUysal.control_logTerm_zero_at_requirement"],
})
