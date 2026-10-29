import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "SENGUPTA AND HOGUE (2014), RISK AND ORGANIZATIONAL STRUCTURES AND BANK CAPITAL RATIOS, KC FED ECONOMIC REVIEW, FORTHCOMING VERSION",
    "pdf": "sengupta_hogue_2014.pdf",
    "sha256": "8848d4df684974c7046fe102273ea8b1fd2ed0bba68428828dc5a311bccdc8dc",
    "pages": (5, 22),
    "offset": 4,
    "prefix": "SH",
    "fabricated": "Non-BHC banks are associated with a 1.06-percentage-point lower Tier 1 leverage ratio",
    "wrong_page": 12,
    "controls": ['SenguptaHogue.control_rounding', 'SenguptaHogue.control_riskbased_not_leverage'],
})
