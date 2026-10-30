import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "ENGLE AND SIRIWARDANE (2018), RFS 31(2):449-492",
    "pdf": "engle_siriwardane_2018.pdf",
    "sha256": "0e7aae3d7256bd4630f478ec56f3eff698ee6a23ecfd7e4c4488503f853af090",
    "pages": (449, 492),
    "offset": 447,
    "prefix": "ES",
    "fabricated": "Volatility asymmetry is mostly explained by a mechanical leverage effect, not exposure to the aggregate market.",
    "wrong_page": 450,
    "controls": ["EngleSiriwardane.control_symmetric_without_gamma",
                 "EngleSiriwardane.control_market_share_is_not_eighty"],
})
