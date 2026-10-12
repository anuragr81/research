import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "HOLOD AND PEEK (2010), WORKING PAPER OF 5 DECEMBER 2006",
    "pdf": "holod_peek_2006.pdf",
    "sha256": "04094a0fcf50d073d452201a50551224d056931055f8fb7da2687414539a9f01",
    "pages": (1, 44),
    "offset": -1,
    "prefix": "HP",
    "fabricated": "CapTrans is measured as the sum of dividends (with a positive sign), bank capital sale and acquisition",
    "wrong_page": 20,
    "controls": ["HolodPeek.control_dividends_enter_negatively",
                 "HolodPeek.control_not_three_times"],
})
