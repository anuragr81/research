import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BLACK AND PEROLD (1992), JEDC 16(3-4):403-426",
    "pdf": "black_perold_1992.pdf",
    "sha256": "c25eb94e974e09fabecdf1aefa2d9d9d43a39be70dd07d1de4f51d1a697b97dd",
    "pages": (403, 426),
    "offset": 402,
    "prefix": "BP",
    "fabricated": "maintains the portfolio's risk exposure a constant fraction of wealth, with no floor",
    "wrong_page": 410,
    "controls": ["BlackPerold.control_ratio_needs_multiplier_branch",
                 "BlackPerold.control_limit_needs_fixed_floor"],
})
