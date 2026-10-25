import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BEHN, DAMINATO AND SALLEO (2019), A DYNAMIC MODEL OF BANK BEHAVIOUR UNDER MULTIPLE REGULATORY CONSTRAINTS",
    "pdf": "behn_daminato_salleo_2019.pdf",
    "sha256": "25d58c20f4f7d1d394c456477b16c7e8f5006fee26b0db1f52ff3015fe07393c",
    "pages": (1, 61),
    "offset": -1,
    "prefix": "BDS",
    "fabricated": "the cost of breaching the requirement grows with the size of the shortfall",
    "wrong_page": 12,
    "controls": ["BehnEtAl.control_zero_exponent_is_fixed"],
})
