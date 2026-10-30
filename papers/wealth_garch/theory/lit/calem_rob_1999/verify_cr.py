import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "CALEM AND ROB (1996), THE IMPACT OF CAPITAL-BASED REGULATION ON BANK RISK-TAKING",
    "pdf": "calem_rob_1996.pdf",
    "sha256": "022ee640da497d4f9e2f1799bdf1f5112e6c8015c11477479e1477087f7713eb",
    "pages": (1, 46),
    "offset": -1,
    "prefix": "CR",
    "fabricated": "a severely undercapitalized bank takes minimal risk in an effort to improve its capital position",
    "wrong_page": 20,
    "controls": ["CalemRob.control_bound_needs_positive_slope", "CalemRob.control_monotone_is_not_u"],
})
