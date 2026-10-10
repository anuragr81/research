import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BARBERIS, HUANG AND SANTOS (2001), QJE 116(1):1-53",
    "pdf": "bhs_qje2001.pdf",
    "sha256": "271295d5aacdedccda5547042089d62c29a5d0ec8dab45d120b27b58d121ed3f",
    "pages": (1, 53),
    "offset": 0,
    "prefix": "QJE",
    "fabricated": "the volatility of log returns in this model rises with the degree of loss aversion",
    "wrong_page": 46,
    "controls": ["BarberisHuangSantos.control_dispersion_depends_on_state",
                 "BarberisHuangSantos.control_concavity_needs_lam_ge_one"],
})
