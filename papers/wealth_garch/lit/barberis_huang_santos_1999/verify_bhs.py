import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BARBERIS, HUANG AND SANTOS, NBER WP 7220 (1999)",
    "pdf": "bhs_w7220.pdf",
    "sha256": "dca360482ae82da568a5b366ad699910258fa3cb3e940f5a4f4d6f0bf3cd6c4b",
    "pages": (1, 50),
    "offset": 0,
    "prefix": "BHS",
    "fabricated": "the volatility of log returns in this model rises with the level of loss aversion",
    "wrong_page": 17,
    "controls": ["BarberisHuangSantos.control_dispersion_depends_on_state",
                 "BarberisHuangSantos.control_concavity_needs_lam_ge_one"],
})
