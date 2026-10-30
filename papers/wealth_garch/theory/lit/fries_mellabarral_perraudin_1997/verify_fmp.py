import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FRIES, MELLA-BARRAL AND PERRAUDIN (1997), OPTIMAL BANK REORGANIZATION",
    "pdf": "fries_mellabarral_perraudin_1997.pdf",
    "sha256": "849dd28ecd552942eab3d289b1c0e388d951d3ca51cc2cc5488a05cac5f51b76",
    "pages": (441, 468),
    "offset": 440,
    "prefix": "FMP",
    "fabricated": "the willingness of equity-holders to inject capital never constrains banking policy",
    "wrong_page": 460,
    "controls": ["FriesEtAl.control_factor_needs_negative_root", "FriesEtAl.control_gain_needs_noise"],
})
