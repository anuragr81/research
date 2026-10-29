import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "JUSELIUS, PONTE MARQUES AND TARASHEV (2025), BANKS' REGULATORY RISK TOLERANCE, VERSION OF 20 OCTOBER 2025",
    "pdf": "sbfc2025_p170.pdf",
    "sha256": "747c48c3030b26c6b222c98621ea89565f4edfb45afb5ad196ba1fa62d5220de",
    "pages": (0, 29),
    "offset": -1,
    "prefix": "JPT",
    "fabricated": "The implication is that an eight-standard-deviation shock is needed to wipe out the MB target on impact in both regimes.",
    "wrong_page": 5,
    "controls": ['JuseliusEtAl.control_post_multiple_not_three', 'JuseliusEtAl.control_us_lower_volatility'],
})
