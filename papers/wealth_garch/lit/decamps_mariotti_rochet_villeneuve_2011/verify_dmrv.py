import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "DECAMPS, MARIOTTI, ROCHET AND VILLENEUVE (2011), FREE CASH FLOW, ISSUANCE COSTS, AND STOCK PRICES",
    "pdf": "JF_unknown.pdf",
    "sha256": "9313eeb2e141a186cb97ce9430d3c1be5fc20023a746c233e26935410650321c",
    "pages": (1501, 1544),
    "offset": 1500,
    "prefix": "DMRV",
    "fabricated": "the issuance trigger rises with the fixed cost of issuing new shares",
    "wrong_page": 1510,
    "controls": ["DecampsEtAl.control_no_fixed_cost_flat"],
})
