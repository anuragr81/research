import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "MILNE AND WHALLEY (2001), BANK CAPITAL REGULATION AND INCENTIVES FOR RISK-TAKING",
    "pdf": "milne_whalley_2001.pdf",
    "sha256": "5f57f39e5432f23e56d01cdbbd59124f7d827ed22df2ee31702427657d37c7d4",
    "pages": (1, 51),
    "offset": -1,
    "prefix": "MW",
    "fabricated": "Forward looking banks with sufficient franchise value hold no capital in excess of the regulatory minimum.",
    "wrong_page": 3,
    "controls": ["MilneWhalley.control_buffer_needs_square", "MilneWhalley.control_shift_needs_same_d"],
})
