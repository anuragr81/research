import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "GLOVER, JOHNSON, EVATT AND CHENG (2023), CAPITAL IDEAS: OPTIMAL CAPITAL ACCUMULATION STRATEGIES FOR A BANK AND ITS REGULATOR",
    "pdf": "glover_johnson_evatt_cheng_2023.pdf",
    "sha256": "18dce30123a04a5259355e3f0508f1a464fa3a334e8723f6b137bd7522d83b73",
    "pages": (1, 41),
    "offset": 0,
    "prefix": "GJEC",
    "fabricated": "the bank pays a fixed cost each time it issues new equity",
    "wrong_page": 12,
    "controls": ["GloverEtAl.control_free_issuance_no_breach_cost"],
})
