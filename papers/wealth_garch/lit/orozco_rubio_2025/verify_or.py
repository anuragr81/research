import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "OROZCO AND RUBIO (2025), REGULATORY CAPITAL MANAGEMENT TO EXCEED THRESHOLDS, JMCB 57(6)",
    "pdf": "orozco_rubio_2025.pdf",
    "sha256": "dfafa742e5bc1cf61ebc88d86f10d4af6c24d30da842fba7ce59d2b3bc13a284",
    "pages": (1421, 1464),
    "offset": 1420,
    "prefix": "OR",
    "fabricated": "We document no discontinuity at the 10% regulatory capital threshold, where banks receive benefits for exceeding it.",
    "wrong_page": 1430,
    "controls": ["OrozcoRubio.control_floor_needs_buffer", "OrozcoRubio.control_margin_not_buffer"],
})
