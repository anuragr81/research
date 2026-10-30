import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LOKKA AND ZERVOS (2008), PREPRINT DATED 14 JULY 2005",
    "pdf": "lokka_zervos_2008.pdf",
    "sha256": "2385581e05465444599f8d59bed5cfb6fded7522b0e8b92d7bce246f67083e45",
    "pages": (1, 14),
    "offset": 0,
    "prefix": "LZ",
    "fabricated": "Assuming fixed issuance of equity costs, we derive closed form solutions and we completely characterise the optimal strategies.",
    "wrong_page": 5,
    "controls": ["LokkaZervos.control_concave_needs_below_barrier",
                 "LokkaZervos.control_coeffs_need_distinct_roots"],
})
