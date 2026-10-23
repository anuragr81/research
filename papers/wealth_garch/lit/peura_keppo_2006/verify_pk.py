import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "PEURA AND KEPPO (2006), JOURNAL OF BUSINESS 79(4)",
    "pdf": "peura_keppo_2006_jstor.pdf",
    "sha256": "6866d418083a098701a0d6fc57ad80ac11a30009cd452995621d6846c1fe98a6",
    "pages": (2163, 2201),
    "offset": 2161,
    "prefix": "PK",
    "fabricated": "Our main modeling innovation is to impose a fixed cost on capital issues.",
    "wrong_page": 2170,
    "controls": ["PeuraKeppo.control_dplus_needs_positive_rho",
                 "PeuraKeppo.control_paste_needs_distinct_roots",
                 "PeuraKeppo.control_delay_lowers_trigger_at_high_K"],
})
