import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "VO (2009), OPTIMALITY OF PROMPT CORRECTIVE ACTION IN A CONTINUOUS-TIME MODEL WITH RECAPITALIZATION POSSIBILITY",
    "pdf": "vo_2009.pdf",
    "sha256": "27d592a3c017bc3ba5e98090c87c5ddc5aaa1786d7bf2c050695852fccf2e207",
    "pages": (1, 24),
    "offset": -3,
    "prefix": "VO",
    "fabricated": "The banker pays a fixed cost each time capital is injected into the bank.",
    "wrong_page": 5,
    "controls": ["Vo.control_slopes_merge_at_zero"],
})
