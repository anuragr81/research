import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BOLTON, CHEN AND WANG (2011), ACCEPTED MANUSCRIPT, JOURNAL OF FINANCE",
    "pdf": "bcw_2011.pdf",
    "sha256": "8f70e72e42be16b8d0906e53a28d0e4aabb1f7eef94f256cb08a667153ac9600",
    "pages": (1, 43),
    "offset": -2,
    "prefix": "BCW",
    "fabricated": "While this double-barrier policy is new, our model provides substantial new insight",
    "wrong_page": 7,
    "controls": ["BoltonChenWang.control_sensitivity_needs_concavity",
                 "BoltonChenWang.control_ifb_needs_root_condition"],
})
