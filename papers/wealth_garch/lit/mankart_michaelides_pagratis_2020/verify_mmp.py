import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "MANKART, MICHAELIDES AND PAGRATIS (2020), NBER SI DRAFT OF MAY 2016",
    "pdf": "mankart_michaelides_pagratis_2016.pdf",
    "sha256": "3a0e6caf7edbfe7a53b4a0fb2a14b0893b3d4f8d8d20a60e77008bbbd5cd8cc0",
    "pages": (1, 57),
    "offset": -2,
    "prefix": "MMP",
    "fabricated": "Banks raise equity capital both internally through retained earnings and by seasoned equity issuance",
    "wrong_page": 10,
    "controls": ["MankartMichaelidesPagratis.control_min_equity_needs_limit_above_one",
                 "MankartMichaelidesPagratis.control_linear_objective_not_strictly_concave"],
})
