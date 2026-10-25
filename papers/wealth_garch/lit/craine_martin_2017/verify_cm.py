import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "CRAINE AND MARTIN (2017), SAFE CAPITAL RATIOS FOR BANK HOLDING COMPANIES, REVISED DRAFT",
    "pdf": "craine_martin_2017.pdf",
    "sha256": "08e8a70a7332e8548e5ab881025420235e739682f621048169c4c84eae7e4879",
    "pages": (1, 22),
    "offset": 0,
    "prefix": "CM",
    "fabricated": "Book value capital ratios move a great deal in response to severe economic distress.",
    "wrong_page": 5,
    "controls": ['CraineMartin.control_scr_needs_loss_below_one', 'CraineMartin.control_ratio_not_eight'],
})
