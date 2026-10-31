import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "MILNE AND ROBERTSON (1996), READ AS THE DRAFT OF FEBRUARY 1994",
    "pdf": "milne_robertson_1996.pdf",
    "sha256": "aa1907d5b12982714e944e0a18964d20e4796661ed5b26dbef2884b8895c8840",
    "pages": (1, 40),
    "offset": -2,
    "prefix": "MR",
    "fabricated": "This implies that the firm can freely raise new equity finance.",
    "wrong_page": 20,
    "controls": ["MilneRobertson.control_sign_needs_positive_ratio", "MilneRobertson.control_dividend_above_barrier"],
})
