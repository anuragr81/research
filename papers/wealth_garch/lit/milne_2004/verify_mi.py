import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "MILNE (2004), THE INVENTORY PERSPECTIVE ON BANK CAPITAL",
    "pdf": "milne_2004.pdf",
    "sha256": "8ee31b60443a236414c49d43299c3d75e547925a8b62763cddf21fff8dc5a8d6",
    "pages": (1, 43),
    "offset": 0,
    "prefix": "MI",
    "fabricated": "the bank pays a running cost that grows with the shortfall below the regulatory minimum",
    "wrong_page": 5,
    "controls": ["Milne2004.control_free_issue_no_loss"],
})
