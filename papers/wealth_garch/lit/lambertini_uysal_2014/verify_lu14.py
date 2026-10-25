import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LAMBERTINI AND UYSAL (2014), MACROECONOMIC IMPLICATIONS OF BANK CAPITAL REQUIREMENTS",
    "pdf": "lambertini_uysal_2014.pdf",
    "sha256": "25f001a4e84022a8d64f61d77451a8b2bd65d63f760ef05cfc0e71ed7fd676a0",
    "pages": (1, 47),
    "offset": 0,
    "prefix": "LU14",
    "fabricated": "the bank pays a fixed cost each time it issues new shares",
    "wrong_page": 5,
    "controls": ["LambertiniUysal.control_logTerm_zero_at_requirement"],
})
