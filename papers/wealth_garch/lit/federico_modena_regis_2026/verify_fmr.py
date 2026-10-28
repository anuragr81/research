import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FEDERICO, MODENA AND REGIS (2026), COORDINATING BANK DIVIDEND AND CAPITAL REGULATION",
    "pdf": "federico_modena_regis_2026.pdf",
    "sha256": "e940148247a33976f4d65f323a0cdb41b18e443dc0844e5f0ea752dbf55253c3",
    "pages": (1, 64),
    "offset": -2,
    "prefix": "FMR",
    "fabricated": "the bank pays a running penalty that grows with the shortfall below the capital requirement",
    "wrong_page": 9,
    "controls": ["FedericoEtAl.control_no_impatience_cutoff_is_one"],
})
