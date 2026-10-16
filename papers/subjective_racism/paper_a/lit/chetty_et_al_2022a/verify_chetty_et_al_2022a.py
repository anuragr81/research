"""Our reading of Chetty et al. (2022), "Social capital I: measurement and
associations with economic mobility", Nature 608, checked against the cached
source. Pages are PDF pages. Quotations are checked against a reading-order
extraction of the pinned PDF (CA-D1)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "chetty_et_al_2022a",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "chetty_2022_sc1.pdf"),
    "pdf_sha256": "16920674a6efccb6c7f95457e757c78da53a01d6cd5f38be8462996440dd927a",
    "text": os.path.join(CACHE, "chetty_2022_sc1_read.txt"),
    "text_sha256": "d374a34c4ce0c43ebd3cc1806edfa171e4dc2eeba38df382db16fb7c47967f16",
    "pages": (1, 30),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (3, "Connectedness is higher between deciles that are further apart."),
    "controls": ["ChettyEtAl2022a.control_equal_friends", "ChettyEtAl2022a.control_EC_mean_of_ratios"],
})
