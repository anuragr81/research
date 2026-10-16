"""Our reading of Chetty et al. (2022), "Social capital II: determinants of
economic connectedness", Nature 608, checked against the cached source. Pages
are PDF pages. Quotations are checked against a reading-order extraction of
the pinned PDF (CB-D1)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "chetty_et_al_2022b",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "chetty_2022_sc2_nature.pdf"),
    "pdf_sha256": "930d3b6496712168305177e1c3e2a21de65cd32736d3c68a50a90439b78b4ee9",
    "text": os.path.join(CACHE, "chetty_2022_sc2_nature_read.txt"),
    "text_sha256": "108f6c6257f4a197073986ae46a9d17b6d03dc3ec90908a52e1780a6bae98685",
    "pages": (1, 31),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (9, "Friending bias is lower in more racially diverse schools"),
    "controls": ["ChettyEtAl2022b.control_order_matters", "ChettyEtAl2022b.control_tcec_printed",
                 "ChettyEtAl2022b.control_bias_zero_iff_equal_friends"],
})
