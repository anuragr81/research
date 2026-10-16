"""Our reading of Dahis, Nix and Qian (2019), NBER Working Paper 26465 in its
June 2020 revision, checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "dahis_nix_qian_2019",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "dahis_nix_qian_2019.pdf"),
    "pdf_sha256": "11cca560e4b083adbe336a8c053aeab1dd2eb870e5a8bd5a2551f69bec0bec9a",
    "text": os.path.join(CACHE, "dahis_nix_qian_2019.txt"),
    "text_sha256": "fc187e4fbfcdf29a0a69f3ce32565435cfefd6a0754aa36e433cc7a739700fbc",
    "pages": (1, 79),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (34, "a man with a higher occupational income score is less likely to pass for white"),
    "controls": ["DahisEtAl2019.control_backward_needed", "DahisEtAl2019.control_no_selection"],
})
