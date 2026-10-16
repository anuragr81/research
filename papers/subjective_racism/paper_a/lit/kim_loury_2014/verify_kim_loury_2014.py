"""Our reading of Kim and Loury (2014), MPRA Paper 54950, the version of
31 March 2014, checked against the cached source. Pages are PDF pages, and
PDF page 1 is the MPRA cover."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/novelty")
run({
    "slug": "kim_loury_2014",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "mpra54950.pdf"),
    "pdf_sha256": "599ca92393150b5b90a607c2125a42ffb6638f1ea3b60740d5cdf057a64104eb",
    "text": os.path.join(CACHE, "mpra54950.txt"),
    "text_sha256": "12a1657a72b69e7dea88b6205a468b1a6a10309c58474128f3d281a28f8f36e9",
    "pages": (1, 38),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (9, "a worker receives the benefit of the doubt whatever the employer believes about the rate of human capital acquisition"),
    "controls": ["KimLoury2014.control_low_not_equilibrium_without_threshold",
                 "KimLoury2014.control_beta_strictAnti"],
})
