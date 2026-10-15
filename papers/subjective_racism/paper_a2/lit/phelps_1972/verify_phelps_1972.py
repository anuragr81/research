"""Our reading of Phelps (1972), checked against the cached source."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a")
run({
    "slug": "phelps_1972",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "phelps_1972.pdf"),
    "pdf_sha256": "ab6f48705810e1369077466e2968249ab8c30bd6cc748bdecb6fc5dcf7a877fd",
    "text": os.path.join(CACHE, "phelps_1972.txt"),
    "text_sha256": "fd143d1e006db97c6a619311785431ed716fc5c5ccc5a090d49f8e672fe810a6",
    "ocr": True,
    "pages": (659, 661),
    "first_page": 659,
    "first_index": 0,
    "fabricated": (659, "the employer revises his belief about the group after each applicant"),
    "controls": ["Phelps1972.control_no_crossing_equal_variance", "Phelps1972.control_weight_below_one"],
})
