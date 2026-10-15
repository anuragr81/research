"""Our reading of Arrow (1971), the Princeton working paper, checked
against the cached source. Pages are PDF pages of the scan."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a")
run({
    "slug": "arrow_1971",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "arrow_1971.pdf"),
    "pdf_sha256": "5fb8d80d19cdebc9b2a64e604371f8b5740bc67830922b32c426725143150473",
    "text": os.path.join(CACHE, "arrow_1971.txt"),
    "text_sha256": "83c6fc44f0508a14d65d33daca77105e3c09f9ebc47cedb0498af92cefa9889f",
    "ocr": True,
    "pages": (1, 37),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (33, "employers revise their estimates downward each time a qualified worker is observed"),
    "controls": ["Arrow1971.control_equal_beliefs", "Arrow1971.control_no_personnel_cost"],
})
