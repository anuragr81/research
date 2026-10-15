"""Our reading of Jeffrey (1983), chapter 11 and the preface, checked against
the cached source. Pages are PDF pages of the full scan."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a")
PAGE_MAP = {8: 0}
PAGE_MAP.update({p: p - 174 for p in range(175, 195)})
run({
    "slug": "jeffrey_1983",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "jeffrey_logic_of_decision.pdf"),
    "pdf_sha256": "6fbdb1d17086ac1cc5732d7c4b8401f5711b0b0298340f22ae0c228caa3fb0e1",
    "text": os.path.join(CACHE, "jeffrey_1983_read.txt"),
    "text_sha256": "b0ecda7d1d18a37961a8594a0234476be685df8659f8ba9689f6b7c8217cccb5",
    "ocr": True,
    "pages": (8, 194),
    "page_map": PAGE_MAP,
    "fabricated": (183, "two applications of (11-2) always yield the same belief function in either order"),
    "controls": ["Jeffrey1983.control_not_commutative", "Jeffrey1983.control_rigidity_needed"],
})
