"""Our reading of Corneo and Jeanne (1994), University of Bonn Discussion
Paper A-461, checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "corneo_jeanne_1994",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "corneo_jeanne_1997_dp.pdf"),
    "pdf_sha256": "2f315ec019f7f38f17f3a545f6a94892a25509edd3c31da41af26b2145680c57",
    "text": os.path.join(CACHE, "corneo_jeanne_1997_dp.txt"),
    "text_sha256": "fe244470b27547c19369e717c11e49a0d50bfd294437bf82c8c0d35c96d38c8d",
    "pages": (1, 24),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (6, "Thus the message contained in conspicuous consumption is interpreted differently by rich and poor spectators."),
    "controls": ["CorneoJeanne1994.control_conformist_price_rises", "CorneoJeanne1994.control_rising_rank_utility"],
})
