"""Our reading of Rothschild and White (1993), the NBER chapter "The University
in the Marketplace: Some Insights and Some Puzzles", checked against the cached
source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "rothschild_white_1993",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "rothschild_white_1993_chapter.pdf"),
    "pdf_sha256": "7e00c89408f05f48c2b2e18b7cb3517efd474c696fc55e5a4525938c21bcf6aa",
    "text": os.path.join(CACHE, "rothschild_white_1993_chapter.txt"),
    "text_sha256": "f54969e6479225243a8149cbe804167c9cce4e314b46de9f71ba48a06dc88a0b",
    "pages": (1, 33),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (8, "Allocations differ in efficiency even when there is no synergy."),
    "controls": ["RothschildWhite1993.control_no_synergy",
                 "RothschildWhite1993.control_perfect_correlation",
                 "RothschildWhite1993.control_personalised_separates"],
})
