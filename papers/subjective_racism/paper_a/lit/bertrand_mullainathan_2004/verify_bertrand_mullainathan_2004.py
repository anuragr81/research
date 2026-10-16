"""Our reading of Bertrand and Mullainathan (2004), checked against the cached
source. Pages are journal pages, 991 to 1013, which are PDF pages 1 to 23. The
publisher's cited-by list on PDF pages 24 to 162 is outside the range."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "bertrand_mullainathan_2004",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "bertrand_mullainathan_2004.pdf"),
    "pdf_sha256": "796b4bd2fe98000bb7f1d522f15145b47028f7d84030278ab9ed3ee455a18281",
    "text": os.path.join(CACHE, "bertrand_mullainathan_2004.txt"),
    "text_sha256": "98b570f29ef3b5dd077281e68f4edea664dfc0051234f6ff80431e8fff5805ac",
    "pages": (991, 1013),
    "first_page": 991,
    "first_index": 0,
    "fabricated": (991, "We also find strong evidence that employers are inferring social class from the names."),
    "controls": ["BertrandMullainathan2004.control_group_sufficiency_zero",
                 "BertrandMullainathan2004.control_unequal_weights_gap_moves"],
})
