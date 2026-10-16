"""Our reading of Cole, Mailath and Postlewaite, CARESS Working Paper 91-14
(revised June 1991), checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "cole_mailath_postlewaite_1991",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "cole_mailath_postlewaite_1992_wp.pdf"),
    "pdf_sha256": "48ec38df87e7841df49b897f7b6fbc7e51c3c86dffe4ea7f10de5b6530a2974d",
    "text": os.path.join(CACHE, "cole_mailath_postlewaite_1992_wp.txt"),
    "text_sha256": "bada0e267f5725ec4aec18dbc8bb1f8e29ed70883227b4bf8e5791aa43a53e74",
    "pages": (1, 36),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (7, "matching is voluntary and takes place based upon incomplete information"),
    "controls": ["ColeEtAl1991.control_zero_status_deviates",
                 "ColeEtAl1991.control_printed_value_not_mean"],
})
