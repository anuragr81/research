"""Our reading of Hanson and Hawley (2011), the Georgia State University
Experimental Economics Center working paper 2011-05, checked against the cached
source. Pages are PDF pages 1 to 58. PDF page 1 is the repository's cover
sheet."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "hanson_hawley_2011",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "hanson_hawley_2011_wp.pdf"),
    "pdf_sha256": "02e1f3394f4d8d8c9bcb841296cc0124e97757fcc8dd7890c89897d966d289c2",
    "text": os.path.join(CACHE, "hanson_hawley_2011_wp.txt"),
    "text_sha256": "3b8d71dfe944141a8527ab72cf186c5d084dc96bfbf4c0bce26d616c494d62df",
    "pages": (1, 58),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (2, "Generally, discrimination occurs against African American names; however, when the content of the e-mail messages insinuates home-seekers with high social class, discrimination is unchanged."),
    "controls": ["HansonHawley2011.control_pooled_rates_differ",
                 "HansonHawley2011.control_partial_signal_gap",
                 "HansonHawley2011.control_threshold_between"],
})
