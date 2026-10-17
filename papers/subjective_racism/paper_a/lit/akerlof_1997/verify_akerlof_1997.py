"""Our reading of Akerlof (1997), Econometrica 65(5), 1005-1027, the JSTOR copy
the author supplied on 17 October 2026, checked against the cached source.
Pages are PDF pages. PDF page 1 is the JSTOR cover, and PDF page n is journal
page 1003 + n."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "akerlof_1997",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "akerlof_1997.pdf"),
    "pdf_sha256": "09c0c9d313d90ecf5511e2207d02aff7ddf0b71b66c648876f32fcbec25f021f",
    "text": os.path.join(CACHE, "akerlof_1997.txt"),
    "text_sha256": "cac16cc2585b65178810f0da12eb703b57076f95daf700e4d6f8a21eb001eaaf",
    "pages": (1, 24),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (7, "I have considered quadratic utility functions to be the general case."),
    "controls": ["Akerlof1997.control_conform_outside_zone",
                 "Akerlof1997.control_benefit_range_differs"],
})
