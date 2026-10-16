"""Our reading of Eeckhout (2006), checked against the cached source. Pages are
PDF pages; PDF page n is journal page n + 30."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "eeckhout_2006",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "eeckhout_2006.pdf"),
    "pdf_sha256": "3ea91484804c45fe713f7e142ee2b188c7babb810ff05ddca01b3466f70380bb",
    "text": os.path.join(CACHE, "eeckhout_2006.txt"),
    "text_sha256": "7eb8e21f6c7953ac70362b4126423d5ee4985d2c9c6c7dc3fc21ddca545a9095",
    "pages": (1, 23),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (4, "colour is correlated with the unobserved quality of a partner"),
    "controls": ["Eeckhout2006.control_prop3_equal_shares",
                 "Eeckhout2006.control_equal_shares_no_loss"],
})
