"""Our reading of Mill and Stein (2012), the working paper of December 2012,
checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "mill_stein_2012",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "mill_stein_2012.pdf"),
    "pdf_sha256": "19df2e69f6b9c44d07a45c07853fb66cf4609390f64aba6412b9c33e6181bee2",
    "text": os.path.join(CACHE, "mill_stein_2012.txt"),
    "text_sha256": "63a7631a6129ac102fbe699fcdd8a25c05774c9b205ee4be8e1ca40e7948da61",
    "pages": (1, 60),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (37, "we see that passers were negatively selected on education"),
    "controls": ["MillStein2012.control_family_effect", "MillStein2012.control_equal_slopes"],
})
