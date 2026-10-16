"""Our reading of Craig and Fryer (2017), NBER Working Paper 23811, checked
against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "craig_fryer_2017",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "complementary_bias_w23811.pdf"),
    "pdf_sha256": "86f10e79e6c918fe8d582594343cc6872a4dd2de7aa315772c9f847dae558707",
    "text": os.path.join(CACHE, "complementary_bias_w23811.txt"),
    "text_sha256": "696451d21529e487a34ae4c44d26817bda1d3562323082f22a9c63483d6ca402",
    "pages": (1, 50),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (42, "the benefit to workers from being matched to a good firm depends on their own investment decision"),
    "controls": ["CraigFryer2017.control_one_sided_zero_fails", "CraigFryer2017.control_minority_breaks_U"],
})
