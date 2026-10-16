"""Our reading of Fryer and Jackson (2008), checked against the cached source.
Pages are PDF pages of the bepress file. Journal page = PDF page - 2."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "fryer_jackson_2008",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "fryer_jackson_2008.pdf"),
    "pdf_sha256": "eac29b938f859f35921aa4e3f0046e9aabdd9b24790e999531638c6868d89af2",
    "text": os.path.join(CACHE, "fryer_jackson_2008.txt"),
    "text_sha256": "cdf5ab0d7f9d7d636e4a6f189bc6d2afb0cb9d0c5efb030c851c719408781d93",
    "pages": (1, 44),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (32, "In our model, discriminating behavior disappears once skills are observable"),
    "controls": ["FryerJackson2008.control_no_minority", "FryerJackson2008.control_two_categories"],
})
