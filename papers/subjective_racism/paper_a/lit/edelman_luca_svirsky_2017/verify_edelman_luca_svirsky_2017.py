"""Our reading of Edelman, Luca and Svirsky (2017), the accepted version of the
AEJ: Applied Economics article, checked against the cached source. Pages are
PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "edelman_luca_svirsky_2017",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "edelman_luca_svirsky_2017.pdf"),
    "pdf_sha256": "b28a4e16965f02c676c0bfbb4b56471d22cbdc66c47a2104f0f248736179ac77",
    "text": os.path.join(CACHE, "edelman_luca_svirsky_2017.txt"),
    "text_sha256": "bd6388cbd46370b5e156163b0489232774f97803242d02a0f9aef359778c3469",
    "pages": (1, 36),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (3, "When we restrict our analysis to hosts who have had an African-American guest in the recent past, discrimination doubles"),
    "controls": ["EdelmanEtAl2017.control_no_interaction",
                 "EdelmanEtAl2017.control_simple_yes_not_fifty",
                 "EdelmanEtAl2017.control_sure_replacement_costless"],
})
