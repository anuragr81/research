"""Our reading of Currarini, Jackson and Pin, working paper revised 29 July
2008, checked against the cached source. Pages are PDF pages, equal to the
printed pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "currarini_jackson_pin_2008",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "cjp_2009_wp.pdf"),
    "pdf_sha256": "6a46cd210f46ef743c8a1703688a5a085c205aec4403c51abe1cd7df7610dfb9",
    "text": os.path.join(CACHE, "cjp_2009_wp.txt"),
    "text_sha256": "8c25072024abde4c9412fbf4824e032e6ae9179096f7e824f65a3019ea1830b0",
    "pages": (1, 46),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (19, "If the matching process is unbiased, then for any preferences and any steady state equilibrium, every type can be homophilous at once"),
    "controls": ["CurrariniEtAl2008.control_type_sensitive_numbers_differ",
                 "CurrariniEtAl2008.control_biased_matching_all_homophilous"],
})
