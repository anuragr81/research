"""Our reading of Grogger, "Speech and Wages", working paper Version 8 of
2 February 2018, checked against the cached source. Pages are PDF pages. This
is not the Grogger (2011) article."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "grogger_2018",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "grogger_speech_wp.pdf"),
    "pdf_sha256": "c0909b7b986647e698392bd400b5dfbcde84676834fe080b4c380fa3b8712994",
    "text": os.path.join(CACHE, "grogger_speech_wp.txt"),
    "text_sha256": "7d34ca33252b1c6f230ae26e23830392683e207be87b11ba0dcbda9499830c97",
    "pages": (1, 64),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (25, "the speech-related wage gap among blacks is fully explained by AFQT scores and family background"),
    "controls": ["Grogger2018.control_no_sorting", "Grogger2018.control_doubling_needed"],
})
