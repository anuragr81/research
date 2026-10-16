"""Our reading of Hitsch, Hortaçsu and Ariely (2006), MIT Sloan Working Paper
4603-06, checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "hitsch_hortacsu_ariely_2006",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "hitsch_2006.pdf"),
    "pdf_sha256": "f0def83bd731e103da024dd1844707a7d52b5e33abb743ba83f8cc28c5bbf40f",
    "text": os.path.join(CACHE, "hitsch_2006.txt"),
    "text_sha256": "4698b2a0bad85a255ab4e08e2c7191e8181e6d9ac064fd94f65e029a89f6e294",
    "pages": (1, 63),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (24, "Note that these results do not control for other observable user attributes, such as income and education."),
    "controls": ["HitschEtAl2006.control_identified_quadratic", "HitschEtAl2006.control_distance_terms_break_assumption1"],
})
