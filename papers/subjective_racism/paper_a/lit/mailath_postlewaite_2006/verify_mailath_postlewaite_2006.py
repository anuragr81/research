"""Our reading of Mailath and Postlewaite, "Social Assets", PIER Working Paper
06-003 (January 5, 2006), checked against the cached source. Pages are PDF
pages of the cached file; the printed page is the PDF page minus 1."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "mailath_postlewaite_2006",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "mailath_postlewaite_2006_wp06.pdf"),
    "pdf_sha256": "3ec1560c4956c2c0c2eb778a16236711812e87ca26a0e0aff96814ab3f7da7d9",
    "text": os.path.join(CACHE, "mailath_postlewaite_2006_wp06.txt"),
    "text_sha256": "8e71e59236fc19d0a2af2687942fca6e8b173cef5ca9be87645eea96ac760827",
    "pages": (1, 41),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (30, "agents cannot observe a partner’s income and infer it from the attribute"),
    "controls": ["MailathPostlewaite2006.control_no_altruism",
                 "MailathPostlewaite2006.control_risk_neutral"],
})
