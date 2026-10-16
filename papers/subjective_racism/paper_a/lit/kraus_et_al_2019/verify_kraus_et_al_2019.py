"""Our reading of Kraus, Torrez, Park and Ghayebi (2019), checked against the
cached PubMed Central HTML text. The text has no page numbers, so it is one
page and every quotation cites page 1."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "kraus_et_al_2019",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "kraus_2019.html"),
    "pdf_sha256": "c9470e6ea5b65074ad8517ec9a85cef10c2023040eebb8491170e48a074d073f",
    "text": os.path.join(CACHE, "kraus_2019.txt"),
    "text_sha256": "be6760068856f9d44deb0044f201de4fc751b5cec9bc869ca208eea1b4ec9967",
    "pages": (1, 1),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (1, "social class judgments were significantly more accurate relative to speaker race"),
    "controls": ["KrausEtAl2019.control_majority_beats_half", "KrausEtAl2019.control_no_gap_chance"],
})
