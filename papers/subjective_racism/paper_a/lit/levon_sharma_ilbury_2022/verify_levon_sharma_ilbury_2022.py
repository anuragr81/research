"""Our reading of Levon, Sharma and Ilbury (2022), Speaking Up. Accents and
Social Mobility, The Sutton Trust, November 2022, checked against the cached
source. Pages are PDF pages. The printed page is the PDF page minus 1."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "levon_sharma_ilbury_2022",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "sutton_trust_2022.pdf"),
    "pdf_sha256": "7e30567705ba4f5dd2dbbb9486228cc50239995021897c6c9137dd0475a1319d",
    "text": os.path.join(CACHE, "sutton_trust_2022.txt"),
    "text_sha256": "538a65f624890b03b431da99cc741ed5d380722bff5c71f979e4c6e7f67fc02a",
    "pages": (1, 40),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (36, "standard accent speakers feel much more pressure to modify their accents"),
    "controls": ["LevonEtAl2022.control_not_identified",
                 "LevonEtAl2022.control_unweighted_not_24"],
})
