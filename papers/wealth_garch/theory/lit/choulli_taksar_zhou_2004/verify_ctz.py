import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "CHOULLI, TAKSAR AND ZHOU (2004), ARXIV REPRINT math/0503541v1",
    "pdf": "choulli_taksar_zhou_math0503541.pdf",
    "sha256": "d5c8ecb5ac8238245de811cb8fb2bf4acdeee7dbc59cd7780066a417d2ec4cbf",
    "pages": (1, 29),
    "offset": 0,
    "prefix": "CTZ",
    "fabricated": "some business activities are never redundant, that is, under the optimal policy they will always be used in every scenario.",
    "wrong_page": 5,
    "controls": ["ChoulliTaksarZhou.control_printed_exponent_fails",
                 "ChoulliTaksarZhou.control_printed_ktilde_fails"],
})
