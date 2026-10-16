"""Our reading of Crabtree, Gaddis, Holbein and Larsen (2022), checked against
the cached source. Pages are PDF pages; journal page = PDF page + 453."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "crabtree_et_al_2022",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "crabtree_2022.pdf"),
    "pdf_sha256": "5ad9c1fc980d5a6940e62cd8a6f9e3b944075d83185fb4b56def4e2862b97291",
    "text": os.path.join(CACHE, "crabtree_2022.txt"),
    "text_sha256": "128f7a8a3cfdc30b859320806811d49c89b4bd6332cb2c417c420e571cadc408",
    "pages": (1, 19),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (1, "names commonly used to signal race/ethnicity do not influence perceptions about socioeconomic status and social class"),
    "controls": ["CrabtreeEtAl2022.control_name_heterogeneity", "CrabtreeEtAl2022.control_gap_persists"],
})
