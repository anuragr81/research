"""Our reading of Moav and Neeman (2012), checked against the cached source.
Pages are PDF pages; the journal page is the PDF page plus 932."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "moav_neeman_2012",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "moav_neeman_2012.pdf"),
    "pdf_sha256": "fb6e40bd940465af8607d1bf6d8964a834b2a4ba79d418fc34546621a5e364c0",
    "text": os.path.join(CACHE, "moav_neeman_2012.txt"),
    "text_sha256": "b915559b7975cd3b5b8e2cf298c129e82ad586f238f5362ab25b5e7b892e142a",
    "pages": (1, 24),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (15, "Thus, the variance of income given human capital determines the equilibrium expenditure on conspicuous consumption."),
    "controls": ["MoavNeeman2012.control_prop2_constant_floor", "MoavNeeman2012.control_unobserved_constant"],
})
