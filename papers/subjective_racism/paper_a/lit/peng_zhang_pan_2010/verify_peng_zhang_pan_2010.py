"""Our reading of Peng, Zhang and Pan (2010), IJUFKS 18(5), pp. 539-564,
checked against the cached source. Pages are PDF pages; PDF page n is journal
page 538 + n."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "peng_zhang_pan_2010",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "peng_2010.pdf"),
    "pdf_sha256": "ecda7d4b87fd77974ec999be69dd8aaf5d98e68e1138e719ef8e0bdb3e7149b0",
    "text": os.path.join(CACHE, "peng_2010.txt"),
    "text_sha256": "319cdf2d9fae69ea5ac42a8747ef76e642070f548448510a576f657087bab2a5",
    "pages": (1, 26),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (8, "a soft evidence cannot be converted into a virtual evidence, even for a single update"),
    "controls": ["PengEtAl2010.control_two_virtual_not_soft",
                 "PengEtAl2010.control_dominance_needed"],
})
