"""Our reading of Coate and Loury (1993), checked against the cached source."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a")
run({
    "slug": "coate_loury_1993",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "coate_loury_1993.pdf"),
    "pdf_sha256": "a3389b46de62a8cd41284f9a2b150fa8c0e6e691411fb37b91c4f50782259405",
    "text": os.path.join(CACHE, "coate_loury_1993.txt"),
    "text_sha256": "857009c7aca9f88b4790000d68dcaf17e3e5d70d212b7f6bd1b79f7da98ed251",
    "pages": (1220, 1240),
    "first_page": 1220,
    "first_index": 1,
    "fabricated": (1226, "every solution of (3) is locally stable under the obvious adjustment process"),
    "controls": ["CoateLoury1993.control_start_at_colorblind", "CoateLoury1993.control_half_needed"],
})
