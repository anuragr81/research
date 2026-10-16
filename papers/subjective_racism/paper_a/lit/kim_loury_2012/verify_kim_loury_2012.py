"""Our reading of Kim and Loury (2012), KDI Journal of Economic Policy 34(2),
pp. 1-15, checked against the cached source. Pages are PDF pages of the whole
issue, in which the article is PDF pages 4 to 18 (journal page = PDF page - 3)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/novelty")
run({
    "slug": "kim_loury_2012",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "kl_kdi.pdf"),
    "pdf_sha256": "b97e4d37ac881db3b0faec4d3cd7ff1b3291ee198e8eeebe6945833688a897e1",
    "text": os.path.join(CACHE, "kl_kdi.txt"),
    "text_sha256": "d4b3f712ef5853bcff4e3bbe6b459203260fc71181805d770ca166ac9f07a3b7",
    "pages": (4, 18),
    "first_page": 4,
    "first_index": 3,
    "fabricated": (14, "The disproportionately less talented workers, whose human capital investment costs (c) are relatively higher, choose the 'affect' that corresponds to the greater return to human capital investment"),
    "controls": ["KimLoury2012.control_reputation_without_return",
                 "KimLoury2012.control_no_selection_equal_return"],
})
