"""Our reading of Kim and Loury (2009), MPRA Paper No. 18765, checked against
the cached source. Pages are PDF pages, page 1 being the MPRA cover."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/novelty")
run({
    "slug": "kim_loury_2009",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "mpra18765.pdf"),
    "pdf_sha256": "b898397bf690951d4305d2ade4f0c8d351aa82ffb0ecd4d0eeac7e834676de27",
    "text": os.path.join(CACHE, "mpra18765.txt"),
    "text_sha256": "11a605f0875f13d5a4133664c4ab38345a6effab6cf20f2b4a245585023cbd5d",
    "pages": (1, 42),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (25, "if the market can learn the true characteristics of workers very quickly, "
                       "the group disparity caused by the difference of initial group reputations will persist"),
    "controls": ["KimLoury2009.control_no_low_state_if_cutoff_low",
                 "KimLoury2009.control_equal_informativeness",
                 "KimLoury2009.control_employer_ranking_needs_threshold"],
})
