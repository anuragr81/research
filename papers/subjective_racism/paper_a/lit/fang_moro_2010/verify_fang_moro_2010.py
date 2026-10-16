"""Our reading of Fang and Moro (2010), NBER working paper 15860, checked
against the cached source. Pages are PDF pages; PDF page n is printed page
n - 4 from PDF page 5."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "fang_moro_2010",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "fang_moro_2011.pdf"),
    "pdf_sha256": "30fc4e8fcc04b9970f705ffafab2f97e7462a78003141e336d971a04db98e31a",
    "text": os.path.join(CACHE, "fang_moro_2011.txt"),
    "text_sha256": "ce41bc901fc351e996b215b63cf181cd385f8c7853f061298896479beae6a09e",
    "pages": (1, 78),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (7, "statistical discrimination has been applied mostly to social contact between strangers"),
    "controls": ["FangMoro2010.control_condVar_zero_spread",
                 "FangMoro2010.control_survey_pooled_variance"],
})
