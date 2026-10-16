"""Our reading of Kraus, Park and Tan (2017), the accepted manuscript, checked
against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "kraus_park_tan_2017",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "kraus_park_tan_2017.pdf"),
    "pdf_sha256": "49c866cc610311a3c6d7b774f1c400a6692c5b5bbd03b256a3623d60cd4cb853",
    "text": os.path.join(CACHE, "kraus_park_tan_2017.txt"),
    "text_sha256": "4568bdd45d6a7febcad69ee39f1bf3c3a494f2b37d789ce06625fc13d6bf2148",
    "pages": (1, 45),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (8, "these associations disappeared after accounting for the race and gender of participants"),
    "controls": ["KrausEtAl2017.control_nonuniform_categories", "KrausEtAl2017.control_vertical_common_ranking"],
})
