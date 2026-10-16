"""Our reading of Labov (1964), the Columbia Ph.D. dissertation "The Social
Stratification of English in New York City" (University Microfilms 67-9411),
checked against the cached source. Pages are PDF pages, 1 to 646. The cache file
is named labov_1966 although the version read is the 1964 dissertation (LV-D1).
The text layer is the ABBYY OCR of the microfilm, with soft hyphens at line
breaks, so some quotations carry elisions at those points (LV-D2)."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "labov_1964",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "labov_1966.pdf"),
    "pdf_sha256": "9eb8bc5cd9315a86feddda2bd4a3da95c373e6dfa5811ba10d10dbc067137b2b",
    "text": os.path.join(CACHE, "labov_1966.txt"),
    "text_sha256": "00a77dd35686c4e309df8342fb58bb3e2fa0a5411ef74dd0959b1cf90f7e6736",
    "pages": (1, 646),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (474, "In this measure of linguistic insecurity, the upper middle class outdistances all others."),
    "controls": ["Labov1964.control_wage_order_not_stratified",
                 "Labov1964.control_posterior_eq_prior_unstratified"],
})
