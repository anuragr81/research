"""Our reading of Aigner and Cain (1977), checked against the cached source.
Pages are PDF pages of the JSTOR scan; PDF page n is journal page 173 + n."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "aigner_cain_1977",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "aigner_cain_1977.pdf"),
    "pdf_sha256": "41b43241bbd2104ad02121619ec0d7fe6c8081ddf4a33112aee65051bb650a74",
    "text": os.path.join(CACHE, "aigner_cain_1977.txt"),
    "text_sha256": "4a23b8083e5c61e09e85033f822760c89c6e2d1fe125076c550d84c3dba5eea5",
    "pages": (1, 14),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (8, "a risk-neutral employer will attempt to hire from the group with the smaller conditional variance"),
    "controls": ["AignerCain1977.control_risk_neutral", "AignerCain1977.control_no_spread"],
})
