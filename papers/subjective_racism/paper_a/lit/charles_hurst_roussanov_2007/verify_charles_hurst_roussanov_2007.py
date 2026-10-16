"""Our reading of Charles, Hurst and Roussanov (2007), NBER Working Paper 13392,
checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "charles_hurst_roussanov_2007",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "charles_hurst_roussanov_2009.pdf"),
    "pdf_sha256": "f386f956629c4d2379e00c4c8644992976106b2cfa39c6b2d6cf8fd3c6239e62",
    "text": os.path.join(CACHE, "charles_hurst_roussanov_2009.txt"),
    "text_sha256": "21630c72500a7805f65a3028cdcc91d7edf56fa29ff949c456b1308bd28444f1",
    "pages": (1, 65),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (22, "outsiders infer a person's income from the person's race alone, using the variance of group income"),
    "controls": ["CharlesEtAl2007.control_status_gain_rising", "CharlesEtAl2007.control_soc_needed"],
})
