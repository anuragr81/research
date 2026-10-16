"""Our reading of Levon, Sharma, Watt and Perry (2020), the Accent Bias in
Britain project report of February 2020, checked against the cached source.
Pages are PDF pages. The printed page is the PDF page minus 2."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "levon_et_al_2020",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "accent_bias_britain_2020.pdf"),
    "pdf_sha256": "a2bb6ae755fe0ad7c91591497d6bd4618109e8a1dfb8d45e55520925520e4941",
    "text": os.path.join(CACHE, "accent_bias_britain_2020.txt"),
    "text_sha256": "27bf7e07bad459bdf34f3ef6b60a314b4bd40707127bac405be3980b8541e48c",
    "pages": (1, 28),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (22, "lawyers and law firm recruiters consistently rated responses in Received Pronunciation higher, regardless of quality"),
    "controls": ["LevonEtAl2020.control_partial_signal",
                 "LevonEtAl2020.control_signal_can_widen_gap",
                 "LevonEtAl2020.control_additive_accent_effect"],
})
