"""Our reading of Laouénan and Rathelot (2022), AEJ: Applied Economics 14(1),
checked against the cached source. Pages are PDF pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "laouenan_rathelot_2022",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "laouenan_rathelot_2022.pdf"),
    "pdf_sha256": "4b567652b5883dad8732dd3bab05b8870a94b6f3f19c8ff5d8ac1122a5c77589",
    "text": os.path.join(CACHE, "laouenan_rathelot_2022.txt"),
    "text_sha256": "3c753d24ab5ca78528967f8693b04bb8856bc685630c097aacf754a170ca42d1",
    "pages": (1, 26),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (2, "If the price gap is due to taste-based discrimination, the price gap should decrease with the number of reviews and tend to zero"),
    "controls": ["LaouenanRathelot2022.control_taste_gap_survives",
                 "LaouenanRathelot2022.control_equal_means_needed",
                 "LaouenanRathelot2022.control_printed_p0"],
})
