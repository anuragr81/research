import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "CONSTANTINIDES AND RICHARD (1978), EXISTENCE OF OPTIMAL SIMPLE POLICIES FOR DISCOUNTED-COST INVENTORY AND CASH MANAGEMENT",
    "pdf": "constantinides_richard_1978.pdf",
    "sha256": "6194bc222dfdaf00a6162251ede6b7d74441681cd3762dd54dee814b0b8a5c36",
    "pages": (620, 636),
    "offset": 618,
    "ocr": "constantinides_richard_1978_ocr",
    "prefix": "CR",
    "fabricated": "the lower trigger d is a decreasing function of the penalty cost p",
    "wrong_page": 625,
    "controls": ["Constantinides.control_printed_factor", "Constantinides.control_gap_alone_does_not_pin"],
})
