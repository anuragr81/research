import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BARON (2020), ACCEPTED MANUSCRIPT OF 13 JANUARY 2020 WITH ONLINE APPENDIX",
    "pdf": "baron_2020.pdf",
    "sha256": "5052f9fc20402a9e208b7aced7897504341233b0b0f7addd47a36e98c63a0891",
    "pages": (1, 127),
    "offset": 0,
    "prefix": "BAR",
    "fabricated": "This paper finds that for large U.S. commercial banks over the period 1980\u20132012, banks raise and retain more common equity during credit expansions",
    "wrong_page": 30,
    "controls": ["Baron.control_zero_denominator_leaves_a8_unsolved",
                 "Baron.control_landesbank_coefficients_not_swapped"],
})
