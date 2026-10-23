import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "DAI, HUANG AND KEPPO (2019), READ AS CHAPTER 2 OF arXiv:2107.02242v1",
    "pdf": "dai_huang_keppo_2019_thesis.pdf",
    "sha256": "985a281d96915a841b984e137c1ecdbed0cdf2dd9358c2a2ac84d50d5c73ae50",
    "pages": (1, 167),
    "offset": 0,
    "prefix": "DHK",
    "fabricated": "has been published on Journal of Financial Economics",
    "wrong_page": 76,
    "controls": ["DaiHuangKeppo.control_proportional_columns_singular"],
})
