import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "BONAPARTE, COOPER AND ZHU (2011), CONSUMPTION SMOOTHING AND PORTFOLIO REBALANCING",
    "pdf": "bonaparte_cooper_zhu_2011.pdf",
    "sha256": "9079f17d381bc621301cadb0684b104a66c058c338804770745e62eb57903fdd",
    "pages": (1, 25),
    "offset": -2,
    "prefix": "BCZ",
    "fabricated": "the policy thresholds of the inaction region identify the adjustment cost and risk aversion",
    "wrong_page": 12,
    "controls": ["BonaparteCooper.control_no_cost_at_one"],
})
