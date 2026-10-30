import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "ALVAREZ, LE BIHAN AND LIPPI (2014), SMALL AND LARGE PRICE CHANGES AND THE PROPAGATION OF MONETARY SHOCKS",
    "pdf": "alvarez_lebihan_lippi_2014.pdf",
    "sha256": "7c817424737889c9ceb2ba9b674e6087f59c98f3e172cf18815635e0a339e0ae",
    "pages": (1, 47),
    "offset": -2,
    "prefix": "ALL",
    "fabricated": "the thresholds of the inaction region identify the menu cost and the free adjustment rate",
    "wrong_page": 10,
    "controls": ["AlvarezLeBihanLippi.control_same_direction_not_identified", "AlvarezLeBihanLippi.control_threshold_sees_sum_only"],
})
