import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "UNITED STATES CODE (2023), 12 U.S.C. 1831o, PROMPT CORRECTIVE ACTION",
    "pdf": "usc_1831o_2023.pdf",
    "sha256": "34b1b2691c0d95112832f06f3da7bfdbfbde315ec94d8f53846a976fe9fc0f94",
    "pages": (1161, 1171),
    "offset": 1160,
    "layout": False,
    "prefix": "PCA",
    "fabricated": "The purpose of this section is to resolve the problems of insured depository institutions at the least possible cost to shareholders.",
    "wrong_page": 1163,
    "controls": ['MeasurementMap.control_distribution_needs_positive_rwa', 'MeasurementMap.control_critical_needs_positive_assets'],
})
