import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "GROPP AND HEIDER (2009), THE DETERMINANTS OF BANK CAPITAL STRUCTURE, ECB WP 1096",
    "pdf": "gropp_heider_2009.pdf",
    "sha256": "6a19595009559e7f3d664d3eb6c6359dc8c2fb1a4549fbcd033beed2528124d8",
    "pages": (1, 50),
    "offset": -1,
    "prefix": "GH09",
    "fabricated": "The fixed effect accounts for 12% of book leverage and for 76% of market leverage.",
    "wrong_page": 10,
    "controls": ['GroppHeider.control_speed_needs_fe', 'GroppHeider.control_close_not_total'],
})
