import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LANG AND MENNO (2026), A STRUCTURAL MODEL OF CAPITAL BUFFER USABILITY, ECB WP 3188",
    "pdf": "lang_menno_2026.pdf",
    "sha256": "3299cc271b038351336ddb3d40b1eb85ca6902f6fe7cb14d3417f1644ed816cd",
    "pages": (1, 49),
    "offset": -1,
    "prefix": "LM",
    "fabricated": "For tractability, we restrict the model to 20 periods.",
    "wrong_page": 20,
    "controls": ['LangMenno.control_cbr_needs_buffer', 'LangMenno.control_us_floor_not_ecb'],
})
