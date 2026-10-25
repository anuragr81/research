import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LANG AND MENNO (2026), A STRUCTURAL MODEL OF CAPITAL BUFFER USABILITY",
    "pdf": "lang_menno_2026.pdf",
    "sha256": "3299cc271b038351336ddb3d40b1eb85ca6902f6fe7cb14d3417f1644ed816cd",
    "pages": (1, 49),
    "offset": -1,
    "prefix": "LM",
    "fabricated": "Stigma costs rise in proportion to the size of the shortfall below the CBR.",
    "wrong_page": 7,
    "controls": ["LangMenno.control_graduated_flat_at_one"],
})
