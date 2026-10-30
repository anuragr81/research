import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "LI, YU AND ZHANG (2023), arXiv:2108.02648v4",
    "pdf": "lyz_2108.02648v4.pdf",
    "sha256": "5a02fd5d1af9ca926586e0b3e0af9d5a6540fb73841ee324cead686e754d1390",
    "pages": (1, 38),
    "offset": 0,
    "prefix": "LYZ",
    "fabricated": "our asymptotic limits coincide with the ones in the Merton's problem for every reference degree parameter",
    "wrong_page": 12,
    "controls": ["LiYuZhang.control_limitTerm_depends_on_k_when_equal",
                 "LiYuZhang.control_roots_need_distinct",
                 "LiYuZhang.control_U_not_concave"],
})
