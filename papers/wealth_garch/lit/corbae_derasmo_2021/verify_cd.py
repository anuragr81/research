import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "CORBAE AND D'ERASMO (2021), CAPITAL BUFFERS IN A QUANTITATIVE MODEL OF BANKING INDUSTRY DYNAMICS, FRB PHILADELPHIA WP 21-24",
    "pdf": "phil_wp21_24.pdf",
    "sha256": "996657a2ba434df23ac5787224c370ff59371289e5e9fc6748aa6e65b1e5ecbf",
    "pages": (1, 50),
    "offset": -2,
    "prefix": "CD",
    "fabricated": "big banks have more volatile funding inflows, which is one important factor explaining why they hold a smaller capital buffer in our model.",
    "wrong_page": 10,
    "controls": ['CorbaeDErasmo.control_gap_not_closed', 'CorbaeDErasmo.control_big_overpredicted'],
})
