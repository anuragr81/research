import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "PAGEL (2016), JEEA 14(2), AUTHOR'S ACCEPTED MANUSCRIPT",
    "pdf": "pagel_2016.pdf",
    "sha256": "288146428a7f2dfe52a1d1d010905e990f6c7622e5cef5a200d2dea162ddc248",
    "pages": (1, 51),
    "offset": 0,
    "prefix": "PG",
    "fabricated": "as the preferences feature sluggish belief updating in every period, which is true in the data.",
    "wrong_page": 5,
    "controls": ["Pagel.control_rho_flat_at_psi_eq_gammaQ",
                 "Pagel.control_weight_flat_at_lam_one",
                 "Pagel.control_gross_dispersion_depends_on_state"],
})
