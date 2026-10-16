"""Our reading of Pęski and Szentes (2012), the BFI working paper, checked
against the cached source. Pages are PDF pages; PDF page 1 is the BFI cover."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "peski_szentes_2012",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "peski_szentes_bfi.pdf"),
    "pdf_sha256": "2e311476a5a57e67851bf02a25b90000e618b79fdedb7bea6260d7a612fbd876",
    "text": os.path.join(CACHE, "peski_szentes_bfi.txt"),
    "text_sha256": "de72ee18a821e46711008a90e351acd8a3c29a3c28c785e724bd6f8a35779ceb",
    "pages": (1, 34),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (7, "the employer infers the productivity of the worker from his physical colour"),
    "controls": ["PeskiSzentes2012.control_no_social_colour",
                 "PeskiSzentes2012.control_below_threshold_no_discrimination"],
})
