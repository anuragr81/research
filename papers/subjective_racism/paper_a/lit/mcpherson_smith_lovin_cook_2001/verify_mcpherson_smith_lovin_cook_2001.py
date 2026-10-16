"""Our reading of McPherson, Smith-Lovin and Cook (2001), checked against the
cached source. Pages are journal pages, 415 to 444."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "mcpherson_smith_lovin_cook_2001",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "mcpherson_2001.pdf"),
    "pdf_sha256": "22f8c389464c1c8a4c56940ed4e15041e80fc7eddc3cd8f6e7e62babe61a87ad",
    "text": os.path.join(CACHE, "mcpherson_2001.txt"),
    "text_sha256": "bd6810331b7e21688d5a866f2bd7358656cb8654b02aa48e6c59dc71110a7b10",
    "pages": (415, 444),
    "first_page": 415,
    "first_index": 0,
    "fabricated": (422, "Marsden (1988) found a strong social distance effect for race, after taking baseline and inbreeding into account."),
    "controls": ["McPhersonEtAl2001.control_no_consolidation", "McPhersonEtAl2001.control_same_different_flat"],
})
