import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

# The author's PDF carries printed page 1923 twice (PDF pages 23 and 24), so
# printed pages 1924 to 1938 are one PDF page later than the offset of 1900
# predicts, and printed page 1939 is absent (AH-D1 in CLAIMS.md).
PDF_PAGE = {n: n - 1900 for n in range(1901, 1924)}
PDF_PAGE.update({n: n - 1899 for n in range(1924, 1939)})

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "ANDRIES AND HADDAD (2020), JPE 128(5):1901-1939, PAGE IMAGES WITH OCR",
    "pdf": "andries_haddad_2020.pdf",
    "sha256": "871deba550e4fc84a1d5db25fcf0c311917d1cd4d40d093b5fba63a99fc1f015",
    "pages": (1901, 1938),
    "offset": 1900,
    "pdf_page": PDF_PAGE,
    "ocr": "andries_haddad_2020_ocr",
    "prefix": "AH",
    "fabricated": "Our agent fears the quantity rather than the flow of information.",
    "wrong_page": 1905,
    "controls": ["AndriesHaddad.control_no_aversion_at_theta_zero",
                 "AndriesHaddad.control_ce2_degenerate"],
})
