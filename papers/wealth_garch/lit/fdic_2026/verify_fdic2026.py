import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "FDIC (2026), ALL FINANCIAL REPORTS, VARIABLE DEFINITIONS",
    "pdf": "fdic_all_financial_reports.xlsx",
    "sha256": "6413c93064c16c63d4850221b25181f03ea51d086cd8f68d365cb070f383ab65",
    "sheet": "Reference-Variables&Definitions",
    "prefix": "FDIC",
    "fabricated": "The sum of all assets owned by the institution including off-balance-sheet accounts.",
    "wrong_page": "LIAB",
    "controls": ["MeasurementMap.control_rwa_needs_zero_weight_on_riskless",
                 "MeasurementMap.control_leverage_needs_l0_lt_one"],
})
