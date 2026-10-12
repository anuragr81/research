import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run

run({
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "title": "JOKIPII AND MILNE (2008), JOURNAL OF BANKING & FINANCE 32",
    "pdf": "jokipii_milne_2008.pdf",
    "sha256": "99bb1800ec43831d043f189312da5cfcf94d1e3fb0148ffbf9b0375e0394da85",
    "pages": (1440, 1451),
    "offset": 1439,
    "layout": False,
    "prefix": "JM",
    "fabricated": "We investigate the possibility of asymmetries in these costs, since it may be easier to reduce capital",
    "wrong_page": 1448,
    "controls": ["JokipiiMilne.control_rate_is_not_lag",
                 "JokipiiMilne.control_eu15_average_not_303"],
})
