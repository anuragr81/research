"""Our reading of Goffman (1951), "Symbols of Class Status", in the authorised
Spanish translation by Servando Ortoll, "Símbolos de status de clase",
Universidad de Sonora, checked against the cached source. The English original
is not read. Pages are PDF pages of the translation, equal to its printed
pages."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from litcheck import run  # noqa: E402

CACHE = os.path.expanduser("~/.cache/paper_a/sources")
run({
    "slug": "goffman_1951_es",
    "dir": os.path.dirname(os.path.abspath(__file__)),
    "pdf": os.path.join(CACHE, "goffman_1951_es.pdf"),
    "pdf_sha256": "eee9d3ac5725356912f9d949f52bfe213399904e1ed1757cd082458807068dbc",
    "text": os.path.join(CACHE, "goffman_1951_es.txt"),
    "text_sha256": "ebe1c1dce71691defe3ef0e9e781750eef0f69231de4f4b7f28b5e6393d53ffa",
    "pages": (1, 16),
    "first_page": 1,
    "first_index": 0,
    "fabricated": (14, "su asociación con estos símbolos aumenta el valor de estas señales ante los ojos de los demás"),
    "controls": ["Goffman1951es.control_counterfeit",
                 "Goffman1951es.control_association_from_above",
                 "Goffman1951es.control_alternative_restrictions"],
})
