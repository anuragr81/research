"""Suite for Weber (1978). The claims are interpretive and rest on quotation, so
the suite checks the quotations themselves: every quotation in CLAIMS.md has a
page and is found in the archive.org text of the 1978 edition, allowing for OCR
noise in the scan. A fabricated quotation must not be found.
"""

import difflib
import pathlib
import re
import sys
import urllib.request

HERE = pathlib.Path(__file__).resolve().parent
URL = ("https://archive.org/stream/MaxWeberEconomyAndSociety/"
       "MaxWeberEconomyAndSociety_djvu.txt")
CACHE = pathlib.Path.home() / ".cache" / "entry_contest" / "weber_economy_and_society.txt"
THRESHOLD = 0.9
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def source_text():
    if CACHE.exists():
        return CACHE.read_text()
    import html
    raw = urllib.request.urlopen(URL, timeout=120).read().decode("utf-8", "replace")
    m = re.search(r"<pre[^>]*>(.*?)</pre>", raw, re.S)
    text = html.unescape(m.group(1)) if m else raw
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    CACHE.write_text(text)
    return text


def norm(s):
    s = re.sub(r"-\s*\n\s*", "", s)
    s = s.lower()
    s = re.sub(r"[^a-z]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def best_ratio(quote, text_norm):
    """Best similarity between the quotation and a passage of the scan of the
    same length, the passage found by anchoring on any three consecutive words
    of the quotation that the scan reproduces exactly."""
    q = norm(quote)
    qw = q.split()
    best = 0.0
    for k in range(max(1, len(qw) - 2)):
        anchor = " ".join(qw[k:k + 3])
        offset = len(" ".join(qw[:k])) + (1 if k else 0)
        for m in re.finditer(re.escape(anchor), text_norm):
            start = max(0, m.start() - offset)
            window = text_norm[start:start + len(q)]
            best = max(best, difflib.SequenceMatcher(None, q, window).ratio())
        if best >= THRESHOLD:
            return best
    return best


print("=" * 72)
print("WEBER 1978 (quotations checked against the archive.org text)")
print("=" * 72)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(WB-\d+)\s*\|\s*([0-9-]+)\s*\|\s*\"([^\"]+)\"", claims, re.M)
check("WB-L0 every quotation row has an ID, a page and a quotation", len(rows) >= 3, f"{len(rows)} rows")

try:
    text = source_text()
    ok_src = "ECONOMY" in text[:2000] and "1978 by The Regents" in text[:6000]
    check("WB-L1 the source text is the 1978 University of California Press printing", ok_src)
except Exception as e:  # network failure with no cache
    check("WB-L1 the source text is available", False, str(e)[:120])
    sys.exit(1)

tn = norm(text)
for rid, page, quote in rows:
    r = best_ratio(quote, tn)
    check(f"{rid} (p.{page}) is in the source", r >= THRESHOLD, f"match {r:.2f}")

fake = "Status is determined entirely by the size of the fee that each challenger pays at entry."
r = best_ratio(fake, tn)
check("WB-C1 control: a fabricated quotation is not found", r < THRESHOLD, f"best match {r:.2f}")

fails = results.count(False)
print("=" * 72)
print(f"WEBER SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
