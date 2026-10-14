"""Suite for the primary sources behind the Research question (MANUSCRIPT.tex, Section 1).

It pins each cached document by sha256, extracts its text (pdftotext for the Chiba PDFs,
tag-stripping for the Normattiva HTML), checks every quotation in CLAIMS.md verbatim with
whitespace removed, checks that the "+4" sits on the same table row as the unlicensed-care
item, and runs a reversed-quote control.
"""

import hashlib
import html
import pathlib
import re
import shutil
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
CACHE = pathlib.Path.home() / ".cache" / "entry_contest" / "applications"
PINS = {
    "chiba_senkoukijun_r9.pdf": "7c8c161d2fe08c0038aab645528831edd96a4f2d643b9393960c784994f2ebf3",
    "chiba_kaisei_r8.pdf": "977fdbcdb8bccd4799260f76aab2e7e30842e302e954d6820cc7c58dbb9e2583",
    "normattiva_dlgs36_2023_art108.html": "70a9cc2353adc22de6c851c63ece7a3ff5f86f98b2c7c0628862037b5e1e01b5",
    "normattiva_dlgs198_2006_art46bis.html": "07d77589ec70953fafad9600ee89b3144fc1a71e3293e842f13fb5831128252e",
    "takarazuka_chinjo21_2023.pdf": "fa4f3b32911689284e020f481c025b46ab61fd0eb011a740b976d6b07ef012d4",
}
SOURCES = {
    "chiba_senkoukijun_r9.pdf": "https://www.city.chiba.jp/kodomomirai/yojikyoiku/unei/2027_riyoushinsei/documents/r9senkoukizyun.pdf",
    "chiba_kaisei_r8.pdf": "https://www.city.chiba.jp/kodomomirai/yojikyoiku/unei/documents/chibashi_hoiku_toriatukjaiyoryo_kaisei.pdf",
    "normattiva_dlgs36_2023_art108.html": "https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2023-03-31;36~art108",
    "normattiva_dlgs198_2006_art46bis.html": "https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2006-04-11;198~art46bis",
    "takarazuka_chinjo21_2023.pdf": "https://www.city.takarazuka.hyogo.jp/_res/projects/default_project/_page_/001/046/139/chinjo21.pdf",
}
results = []


def check(name, ok, detail=""):
    results.append(bool(ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def squash(s):
    return re.sub(r"\s+", "", s)


def text_of(name):
    path = CACHE / name
    if name.endswith(".pdf"):
        pt = shutil.which("pdftotext")
        return subprocess.run([pt, "-layout", str(path), "-"], capture_output=True, text=True).stdout
    raw = path.read_text(encoding="utf-8", errors="ignore")
    return html.unescape(re.sub(r"<[^>]+>", " ", raw))


print("=" * 72)
print("APPLICATIONS 2026 (primary sources for the Research question)")
print("=" * 72)

texts = {}
for name, pin in PINS.items():
    path = CACHE / name
    if not path.exists():
        check(f"AP-Q1 {name} is cached", False, f"download {SOURCES[name]} to {path}")
        continue
    d = hashlib.sha256(path.read_bytes()).hexdigest()
    check(f"AP-Q1 {name} is the document read", d == pin, d[:16])
    texts[name] = text_of(name)

claims = (HERE / "CLAIMS.md").read_text()
rows = re.findall(r"^\|\s*(AP-\d+)\s*\|\s*([\w.\-]+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
check("AP-Q0 every quotation row has an ID, a document and a quotation", len(rows) >= 10, f"{len(rows)} rows")
for rid, doc, quote in rows:
    t = texts.get(doc, "")
    check(f"{rid} ({doc}) is in the document verbatim", bool(t) and squash(quote) in squash(t))

chiba = texts.get("chiba_senkoukijun_r9.pdf", "")
lines = chiba.split("\n")
row = [i for i, l in enumerate(lines) if "認可外保育施設又はベビーシッター" in l]
near = row and any("＋４" in lines[j] for j in range(row[0], min(row[0] + 3, len(lines))))
check("AP-P1 the +4 sits on the unlicensed-care row of the Chiba table", bool(near))

flip = squash("点数の低い方から、入所の決定をします。")
check("AP-C1 control: AP-6 with highest replaced by lowest is not in the document",
      flip not in squash(texts.get("chiba_kaisei_r8.pdf", "")))
flip2 = squash("il minor punteggio da attribuire alle imprese")
check("AP-C2 control: AP-8 with maggior replaced by minor is not in the statute",
      flip2 not in squash(texts.get("normattiva_dlgs36_2023_art108.html", "")))

fails = results.count(False)
print("=" * 72)
print(f"APPLICATIONS SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
