"""Suite for the primary sources behind the Research question (MANUSCRIPT.tex, Section 1).

It pins each cached document by sha256, extracts its text (pdftotext for the PDFs, in layout
and in reading order; tag-stripping for the HTML), checks every quotation in CLAIMS.md verbatim with
whitespace removed, checks that the "+4" sits on the same table row as the unlicensed-care
item, and runs a reversed-quote control.

It also checks the section "Application search of 14 October 2026" of LITERATURE.tex:
every \\aq{key}{text} against that paper's abstract in abstracts_2026.json or
abstracts_2026b.json (both pinned), with
the source named in the [A, ...] tag; every \\pq or \\pqj{document}{text} against the cached
document; and every citation key against refs.bib.
"""

import hashlib
import html
import json
import pathlib
import re
import shutil
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent
ABSTRACTS_PIN = "bf146484373fde7730aa04f3f964e2efb4f61939532c50caa4ffe11d8c51977f"
ABSTRACTS2_PIN = "013874f63b260aa359f2f92a5deb0e380f150ac24ebf7dca5f96155168d82bf9"
CACHE = pathlib.Path.home() / ".cache" / "entry_contest" / "applications"
PINS = {
    "chiba_senkoukijun_r9.pdf": "7c8c161d2fe08c0038aab645528831edd96a4f2d643b9393960c784994f2ebf3",
    "chiba_kaisei_r8.pdf": "977fdbcdb8bccd4799260f76aab2e7e30842e302e954d6820cc7c58dbb9e2583",
    "normattiva_dlgs36_2023_art108.html": "70a9cc2353adc22de6c851c63ece7a3ff5f86f98b2c7c0628862037b5e1e01b5",
    "normattiva_dlgs198_2006_art46bis.html": "07d77589ec70953fafad9600ee89b3144fc1a71e3293e842f13fb5831128252e",
    "takarazuka_chinjo21_2023.pdf": "fa4f3b32911689284e020f481c025b46ab61fd0eb011a740b976d6b07ef012d4",
    "rics/apc_candidate_guide_2026.pdf": "2b849fce755852d75b050ac23b0ac5b3459511133c09e5848ec00fe0eb774743",
    "rics/vr_faq_2025.pdf": "6838e1ad66cffa9d7159a9de42e4d90afda2b69e7c3c2a21fd7d0c4eb9b60afe",
    "rics/join_fees.html": "e0fd7d957ac3685ec810235da62413ccc995cef4d8cf43967e32f4adc583d249",
    "rics/gov_regulated_profession.html": "792b4d0572a2d77366be2612b94d2cff277855387268ba9cafe362d767c87bec",
}
SOURCES = {
    "chiba_senkoukijun_r9.pdf": "https://www.city.chiba.jp/kodomomirai/yojikyoiku/unei/2027_riyoushinsei/documents/r9senkoukizyun.pdf",
    "chiba_kaisei_r8.pdf": "https://www.city.chiba.jp/kodomomirai/yojikyoiku/unei/documents/chibashi_hoiku_toriatukjaiyoryo_kaisei.pdf",
    "normattiva_dlgs36_2023_art108.html": "https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2023-03-31;36~art108",
    "normattiva_dlgs198_2006_art46bis.html": "https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2006-04-11;198~art46bis",
    "takarazuka_chinjo21_2023.pdf": "https://www.city.takarazuka.hyogo.jp/_res/projects/default_project/_page_/001/046/139/chinjo21.pdf",
    "rics/apc_candidate_guide_2026.pdf": "https://www.rics.org/content/dam/ricsglobal/documents/candidates/RICS-APC-Candidate-Guide_August-2026.pdf",
    "rics/vr_faq_2025.pdf": "https://www.rics.org/content/dam/ricsglobal/documents/regulation/VR-FAQs-2025.pdf",
    "rics/join_fees.html": "https://www.rics.org/join-rics/join-fees",
    "rics/gov_regulated_profession.html": "https://www.regulated-professions.service.gov.uk/professions/chartered-valuation-surveyor",
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
        # layout mode keeps table rows together; reading order mode keeps the columns of a
        # two-column page apart. A quotation may be found in either.
        return "\f".join(subprocess.run([pt, m, str(path), "-"], capture_output=True, text=True).stdout
                         for m in ("-layout", "-raw"))
    raw = path.read_text(encoding="utf-8", errors="ignore")
    raw = re.sub(r"<script.*?</script>|<style.*?</style>", " ", raw, flags=re.S)
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
rows = re.findall(r"^\|\s*(AP-\d+)\s*\|\s*([\w./\-]+)\s*\|\s*\"(.+?)\"\s*\|", claims, re.M)
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
flip3 = squash("Reserved activities Some - restrictions apply to use of title only")
check("AP-C4 control: AP-16 with None replaced by Some is not on the register page",
      flip3 not in squash(texts.get("rics/gov_regulated_profession.html", "")))
flip2 = squash("il minor punteggio da attribuire alle imprese")
check("AP-C2 control: AP-8 with maggior replaced by minor is not in the statute",
      flip2 not in squash(texts.get("normattiva_dlgs36_2023_art108.html", "")))

print()
print("LITERATURE.tex, section Application search of 14 October 2026")
abs_path = HERE / "abstracts_2026.json"
d = hashlib.sha256(abs_path.read_bytes()).hexdigest()
check("AP-A0 abstracts_2026.json is the record built on 14 October 2026", d == ABSTRACTS_PIN, d[:16])
abstracts = json.loads(abs_path.read_text())
abs2_path = HERE / "abstracts_2026b.json"
d2 = hashlib.sha256(abs2_path.read_bytes()).hexdigest()
check("AP-A0b abstracts_2026b.json is the record built on 15 October 2026", d2 == ABSTRACTS2_PIN, d2[:16])
abstracts2 = json.loads(abs2_path.read_text())
check("AP-A0c the two abstract records share no key", not set(abstracts) & set(abstracts2))
abstracts.update(abstracts2)
lit = (ROOT / "LITERATURE.tex").read_text()
m = re.search(r"\\section\{Application search of 14 October 2026\}(.*?)\\section\{", lit, re.S)
sec = m.group(1) if m else ""
check("AP-A1 the section is in LITERATURE.tex", bool(sec))


def delatex(t):
    return t.replace("\\%", "%").replace("\\&", "&").replace("~", " ")


def in_abstract(key, text):
    a = html.unescape(abstracts.get(key, {}).get("abstract") or "")
    return bool(a) and squash(delatex(text)) in squash(a)


aqs = re.findall(r"\\aq\{(\w+)\}\{([^{}]*)\}\s*\[A, (\w+)", sec)
check("AP-A2 the section quotes at least 40 abstracts, each tagged with its source",
      len(aqs) >= 40 and len(aqs) == sec.count("\\aq{"), f"{len(aqs)} quotations")
for key, text, src in aqs:
    check(f"AP-A3 {key}: quotation is in its abstract verbatim", in_abstract(key, text))
    check(f"AP-A4 {key}: tagged source {src} is the abstract's source",
          src == abstracts.get(key, {}).get("abstract_source"))
pqs = re.findall(r"\\pqj?\{([\w./\-]+)\}\{([^{}]*)\}", sec)
check("AP-A5 the section quotes the primary documents", len(pqs) >= 9, f"{len(pqs)} quotations")
for doc, text in pqs:
    t = texts.get(doc, "")
    check(f"AP-A6 {doc}: quotation is in the document verbatim", bool(t) and squash(text) in squash(t),
          text[:24])
bib = set(re.findall(r"^@\w+\{([^,]+),", (ROOT / "refs.bib").read_text(), re.M))
cited = set(k.strip() for g in re.findall(r"\\cite[pt]?\{([^}]*)\}", sec) for k in g.split(","))
missing = sorted(cited - bib)
check("AP-A7 every key cited in the section is in refs.bib", not missing, ", ".join(missing))
check("AP-C3 control: AP-A3 with higher replaced by lower is not in Adiri and Yechiali",
      not in_abstract("AdiriYechiali1974", "The lower the priority of the queue, the higher the toll fee"))

fails = results.count(False)
print("=" * 72)
print(f"APPLICATIONS SUMMARY: {len(results)} checks, {fails} failures")
print("=" * 72)
sys.exit(1 if fails else 0)
