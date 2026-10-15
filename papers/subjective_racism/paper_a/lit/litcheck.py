"""Shared checks for the paper records under lit/. Each record's verify_*.py
calls run() with its own configuration. A record checks our reading of a
source and never the source itself.

  LC-1 the quotation rows of CLAIMS.md parse, and every page lies in the
       paper's range
  LC-2 every quotation is found, by exact normalised containment, in the
       cached text of the page it cites; a fabricated quotation is the
       control and must not be found
  LC-3 the cached PDF and text match their pinned sha256
  LC-4 the Lean file named in LEAN is a build root with no sorry, every Lean
       name cited in CLAIMS.md is declared, and every cited theorem passes the
       axiom audit
  LC-5 the named controls are cited and declared
"""
import hashlib
import os
import re
import shutil
import subprocess
import sys
import tempfile

LIT = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(LIT)
PROJ = os.path.join(ROOT, "lean", "mathlib")
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}


def normalise(s):
    s = s.replace("’", "'").replace("‘", "'").replace("“", '"').replace("”", '"')
    s = s.replace("``", '"').replace("''", '"')
    s = re.sub(r"-\n(?=[a-z])", "", s)
    return re.sub(r"\s+", " ", s).strip()


def quotation_rows(claims):
    rows = []
    for ln in claims.splitlines():
        m = re.match(r"^\|\s*([A-Z]+-Q\d+)\s*\|\s*([0-9]+)\s*\|\s*(.+?)\s*\|\s*(.+?)\s*\|\s*$", ln)
        if m:
            rows.append((m.group(1), int(m.group(2)), m.group(3), m.group(4)))
    return rows


def fragments(quote):
    q = quote.strip()
    if q.startswith('"') and q.endswith('"'):
        q = q[1:-1]
    return [normalise(f) for f in q.split("[...]") if normalise(f)]


def found(quote, page_text, ocr=False):
    t = normalise(page_text)
    frags = fragments(quote)
    if ocr:
        t = re.sub(r"\s+", "", t)
        frags = [re.sub(r"\s+", "", f) for f in frags]
    pos = 0
    for f in frags:
        i = t.find(f, pos)
        if i < 0:
            return False
        pos = i + len(f)
    return True


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        h.update(fh.read())
    return h.hexdigest()


def declarations(path):
    names, ns = [], []
    for ln in open(path):
        m = re.match(r"\s*namespace\s+([A-Za-z0-9_.']+)", ln)
        if m:
            ns.append(m.group(1))
            continue
        m = re.match(r"\s*end\s+([A-Za-z0-9_.']+)\s*$", ln)
        if m and ns and ns[-1] == m.group(1):
            ns.pop()
            continue
        m = re.match(r"\s*(?:theorem|lemma)\s+([^\s:(\[{]+)", ln)
        if m:
            names.append(".".join(ns + [m.group(1)]))
    return names


def run(cfg):
    here = cfg["dir"]
    results = []

    def check(name, ok, detail=""):
        results.append(bool(ok))
        print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))

    print("=" * 72)
    print(f"PAPER RECORD {cfg['slug']}")
    print("=" * 72)
    claims = open(os.path.join(here, "CLAIMS.md")).read()
    rows = quotation_rows(claims)
    pmap = cfg.get("page_map")
    lo, hi = cfg["pages"]
    if pmap:
        bad_pages = [r[0] for r in rows if r[1] not in pmap]
    else:
        bad_pages = [r[0] for r in rows if not lo <= r[1] <= hi]
    check("LC-1 quotation rows parse and their pages lie in the paper's range",
          rows and not bad_pages, f"{len(rows)} rows" + (f", out of range {bad_pages}" if bad_pages else ""))

    pdf, txt = cfg["pdf"], cfg["text"]
    have = os.path.exists(pdf) and os.path.exists(txt)
    if not have:
        check("LC-3 the cached source is present", False, f"missing {pdf} or {txt}")
    else:
        check("LC-3 the cached PDF matches its pinned sha256", sha(pdf) == cfg["pdf_sha256"], sha(pdf))
        check("LC-3 the cached text matches its pinned sha256", sha(txt) == cfg["text_sha256"], sha(txt))
        pages = open(txt).read().split("\f")

        def page_text(p):
            if pmap:
                i = pmap.get(p, -1)
                return pages[i] if 0 <= i < len(pages) else ""
            i = p - cfg["first_page"] + cfg["first_index"]
            return pages[i] if 0 <= i < len(pages) else ""

        ocr = cfg.get("ocr", False)
        if ocr:
            print("      text is OCR of a scan; quotations are compared with whitespace removed")
        missing = [r[0] for r in rows if not found(r[2], page_text(r[1]), ocr)]
        check("LC-2 every quotation is found on the page it cites", not missing, f"not found {missing}")
        fake = cfg["fabricated"]
        check("LC-2 control, a fabricated quotation is not found",
              not found(fake[1], page_text(fake[0]), ocr) and not any(found(fake[1], pt, ocr) for pt in pages))
        if rows:
            r0 = rows[0]
            if pmap:
                keys = sorted(pmap)
                wrong = next(k for k in keys if k != r0[1])
            else:
                wrong = r0[1] + 1 if r0[1] < hi else r0[1] - 1
            check("LC-2 control, a true quotation is not found on a wrong page",
                  not found(r0[2], page_text(wrong), ocr))

    lean_ptr = open(os.path.join(here, "LEAN")).read().strip()
    module = os.path.splitext(os.path.basename(lean_ptr))[0]
    path = os.path.join(ROOT, lean_ptr)
    lakefile = open(os.path.join(PROJ, "lakefile.toml")).read()
    roots = re.findall(r'"([A-Za-z0-9_]+)"', "".join(re.findall(r"roots\s*=\s*\[([^\]]*)\]", lakefile)))
    check("LC-4 the Lean file is a build root", os.path.exists(path) and module in roots, lean_ptr)
    src = open(path).read() if os.path.exists(path) else ""
    check("LC-4 the Lean file has no sorry", not re.search(r"\bsorry\b", src))
    declared = set(declarations(path)) if src else set()
    cited = set(re.findall(rf"`({re.escape(module)}\.[A-Za-z0-9_']+)`", claims))
    undeclared = sorted(cited - declared)
    check("LC-4 every Lean name cited in CLAIMS.md is declared", cited and not undeclared, f"{undeclared}")
    lake = shutil.which("lake")
    if lake and cited and not undeclared:
        with tempfile.TemporaryDirectory() as td:
            audit = os.path.join(td, "audit.lean")
            with open(audit, "w") as fh:
                fh.write(f"import {module}\n")
                for n in sorted(cited):
                    fh.write(f"#print axioms {n}\n")
            q = subprocess.run([lake, "env", "lean", audit], cwd=PROJ, capture_output=True, text=True)
        out = q.stdout + q.stderr
        bad, seen = {}, 0
        for n in cited:
            m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
            if m:
                seen += 1
                used = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
                if used - ALLOWED:
                    bad[n] = sorted(used - ALLOWED)
        check("LC-4 every cited theorem passes the axiom audit", seen == len(cited) and not bad,
              f"audited {seen} of {len(cited)}" + (f", disallowed {bad}" if bad else ""))
    else:
        check("LC-4 every cited theorem passes the axiom audit", False, "lake missing or names undeclared")
    ctrl = cfg["controls"]
    absent = [c for c in ctrl if c not in cited or c not in declared]
    check("LC-5 the named controls are cited and declared", ctrl and not absent, f"{absent}")

    nf = results.count(False)
    print("=" * 72)
    print(f"{cfg['slug'].upper()} SUMMARY: {len(results)} checks, {nf} failures")
    print("=" * 72)
    sys.exit(1 if nf else 0)
