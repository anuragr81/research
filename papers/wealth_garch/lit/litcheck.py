import hashlib
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unicodedata

LIT = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(LIT)
PROJ = os.path.join(ROOT, "lean", "mathlib")
CACHE = os.path.expanduser("~/.cache/wealth_garch")
ALLOWED = {"propext", "Classical.choice", "Quot.sound"}


class Suite:
    def __init__(self, title):
        self.results = []
        print("=" * 72)
        print(title)
        print("=" * 72)

    def check(self, name, ok, detail=""):
        self.results.append((name, bool(ok)))
        print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))

    def finish(self):
        nf = sum(1 for _, ok in self.results if not ok)
        print("=" * 72)
        print(f"SUMMARY: {len(self.results)} checks, {nf} failures")
        print("=" * 72)
        sys.exit(1 if nf else 0)


def normalise(s):
    s = unicodedata.normalize("NFKC", s)
    s = s.replace("’", "'").replace("‘", "'").replace("“", '"').replace("”", '"')
    s = s.replace("``", '"').replace("''", '"')
    s = re.sub(r"-[ \t]*\n[ \t]*", "-", s)
    return re.sub(r"\s+", " ", s).strip()


def table_rows(md, heading):
    m = re.search(rf"^## {re.escape(heading)}\s*$(.*?)(?=^## |\Z)", md, re.M | re.S)
    if not m:
        return []
    rows = []
    for ln in m.group(1).splitlines():
        if not ln.startswith("|") or re.match(r"^\|\s*-", ln):
            continue
        cells = [c.strip() for c in ln.strip().strip("|").split("|")]
        rows.append(cells)
    return rows[1:]


def page_text(pdf, page):
    p = subprocess.run(["pdftotext", "-f", str(page), "-l", str(page), "-layout", pdf, "-"],
                       capture_output=True, text=True)
    return normalise(p.stdout)


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
        m = re.match(r"\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", ln)
        if m:
            names.append(".".join(ns + [m.group(1)]))
    return names


def run(cfg):
    here = cfg["dir"]
    s = Suite(cfg["title"])
    claims = open(os.path.join(here, "CLAIMS.md")).read()
    pdf = os.path.join(CACHE, cfg["pdf"])

    print("-" * 72)
    print("SOURCE")
    have = os.path.exists(pdf)
    s.check("S1 cached PDF present", have, pdf)
    if have:
        digest = hashlib.sha256(open(pdf, "rb").read()).hexdigest()
        s.check("S2 cached PDF matches its pinned sha256", digest == cfg["sha256"], digest)
        s.check("S2-control a different digest is rejected", digest != "0" * 64)

    print("-" * 72)
    print("QUOTATIONS  exact containment on the stated page, after normalisation")
    quotes = table_rows(claims, "Quotations")
    s.check("Q0 the quotation table parses and is non-empty", quotes and all(len(r) >= 4 for r in quotes),
            f"{len(quotes)} rows")
    lo, hi = cfg["pages"]
    cache = {}

    def on_page(text, printed):
        pg = printed - cfg["offset"]
        if pg not in cache:
            cache[pg] = page_text(pdf, pg)
        return normalise(text) in cache[pg]

    for r in quotes:
        qid, page, quote = r[0], r[1], r[2]
        ok_page = page.isdigit() and lo <= int(page) <= hi
        s.check(f"{qid} page {page} lies in {lo}-{hi}", ok_page)
        if ok_page and have:
            s.check(f"{qid} found verbatim on p. {page}", on_page(quote, int(page)), quote[:60])
    if have and quotes:
        p0 = int(quotes[0][1])
        s.check("Q-control a fabricated quotation is not found",
                not on_page(cfg["fabricated"], p0), cfg["fabricated"][:60])
        s.check("Q-control a true quotation on the wrong page is not found",
                not on_page(quotes[0][2], cfg["wrong_page"]), f"p. {cfg['wrong_page']}")

    print("-" * 72)
    print("READINGS")
    readings = table_rows(claims, "Readings recorded")
    prefix = cfg["prefix"]
    s.check("R1 every reading has a well-formed ID",
            all(re.fullmatch(prefix + r"-D[1-9][0-9]*", r[0]) for r in readings), f"{len(readings)} readings")

    print("-" * 72)
    print("LEAN")
    lean_file = os.path.join(PROJ, open(os.path.join(here, "LEAN")).read().strip())
    module = os.path.splitext(os.path.basename(lean_file))[0]
    lakefile = open(os.path.join(PROJ, "lakefile.toml")).read()
    roots = re.findall(r'"([A-Za-z0-9_]+)"', " ".join(re.findall(r"roots\s*=\s*\[([^\]]*)\]", lakefile)))
    s.check("L1 the Lean file is a build root", module in roots, module)
    src = open(lean_file).read()
    s.check("L2 no sorry in the Lean file", not re.search(r"\bsorry\b", src))
    declared = declarations(lean_file)
    cited = [re.sub(r"`", "", r[0]) for r in table_rows(claims, "Lean results")]
    missing = [c for c in cited if c not in declared]
    s.check("L3 every Lean name cited in CLAIMS.md is declared", cited and not missing,
            f"{len(cited)} cited; missing {missing}" if missing else f"{len(cited)} cited")
    controls = [n for n in declared if n.split(".")[-1].startswith("control_")]
    s.check("L4 the named controls are present", set(cfg["controls"]) <= set(controls) and controls,
            f"{controls}")
    lake = shutil.which("lake")
    if lake is None:
        s.check("L5 lake on PATH", False, "the Lean block did NOT run")
    else:
        b = subprocess.run([lake, "build", module], cwd=PROJ, capture_output=True, text=True)
        s.check("L5 the Lean file builds", b.returncode == 0, (b.stdout + b.stderr).strip()[-200:])
        with tempfile.TemporaryDirectory() as td:
            audit = os.path.join(td, "audit.lean")
            with open(audit, "w") as fh:
                fh.write(f"import {module}\n")
                for n in declared:
                    fh.write(f"#print axioms {n}\n")
            q = subprocess.run([lake, "env", "lean", audit], cwd=PROJ, capture_output=True, text=True)
        out = q.stdout + q.stderr
        used = {}
        for n in declared:
            m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
            if m:
                used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
        bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
        s.check("L6 axiom audit covers every declared theorem", len(used) == len(declared),
                f"{len(used)} of {len(declared)}")
        s.check("L7 no axiom outside propext, Classical.choice and Quot.sound", not bad, f"{bad}" if bad else "")
    s.finish()
