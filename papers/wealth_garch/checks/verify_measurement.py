import glob
import hashlib
import os
import re
import shlex
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from verify_manuscript import lean_declarations, macro_calls, split_list, strip_comments

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STATUS = {"Rule", "Report", "Calibration", "Unobserved", "Output"}
REGULATORY = ("B3-Q", "LCR-Q", "RCR-Q", "FDIC-Q")
OUTCOMES = {"Tracked", "Holds", "Fails", "Not measured"}
CALIBRATION = ("BCVW-Q",)
COVERAGE = [r"$x$", r"$\pi$", r"$X-L$", r"$a_1$", r"$a_2$", r"$a_3$", r"$q$", r"$\sigma$", r"$\mu_s$",
            r"$r$", r"$\sigma_L$", r"$c$", r"$\gamma$", r"$\mu_L$", r"$r_L$", r"$\rho_L$", r"$\kappa$",
            r"$K$", r"$\lambda_S$", r"$y^*$", r"$x_L$", r"$y_{\text{post}}$", r"$\zeta$", r"$\nu_1^2$",
            r"$\nu_3^2$", r"$\nu_1^2/\nu_3^2$", r"$z_t$", r"$g$", r"$b$", r"$\delta_p$", r"$\phi^-$", r"$\phi^+$"]
results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


def lit_ids():
    ids = set()
    for f in glob.glob(os.path.join(ROOT, "lit", "*", "CLAIMS.md")):
        for ln in open(f):
            m = re.match(r"\|\s*([A-Z0-9]+-[QD][0-9]+)\s*\|", ln)
            if m:
                ids.add(m.group(1))
    return ids


def manuscript_rows():
    tex = strip_comments(open(os.path.join(ROOT, "MANUSCRIPT.tex")).read())
    return set(re.findall(r"\\[kmlc]row\{([KMLC][0-9]+)\}", tex))


def analyse(tex, lean, lits, rows):
    grows = macro_calls(strip_comments(tex), "grow", 6)
    out = {}
    ids = [r[0].strip() for r in grows]
    expected = [f"G{i}" for i in range(1, len(ids) + 1)]
    out["MM-1"] = (ids == expected and ids, f"ids {ids}")
    bad = [(r[0], r[4].strip()) for r in grows if r[4].strip() not in STATUS]
    out["MM-2"] = (not bad, f"{bad}")
    unres = []
    for r in grows:
        items = split_list(r[5])
        if not items:
            unres.append((r[0], "(none)"))
        for it in items:
            if not (it in lits or it in lean or it in rows):
                unres.append((r[0], it))
    out["MM-3"] = (not unres, f"{unres}")
    weak = []
    for r in grows:
        st, items = r[4].strip(), split_list(r[5])
        if st in {"Rule", "Report"}:
            need = any(i.startswith(REGULATORY) for i in items)
        elif st == "Calibration":
            need = any(i.startswith(CALIBRATION) for i in items)
        else:
            need = any(i in rows for i in items)
        if not need:
            weak.append((r[0], st))
    out["MM-4"] = (not weak, f"{weak}")
    pieces = {p.strip() for r in grows for p in r[1].split(",")}
    missing = [s for s in COVERAGE if s not in pieces]
    out["MM-5"] = (not missing, f"no row for {missing}")
    trows = macro_calls(strip_comments(tex), "trow", 6)
    tids = [r[0].strip() for r in trows]
    bad6 = [] if tids == ids and tids else [("ids", tids)]
    for r in trows:
        outcome, items = r[4].strip(), split_list(r[5])
        if outcome not in OUTCOMES:
            bad6.append((r[0], outcome))
        for it in items or ["(none)"]:
            if not (it in lits or it in lean or it in rows or it in ids or
                    ("/" in it and os.path.exists(os.path.join(ROOT, it)))):
                bad6.append((r[0], it))
        if outcome in {"Holds", "Fails"} and not any(re.search(r"/results_[^/]+\.md$", i) for i in items):
            bad6.append((r[0], "no results file"))
    out["MM-6"] = (not bad6, f"{bad6}")
    return out, len(grows)


def code_block(text):
    m = re.search(r"^```\n(.*?)^```", text, re.M | re.S)
    return m.group(1) if m else None


def pin_mismatches(text, here):
    bad = []
    pins = re.findall(r"^Pinned: (\S+) sha256 ([0-9a-f]{64})$", text, re.M)
    for path, digest in pins:
        full = os.path.join(here, path)
        if not os.path.exists(full) or hashlib.sha256(open(full, "rb").read()).hexdigest() != digest:
            bad.append(path)
    return bad if pins else ["(no pins)"]


def reproduce(path, text=None):
    here = os.path.dirname(os.path.join(ROOT, path))
    text = open(os.path.join(ROOT, path)).read() if text is None else text
    m = re.search(r"^Reproduce: (.+)$", text, re.M)
    expected = code_block(text)
    if not m or expected is None:
        return False, "no Reproduce line or no output block"
    bad = pin_mismatches(text, here)
    if bad:
        return False, f"pins differ {bad}"
    run = subprocess.run(shlex.split(m.group(1)), cwd=here, capture_output=True, text=True)
    if run.returncode != 0:
        return False, run.stderr.strip()[-200:]
    return run.stdout == expected, "" if run.stdout == expected else "output differs from the recorded block"


GOOD = r"""
\grow{G1}{$x$}{r}{c}{Report}{RCR-Q4, MeasurementMap.leverage_of_state}
\grow{G2}{$K$}{r}{c}{Unobserved}{K2}
\trow{G1}{m}{d}{p}{Fails}{FDIC-Q1, 06_empirical_starter/results_model_and_data.md}
\trow{G2}{m}{d}{p}{Not measured}{K2}
"""

BAD = r"""
\grow{G1}{$x$}{r}{c}{Report}{BCVW-Q14}
\grow{G1}{$K$}{r}{c}{Guess}{XX-Q9, MeasurementMap.no_such_theorem}
\grow{G4}{$q$}{r}{c}{Output}{BCVW-Q14}
\trow{G1}{m}{d}{p}{Fails}{FDIC-Q1}
\trow{G2}{m}{d}{p}{Partly}{06_empirical_starter/no_such_results.md}
"""

lean = lean_declarations()
lits = lit_ids()
rows = manuscript_rows()

print("=" * 72)
print("MEASUREMENT MAP (MEASUREMENT_MAP.tex)")
print("=" * 72)
print(f"      Lean declarations {len(lean)}; lit IDs {len(lits)}; manuscript rows {len(rows)}")
tex = open(os.path.join(ROOT, "MEASUREMENT_MAP.tex")).read()
real, n = analyse(tex, lean, lits, rows)
print(f"      rows in MEASUREMENT_MAP.tex: {n}")
names = {"MM-1": "row IDs G1, G2, ... unique and in order",
         "MM-2": "every status is Rule, Report, Calibration, Unobserved or Output",
         "MM-3": "every evidence item is a lit/ ID, a declared Lean theorem or a manuscript row",
         "MM-4": "Rule and Report rows cite a regulatory quotation, Calibration rows a BCVW quotation, Unobserved and Output rows a manuscript row",
         "MM-5": "every model object of the coverage list has a row",
         "MM-6": "the results table has one row per map row, outcomes from the list, evidence that resolves, and a results file behind every Holds or Fails"}
for k in sorted(real):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

print("-" * 72)
print("MM-7  every cited results file reproduces from its pinned data")
cited = sorted({i for r in macro_calls(strip_comments(tex), "trow", 6) for i in split_list(r[5])
                if re.search(r"/results_[^/]+\.md$", i)})
for f in cited:
    ok, detail = reproduce(f)
    check(f"MM-7 {f}", ok, detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass MM-1 to MM-4 and MM-6; a faulty one must fail every rule")
good, _ = analyse(GOOD, lean, lits, rows)
bad, _ = analyse(BAD, lean, lits, rows)
for k in sorted(good):
    if k != "MM-5":
        check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])
sample = "Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 " + "0" * 64 + "\n"
check("MM-7-control a wrong pin is caught",
      pin_mismatches(sample, os.path.join(ROOT, "06_empirical_starter")) == ["fdic_distress_panel_2015_2024.csv.gz"])
check("MM-7-control a file with no pins is caught", pin_mismatches("no pins here", ROOT) == ["(no pins)"])
target = "06_empirical_starter/results_dividend_location.md"
altered = open(os.path.join(ROOT, target)).read().replace("paid 56.2%", "paid 56.3%")
check("MM-7-control a results file with one altered figure is caught",
      "paid 56.3%" in altered and not reproduce(target, altered)[0])

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MEASUREMENT SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
