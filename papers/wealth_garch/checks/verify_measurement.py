import glob
import hashlib
import os
import re
import shlex
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from verify_manuscript import lean_declarations, macro_calls, order_problem, run_under, split_list, strip_comments
from computed import code_block, pin_mismatches, reproduce

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STATUS = {"Rule", "Report", "Calibration", "Unobserved", "Output"}
REGULATORY = ("B3-Q", "LCR-Q", "RCR-Q", "FDIC-Q", "PCA-Q", "OCC-Q", "OCCL-Q", "ASMT-Q")
OUTCOMES = {"Tracked", "Holds", "Fails", "Not measured"}
TERM_STATUS = {"Agreed", "In use", "Proposed"}
BANNED = ["loss aversion", "payout barrier", "injection trigger", "recapitalisation boundary", "volatility ratio"]
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
    nrows = macro_calls(strip_comments(tex), "nrow", 5)
    names = [r[0].strip().lower() for r in nrows]
    symbols = [s.strip() for r in nrows for s in r[1].split(", ") if s.strip() != "none"]
    bad8 = [n for n in names if names.count(n) > 1] + [s for s in symbols if symbols.count(s) > 1]
    bad8 += [(r[0], r[3].strip()) for r in nrows if r[3].strip() not in TERM_STATUS]
    bad8 += [r[0] for r in nrows if r[3].strip() == "Proposed" and not r[4].strip()]
    out["MM-8"] = (nrows and not bad8, f"{bad8}" if nrows else "no terminology rows")
    bad15 = []
    for macro, prefix in (("entrow", "N"), ("actrow", "A")):
        erows = macro_calls(strip_comments(tex), macro, 6)
        eids = [r[0].strip() for r in erows]
        if eids != [f"{prefix}{i}" for i in range(1, len(eids) + 1)]:
            bad15.append(("ids", eids))
        for r in erows:
            items = split_list(r[5])
            if r[4].strip() not in {"Rule", "Report"}:
                bad15.append((r[0], r[4].strip()))
            bad15 += [(r[0], it) for it in items if it not in lits]
            if not any(i.startswith(REGULATORY) for i in items):
                bad15.append((r[0], "no regulatory or report source"))
    out["MM-15"] = (not bad15, f"{bad15}")
    return out, len(grows)


def retired(tex):
    out = set(BANNED)
    for r in macro_calls(strip_comments(tex), "nrow", 5):
        out |= {s.strip().lower() for s in r[4].split(",") if s.strip() and s.strip() != "none"}
    return sorted(out)


def summary_problems(manuscript, tex):
    m = re.search(r"% plain-summary begin\n(.*?)% plain-summary end", manuscript, re.S)
    if not m:
        return ["no plain summary"]
    body = m.group(1)
    words = len(body.split())
    bad = [] if 270 <= words <= 330 else [f"{words} words"]
    bad += [c for c in "$:" if c in body]
    bad += [n for n in retired(tex) if n in body.lower()]
    return bad


def retired_uses(manuscript, tex):
    rows = macro_calls(strip_comments(tex), "nrow", 5)
    mp = [ln for ln in strip_comments(tex).splitlines() if not ln.lstrip().startswith("\\nrow{")]
    mp += [" ".join(r[:4]) for r in rows]
    ms = [ln for ln in strip_comments(manuscript).splitlines() if not ln.lstrip().startswith("\\lrow{")]
    ms = [s for s in re.split(r"(?<=\.)\s+", " ".join(ms)) if "\\cite" not in s]
    found = []
    for label, lines in (("MANUSCRIPT.tex", ms), ("MEASUREMENT_MAP.tex", mp)):
        flat = " ".join(" ".join(lines).replace("$", "").lower().split())
        found += [(label, n) for n in retired(tex) if n in flat]
    return found


def glossary_strays(manuscript, tex):
    m = re.search(r"\\section\{Terminology\}.*?\\endhead(.*?)\\bottomrule", strip_comments(manuscript), re.S)
    if not m:
        return ["no terminology section"]
    names = {r[0].strip().lower() for r in macro_calls(strip_comments(tex), "nrow", 5)}
    terms = [ln.split("&")[0].strip() for ln in m.group(1).splitlines() if "&" in ln]
    return [t for t in terms if t.lower() not in names]


def unattested(manuscript, tex):
    m = re.search(r"% plain-summary begin\n.*?% plain-summary end", manuscript, re.S)
    text = strip_comments(manuscript.replace(m.group(0), "") if m else manuscript).lower()
    body = strip_comments(tex)
    body = body[body.index("\\section{Map}"):].lower() if "\\section{Map}" in body else ""
    return [r[0].strip() for r in macro_calls(strip_comments(tex), "nrow", 5)
            if r[3].strip() in {"Agreed", "In use"} and r[0].strip().lower() not in text + body]


GOOD = r"""
\grow{G1}{$x$}{r}{c}{Report}{RCR-Q4, MeasurementMap.leverage_of_state}
\grow{G2}{$K$}{r}{c}{Unobserved}{K2}
\trow{G1}{m}{d}{p}{Fails}{FDIC-Q1, 06_empirical_starter/results_model_and_data.md}
\trow{G2}{m}{d}{p}{Not measured}{K2}
\nrow{equity}{$X-L$}{m}{In use}{none}
\nrow{buffer variance}{$\zeta$}{m}{Proposed}{z-volatility}
\entrow{N1}{bank}{r}{c}{Report}{FDIC-Q14}
\actrow{A1}{bank pays dividends to investors}{r}{c}{Report}{FDIC-Q8}
"""

BAD = r"""
\grow{G1}{$x$}{r}{c}{Report}{BCVW-Q14}
\grow{G1}{$K$}{r}{c}{Guess}{XX-Q9, MeasurementMap.no_such_theorem}
\grow{G4}{$q$}{r}{c}{Output}{BCVW-Q14}
\trow{G1}{m}{d}{p}{Fails}{FDIC-Q1}
\trow{G2}{m}{d}{p}{Partly}{06_empirical_starter/no_such_results.md}
\nrow{equity}{$X-L$}{m}{In use}{none}
\nrow{Equity}{$\zeta$}{m}{Maybe}{}
\entrow{N2}{bank}{r}{c}{Output}{XX-Q1}
\actrow{A1}{bank pays dividends to investors}{r}{c}{Unobserved}{K2}
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
         "MM-6": "the results table has one row per map row, outcomes from the list, evidence that resolves, and a results file behind every Holds or Fails",
         "MM-8": "terminology names and symbols unique, statuses Agreed, In use or Proposed, and every Proposed name says what it replaces",
         "MM-15": "entity rows N1, N2, ... and action rows A1, A2, ... in order, status Report or Rule, and every evidence item a lit/ ID with a regulatory or report source"}
for k in sorted(real):
    ok, detail = real[k]
    check(f"{k} {names[k]}", ok, "" if ok else detail)

ledger = subprocess.run([sys.executable, os.path.join(ROOT, "00_reader", "ledger_model.py")], capture_output=True, text=True)
check("MM-16 the ledger's nodes are the map's entities, named by its terminology (00_reader/ledger_model.py)",
      ledger.returncode == 0, "" if ledger.returncode == 0 else ledger.stdout.strip().splitlines()[-1])

term_symbols = {s.strip() for r in macro_calls(strip_comments(tex), "nrow", 5) for s in r[1].split(", ")}
missing9 = [s for s in COVERAGE if s not in term_symbols]
check("MM-9 every model object of the coverage list has a plain-English name", not missing9, f"{missing9}")
manuscript = open(os.path.join(ROOT, "MANUSCRIPT.tex")).read()
bad10 = summary_problems(manuscript, tex)
check("MM-10 the manuscript's plain summary has 270 to 330 words, no symbols, no colons and no retired name",
      not bad10, f"{bad10}")

bad11 = unattested(manuscript, tex)
check("MM-11 every Agreed or In use name appears in the manuscript outside the plain summary or in the map's rows",
      not bad11, f"{bad11}")

bad12 = retired_uses(manuscript, tex)
check("MM-12 no retired name appears in the manuscript outside its literature rows and sentences citing a paper, or in the map outside the Replaces column",
      not bad12, f"{bad12}")

bad13 = glossary_strays(manuscript, tex)
check("MM-13 every term of the manuscript's Terminology section is a name of the map's terminology table",
      not bad13, f"{bad13}")

print("-" * 72)
print("MM-7  every cited results file reproduces from its pinned data")
cited = sorted({i for r in macro_calls(strip_comments(tex), "trow", 6) for i in split_list(r[5])
                if re.search(r"/results_[^/]+\.md$", i)})
for f in cited:
    ok, detail = reproduce(f)
    check(f"MM-7 {f}", ok, detail)

print("-" * 72)
print("MM-14 standing results files reproduce from their pinned files, after their pre-registrations")
for standing in ("common_data/results_common_data.md", "replication/gordon_hirtle_2026/results_stage1.md",
                 "replication/gordon_hirtle_2026/results_stage1_3.md", "replication/gordon_hirtle_2026/results_chart2.md",
                 "replication/gordon_hirtle_2026/results_stage2.md", "replication/berger_2008/results_stage1.md",
                 "replication/berger_2008/results_stage1_2.md",
                 "replication/mankart_michaelides_pagratis_2020/results_stage1.md",
                 "replication/mankart_michaelides_pagratis_2020/results_validation_stage2.md",
                 "replication/mankart_michaelides_pagratis_2020/results_validation_stage2_a1.md",
                 "replication/mankart_michaelides_pagratis_2020/results_nonfinite_stage2.md",
                 "replication/mankart_michaelides_pagratis_2020/results_runaway_stage2.md",
                 "comparison/results_facts.md", "comparison/results_pk.md",
                 "comparison/results_mmp.md", "comparison/results_bound.md", "comparison/results_reward.md",
                 "comparison/results_mechanisms.md", "comparison/results_scale.md",
                 "comparison/results_judged.md", "comparison/results_test_judged.md",
                 "comparison/results_match.md", "comparison/results_extend.md",
                 "comparison/results_diag_f2.md", "comparison/results_own_loss.md",
                 "comparison/results_book_vol.md", "comparison/results_access.md",
                 "comparison/results_do_nothing.md"):
    ok, detail = reproduce(standing)
    check(f"MM-14 {standing} reproduces", ok, detail)
    pres = run_under(standing)
    check(f"MM-14 {standing} names its pre-registrations, each committed before it",
          pres and not any(order_problem(p, standing) for p in pres), f"{pres}")

for software in ("replication/berger_2008/results_validation.md",):
    ok, detail = reproduce(software)
    check(f"MM-14 {software} (software validation, no pre-registration) reproduces", ok, detail)

print("-" * 72)
print("CONTROLS  a well-formed sample must pass MM-1 to MM-4 and MM-6; a faulty one must fail every rule")
good, _ = analyse(GOOD, lean, lits, rows)
bad, _ = analyse(BAD, lean, lits, rows)
for k in sorted(good):
    if k != "MM-5":
        check(f"{k}-control passes on the well-formed sample", good[k][0], "" if good[k][0] else good[k][1])
    check(f"{k}-control fails on the faulty sample", not bad[k][0])
sample = "Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 " + "0" * 64 + "\n"
good_summary = "% plain-summary begin\n" + " ".join(["word"] * 300) + "\n% plain-summary end\n"
check("MM-10-control a 300-word summary with no symbol passes", summary_problems(good_summary, tex) == [])
check("MM-10-control a summary using a retired name is caught",
      summary_problems(good_summary.replace("word word", "loss aversion", 1), tex) != [])
check("MM-10-control a summary with a symbol is caught", summary_problems(good_summary.replace("word", "$x$", 1), tex) != [])
check("MM-10-control a 200-word summary is caught",
      summary_problems("% plain-summary begin\n" + " ".join(["word"] * 200) + "\n% plain-summary end\n", tex) != [])
check("MM-11-control an In use name found nowhere is caught",
      unattested("text", "\\nrow{no such name}{$s$}{m}{In use}{none}\n\\section{Map}\n") == ["no such name"])
small_map = ("\\nrow{buffer variance}{$\\zeta$}{The variance of the capital buffer.}{Agreed}{z-volatility}\n"
             "\\nrow{deficit limit}{$\\nu_1^2$}{m}{Agreed}{saturation limits}\n")
check("MM-12-control a clean manuscript and map pass", retired_uses("buffer variance\n", small_map) == [])
check("MM-12-control a retired name written with symbols in a model row is caught",
      retired_uses("\\mrow{M2}{the $z$-volatility}\n", small_map) == [("MANUSCRIPT.tex", "z-volatility")])
check("MM-12-control another paper's own term in a literature row passes",
      retired_uses("\\lrow{L2}{barberis2001}{a constant level of loss aversion}\n", small_map) == [])
check("MM-12-control another paper's own term in a sentence citing it passes",
      retired_uses("Our model differs. \\citet{barberis2001} find loss aversion silent.\n", small_map) == [])
check("MM-12-control a banned name in a sentence citing no paper is caught",
      retired_uses("\\citet{barberis2001} differ. The asymmetry parameter is loss aversion.\n", small_map)
      == [("MANUSCRIPT.tex", "loss aversion")])
check("MM-12-control a retired name split across lines is caught",
      retired_uses("the saturation\nlimits\n", small_map) == [("MANUSCRIPT.tex", "saturation limits")])
check("MM-12-control a retired name in a map row outside the Replaces column is caught",
      retired_uses("", small_map + "\\grow{G16}{$\\zeta$}{The z-volatility.}{x}{Output}{M3}\n")
      == [("MEASUREMENT_MAP.tex", "z-volatility")])
check("MM-13-control a glossary term not in the map is caught",
      glossary_strays("\\section{Terminology}\n\\endhead\nLog coordinate & $z$ & m \\\\\nCapital buffer & $z$ & m \\\\\n\\bottomrule\n", tex)
      == ["Log coordinate"])
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
