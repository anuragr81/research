import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ENH = os.path.dirname(HERE)
ROOT = os.path.dirname(ENH)
sys.path.insert(0, os.path.join(ROOT, "checks"))
import verify_manuscript as vm

def map_names():
    tables = open(os.path.join(ENH, "map_tables.tex")).read()
    return {r[1].strip() for r in vm.macro_calls(vm.strip_comments(tables), "mm", 6)}


def player_assumption(tex, mids, eids, lids):
    tex = vm.strip_comments(tex)
    prows = vm.macro_calls(tex, "prow", 6)
    arows = vm.macro_calls(tex, "arow", 6)
    out = {}
    ids = [(r[0].strip(), "P") for r in prows] + [(r[0].strip(), "A") for r in arows]
    bad = [i for i, p in ids if not re.fullmatch(p + r"[1-9][0-9]*", i)]
    dup = sorted({i for i, _ in ids if [j for j, _ in ids].count(i) > 1})
    a_links = [(r[0].strip(), x) for r in arows for x in vm.split_list(r[4]) if x not in mids]
    covered = {x for r in arows for x in vm.split_list(r[4])}
    bare = sorted(m for m in mids if m not in covered)
    out["MS-8"] = (not bad and not dup and not a_links and not bare and bool(arows),
                   f"bad ids {bad}; duplicates {dup}; assumption links {a_links}; model rows on no assumption {bare}")
    p_links = [(r[0].strip(), x) for r in prows for x in vm.split_list(r[5]) if x not in mids | eids | lids]
    out["MS-9"] = (not p_links and bool(prows), f"player links {p_links}")
    return out, arows


def ids_of(tex, macro, nargs):
    return {r[0].strip() for r in vm.macro_calls(vm.strip_comments(tex), macro, nargs)}


GOOD_PA = r"""
\mrow{M1}{c}{$x$}{SmoothFit.smooth_fit}
\erow{E1}{c}{holds}{M1}{buffer}{x}
\lrow{L1}{b}{w}{q}{1}{M1}
\prow{P1}{bank}{c}{s}{l}{M1, E1, L1}
\arow{A1}{a}{plain words}{$x$}{M1}{r}
"""

BAD_PA = r"""
\mrow{M1}{c}{$x$}{SmoothFit.smooth_fit}
\mrow{M2}{c}{$x$}{SmoothFit.smooth_fit}
\prow{P1}{bank}{c}{s}{l}{X9}
\arow{A1}{a}{words with $x$}{$y$}{M7}{r}
\arow{A1}{a}{b}{$x$}{M1}{r}
"""

def main():
    lean = vm.lean_declarations()
    bibs = vm.bib_keys()
    lit = vm.lit_records()
    tex = open(os.path.join(ENH, "ENHANCEMENT.tex")).read()
    gids = map_names()

    print("=" * 72)
    print("ENHANCEMENT SKELETON (enhancement/ENHANCEMENT.tex)")
    print("=" * 72)
    print(f"      Lean declarations found: {len(lean)}; bib keys: {len(bibs)}; lit keys: {len(lit)}; map names: {len(gids)}")
    real, counts = vm.analyse(tex, lean, bibs, lit, gids)
    print(f"      rows: {counts}")
    names = {"MS-1": "row IDs well formed and unique",
             "MS-2": "introduction rows point to model or empirical rows only",
             "MS-3": "conclusion rows point to model, literature or empirical rows only",
             "MS-4": "every model row names audited Lean theorems and has one matching proof",
             "MS-5": "every literature row quotes its lit/ record verbatim",
             "MS-6": f"plain English at most {vm.PROSE_LIMIT:.0%} of every proof",
             "MS-7": "every empirical row has a verdict, existing model and map rows, and results files that reproduce, each after its pre-registrations",
             "MS-8": "assumption rows well formed, pointing to model rows, and every model row rests on an assumption",
             "MS-9": "player rows point to model, empirical or literature rows only"}
    mids, eids, lids = ids_of(tex, "mrow", 4), ids_of(tex, "erow", 6), ids_of(tex, "lrow", 6)
    pa, _ = player_assumption(tex, mids, eids, lids)
    real.update(pa)
    for k in sorted(real, key=lambda s: int(s.split("-")[1])):
        ok, detail = real[k]
        vm.check(f"{k} {names[k]}", ok, "" if ok else detail)

    print("-" * 72)
    print("CONTROLS  a well-formed sample must pass; a faulty one must fail")
    for label, sample, want in (("well-formed", GOOD_PA, True), ("faulty", BAD_PA, False)):
        m, e, l = ids_of(sample, "mrow", 4), ids_of(sample, "erow", 6), ids_of(sample, "lrow", 6)
        res, _ = player_assumption(sample, m, e, l)
        for k in ("MS-8", "MS-9"):
            vm.check(f"{k}-control {'passes' if want else 'fails'} on the {label} sample", res[k][0] == want)
    good, _ = vm.analyse(vm.GOOD, lean, bibs, lit)
    bad, _ = vm.analyse(vm.BAD, lean, bibs, lit)
    for k in sorted(good):
        vm.check(f"{k}-control passes on the parent's well-formed sample", good[k][0])
        vm.check(f"{k}-control fails on the parent's faulty sample", not bad[k][0])

    nf = sum(1 for _, ok in vm.results if not ok)
    print("=" * 72)
    print(f"ENHANCEMENT SUMMARY: {len(vm.results)} checks, {nf} failures")
    print("=" * 72)
    sys.exit(1 if nf else 0)


if __name__ == "__main__":
    main()
