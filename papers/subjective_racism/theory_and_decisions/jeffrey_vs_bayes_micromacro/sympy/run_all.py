#!/usr/bin/env python3
"""
Run every SymPy verification script for PAPER_B_MANUSCRIPT.tex and report a
summary.  Exit status is 0 iff every check in every script passed.

    python3 run_all.py            # all scripts
    python3 run_all.py DIV PRO    # only scripts whose name matches
"""
import importlib
import sys
import time

SCRIPTS = [
    ("verify_IMM",    "Proposition IMM -- exact immunity"),
    ("verify_DIV",    "Proposition DIV -- micro divergence (+ Appendix A.1)"),
    ("verify_ASC",    "Lemma ASC       -- individual association immunity"),
    ("verify_SEP",    "Lemma SEP       -- separability (+ Appendix A.2)"),
    ("verify_SCR",    "Lemma SCR       -- first-order score gap"),
    ("verify_DEC",    "Proposition DEC -- belief-level decoupling"),
    ("verify_DRF",    "Proposition DRF -- marginal-probability drift"),
    ("verify_LOS",    "Theorem LOS     -- surplus-weighted loss (+ Sec 4.4, App A.3)"),
    ("verify_SHR",    "Proposition SHR -- sequence-affected share"),
    ("verify_PRO",    "Proposition PRO -- uniqueness of the protected statistic"),
    ("verify_ORD",    "Proposition ORD -- between-order contrast (benchmark-free)"),
    ("check_zero_slope_identification",
     "Anchoring family -- zero-slope mechanism identification"),
    ("verify_tables", "Tables 1 and 2  -- the order summary, end to end"),
    ("verify_example", "Worked example  -- every number printed in Setup"),
]


def main(argv):
    picks = argv[1:]
    results = []
    for mod, desc in SCRIPTS:
        if picks and not any(p.lower() in mod.lower() for p in picks):
            continue
        t0 = time.time()
        m = importlib.import_module(mod)
        ok = m.main()
        results.append((mod, desc, ok, time.time() - t0))

    print("\n" + "=" * 78)
    print("SUMMARY")
    print("=" * 78)
    for mod, desc, ok, dt in results:
        print(f"  {'PASS' if ok else 'FAIL'}  {desc:<62} {dt:6.1f}s")
    n_ok = sum(1 for *_, ok, _ in results if ok)
    print(f"\n  {n_ok}/{len(results)} scripts passed")
    return 0 if n_ok == len(results) else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
