"""
level_sensitivity.py

Reads the resultK_*.csv files produced by headless_K_single.m (one file
per K value in the sweep) and reports how the two threshold LEVELS --
recap_edge (the trigger, x_L) and y_star (the dividend barrier) -- respond
to K. This is the reverse-direction check the near-orthogonality claim is
missing: KSW already shows the GAP (trig_to_target) responds to K; this
checks whether the LEVELS also move, which the paper does not currently
report either way.

Does NOT run any solve. Requires real resultK_*.csv files, produced by
running headless_K_single.m at K in {0, 0.001, 0.005, 0.01, 0.02, 0.05}
with lambda_S_arg=1 and the REAL settings (N=301, max_iter=3000) --
the settings actually used for the KSW numbers in PROOFS_v2.tex, not
the reduced smoke-test settings used to check this script runs.

Usage:
    python3 level_sensitivity.py resultK_0.00000.csv resultK_0.00100.csv \
        resultK_0.00500.csv resultK_0.01000.csv resultK_0.02000.csv \
        resultK_0.05000.csv
"""

import csv
import math
import sys


def load_rows(paths):
    rows = []
    for p in paths:
        with open(p, newline="") as f:
            r = next(csv.DictReader(f))
            rows.append(r)
    rows.sort(key=lambda r: float(r["K"]))
    return rows


def report(rows):
    print(f"{'K':>10} {'converged':>10} {'recap_edge':>12} {'y_star':>10} {'gap':>10}")
    for r in rows:
        print(f"{float(r['K']):>10.5f} {int(r['converged']):>10} "
              f"{float(r['recap_edge']):>12.6f} {float(r['y_star']):>10.6f} "
              f"{float(r['trig_to_target']):>10.6f}")

    if not all(int(r["converged"]) for r in rows):
        print("\nWARNING: at least one run did not converge. Do not trust "
              "slopes computed across a mix of converged and unconverged "
              "points -- rerun the unconverged ones with more iterations "
              "before proceeding, exactly as the paper's own K=0.05 case "
              "was flagged as degenerate at N=301.")

    nz = [r for r in rows if float(r["K"]) > 0]
    if len(nz) >= 2:
        logK = [math.log(float(r["K"])) for r in nz]
        for field, label in [("recap_edge", "trigger level"), ("y_star", "barrier level")]:
            vals = [float(r[field]) for r in nz]
            # simple two-point and full-OLS slope, log(level) on log(K) --
            # only meaningful if levels are monotone and one-signed; report
            # raw range first since a near-flat level makes a log-log slope
            # meaningless (division by a near-zero denominator in log space)
            spread = max(vals) - min(vals)
            print(f"\n{label}: range over the nonzero-K sweep = {spread:.6f} "
                  f"(min {min(vals):.6f}, max {max(vals):.6f})")
            print(f"  compare this spread to the trig_to_target spread "
                  f"({max(float(r['trig_to_target']) for r in nz) - min(float(r['trig_to_target']) for r in nz):.6f}) "
                  f"-- the near-orthogonality claim needs this to be small "
                  f"relative to that, not merely nonzero.")

    print("\nThis script reports numbers. It does not decide whether they "
          "are 'small enough' to support near-orthogonality -- that is a "
          "judgement call for the write-up, made after seeing real numbers, "
          "not before.")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    rows = load_rows(sys.argv[1:])
    report(rows)
