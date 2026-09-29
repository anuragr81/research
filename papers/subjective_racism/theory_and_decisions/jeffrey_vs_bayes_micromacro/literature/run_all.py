#!/usr/bin/env python3
"""
Run every literature check script (literature/*/sympy/*.py) and report a
summary.  These check the cited papers' own claims, not Paper B's; the
paper's own suite is ../sympy/run_all.py.  Exit status is 0 iff every script
exits 0.

    python3 run_all.py            # all scripts
    python3 run_all.py bhw cripps # only scripts whose path matches

Excluded: bohren_imas_rosenberg2019/sympy/check_reversal.py, kept as the
record of a derivation that was attempted and blocked; it exits 1 by design
(see notes/citation_audit.md, item D4).
"""
import glob
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
EXCLUDED = {"bohren_imas_rosenberg2019/sympy/check_reversal.py"}


def main(argv):
    picks = [p.lower() for p in argv[1:]]
    scripts = sorted(
        os.path.relpath(p, HERE)
        for p in glob.glob(os.path.join(HERE, "*", "sympy", "*.py"))
    )
    results = []
    for rel in scripts:
        if rel in EXCLUDED:
            continue
        if picks and not any(p in rel.lower() for p in picks):
            continue
        t0 = time.time()
        proc = subprocess.run(
            [sys.executable, "-B", os.path.basename(rel)],
            cwd=os.path.join(HERE, os.path.dirname(rel)),
            capture_output=True,
            text=True,
        )
        ok = proc.returncode == 0
        results.append((rel, ok, time.time() - t0))
        if not ok:
            print(f"--- {rel} (exit {proc.returncode})")
            print(proc.stdout[-2000:])
            print(proc.stderr[-2000:])

    print("\n" + "=" * 78)
    print("SUMMARY (literature checks)")
    print("=" * 78)
    for rel, ok, dt in results:
        print(f"  {'PASS' if ok else 'FAIL'}  {rel:<66} {dt:6.1f}s")
    n_ok = sum(1 for _, ok, _ in results if ok)
    print(f"\n  {n_ok}/{len(results)} scripts passed")
    return 0 if n_ok == len(results) else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
