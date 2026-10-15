import hashlib

import numpy as np

from mmp_config import model
from mmp_keep import solve_keep
from mmp_model import evaluate, optimise

SWEEPS, STALL = 60, 30


def small(size):
    return model(size, ne=21, nl=21, nw=7, ng=3)


def args_of(m):
    return (m["eg"], m["lg"], m["wg"], m["Pw"], m["Pb"], m["gn"], m["gw"], m["rD"], m["rL"], m["rS"], m["par"])


def run_old(m, howard, detail):
    vd = m["par"][13]
    args = args_of(m)
    V = np.full((len(m["eg"]), len(m["lg"]), len(m["wg"]), m["Pb"].shape[0]), vd)
    prev = None
    changes, short, moving = [], [], []
    for k in range(SWEEPS):
        Vopt, pol = optimise(V, *args, m["qx"], m["qn"], m["qf"], m["nmax"], m["fmax"], m["fix_nf"], m["nm_iters"])
        span = float(Vopt.max() - vd)
        if prev is not None:
            gap = evaluate(V, prev, *args) - Vopt
            short.append((int(np.sum(gap > 1e-9 * span)), float(gap.max() / span)))
        Vn = Vopt
        for _ in range(howard):
            Vn = evaluate(Vn, pol, *args)
        d = Vn - V
        changes.append(float(np.max(np.abs(d))))
        if detail and k >= STALL:
            moving.append((int(np.sum(np.abs(d) > 1e-3 * span)), d, prev, pol, span))
        V, prev = Vn, pol
    return changes, short, moving


def show_changes(label, changes):
    print(f"  {label}: largest change by sweep")
    for k in range(0, SWEEPS, 10):
        print("   " + " ".join(f"{c:.3e}" for c in changes[k:k + 10]))


def main():
    for f in ("mmp_model.py", "mmp_config.py", "mmp_keep.py", "diagnose_stage2.py"):
        print(f"{f} sha256 {hashlib.sha256(open(f, 'rb').read()).hexdigest()}")
    for size in ("large", "small"):
        m = small(size)
        vd = m["par"][13]
        print(f"\n===== {size}: 21x21 equity and loan points, 7 write-off nodes, 3 quadrature nodes; v^d {vd:.4e}")
        changes, short, moving = run_old(m, 500, True)
        print("D1. Pre-registered solver, 500 evaluation steps")
        show_changes("500 steps", changes)
        print("D2. Cells where the optimiser falls short of the previous policy (more than 1e-9 of the value range)")
        for k in range(0, len(short), 10):
            print("   " + " ".join(f"{n:4d}" for n, _ in short[k:k + 10]))
        worst = max(short, key=lambda t: t[1])
        print(f"  sweeps with a shortfall: {sum(n > 0 for n, _ in short)} of {len(short)}; "
              f"largest shortfall {worst[1]:.3e} of the value range")
        print(f"D3. Cells moving by more than 1e-3 of the value range, sweeps {STALL} to {SWEEPS - 1}")
        counts = [c for c, _, _, _, _ in moving]
        print(f"  per sweep: min {min(counts)}, max {max(counts)}")
        c, d, prev, pol, span = moving[-1]
        idx = np.argsort(-np.abs(d), axis=None)[:10]
        for flat in idx:
            ie, il, iw, ib = np.unravel_index(flat, d.shape)
            a, b = prev[ie, il, iw, ib], pol[ie, il, iw, ib]
            print(f"   e {m['eg'][ie]:.3f} l {m['lg'][il]:.3f} w {100 * m['wg'][iw]:+.3f}% b {ib}: change {d[ie, il, iw, ib] / span:+.3e} "
                  f"of range; policy (x, n, f) before ({a[0]:.4f}, {a[1]:+.4f}, {a[2]:.4f}) "
                  f"after ({b[0]:.4f}, {b[1]:+.4f}, {b[2]:.4f})")
        print("D4. Pre-registered solver with fewer evaluation steps")
        for howard in (0, 20):
            ch, _, _ = run_old(m, howard, False)
            show_changes(f"{howard} steps", ch)
        print("D5. Amendment A1 (keep the previous policy), 500 evaluation steps, tolerance 1e-6 of the value range")
        log = []
        V, pol, outer, diff, ok, fall = solve_keep(m, rel=1e-6, howard=500, max_outer=400, log=log.append)
        print(f"  {'converged' if ok else 'not converged'} at sweep {outer}, last change {diff:.3e}; "
              f"largest fall in any cell {fall:.3e} of the value range")
        print("  " + "\n  ".join(log[:5] + ["..."] + log[-5:] if len(log) > 10 else log))


if __name__ == "__main__":
    main()
