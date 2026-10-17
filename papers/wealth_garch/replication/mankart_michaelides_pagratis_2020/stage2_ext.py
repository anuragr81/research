import hashlib
import sys
import time

from mmp_a1 import solve_a1
from mmp_grid import model_ext
from mmp_sim import simulate_counts
from stage2 import THEIRS, moments, verdict


def run(size, npts, etop, log):
    m = model_ext(size, npts, etop)
    t0 = time.time()
    V, pol, outer, diff, ok, fall = solve_a1(m, rel=1e-6, howard=500, max_outer=400, log=log)
    log(f"{size} {npts} points below 0.4, e_top {etop:g}: {'converged' if ok else 'NOT converged'} after {outer} sweeps, "
        f"last change {diff:.1e}, largest fall {fall:.1e}, {time.time() - t0:.0f}s")
    rec, counts, emax = simulate_counts(m, V, pol, size)
    mom = moments(rec)
    print(f"{size} {npts}: {'converged' if ok else 'not converged'} after {outer} sweeps; counts {counts}; "
          f"largest e {emax}; moments {mom}", flush=True)
    return mom, outer, diff, ok, counts


def main():
    etop = float(sys.argv[1])
    for f in ("mmp_model.py", "mmp_config.py", "mmp_a1.py", "mmp_grid.py", "mmp_sim.py", "stage2.py", "stage2_ext.py"):
        print(f"{f} sha256 {hashlib.sha256(open(f, 'rb').read()).hexdigest()}")
    results = {}
    for npts in (41, 61):
        for size in ("large", "small"):
            results[(size, npts)] = run(size, npts, etop, lambda s: print(s, file=sys.stderr, flush=True))
    for size in ("large", "small"):
        mom41, outer41, diff41, ok41, c41 = results[(size, 41)]
        mom61, outer61, diff61, ok61, c61 = results[(size, 61)]
        print(f"\n===== {size}, e_top {etop:g}: 41 points ({'converged' if ok41 else 'NOT converged'}, {outer41} sweeps, "
              f"{c41['all']['not finite']} non-finite and {c41['all']['e above top']} above-grid bank-quarters); "
              f"61 points ({'converged' if ok61 else 'NOT converged'}, {outer61} sweeps, {c61['all']['not finite']} "
              f"non-finite and {c61['all']['e above top']} above-grid bank-quarters)")
        holds = 0
        for name, theirs in THEIRS[size].items():
            a, b = mom41[name], mom61[name]
            ok = verdict(a, theirs, name) and ok41
            holds += ok
            moved = abs(b - a) > 0.10 * abs(a) if a != 0 else b != 0
            print(f"  {name:20s} ours {a:8.4f} [{theirs}] -> {'reproduces' if ok else 'differs'}; "
                  f"61 points {b:8.4f}{' (moves more than 10%: not converged)' if moved else ''}")
        print(f"  {size}: {holds} of {len(THEIRS[size])} moments reproduce")


if __name__ == "__main__":
    main()
