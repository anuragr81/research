import hashlib
import sys
import time

import numpy as np

from mmp_a1 import solve_a1
from mmp_grid import model_ext
from mmp_sim import simulate_counts

etop = float(sys.argv[1])
for f in ("mmp_model.py", "mmp_config.py", "mmp_a1.py", "mmp_grid.py", "mmp_sim.py", "stage2.py", "select_grid.py"):
    print(f"{f} sha256 {hashlib.sha256(open(f, 'rb').read()).hexdigest()}")
passed = True
for size in ("large", "small"):
    m = model_ext(size, 41, etop)
    t0 = time.time()
    V, pol, outer, diff, ok, fall = solve_a1(m, rel=1e-6, howard=500, max_outer=400,
                                             log=lambda s: print(s, file=sys.stderr, flush=True))
    np.savez(f"solved_{size}_41_e{etop:g}.npz", V=V, pol=pol)
    rec, counts, emax = simulate_counts(m, V, pol, size)
    print(f"{size}, e_top {etop:g} ({len(m['eg'])} equity points): {'converged' if ok else 'NOT converged'} after {outer} "
          f"sweeps, last change {diff:.1e}, largest fall {fall:.1e}, {time.time() - t0:.0f}s")
    for span in ("all", "last 500"):
        print(f"  {span} quarters: {counts[span]}, largest e {emax[span]:.4g}")
    size_ok = ok and counts["all"]["not finite"] == 0 and counts["all"]["e above top"] == 0
    print(f"  {size}: {'passes' if size_ok else 'fails'}; l above the loan grid in {counts['all']['l above top']} bank-quarters")
    passed &= size_ok
print(f"e_top {etop:g}: {'adopted' if passed else 'not adopted'}")
