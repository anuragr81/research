import hashlib
import sys
import time

import numpy as np

from mmp_a1 import solve_a1
from mmp_config import model

size, npts = sys.argv[1], int(sys.argv[2])
t0 = time.time()
m = model(size, ne=npts, nl=npts)
V, pol, outer, diff, ok, fall = solve_a1(m, rel=1e-6, howard=500, max_outer=400, log=lambda s: print(s, file=sys.stderr, flush=True))
name = f"solved_{size}_{npts}.npz"
np.savez(name, V=V, pol=pol)
print(f"{size} {npts}x{npts}: {'converged' if ok else 'NOT converged'} after {outer} sweeps, last change {diff:.1e}, "
      f"largest fall {fall:.1e}, {time.time() - t0:.0f}s")
print(f"V sha256 {hashlib.sha256(V.tobytes()).hexdigest()}")
print(f"pol sha256 {hashlib.sha256(pol.tobytes()).hexdigest()}")
print(f"saved {name}")
