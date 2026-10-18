import hashlib
from functools import partial
from multiprocessing import Pool

from sample import ANNUAL, RATES, R_REF, samples
from stage2 import DELTA, solve
from window import barrier_paths, load_years, observed, pooled, quarters, ratio, show

SEED = 20261018
START = "20041231"


def one(minimum, b):
    return barrier_paths(b, minimum, R_REF, b["x0_train"], b["asset_rwa_train"], DELTA, SEED)


def main():
    for p in [ANNUAL, RATES]:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, _ = samples()
    q = load_years(range(2004, 2015))
    qs = quarters(2005, 2014)
    for name in ("B", "A"):
        banks = []
        for b in samp[name]:
            x0 = ratio(q, b["cert"], START)
            if x0 is not None:
                rec = q[b["cert"]][START]
                banks.append(dict(b, x0_train=x0, asset_rwa_train=rec["ASSET"] / rec["RWAJT"]))
        print(f"\n===== sample {name}: {len(samp[name])} banks, {len(samp[name]) - len(banks)} without a 2004Q4 buffer, "
              f"{len(banks)} used")
        print("observed, 2005-2014")
        show(observed(banks, q, qs, R_REF))
        with Pool() as pool:
            solved = pool.map(solve, banks)
            for label, minimum in (("(a) baseline, minimum 8%", 0.08), ("(b) proxy, minimum at R = 10.5%", 0.105)):
                out = pool.map(partial(one, minimum), solved)
                print(f"{label}: {sum(o[5] for o in out)} banks start at or below the minimum")
                show(pooled(out))


if __name__ == "__main__":
    main()
