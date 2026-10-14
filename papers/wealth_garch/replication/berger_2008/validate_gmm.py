import contextlib
import hashlib
import io
import os
import re
import subprocess
import sys

import pandas as pd
from pydynpd import regression

from gmm import system_gmm

CACHE = os.path.expanduser("~/.cache/wealth_garch")
PDF = os.path.join(CACHE, "roodman_2008_wp103.pdf")
DATA = os.path.join(CACHE, "abdata_r13.dta")


def page(p):
    return subprocess.run(["pdftotext", "-layout", "-f", str(p), "-l", str(p), PDF, "-"], capture_output=True, text=True).stdout


def fit(command, df):
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        regression.abond(command, df, ["id", "year"])
    text = buf.getvalue()
    coef = float(re.search(r"\|\s*L1\.n\s*\|\s*(-?[\d.]+)\s*\|\s*([\d.]+)", text).group(1))
    se = float(re.search(r"\|\s*L1\.n\s*\|\s*(-?[\d.]+)\s*\|\s*([\d.]+)", text).group(2))
    nobs = int(re.search(r"Number of obs = (\d+)", text).group(1))
    ninst = int(re.search(r"Number of instruments = (\d+)", text).group(1))
    return coef, se, nobs, ninst


def main():
    for f in (PDF, DATA):
        print(f"{os.path.basename(f)} sha256 {hashlib.sha256(open(f, 'rb').read()).hexdigest()}")
    df = pd.read_stata(DATA)
    yrs = " ".join(f"yr{y}" for y in range(1979, 1985))
    cmd = f"n L1.n L2.n w L1.w k L1.k L2.k ys L1.ys L2.ys {yrs} | gmm(n, 2:.) iv(w L1.w k L1.k L2.k ys L1.ys L2.ys {yrs}) | nolevel"
    one = fit(cmd + " onestep", df)
    two = fit(cmd, df)
    p29, p30 = page(29), page(30)
    print(f"Roodman p. 29 prints one-step difference GMM L1.n .6862261 (se .1445943): "
          f"{'.6862261' in p29 and '.1445943' in p29}")
    print(f"Roodman p. 30 prints 'the two-step one of 0.629': {'the two-step one of 0.629' in p30}")
    print(f"pydynpd one-step: L1.n {one[0]:.7f} se {one[1]:.7f}, {one[2]} obs, {one[3]} instruments")
    print(f"pydynpd two-step: L1.n {two[0]:.7f}")
    print(f"one-step matches to 7 decimals: {f'{one[0]:.7f}' == '0.6862261' and f'{one[1]:.7f}' == '0.1445943'}")
    print(f"two-step matches to 3 decimals: {f'{two[0]:.3f}' == '0.629'}")
    yr = [f"yr{y}" for y in range(1979, 1985)]
    reg = ["L1.n", "L2.n", "w", "L1.w", "k", "L1.k", "L2.k", "ys", "L1.ys", "L2.ys"] + yr
    ours = system_gmm(df, "n", reg, [("n", 2, 99)], ["w", "L1.w", "k", "L1.k", "L2.k", "ys", "L1.ys", "L2.ys"] + yr, level=False)
    print(f"gmm.py difference GMM: L1.n one-step {ours['b1'][0]:.7f} se {ours['se1'][0]:.7f}, two-step {ours['b2'][0]:.7f}, "
          f"{ours['nobs']} obs, {ours['ninst']} instruments")
    match = (round(ours['b1'][0], 7) == 0.6862261 and round(ours['se1'][0], 7) == 0.1445943
             and round(ours['b2'][0], 3) == 0.629)
    print(f"gmm.py matches Roodman: {match}")
    sys_cmd = "n L1.n w L1.w k ys | gmm(n, 2:4) gmm(w, 2:3) iv(k ys)"
    ref = {}
    for opt in ("onestep", ""):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            regression.abond(sys_cmd + " | " + opt, df, ["id", "year"])
        rows = re.findall(r"\|\s*(L1\.n|w|L1\.w|k|ys|_con)\s*\|\s*(-?[\d.]+)\s*\|\s*([\d.]+)", buf.getvalue())
        ref[opt or "twostep"] = [(float(b), float(s)) for _, b, s in rows]
    mine = system_gmm(df, "n", ["L1.n", "w", "L1.w", "k", "ys"], [("n", 2, 4), ("w", 2, 3)], ["k", "ys"])
    gaps = [abs(a - b) for (b, s), a in zip(ref["onestep"], mine["b1"])] + [abs(a - s) for (b, s), a in zip(ref["onestep"], mine["se1"])]
    gaps += [abs(a - b) for (b, s), a in zip(ref["twostep"], mine["b2"])] + [abs(a - s) for (b, s), a in zip(ref["twostep"], mine["se2"])]
    print(f"gmm.py system GMM against pydynpd: {len(gaps)} coefficients and standard errors, largest gap {max(gaps):.1e}, "
          f"{mine['nobs']} obs, {mine['ninst']} instruments, Hansen {mine['hansen']:.3f}")
    print(f"gmm.py matches pydynpd to 7 decimals: {max(gaps) < 5e-8}")


if __name__ == "__main__":
    main()
