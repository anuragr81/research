import csv
import hashlib
import math
import os
import statistics
from functools import partial
from multiprocessing import Pool

import facts
import predict_pk
from judged_risk import CACHE, FIRMS, PRICES, RHO, holders, listed, mean_ratio, paths, sha
from sample import ANNUAL, QUARTERLY, R_REF, RATES, samples
from stage1_barriers import u0
from stage2 import solve
from window import load_years, observed, quarters, ratio

NEW_PRICES = os.path.join(CACHE, "stooq_20261024")
START_TRAIN = "20041231"
ZEROED = {"FHN": ("2008-07-14",), "OSBC": ("2010-09-20", "2013-04-25")}

NEW = [
    ("1883693", "BOKF", "bokf_us_d.csv", "08245d8e7b738b77fd4f6e9c59660b28350273a67c338f2e13e30b4482524065", "BOK Financial Corporation - Common Stock"),
    ("1130249", "CAC", "cac_us_d.csv", "c506541ca857b4c10a2cb25489e962caeb6fca48b2b5ef7ef6f3693e303249f8", "Camden National Corporation - Common Stock"),
    ("1048867", "CBU", "cbu_us_d.csv", "db0784bc38d786fc17fbb36f54c988ad5e875da0262e94118f4d4c71714ed4f8", "Community Financial System, Inc. Common Stock"),
    ("1076262", "CHCO", "chco_us_d.csv", "626e6c294f2dd40aa668125f4f173c46cbd0dda1c03b9417f1ffa619ba3e19cd", "City Holding Company - Common Stock"),
    ("1071276", "FFBC", "ffbc_us_d.csv", "8c9e7e9cfcbb1dbdb46f9746b50009b123e85ba4a019091fb1f4294ae6534174", "First Financial Bancorp. - Common Stock"),
    ("1102312", "FFIN", "ffin_us_d.csv", "90a6f86134fd1d6fee012696a555e4cf7f53fcc1edcb3ef0905672659c0c2da8", "First Financial Bankshares, Inc. - Common Stock"),
    ("1094640", "FHN", "fhn_us_d.csv", "6b5de821054df589cb95aa5fa6ca6d23ff9061c4920c88d7bd66516278806cd8", "First Horizon Corporation Common Stock"),
    ("3005332", "FNB", "fnb_us_d.csv", "1db223eaaa9b7ab8be4b720e936de40d774bb248ed060d43f8f8bf82abdf6c17", "F.N.B. Corporation Common Stock"),
    ("1208559", "FRME", "frme_us_d.csv", "10d8504dea6c7e3560d678104a67443a132579dc5935958a8ba661ee1b0a057c", "First Merchants Corporation - Common Stock"),
    ("1139279", "NBTB", "nbtb_us_d.csv", "34f6bd1e145dc5fa100a856fdac1700b4ccc498c3240d5a5916f15ed8ec54425", "NBT Bancorp Inc. - Common Stock"),
    ("1098303", "ONB", "onb_us_d.csv", "3d37aa56fde1c7d37fb649ae3e64b1a7889625032a0a7d7657cf1153d6038183", "Old National Bancorp - Common Stock"),
    ("1206911", "OSBC", "osbc_us_d.csv", "08ced618c030268d1c798e6b7f3285a1ff28d9b039a828ad010a209521332c6b", "Old Second Bancorp, Inc. - Common Stock"),
    ("1070578", "PEBO", "pebo_us_d.csv", "1f535e6ab1503ecb57884da0def70e73f2d234114315303b7d137250d60e3613", "Peoples Bancorp Inc. - Common Stock"),
    ("1142336", "PRK", "prk_us_d.csv", "a7678bc8f69e051155aa2dd2093b24617216714292f9059fdc28f576f1498718", "Park National Corporation Common Stock"),
    ("1094828", "SFNC", "sfnc_us_d.csv", "99b2c0b93f8380b90a2cbd8663ec813c502aef7a748b106592f8a0bacbebf978", "Simmons First National Corporation - Common Stock"),
    ("2706735", "TCBI", "tcbi_us_d.csv", "a7efff7b01e925e2454fb460398ad28900067bfcd646cc8a6e330ae58ee624c2", "Texas Capital Bancshares, Inc. Common Stock"),
    ("1208595", "THFF", "thff_us_d.csv", "b52d29f5afd6699536ec51d3852a112fc8eea8c0f0c1a27b9727a0430f3f4bc0", "First Financial Corporation  - Common Stock"),
    ("1079562", "TRMK", "trmk_us_d.csv", "41ecd1882b3e053b7aaa99a20bcc637dfec52a737897e66c35ac88e69f46dde6", "Trustmark Corporation - Common Stock"),
]


def sigma_e(path, drop=()):
    closes = []
    with open(path) as fh:
        for r in csv.DictReader(fh):
            if "2005-01-01" <= r["Date"] <= "2014-12-31":
                closes.append((r["Date"], float(r["Close"])))
    changes = [0.0 if d in drop else math.log(c / closes[i - 1][1]) for i, (d, c) in enumerate(closes) if i > 0]
    return math.sqrt(252) * statistics.stdev(changes), len(changes), closes[0][0]


def show(label, res):
    print(label)
    for k, (hit, n) in res.items():
        print(f"  {k}: {hit}/{n} = {100 * hit / n:.1f}%" if n else f"  {k}: 0/0 = n/a")


def pct(res):
    return {k: 100 * h / n for k, (h, n) in res.items() if n}


def miss(label, pred, obs):
    d1, d2 = abs(pred["F1"] - obs["F1"]), abs(pred["F2"] - obs["F2"])
    print(f"  {label}: |dF1| {d1:.1f} + |dF2| {d2:.1f} = {d1 + d2:.1f} points; F1 within 10: {d1 <= 10}; F2 within 10: {d2 <= 10}")
    return d1 + d2


def baseline(pool, banks):
    out = pool.map(predict_pk.simulate, pool.map(solve, banks))
    eps = [e for o in out for e in o[4]]
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out)),
            "F3": (sum(e[0] for e in eps), len(eps)), "F4": (sum(e[1] for e in eps), len(eps))}


def enhanced(pool, banks, minimum, key="sB"):
    out = pool.map(partial(paths, minimum=minimum, key=key), banks)
    return {"F1": (sum(o[0] for o in out), sum(o[1] for o in out)), "F2": (sum(o[2] for o in out), sum(o[3] for o in out))}, \
        sum(o[4] for o in out)


def score_block(pool, label, banks, q, decide):
    print(f"\n----- {label}: {len(banks)} banks; start 2014Q4, 2015-2024")
    obs_raw = facts.observed(banks, q)
    show("observed, 2015-2024", obs_raw)
    base_raw = baseline(pool, banks)
    show("baseline (Peura and Keppo), minimum 8%", base_raw)
    res8, fail8 = enhanced(pool, banks, 0.08)
    show(f"(B) equity volatility, minimum 8%: {fail8} start failed", res8)
    resR, failR = enhanced(pool, banks, R_REF)
    show(f"(B) equity volatility, minimum at R: {failR} start failed", resR)
    resZ, _ = enhanced(pool, banks, 0.08, key="sB_zeroed")
    show("(B) with the three out-of-crisis changes set to zero, minimum 8%", resZ)
    obs = pct(obs_raw)
    print("misses against observed F1 and F2 (points)")
    mb = miss("baseline, minimum 8%", pct(base_raw), obs)
    me = miss("(B), minimum 8%", pct(res8), obs)
    miss("(B), minimum at R", pct(resR), obs)
    miss("(B) zeroed, minimum 8%", pct(resZ), obs)
    if decide:
        print(f"  rule (the {len(banks)} new banks of sample B, minimum 8%): (B) {'beats' if me < mb else 'does not beat'} the baseline ({me:.1f} against {mb:.1f} points)")


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    names = listed()
    firms = {}
    print("\nnew parents: RSSD, ticker, listed name, first date in 2005-2014, changes, sigma_E, sigma_E zeroed")
    for rssd, tick, fname, digest, lname in NEW:
        path = os.path.join(NEW_PRICES, fname)
        assert sha(path) == digest, fname
        assert names[tick] == lname, tick
        se, n, first = sigma_e(path)
        sz = sigma_e(path, ZEROED.get(tick, ()))[0]
        firms[rssd] = (tick, se, sz, True)
        print(f"  {rssd} {tick:5s} {lname} | {first} | {n} | {se:.4f} | {sz:.4f}")
    for rssd, fed, tick, fname, digest, lname in FIRMS:
        path = os.path.join(PRICES, fname)
        assert sha(path) == digest, fname
        se = sigma_e(path)[0]
        firms[rssd] = (tick, se, se, False)
    samp, q = samples()
    train = load_years(range(2004, 2015))
    with Pool() as pool:
        for name in ("B", "A"):
            hc = holders({b["cert"] for b in samp[name]})
            banks = []
            for b in samp[name]:
                f = firms.get(hc.get(b["cert"], ""))
                if f is None:
                    continue
                X = mean_ratio(train, b["cert"])
                banks.append(dict(b, x0_train=b["x0"], X=X, tick=f[0], sB=f[1] * X, sB_zeroed=f[2] * X, new=f[3]))
            new = [b for b in banks if b["new"]]
            print(f"\n===== sample {name}: {len(banks)} covered banks, {len(new)} new")
            for b in sorted(new, key=lambda b: -b["size"]):
                print(f"  {b['cert']:>6s} {b['tick']:5s} x0 {b['x0']:.4f} X {b['X']:.4f} sigma_i {b['sigma']:.5f} sigma_B {b['sB']:.5f} | {b['sB'] / b['sigma']:.2f}")
            bar = sorted(u0(b["mu"], b["sB"], RHO) / b["sigma"] for b in new)
            print(f"  barrier u0 over sigma_i, new banks: min {bar[0]:.1f}, median {statistics.median(bar):.1f}, max {bar[-1]:.1f}")
            score_block(pool, f"sample {name}, new banks", new, q, decide=(name == "B"))
            score_block(pool, f"sample {name}, all covered banks", banks, q, decide=False)
            if name == "B":
                tr = [dict(b, x0_train=ratio(train, b["cert"], START_TRAIN)) for b in new if ratio(train, b["cert"], START_TRAIN) is not None]
                print(f"\n----- sample B, new banks, training window 2005-2014 (reported): {len(tr)} banks with a 2004Q4 buffer")
                show("observed, 2005-2014", observed(tr, train, quarters(2005, 2014), R_REF))
                res8, fail8 = enhanced(pool, tr, 0.08)
                show(f"(B) equity volatility, minimum 8%: {fail8} start failed", res8)
                resR, failR = enhanced(pool, tr, R_REF)
                show(f"(B) equity volatility, minimum at R: {failR} start failed", resR)


if __name__ == "__main__":
    main()
