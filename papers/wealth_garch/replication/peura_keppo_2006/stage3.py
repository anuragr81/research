import hashlib
import statistics
import sys
from multiprocessing import Pool

from scipy.stats import spearmanr

from stage2 import load_panel, load_rates, run_sample, solve, steady

WINDOWS = (("primary, ROA 2015-2024", 2015, 8), ("secondary, ROA 2005-2024", 2005, 15))


def bank_record(cert, panel_s, rates, first, need):
    s = {y: r for y, r in panel_s.items() if first - 1 <= y <= 2024}
    if not s or any(rec["BKCLASS"] != "N" for rec in s.values()):
        return None
    R, X = {}, {}
    for y, rec in s.items():
        if rec["RWAJT"] and rec["RBCT1J"] is not None and rec["RBCT2"] is not None:
            R[y] = rec["RWAJT"]
            X[y] = (rec["RBCT1J"] + rec["RBCT2"]) / R[y]
    years = range(first, 2025)
    roa = [s[y]["NETINC"] / R[y - 1] - rates[y] * X[y - 1] for y in years
           if y in s and y - 1 in R and s[y]["NETINC"] is not None and steady(s, y)]
    buf = [s[y]["RBCRWAJ"] / 100 - 0.08 for y in range(2015, 2025) if y in s and s[y]["RBCRWAJ"]]
    llp = [s[y]["ELNATR"] / s[y]["ASSET"] for y in years if y in s and s[y]["ELNATR"] is not None and s[y]["ASSET"]]
    assets = [s[y]["ASSET"] for y in years if y in s and s[y]["ASSET"]]
    growth = [s[y]["ASSET"] / s[y - 1]["ASSET"] - 1 for y in years
              if y in s and y - 1 in s and s[y]["ASSET"] and s[y - 1]["ASSET"]]
    if len(roa) < need or len(buf) < 5 or not llp:
        return None
    mu, sigma = statistics.fmean(roa), statistics.stdev(roa)
    if mu <= 0:
        return None
    return {"cert": cert, "mu": mu, "sigma": sigma, "buf": statistics.fmean(buf), "bufmax": max(buf),
            "llp": statistics.fmean(llp), "size": statistics.fmean(assets), "growth": statistics.fmean(growth),
            "n": len(roa)}


def spearman(banks, label):
    mean_llp = sum(b["llp"] for b in banks) / len(banks)
    high = [b for b in banks if b["llp"] > mean_llp]
    r1 = spearmanr([b["mu"] for b in banks], [b["sigma"] for b in banks])
    r3 = spearmanr([b["buf"] for b in high], [b["sigma"] for b in high])
    print(f"Spearman, sample {label} (reported, not used): mu-sigma {r1.statistic:+.2f} (p {r1.pvalue:.3f}); "
          f"high-provision buffer-sigma {r3.statistic:+.2f} (p {r3.pvalue:.3f})")


def main(panel_path, rates_path):
    for p in (panel_path, rates_path):
        print(f"{p} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    panel = load_panel(panel_path)
    rates = load_rates(rates_path)
    for label, first, need in WINDOWS:
        banks = [b for b in (bank_record(c, s, rates, first, need) for c, s in panel.items()) if b]
        print()
        print(f"######## WINDOW {label}: institutions {len(panel)}; national banks passing the filters {len(banks)}")
        with Pool() as pool:
            solved = pool.map(solve, sorted(banks, key=lambda b: int(b["cert"])))
        big = sorted(solved, key=lambda b: -b["size"])[:61]
        run_sample(solved, f"A (all national banks), {label}")
        run_sample(big, f"B (61 largest), {label}")
        print()
        spearman(solved, "A")
        spearman(big, "B")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
