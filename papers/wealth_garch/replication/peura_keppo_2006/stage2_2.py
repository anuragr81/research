import hashlib
import sys
from multiprocessing import Pool

from scipy.stats import spearmanr

from stage2 import bank_record, load_panel, load_rates, run_sample, solve, steady


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
    before = {b["cert"] for b in (bank_record(c, s, rates) for c, s in panel.items()) if b}
    banks = [b for b in (bank_record(c, s, rates, screen=True) for c, s in panel.items()) if b]
    removed = sum(1 for c in before for y in range(1985, 2003)
                  if y in panel[c] and y - 1 in panel[c] and not steady(panel[c], y))
    print(f"institutions {len(panel)}; run 1 sample A {len(before)} banks; ROA years removed by the screen "
          f"among them {removed}; banks below 15 ROA values because of the screen "
          f"{len(before - {b['cert'] for b in banks})}")
    print(f"national banks passing the filters with the screen (sample A) {len(banks)}")
    with Pool() as pool:
        solved = pool.map(solve, sorted(banks, key=lambda b: int(b["cert"])))
    big = sorted(solved, key=lambda b: -b["size"])[:61]
    run_sample(solved, "A (all national banks)")
    run_sample(big, "B (61 largest)")
    print()
    spearman(solved, "A")
    spearman(big, "B")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
