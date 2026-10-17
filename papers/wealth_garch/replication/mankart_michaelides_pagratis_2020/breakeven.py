from mmp_config import TABLE2, TABLE4, TAU

for size in ("large", "small"):
    t2, t4 = TABLE2[size], TABLE4[size]
    for state, name in ((0, "expansion"), (1, "recession")):
        rS = (t2["rD"][state] + t2["liquid_spread"][state]) / 100
        required = 1 / t4["beta"] - 1
        e_star = (required - (1 - TAU) * rS) / ((1 - TAU) * 2 * t4["phiE"])
        print(f"{size} {name}: phi_E {t4['phiE']}, beta {t4['beta']}, r_S {rS:.4f}, 1/beta - 1 {required:.4f}, "
              f"break-even e {e_star:.3f}")
for size, eq, dep in (("large", 0.065, 0.638), ("small", 0.067, 0.891)):
    print(f"{size}: their Table 5 equity/assets {eq} over deposits/assets {dep} gives e {eq / dep:.3f}")
