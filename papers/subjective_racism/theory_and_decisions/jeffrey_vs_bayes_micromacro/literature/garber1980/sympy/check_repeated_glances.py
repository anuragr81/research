"""
Garber (1980), "Field and Jeffrey Conditionalization" -- checks of the paper's
numbers. Each check prints PASS/FAIL; the script exits 0 iff all pass.

Field's update with a fixed alpha multiplies the odds of E by e^{2 alpha},
the likelihood ratio; for Garber's .3 -> .4 glance e^{2 alpha} = 14/9
exactly, so every value below is an exact rational.

Lean: lean/Literature/Garber.lean (symlinked in ../lean/).
"""
import sys

import sympy as sp

R = sp.Rational
FAILS = []
HALF_ULP = R(1, 20000)       # "to four places"


def check(name, ok, detail=""):
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
    if not ok:
        FAILS.append(name)


def alpha_of(p, q):                                    # Garber (4) = Field (4)
    return R(1, 2) * sp.log((q / p) / ((1 - q) / (1 - p)))


def field_update(p, a):                                # Garber (3) = Field (5)
    return p * sp.exp(a) / (p * sp.exp(a) + (1 - p) * sp.exp(-a))


def by_odds(p0, lr, n):
    """P_n(E) from the prior p0 and the likelihood ratio lr = e^{2 alpha}."""
    o = p0 / (1 - p0) * lr ** n
    return o / (1 + o)


print("First example: .3 -> .4, repeated (pp. 143-144)")
p0, q0 = R(3, 10), R(4, 10)
alpha = alpha_of(p0, q0)
lr = sp.nsimplify(sp.simplify(sp.exp(2 * alpha)))
check("e^{2 alpha} = 14/9 exactly (the likelihood ratio of one glance)", lr == R(14, 9), f"got {lr}")
check("alpha = .2209 to four places (p. 143)", abs(sp.N(alpha, 30) - R(2209, 10000)) < HALF_ULP,
      f"alpha = {sp.N(alpha, 10)}")

# the iteration itself, symbolically in alpha, agrees with the odds form
vals = [p0]
for _ in range(9):
    vals.append(sp.nsimplify(sp.simplify(field_update(vals[-1], alpha))))
check("iterating eq. (3) nine times equals the odds form (14/9)^n exactly",
      all(sp.simplify(v - by_odds(p0, R(14, 9), n)) == 0 for n, v in enumerate(vals)))
check("closed form P_n = 3*14^n / (3*14^n + 7*9^n)",
      all(v == R(3 * 14 ** n, 3 * 14 ** n + 7 * 9 ** n) for n, v in enumerate(vals)))

garber = [R(3, 10), R(4, 10), R(5091, 10000), R(6173, 10000), R(7150, 10000),
          R(7961, 10000), R(8586, 10000), R(9043, 10000), R(9363, 10000), R(9581, 10000)]
for n, (v, g) in enumerate(zip(vals, garber)):
    check(f"P_{n}(E) = {float(g):.4f} to four places (table, p. 144)", abs(v - g) < HALF_ULP,
          f"exact value {float(v):.6f}")
check("Garber's prose '.5019' (p. 143) is a typo: P_2(E) is not .5019 to four places",
      abs(vals[2] - R(5019, 10000)) > HALF_ULP)
check("after nine repetitions P_9(E) > .95 ('virtually certain')", vals[9] > R(95, 100))
check("after eight repetitions P_8(E) < .95", vals[8] < R(95, 100))

print("Second example: .3 -> .5, five repetitions (p. 144)")
p0b, q0b = R(3, 10), R(5, 10)
alphab = alpha_of(p0b, q0b)
lrb = sp.nsimplify(sp.simplify(sp.exp(2 * alphab)))
check("e^{2 alpha} = 7/3 exactly", lrb == R(7, 3), f"got {lrb}")
seq = [p0b]
for _ in range(5):
    seq.append(sp.nsimplify(sp.simplify(field_update(seq[-1], alphab))))
check("iteration equals the odds form (7/3)^n exactly",
      all(sp.simplify(v - by_odds(p0b, R(7, 3), n)) == 0 for n, v in enumerate(seq)))
check("P_5(E) > .95 ('only five repetitions')", seq[5] > R(95, 100), f"P_5 = {float(seq[5]):.4f}")
check("P_4(E) < .95, so five is the first", seq[4] < R(95, 100), f"P_4 = {float(seq[4]):.4f}")
print("  values: " + ", ".join(f"{float(v):.4f}" for v in seq))

print(f"\n  -> {'all checks passed' if not FAILS else 'FAILURES: ' + ', '.join(FAILS)}")
sys.exit(0 if not FAILS else 1)
