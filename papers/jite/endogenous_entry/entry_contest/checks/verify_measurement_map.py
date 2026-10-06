import itertools

import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


print("=" * 72)
print("MM1  LINEAR FAMILY T(w)=x0+lam(w-x0): RECOVERY OF lam AND x0 FROM TWO RANKS")
print("=" * 72)
wa, wb, x0, lam = sp.symbols("w_a w_b x_0 lambda", real=True)
da = (x0 + lam * (wa - x0)) - wa
db = (x0 + lam * (wb - x0)) - wb
lam_hat = 1 + (da - db) / (wa - wb)
x0_hat = (db * wa - da * wb) / (db - da)
check("MM1a lam = 1 + (d_a-d_b)/(w_a-w_b)", sp.simplify(lam_hat - lam) == 0)
check("MM1b x0 = (d_b w_a - d_a w_b)/(d_b-d_a)", sp.simplify(x0_hat - x0) == 0)
check("MM1c d_b - d_a = (lam-1)(w_b-w_a)",
      sp.expand(db - da - (lam - 1) * (wb - wa)) == 0)
wrong = (da * wa - db * wb) / (db - da)
check("MM1d control: swapped numerator does not recover x0",
      sp.simplify(wrong - x0) != 0)

print("=" * 72)
print("MM2  LINEAR SPREAD ABOUT THE PROFILE MEAN (P9 FORM)")
print("=" * 72)
n = 6
ws = sp.symbols(f"w1:{n + 1}", real=True)
mbar = sum(ws) / n
wp = [mbar + lam * (w - mbar) for w in ws]
check("MM2a spread about the profile mean preserves the profile mean",
      sp.simplify(sum(wp) / n - mbar) == 0)
check("MM2b lam = (w'_j - mbar)/(w_j - mbar)",
      sp.simplify((wp[2] - mbar) / (ws[2] - mbar) - lam) == 0)
other = sp.Symbol("x", real=True)
wq = [other + lam * (w - other) for w in ws]
check("MM2c control: spread about another point shifts the profile mean",
      sp.simplify(sum(wq) / n - mbar) != 0)

print("=" * 72)
print("MM3  P6 WITH THE FACTOR V: mu=0, C=F, G a point mass at 0")
print("=" * 72)
x = sp.Symbol("x", positive=True)
a, m, V = sp.symbols("a m V", positive=True)
F = x**a
integrand6 = sp.powsimp(sp.expand_power_base(F ** (m + 1) * sp.diff(F, x), force=True), force=True)
check("MM3a' integrand F^(m+1) F' is a single power a x^(a(m+2)-1)",
      sp.simplify(integrand6 - a * x ** (a * (m + 2) - 1)) == 0)
intF = sp.integrate(a * x ** (a * (m + 2) - 1), (x, 0, 1), conds="none")
check("MM3a int_0^1 F^(m+1) dF = 1/(m+2), symbolic in m and a",
      sp.simplify(intF - 1 / (m + 2)) == 0)
H0 = sp.limit(x ** (a * (m + 1)), x, 0)
check("MM3b int H_m dG = H_m(0) = 0", H0 == 0)
check("MM3c Delta(m) = V/(m+2) at mu=0", sp.simplify(V * (intF - H0) - V / (m + 2)) == 0)

print("=" * 72)
print("MM4  P7 BASE INTEGRAL WITH THE FACTOR V")
print("=" * 72)
integrand7 = sp.powsimp(sp.expand_power_base(F**m * sp.diff(F, x), force=True), force=True)
check("MM4a' integrand F^m F' is a single power a x^(a(m+1)-1)",
      sp.simplify(integrand7 - a * x ** (a * (m + 1) - 1)) == 0)
intFm = sp.integrate(a * x ** (a * (m + 1) - 1), (x, 0, 1), conds="none")
check("MM4a V int_0^1 F^m dF = V/(m+1), symbolic in m and a",
      sp.simplify(V * intFm - V / (m + 1)) == 0)

print("=" * 72)
print("MM5  P-MU IDENTITY WITH THE FACTOR V (incumbent CDF C=F)")
print("=" * 72)
Fs, Gs = sp.symbols("F G", positive=True)
Q = sp.Symbol("Q", positive=True, integer=True)
check("MM5a V(G^Q - G^(Q-1))F = -V G^(Q-1)(1-G)F, abstract",
      sp.simplify(V * (Gs**Q - Gs ** (Q - 1)) * Fs + V * Gs ** (Q - 1) * (1 - Gs) * Fs) == 0)


def delta0(Fx, Gx, q):
    Hm = Gx ** (q - 1) * Fx
    return V * (sp.integrate(Hm * sp.diff(Fx, x), (x, 0, 1))
                - sp.integrate(Hm * sp.diff(Gx, x), (x, 0, 1)))


for Fx, Gx, q in [(x**2, x, 3), (x**3, x, 5), (x**2, x**sp.Rational(1, 2), 4)]:
    W = Gx ** (q - 1) * (1 - Gx) * Fx
    lhs = delta0(Fx, Gx, q + 1) - delta0(Fx, Gx, q)
    rhs = -V * (sp.integrate(W * sp.diff(Fx, x), (x, 0, 1))
                - sp.integrate(W * sp.diff(Gx, x), (x, 0, 1)))
    check(f"MM5b Delta(0,Q+1)-Delta(0,Q) on F={Fx}, G={Gx}, Q={q}",
          sp.simplify(lhs - rhs) == 0, f"value {sp.nsimplify(lhs / V)}*V")

print("=" * 72)
print("MM6  ENTRY CONDITION INVARIANT TO (u, V) -> (a u + b, a V), a > 0")
print("=" * 72)
u = sp.Function("u")
w, c, b, e = sp.symbols("w c b e", real=True)
kap = u(w) - u(w - c)
kap_new = (a * u(w) + b) - (a * u(w - c) + b)
gap = V * e - kap
gap_new = (a * V) * e - kap_new
check("MM6a kappa scales by a, b cancels", sp.simplify(kap_new - a * kap) == 0)
check("MM6b Delta - kappa scales by a (sign preserved for a>0)",
      sp.simplify(gap_new - a * gap) == 0)
gap_ctrl = (a * V) * e - (u(w) - u(w - c))
check("MM6c control: scaling V alone changes the gap",
      sp.simplify(gap_ctrl - a * gap) != 0)
k_, V_ = sp.symbols("kappa V_", positive=True)
f = sp.Function("f")
inv_at = f(a * k_, a * V_).subs(a, 1 / V_)
check("MM6d a=1/V maps (a kappa, a V) to (kappa/V, 1): an invariant f is f(kappa/V, 1)",
      inv_at == f(k_ / V_, 1))
k1, k2, k3, k4 = sp.symbols("k1:5", positive=True)
tS = k1 + k2
tT = k3 + k4
check("MM6e total kappa difference between two equilibria scales by a (kappa carries no b, MM6a)",
      sp.simplify((a * k3 + a * k4) - (a * k1 + a * k2) - a * (tT - tS)) == 0)
ui = sp.symbols("U1:4", real=True)
uj = sp.symbols("W1:4", real=True)
sumS = sum(a * x_ + b for x_ in ui)
sumT = sum(a * x_ + b for x_ in uj)
check("MM6f sum of payoffs: level moves with b, difference over the same agents scales by a",
      sp.simplify((sumT - sumS) - a * (sum(uj) - sum(ui))) == 0
      and sp.simplify(sumS - a * sum(ui)) == 3 * b)

print("=" * 72)
print("MM7  MARGINAL RANK CAN BE ABSENT FROM AN EQUILIBRIUM (manuscript's example)")
print("=" * 72)
Dl = [12, 10, 1]
kp = [2, 5, 8]
Qn = len(kp)
Dfull = Dl + [0]


def is_eq(S):
    k = len(S)
    ins = all(kp[i] <= Dfull[k - 1] for i in S) if k else True
    outs = all(kp[i] > Dfull[k] for i in range(Qn) if i not in S)
    return ins and outs


eqs = [set(S) for r in range(Qn + 1) for S in itertools.combinations(range(Qn), r) if is_eq(S)]
kstar = max(k for k in range(Qn + 1) if all(kp[j] <= Dfull[j] for j in range(k)))
print(f"      equilibria (0-indexed, rank 0 richest): {eqs}; k* = {kstar}")
check("MM7a every equilibrium has count k*", all(len(S) == kstar for S in eqs))
absent = [S for S in eqs if (kstar - 1) not in S]
check("MM7b some equilibrium omits the rank-k* challenger", len(absent) > 0, f"{absent}")

print("=" * 72)
print("MM8  UPPER END OF THE NON-INVESTOR SCORE SUPPORT")
print("=" * 72)
mu, bb = sp.symbols("mu b", positive=True)
Gcdf = x / (mu * bb)
top = sp.solve(sp.Eq(Gcdf, 1), x)[0]
check("MM8a r uniform on [0,b]: sup supp(G) = mu*b", sp.simplify(top - mu * bb) == 0)
check("MM8b sup supp(G) = mu exactly when b = 1",
      sp.solve(sp.Eq(top, mu), bb) == [1])

print("=" * 72)
print("MM9  THE ENTRY CONDITION IS A COMPARISON OF EXPECTED PAYOFFS")
print("=" * 72)
P1, P0 = sp.symbols("P1 P0", nonnegative=True)
EU1 = (1 - P1) * u(w - c) + P1 * (u(w - c) + V)
EU0 = (1 - P0) * u(w) + P0 * (u(w) + V)
gain_vs_cost = V * (P1 - P0) - (u(w) - u(w - c))
check("MM9a E[U|e=1]-E[U|e=0] = V(P1-P0) - kappa(w)", sp.simplify(EU1 - EU0 - gain_vs_cost) == 0)
EU1_wrong = (1 - P1) * u(w - c) + P1 * (u(w) + V)
check("MM9b control: paying c only when losing breaks the identity",
      sp.simplify(EU1_wrong - EU0 - gain_vs_cost) != 0)

print("=" * 72)
print("MM10 EXPECTED-PAYOFF RANKING: INVARIANT UNDER aU+b, a>0; NOT UNDER A MONOTONE NONLINEAR MAP")
print("=" * 72)
p = sp.Rational(1, 2)
lotA = [(p, 0), (1 - p, 10)]
lotB = [(1, 4)]


def eu(lot, f):
    return sum(q * f(sp.Integer(xv)) for q, xv in lot)


base = lambda t: sp.sqrt(t)
gapAB = eu(lotA, base) - eu(lotB, base)
aff = lambda t: 3 * sp.sqrt(t) + 7
check("MM10a sign of EU(A)-EU(B) unchanged under 3U+7",
      sp.sign(sp.N(eu(lotA, aff) - eu(lotB, aff))) == sp.sign(sp.N(gapAB)),
      f"sign {sp.sign(sp.N(gapAB))}")
mono = lambda t: sp.sqrt(t) ** 4
check("MM10b control: U^4 (monotone on t>=0) reverses the ranking",
      sp.sign(sp.N(eu(lotA, mono) - eu(lotB, mono))) != sp.sign(sp.N(gapAB)))

nfail = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"MEASUREMENT MAP SUMMARY: {len(results)} checks, {nfail} failures")
raise SystemExit(1 if nfail else 0)
