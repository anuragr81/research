"""
Phelps (1972), AER 62(4), 659-661 -- the signal-extraction model and its cases.

Checks, exactly (sympy, rationals):
  (1) eq. (2): a1 = vq/(vq+vmu) minimizes the population MSE (1-a)^2 vq + a^2 vmu,
      with excess (vq+vmu)(a-a1)^2  [Lean: mse_sub_mse_opt];
  (2) footnote 3: the OLS slope sum(yq)/sum(y^2) minimizes the SSR, excess
      (a-ahat)^2 sum(y^2), for a symbolic sample of size 4  [Lean: ssr_sub_ssr_ols];
  (3) the step the paper leaves implicit: under normality the least-squares
      predictor IS the posterior mean.  With q ~ N(m, vq) and y = q + mu,
      mu ~ N(0, vmu), the log posterior density of q given y is maximized (and,
      being Gaussian, centred) at (1-a1) m + a1 y;
  (4) the gap identity pred_B - pred_W = (aB-aW)(y-alpha) - (1-aB) beta
      [Lean: gap_identity];
  (5) Case 1, Case 2 (including the limit var eps -> oo and the crossing score),
      and the Further Case at a concrete parameter point;
  (6) beta = 0 with unequal reliabilities: differential treatment at y != alpha
      [Lean: no_mean_gap_still_differential].
Exit status 0 iff every check passes.
"""
import sys
import sympy as sp

ok = True


def check(name, cond):
    global ok
    cond = bool(cond)
    print(f"[{'PASS' if cond else 'FAIL'}] {name}")
    ok = ok and cond


vq, vmu, a, m, y, q = sp.symbols('vq vmu a m y q', real=True)
vq_p, vmu_p = sp.symbols('vq_p vmu_p', positive=True)


def relratio(l, n):
    return l / (l + n)


# (1) population least squares
a1 = relratio(vq, vmu)
mse = lambda s: (1 - s) ** 2 * vq + s ** 2 * vmu
check("eq.(2): MSE(a) - MSE(a1) = (vq+vmu)(a-a1)^2",
      sp.simplify(mse(a) - mse(a1) - (vq + vmu) * (a - a1) ** 2) == 0)
# and the MSE is indeed E[(q' - a(q'+mu))^2] for uncorrelated q', mu:
Eq2, Emu2, Eqmu = vq, vmu, 0
expanded = Eq2 - 2 * a * (Eq2 + Eqmu) + a ** 2 * (Eq2 + 2 * Eqmu + Emu2)
check("MSE formula = E[(q' - a y')^2] with cov(q', mu) = 0",
      sp.expand(expanded - mse(a)) == 0)

# (2) OLS, footnote 3
ys = sp.symbols('y0:4', real=True)
qs = sp.symbols('q0:4', real=True)
Syy = sum(v ** 2 for v in ys)
Syq = sum(u * v for u, v in zip(ys, qs))
ahat = Syq / Syy
ssr = lambda s: sum((qq - s * yy) ** 2 for yy, qq in zip(ys, qs))
check("fn 3: SSR(a) - SSR(ahat) = (a-ahat)^2 sum y^2",
      sp.simplify(ssr(a) - ssr(ahat) - (a - ahat) ** 2 * Syy) == 0)

# (3) posterior mean under normality
logpost = -(q - m) ** 2 / (2 * vq_p) - (y - q) ** 2 / (2 * vmu_p)
qstar = sp.solve(sp.diff(logpost, q), q)[0]
check("normal model: argmax/mean of q|y is (1-a1) m + a1 y",
      sp.simplify(qstar - ((1 - relratio(vq_p, vmu_p)) * m + relratio(vq_p, vmu_p) * y)) == 0)
check("normal model: log posterior is concave quadratic in q",
      sp.simplify(sp.diff(logpost, q, 2) + 1 / vq_p + 1 / vmu_p) == 0)

# (4) gap identity
alpha, beta, aW, aB = sp.symbols('alpha beta aW aB', real=True)
pred = lambda s, c: (1 - s) * (alpha - beta * c) + s * y
check("gap identity",
      sp.expand(pred(aB, 1) - pred(aW, 0) - ((aB - aW) * (y - alpha) - (1 - aB) * beta)) == 0)

# (5) the cases at a concrete point: alpha=1, beta=1/2, var eta=1, var xi=1
R = sp.Rational
al, be = R(1), R(1, 2)
veta, vxi = R(1), R(1)
predc = lambda s, c, yy: (1 - s) * (al - be * c) + s * yy
# Case 1: var eps = 0, var rho = 0
aC1 = relratio(veta, vxi)
gaps = {predc(aC1, 0, yy) - predc(aC1, 1, yy) for yy in [R(-3), R(0), R(2), R(7)]}
check("Case 1: constant white-minus-black gap (1-a)beta = 1/4 at every score",
      gaps == {(1 - aC1) * be} and (1 - aC1) * be == R(1, 4))
# Case 2: var eps = 1
aW2, aB2 = relratio(veta, vxi), relratio(veta + 1, vxi)
ystar = al + (1 - aB2) * be / (aB2 - aW2)
check("Case 2: aB = 2/3 > aW = 1/2", aB2 == R(2, 3) and aW2 == R(1, 2))
check("Case 2: crossing score y* = 2; equal predictions there",
      ystar == 2 and predc(aB2, 1, ystar) == predc(aW2, 0, ystar))
check("Case 2: black predicted higher above y*, lower below",
      predc(aB2, 1, R(3)) > predc(aW2, 0, R(3)) and predc(aB2, 1, R(1)) < predc(aW2, 0, R(1)))
veps = sp.symbols('veps', positive=True)
check("Case 2: slope -> 1 as var eps -> oo",
      sp.limit(relratio(veta + veps, vxi), veps, sp.oo) == 1)
# Further Case: var rho = 1, var eps = 0
aWf, aBf = relratio(veta, vxi), relratio(veta, vxi + 1)
yf = al - (1 - aBf) * be / (aWf - aBf)
check("Further Case: aW = 1/2 > aB = 1/3", aWf == R(1, 2) and aBf == R(1, 3))
check("Further Case: crossing y = -1; whites predicted lower below it",
      yf == -1 and predc(aBf, 1, R(-2)) > predc(aWf, 0, R(-2))
      and predc(aBf, 1, R(0)) < predc(aWf, 0, R(0)))

# (6) no mean gap, still differential
predc0 = lambda s, c, yy: (1 - s) * (al - 0 * c) + s * yy
check("beta = 0, aB != aW: gap zero only at y = alpha",
      sp.solve(sp.Eq(predc0(aB2, 1, y), predc0(aW2, 0, y)), y) == [al])

print("ALL PASS" if ok else "SOME CHECKS FAILED")
sys.exit(0 if ok else 1)
