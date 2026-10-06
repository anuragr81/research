import numpy as np
import sympy as sp
from scipy import integrate
from scipy.stats import beta as Beta, lognorm

x = sp.symbols("x", positive=True)
res = []


def check(n, ok, d=""):
    res.append((n, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {n}" + (f"   {d}" if d else ""))


print("=" * 72)
print("P6 PROOF.  mu = 0 exactly:  G is a point mass at 0, so G(x)=1 for x>=0.")
print("   H_m = F^m * 1 * C = F^(m+1) when the incumbent invests.")
print("   Int H_m dF = Int_0^1 F^(m+1) dF = 1/(m+2).")
print("   Int H_m dG = H_m(0) = F(0)^(m+1) = 0   provided s > 0 a.s.")
print("   => Delta(m) = 1/(m+2),  independent of Q.")
print("=" * 72)
ok = True
for mm in range(0, 6):
    val = sp.integrate(x ** (mm + 1), (x, 0, 1))
    if sp.simplify(val - sp.Rational(1, mm + 2)) != 0:
        ok = False
check("P6 Int_0^1 F^(m+1) dF = 1/(m+2) exact", ok)
F0 = sp.symbols("F0", nonnegative=True)
ok_bt = all(
    sp.simplify(sp.limit(F0 ** (mm + 1), F0, 0, "+")) == 0 for mm in range(0, 6)
)
ok_bt = ok_bt and sp.simplify((sp.Integer(0)) ** sp.Integer(1)) == 0
check("P6 boundary term F(0)^(m+1)=0 when F(0)=0", bool(ok_bt), "requires s>0 a.s.")

print()
print("=" * 72)
print("P7 PROOF.  UNIFORM BOUND, independent of Q:")
print("   H_m = F^m G^(Q-1-m) C  <=  F^m     since G <= 1 and C <= 1.")
print("   Delta(m) = Int H_m(dF-dG) <= Int H_m dF <= Int F^m dF = V/(m+1).")
print("   Hence  Delta(m) >= kappa  forces  m + 1 <= V/kappa,")
print("   so     k* <= V/kappa      FOR EVERY Q.   Saturation, uniformly.")
print("=" * 72)


def score_FG(mu, dr, ds, n=300000, ngrid=4001):
    r = dr.rvs(size=n)
    s = ds.rvs(size=n)
    Y = mu * r
    X = mu * r + (1 - mu) * s
    hi = max(X.max(), Y.max())
    g = np.linspace(0, hi, ngrid)
    return (
        np.searchsorted(np.sort(X), g, side="right") / n,
        np.searchsorted(np.sort(Y), g, side="right") / n,
        g,
    )


def Delta(F, G, g, m, Q, V=1.0):
    H = F**m * G ** (Q - 1 - m) * F
    return V * (
        integrate.simpson(H * np.gradient(F, g), x=g)
        - integrate.simpson(H * np.gradient(G, g), x=g)
    )


viol = []
for mu in [0.1, 0.3, 0.5, 0.7, 0.9]:
    F, G, g = score_FG(mu, Beta(2, 2), Beta(2, 2), n=150000)
    for Q in [2, 4, 8, 16, 32, 64, 128]:
        for m in range(0, min(Q, 12)):
            d = Delta(F, G, g, m, Q)
            if d > 1.0 / (m + 1) + 1e-3:
                viol.append((mu, Q, m, d, 1 / (m + 1)))
check("P7 uniform bound Delta(m) <= V/(m+1)", len(viol) == 0, f"{len(viol)} violations")

print("      implied cap on k*:")
for kappa in [0.5, 0.2, 0.1, 0.05]:
    print(f"        kappa={kappa}: k* <= V/kappa = {1/kappa:.0f}  for every Q")

F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 2))
obs_ok = True
for kappa in [0.20, 0.10, 0.05]:
    for Q in [4, 16, 64, 256]:
        ds = [Delta(F, G, g, m, Q) for m in range(min(Q, 60))]
        k = sum(1 for d in ds if d >= kappa)
        if k > 1.0 / kappa + 1e-9:
            obs_ok = False
check("P7 observed k* respects the cap", obs_ok)

print()
print("=" * 72)
print("P9 PROOF (LINEAR SPECIAL CASE).  MPS about the mean:")
print("   w -> mean + lam*(w - mean).   For the general statement see")
print("   verify_p9gen.py: the flip is at the PIVOT x0, which coincides")
print("   with the mean only for this linear family.")
print("   New CDF:  Lam'(t) = Lam(mean + (t-mean)/lam).")
print("   For lam > 1:   Lam'(t) < Lam(t)  iff  t > mean.")
print("   Mass above the entry threshold wbar is 1 - Lam(wbar), so it RISES")
print("   iff wbar > mean and FALLS iff wbar < mean.")
print("   => for THIS LINEAR spread the sign flips at  wbar = mean,")
print("      i.e. at entry rate  1 - Lam(mean).")
print("=" * 72)

sigma, mu_ln = 0.45, 0.6
Lam = lognorm(s=sigma, scale=np.exp(mu_ln))
mean_w = Lam.mean()
pivot_rate = 1 - Lam.cdf(mean_w)
print(f"      lognormal(0.6,0.45): mean={mean_w:.4f}, "
      f"predicted sign flip at entry rate = {pivot_rate:.4f}")

rng = np.random.default_rng(4)
base = Lam.rvs(size=200000, random_state=7)


def mass_above(w, t, lam):
    return (np.maximum(w.mean() + (w - w.mean()) * lam, 0) >= t).mean()


ok = True
for q in [0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95]:
    t = np.quantile(base, q)
    m1 = mass_above(base, t, 1.0)
    m2 = mass_above(base, t, 1.6)
    predicted = "RISES" if t > mean_w else "FALLS"
    actual = "RISES" if m2 > m1 + 1e-6 else ("FALLS" if m2 < m1 - 1e-6 else "flat")
    match = predicted == actual
    ok = ok and match
    print(f"      wbar at q={q:.2f} (w={t:6.3f}, mean={mean_w:.3f}): "
          f"predicted {predicted}, actual {actual}  {'OK' if match else 'MISMATCH'}")
check("P9 (linear spread) sign flip at wbar = mean", ok,
      "NB: general case flips at the PIVOT x0, not the mean - see verify_p9gen.py")

print()
print("      exact statement of the CDF crossing, symbolically:")
t_, mn, lam_ = sp.symbols("t mn lam", real=True, positive=True)
arg = mn + (t_ - mn) / lam_
diff = sp.simplify(arg - t_)
print(f"        Lam'(t) argument minus t  =  {sp.factor(diff)}")
print("        for lam>1 this is < 0 iff t > mn, so Lam'(t) < Lam(t) iff t > mn.")
sgn = sp.simplify(sp.factor(diff).subs({lam_: 2, mn: 1, t_: 3}))
check("P9 CDF crossing algebra", sgn < 0, f"at t>mn, lam>1: {sgn} < 0")

print()
print("=" * 72)
nf = sum(1 for _, o in res if not o)
print(f"SUMMARY: {len(res)} checks, {nf} failures")
print("=" * 72)
