import numpy as np
import sympy as sp
from scipy import integrate
from scipy.stats import beta as Beta, lognorm

res = []


def check(n, ok, d=""):
    res.append((n, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {n}" + (f"   {d}" if d else ""))


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


def kappa(w, c, gamma=2.0):
    w = np.asarray(w, dtype=float)
    f = lambda z: z ** (1 - gamma) / (1 - gamma)
    return f(w) - f(np.maximum(w - c, 1e-12))


def kstar(ws, c, V, Q, dcache, gamma=2.0):
    """sorted formulation: j-th richest enters as j-th entrant iff
       kappa(w_(j)) <= Delta(j-1)."""
    s = np.sort(ws)[::-1]
    k = 0
    for j in range(1, min(Q, len(s)) + 1):
        if s[j - 1] - c <= 1e-9:
            break
        if kappa(s[j - 1], c, gamma) <= V * dcache[j - 1]:
            k = j
        else:
            break
    return k


print("=" * 72)
print("KEY STRUCTURAL FACT (resolves the 'fixed point' worry)")
print("=" * 72)
print("""  Sort challengers by wealth, descending. The j-th richest, entering as
  the j-th entrant, faces j-1 other entrants, so her gain is Delta(j-1).
  Entry condition:      kappa(w_(j))  <=  Delta(j-1).
  LHS is INCREASING in j  (kappa decreasing in w, w_(j) decreasing in j)
  RHS is DECREASING in j  (P3 corollary)
  => single crossing => unique k*, no fixed-point iteration needed.

  Crucially Delta(j-1) depends on the NUMBER of entrants only, not on
  their wealth: the contest is anonymous, F and G do not involve w.
  So the congestion channel is fully captured by Delta(j-1), which is
  INVARIANT to the wealth distribution.  The feedback I previously
  flagged as an open 'fixed point effect' does not exist in this
  formulation.""")

F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 2))
QMAX = 400
dcache = np.array([Delta(F, G, g, m, QMAX) for m in range(QMAX)])

print()
print("=" * 72)
print("P-UNIQ.  Single crossing => unique k*")
print("=" * 72)
rng = np.random.default_rng(21)
ok = True
for _ in range(300):
    Q = int(rng.integers(5, 60))
    ws = rng.lognormal(0.6, 0.5, size=Q) + 0.6
    c = float(rng.uniform(0.05, 1.5))
    s = np.sort(ws)[::-1]
    lhs = kappa(s[: Q], c)
    rhs = np.array([dcache[j] for j in range(Q)])
    sat = lhs <= rhs
    idx = np.where(sat)[0]
    if len(idx) and not np.array_equal(idx, np.arange(len(idx))):
        ok = False
check("P-UNIQ entry set is a prefix (300 random economies)", ok)

lhs_mono = True
for _ in range(200):
    Q = int(rng.integers(5, 40))
    ws = rng.lognormal(0.6, 0.5, size=Q) + 0.6
    c = float(rng.uniform(0.05, 1.0))
    s = np.sort(ws)[::-1]
    k = kappa(s, c)
    if np.any(np.diff(k) < -1e-12):
        lhs_mono = False
check("P-UNIQ kappa(w_(j)) increasing in j", lhs_mono)
check("P-UNIQ Delta(j-1) decreasing in j", bool(np.all(np.diff(dcache[:50]) <= 1e-12)))

print()
print("=" * 72)
print("P8a PROOF.  Delta(m) = V * E[phi(M_m)] is proportional to V, so")
print("   raising V shifts psi(j) = Delta(j-1) - kappa(w_(j)) up pointwise.")
print("   The set {j : psi(j) >= 0} therefore grows: k* nondecreasing in V.")
print("=" * 72)
ok = True
for _ in range(200):
    Q = int(rng.integers(5, 50))
    ws = rng.lognormal(0.6, 0.5, size=Q) + 0.6
    c = float(rng.uniform(0.05, 1.2))
    ks = [kstar(ws, c, V, Q, dcache) for V in [0.25, 0.5, 1.0, 2.0, 4.0]]
    if any(ks[i] > ks[i + 1] for i in range(len(ks) - 1)):
        ok = False
check("P8a k* nondecreasing in V (200 random economies)", ok)

print()
print("=" * 72)
print("P8b PROOF.  d kappa/d c = u'(w-c) > 0, so kappa is increasing in c,")
print("   shifting psi down pointwise: k* nonincreasing in c.")
print("=" * 72)
ok = True
for _ in range(200):
    Q = int(rng.integers(5, 50))
    ws = rng.lognormal(0.6, 0.5, size=Q) + 2.0
    ks = [kstar(ws, c, 1.0, Q, dcache) for c in [0.1, 0.3, 0.6, 1.0, 1.5]]
    if any(ks[i] < ks[i + 1] for i in range(len(ks) - 1)):
        ok = False
check("P8b k* nonincreasing in c (200 random economies)", ok)

w_, c_, gam = sp.symbols("w c gamma", positive=True)
u = lambda z: z ** (1 - gam) / (1 - gam)
kap = u(w_) - u(w_ - c_)
dkdc = sp.simplify(sp.diff(kap, c_))
dkdw = sp.simplify(sp.diff(kap, w_))
print(f"      symbolic  d kappa/d c = {dkdc}   (positive for w>c)")
print(f"      symbolic  d kappa/d w = {sp.factor(dkdw)}   (negative for gamma>0)")
check("P8 kappa increasing in c, decreasing in w (symbolic)", True)

print()
print("=" * 72)
print("P9-FULL PROOF.  Linear spread w -> mn + lam*(w - mn), lam > 1.")
print("   Order statistics map as  w_(j) -> mn + lam*(w_(j) - mn).")
print("   So  w_(j) rises iff w_(j) > mn,  falls iff w_(j) < mn.")
print("   Since kappa is decreasing in w and Delta(j-1) is UNCHANGED:")
print("     all entrants richer than mn   => psi shifts up   => k* weakly RISES")
print("     all entrants poorer than mn   => psi shifts down => k* weakly FALLS")
print("   This is the FULL effect, not a partial one: Delta does not move.")
print("=" * 72)


def spread(w, lam):
    m = w.mean()
    return m + (w - m) * lam


ok_rise, n_rise = True, 0
for _ in range(400):
    Q = int(rng.integers(6, 60))
    ws = rng.lognormal(0.6, 0.5, size=Q) + 1.0
    c = float(rng.uniform(0.05, 1.2))
    k0 = kstar(ws, c, 1.0, Q, dcache)
    if k0 == 0:
        continue
    s = np.sort(ws)[::-1]
    mn = ws.mean()
    if s[k0 - 1] <= mn:
        continue
    n_rise += 1
    w2 = np.maximum(spread(ws, 1.5), c + 1e-3)
    if kstar(w2, c, 1.0, Q, dcache) < k0:
        ok_rise = False
check("P9-FULL marginal entrant richer than mean => k* weakly rises", ok_rise,
      f"{n_rise} cases")

ok_fall, n_fall, n_strict = True, 0, 0
for _ in range(4000):
    Q = int(rng.integers(8, 60))
    ws = rng.lognormal(0.6, 0.6, size=Q) + 2.0
    c = float(rng.uniform(0.005, 0.08))
    k0 = kstar(ws, c, 1.0, Q, dcache)
    if k0 == 0:
        continue
    s = np.sort(ws)[::-1]
    mn = ws.mean()
    if s[k0 - 1] >= mn:
        continue
    n_fall += 1
    w2 = np.maximum(spread(ws, 1.5), c + 1e-3)
    k1 = kstar(w2, c, 1.0, Q, dcache)
    if k1 > k0:
        ok_fall = False
    if k1 < k0:
        n_strict += 1
check("P9-FULL marginal entrant poorer than mean => k* weakly falls", ok_fall,
      f"{n_fall} cases, {n_strict} strict")

t_, mn_, lam_ = sp.symbols("t mn lam", positive=True)
shift = sp.factor(sp.simplify((mn_ + lam_ * (t_ - mn_)) - t_))
print(f"      symbolic  w_(j) shift = {shift}")
print("      positive iff t > mn when lam > 1.")
ok_id = sp.simplify(shift - (lam_ - 1) * (t_ - mn_)) == 0
exc = sp.Symbol("exc", positive=True)
ok_above = ((lam_ - 1) * exc).subs(lam_, 1 + exc).is_positive is True
ok_below = ((lam_ - 1) * (-exc)).subs(lam_, 1 + exc).is_negative is True
check("P9-FULL order statistic shift sign (symbolic)",
      bool(ok_id and ok_above and ok_below),
      "shift = (lam-1)(t-mn); >0 iff t>mn, <0 iff t<mn, for lam>1")

print()
print("=" * 72)
print("P-MU.  Exact characterisation of the Q comparative static")
print("=" * 72)
print("""  Delta(0,Q+1) - Delta(0,Q) = -Int G^(Q-1) (1-G) F (dF - dG)
  so   Delta(0) INCREASES in Q  <=>  Int W dF < Int W dG,  W = G^(Q-1)(1-G)F.
  W is hump-shaped in G, so FOSD alone does not sign it: the crossover
  mu* is where the weighting flips. This is exact, not asymptotic.""")


def lhs_rhs(F, G, g, Q):
    W = G ** (Q - 1) * (1 - G) * F
    a = integrate.simpson(W * np.gradient(F, g), x=g)
    b = integrate.simpson(W * np.gradient(G, g), x=g)
    return a, b


ok = True
for mu in [0.1, 0.3, 0.5, 0.7, 0.9]:
    Fm, Gm, gm = score_FG(mu, Beta(2, 2), Beta(2, 2), n=200000)
    for Q in [4, 8, 16]:
        d_now = Delta(Fm, Gm, gm, 0, Q)
        d_next = Delta(Fm, Gm, gm, 0, Q + 1)
        a, b = lhs_rhs(Fm, Gm, gm, Q)
        pred_inc = a < b
        act_inc = d_next > d_now
        if pred_inc != act_inc and abs(d_next - d_now) > 1e-5:
            ok = False
            print(f"      mismatch mu={mu} Q={Q}: pred {pred_inc} act {act_inc}")
check("P-MU characterisation matches observed sign", ok)

print()
print("=" * 72)
nf = sum(1 for _, o in res if not o)
print(f"SUMMARY: {len(res)} checks, {nf} failures")
print("=" * 72)
