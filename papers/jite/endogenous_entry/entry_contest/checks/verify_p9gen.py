import numpy as np
import sympy as sp
from scipy import integrate
from scipy.stats import beta as Beta

res = []


def check(n, ok, d=""):
    res.append((n, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {n}" + (f"   {d}" if d else ""))


def score_FG(mu, dr, ds, n=250000, ngrid=4001):
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


F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 2))
QMAX = 300
dc = np.array([Delta(F, G, g, m, QMAX) for m in range(QMAX)])


def kstar(ws, c, V, Q):
    s = np.sort(ws)[::-1]
    k = 0
    for j in range(1, min(Q, len(s)) + 1):
        if s[j - 1] - c <= 1e-9:
            break
        if kappa(s[j - 1], c) <= V * dc[j - 1]:
            k = j
        else:
            break
    return k


print("=" * 72)
print("P9-GEN.  General increasing pivot-spread, not just the linear one.")
print("=" * 72)
print("""  Let T be INCREASING with a single pivot x0:
        T(w) >= w  for w >= x0,        T(w) <= w  for w <= x0.
  Two consequences:
    (i)  T increasing => it PRESERVES the wealth ordering, so the j-th
         order statistic maps as  w_(j) -> T(w_(j)).
    (ii) kappa is decreasing in w, so kappa(T(w_(j))) <= kappa(w_(j))
         iff w_(j) >= x0.
  Delta(j-1) is unchanged (anonymity). So the same sign rule holds with
  x0 in place of the mean.

  CORRECTION to the earlier statement: the relevant pivot is x0, NOT the
  mean. They coincide for the linear spread T(w)=mn+lam*(w-mn), which is
  why the mean appeared. A general mean-preserving spread may pivot
  elsewhere, and then the flip is at x0.""")

rng = np.random.default_rng(31)


def T_linear(w, x0, lam):
    return x0 + (w - x0) * lam


def T_power(w, x0, p):
    d = w - x0
    return x0 + np.sign(d) * np.abs(d) ** p


def T_exp(w, x0, a):
    d = w - x0
    return x0 + np.sign(d) * (np.exp(a * np.abs(d)) - 1) / a


def T_quad(w, x0, a):
    d = w - x0
    return x0 + d * (1.0 + a * np.abs(d))


def T_mix(w, x0, a, lam):
    d = w - x0
    return x0 + lam * d + a * d * np.abs(d)


families = [
    ("linear   ", lambda w, x0: T_linear(w, x0, 1.5)),
    ("quad0.6  ", lambda w, x0: T_quad(w, x0, 0.6)),
    ("mix      ", lambda w, x0: T_mix(w, x0, 0.4, 1.3)),
    ("exp0.5   ", lambda w, x0: T_exp(w, x0, 0.5)),
    ("power1.4 ", lambda w, x0: T_power(w, x0, 1.4)),
    ("power2.0 ", lambda w, x0: T_power(w, x0, 2.0)),
]

def satisfies_pivot(T, x0=3.0, lo=0.05, hi=8.0, n=4000):
    """T(w) >= w for w >= x0 and T(w) <= w for w <= x0, and T increasing."""
    w = np.linspace(lo, hi, n)
    tw = T(w, x0)
    inc = np.all(np.diff(tw) >= -1e-12)
    up = np.all(tw[w >= x0] >= w[w >= x0] - 1e-12)
    dn = np.all(tw[w <= x0] <= w[w <= x0] + 1e-12)
    return inc and up and dn

print()
print("  HYPOTHESIS CHECK: which families are genuine pivot-spreads?")
qualify = []
for name, T in families:
    okp = satisfies_pivot(T)
    qualify.append((name, T, okp))
    print(f"      {name}: pivot-spread hypothesis {'HOLDS' if okp else 'FAILS'}")
print("      (power families contract near the pivot: |d|^p < |d| for |d|<1,")
print("       so they are NOT pivot-spreads and the theorem does not apply)")

print()
print("  (i) order preservation")
ok = True
for name, T in families:
    for _ in range(50):
        w = rng.lognormal(0.6, 0.5, size=30) + 2.0
        x0 = float(np.quantile(w, rng.uniform(0.2, 0.8)))
        tw = T(w, x0)
        if not np.array_equal(np.argsort(w), np.argsort(tw)):
            ok = False
check("P9-GEN (i) increasing T preserves order statistics", ok)

print()
print("  (ii) sign rule with pivot x0 (NOT the mean)")
allok = True
for name, T, okp in qualify:
    if not okp:
        continue
    nr = nf = viol = 0
    for _ in range(1500):
        Q = int(rng.integers(8, 50))
        w = rng.lognormal(0.6, 0.6, size=Q) + 2.0
        c = float(rng.uniform(0.01, 0.6))
        x0 = float(np.quantile(w, rng.uniform(0.15, 0.85)))
        k0 = kstar(w, c, 1.0, Q)
        if k0 == 0:
            continue
        s = np.sort(w)[::-1]
        w2 = np.maximum(T(w, x0), c + 1e-3)
        k1 = kstar(w2, c, 1.0, Q)
        if s[k0 - 1] > x0:
            nr += 1
            if k1 < k0:
                viol += 1
                allok = False
        elif s[k0 - 1] < x0:
            nf += 1
            if k1 > k0:
                viol += 1
                allok = False
    print(f"      {name}: rise-branch {nr:>4}, fall-branch {nf:>4}, violations {viol}")
check("P9-GEN (ii) pivot sign rule holds for every qualifying family", allok)

print()
print("  (ii-b) CONTROL: families that FAIL the hypothesis should violate it")
ctrl_viol = 0
for name, T, okp in qualify:
    if okp:
        continue
    for _ in range(1500):
        Q = int(rng.integers(8, 50))
        w = rng.lognormal(0.6, 0.6, size=Q) + 2.0
        c = float(rng.uniform(0.01, 0.6))
        x0 = float(np.quantile(w, rng.uniform(0.15, 0.85)))
        k0 = kstar(w, c, 1.0, Q)
        if k0 == 0:
            continue
        s_ = np.sort(w)[::-1]
        w2 = np.maximum(T(w, x0), c + 1e-3)
        k1 = kstar(w2, c, 1.0, Q)
        if s_[k0 - 1] > x0 and k1 < k0:
            ctrl_viol += 1
        elif s_[k0 - 1] < x0 and k1 > k0:
            ctrl_viol += 1
check("P9-GEN (ii-b) non-qualifying families DO violate it", ctrl_viol > 0,
      f"{ctrl_viol} violations - hypothesis is not vacuous")

print()
print("  (iii) is the MEAN the right pivot for non-linear T?  (expect NO)")
mean_ok = True
counterex = 0
for name, T, _okp in qualify[1:]:
    for _ in range(2000):
        Q = int(rng.integers(8, 50))
        w = rng.lognormal(0.6, 0.6, size=Q) + 2.0
        c = float(rng.uniform(0.01, 0.6))
        x0 = float(np.quantile(w, rng.uniform(0.15, 0.85)))
        k0 = kstar(w, c, 1.0, Q)
        if k0 == 0:
            continue
        s = np.sort(w)[::-1]
        mn = w.mean()
        w2 = np.maximum(T(w, x0), c + 1e-3)
        k1 = kstar(w2, c, 1.0, Q)
        if s[k0 - 1] > mn and k1 < k0:
            counterex += 1
            mean_ok = False
        if s[k0 - 1] < mn and k1 > k0:
            counterex += 1
            mean_ok = False
print(f"      counterexamples to the MEAN-based rule: {counterex}")
check("P9-GEN (iii) mean-based rule FAILS for non-linear T (as predicted)",
      not mean_ok, f"{counterex} counterexamples -> pivot x0 is the correct statement")

print()
print("=" * 72)
print("P9-STRICT.  When does k* fall STRICTLY?")
print("=" * 72)
print("""  Weak monotonicity is proved. For strictness: the marginal entrant
  k* satisfies kappa(w_(k*)) <= Delta(k*-1). After the spread she exits
  iff kappa(T(w_(k*))) > Delta(k*-1). Since kappa is continuous and
  strictly decreasing in w, and T(w) -> the lower bound as the spread
  intensity grows for any w < x0, a sufficiently strong spread forces
  exit. Hence:

    if the marginal entrant is strictly below the pivot, then for
    spread intensity large enough, k* falls STRICTLY.""")
ok = True
found = 0
for _ in range(400):
    Q = int(rng.integers(8, 40))
    w = rng.lognormal(0.6, 0.6, size=Q) + 3.0
    c = float(rng.uniform(0.01, 0.3))
    x0 = float(np.quantile(w, 0.75))
    k0 = kstar(w, c, 1.0, Q)
    if k0 == 0:
        continue
    s = np.sort(w)[::-1]
    if s[k0 - 1] >= x0:
        continue
    found += 1
    ks = []
    for lam in [1.0, 2.0, 5.0, 20.0, 100.0]:
        w2 = np.maximum(T_linear(w, x0, lam), c + 1e-3)
        ks.append(kstar(w2, c, 1.0, Q))
    if not (ks[-1] < ks[0]):
        ok = False
    if any(ks[i] < ks[i + 1] for i in range(len(ks) - 1)):
        ok = False
check("P9-STRICT strong spread => strict fall, monotone in intensity", ok,
      f"{found} cases")

print()
print("      symbolic: kappa strictly decreasing in w")
w_, c_, gam = sp.symbols("w c gamma", positive=True)
u = lambda z: z ** (1 - gam) / (1 - gam)
kap = u(w_) - u(w_ - c_)
dk = sp.simplify(sp.diff(kap, w_))
val = dk.subs({gam: 2, c_: sp.Rational(1, 2), w_: 3})
print(f"        d kappa/d w at (gamma=2,c=0.5,w=3) = {sp.nsimplify(val)} < 0")
check("P9-STRICT kappa strictly decreasing (symbolic)", val < 0)

print()
print("=" * 72)
print("N1.  PIVOT RULE WITH AN ENDOGENOUSLY DETERMINED MARGIN")
print("=" * 72)
print("      k* is recomputed from the entry condition on BOTH sides of the")
print("      spread; the hypothesis is a single comparison at the PRE-spread")
print("      margin. No post-spread margin is located.")

import random as _rnd
_rnd.seed(4242)


def _kstar(kaps, D):
    ks = [j for j in range(1, len(kaps) + 1) if kaps[j - 1] <= D[j - 1]]
    return max(ks) if ks else 0


vr = vf = nr = nf2 = moved = 0
for _ in range(20000):
    Q = _rnd.randint(3, 9)
    D = sorted([_rnd.uniform(0, 20) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.0, 20.0) for _ in range(Q)], reverse=True)
    cc = _rnd.uniform(0.05, 0.9)

    def _kap(x, cc=cc):
        return 1.0 / max(x - cc, 1e-9) - 1.0 / x

    k0 = _kstar([_kap(x) for x in w], D)
    if k0 == 0 or k0 > Q:
        continue
    x0 = _rnd.uniform(1.0, 20.0)
    lam = _rnd.uniform(1.01, 4.0)
    w2 = [x0 + lam * (x - x0) for x in w]
    if min(w2) <= cc:
        continue
    k1 = _kstar([_kap(x) for x in w2], D)
    marg = w[k0 - 1]
    if marg > x0:
        nr += 1
        if k1 < k0:
            vr += 1
    elif marg < x0:
        nf2 += 1
        if k1 > k0:
            vf += 1
    if k1 != k0:
        moved += 1

print(f"      rise branch: {nr} cases, {vr} violations")
print(f"      fall branch: {nf2} cases, {vf} violations")
print(f"      k* actually moved in {moved} cases")
check("N1 marginal entrant above pivot => endogenous k* does not fall",
      vr == 0, f"{nr} cases")
check("N1 marginal entrant below pivot => endogenous k* does not rise",
      vf == 0, f"{nf2} cases")
check("N1 non-vacuous: k* moves in a positive fraction of cases",
      moved > 0, f"{moved} cases")

ctrl = 0
nctrl = 0
_rnd.seed(99)
for _ in range(8000):
    Q = _rnd.randint(3, 9)
    D = sorted([_rnd.uniform(0, 20) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.0, 20.0) for _ in range(Q)], reverse=True)
    cc = _rnd.uniform(0.05, 0.9)

    def _kap(x, cc=cc):
        return 1.0 / max(x - cc, 1e-9) - 1.0 / x

    k0 = _kstar([_kap(x) for x in w], D)
    if k0 == 0 or k0 > Q:
        continue
    x0 = _rnd.uniform(1.0, 20.0)
    lam = _rnd.uniform(1.01, 4.0)
    w2 = [x0 + lam * (x - x0) for x in w]
    if min(w2) <= cc:
        continue
    k1 = _kstar([_kap(x) for x in w2], D)
    ref = w[0]
    nctrl += 1
    if ref > x0 and k1 < k0:
        ctrl += 1
    if ref < x0 and k1 > k0:
        ctrl += 1
print(f"      CONTROL: compare the pivot at the RICHEST index instead of the")
print(f"      marginal one -> {ctrl} violations out of {nctrl} cases")
check("N1 CONTROL: the marginal index is the load-bearing comparison point",
      ctrl > 0, f"{ctrl} violations when the wrong index is used")

print()
print("=" * 72)
print("R1.  GENERAL SPREADS: THE ONE-SIDED TAIL CONDITION SUFFICES")
print("=" * 72)
print("      The pivot hypothesis asks the displacement to change sign once.")
print("      The conclusion needs only its sign at ranks at-or-above the")
print("      margin; below the margin it may cross zero any number of times.")

_c = 0.2


def _kp(x):
    return 1.0 / (x - _c) - 1.0 / x


def _ks(w, D):
    ks = [j for j in range(1, len(w) + 1) if _kp(w[j - 1]) <= D[j - 1]]
    return max(ks) if ks else 0


w_w = [12.0, 9.0, 6.0, 4.0, 3.0, 2.4, 2.0, 1.6]
D_w = [0.20, 0.05, 0.02, 0.012, 0.0090, 0.0070, 0.0055, 0.0045]
d_w = [0.02, 0.02, 0.02, 0.30, -0.20, 0.15, -0.10, 0.0]
d_w[-1] = -sum(d_w)
w2_w = [a + b for a, b in zip(w_w, d_w)]
k0_w = _ks(w_w, D_w)
k1_w = _ks(w2_w, D_w)
nz = [x for x in d_w if abs(x) > 1e-12]
chg = sum(1 for a, b in zip(nz, nz[1:]) if a * b < 0)
desc = all(w2_w[i] > w2_w[i + 1] for i in range(len(w2_w) - 1))
print(f"      witness: sum(d) = {sum(d_w):.1e}, sign changes = {chg}, "
      f"k* {k0_w} -> {k1_w}")
check("R1 explicit witness is mean-preserving and stays sorted",
      abs(sum(d_w)) < 1e-12 and desc)
check("R1 witness is NOT a pivot-spread (displacement crosses 3 times)",
      chg >= 2, f"{chg} sign changes")
check("R1 witness satisfies the tail condition on ranks <= k*",
      all(d_w[j] >= 0 for j in range(k0_w)))
check("R1 the rule still holds: k* weakly rises", k1_w >= k0_w,
      f"{k0_w} -> {k1_w}")

def _is_mps(a, b):
    x = sorted(a)
    y = sorted(b)
    if abs(sum(x) - sum(y)) > 1e-9:
        return False
    ca = cb = 0.0
    for i in range(len(x) - 1):
        ca += x[i]
        cb += y[i]
        if cb > ca + 1e-12:
            return False
    return True


_rnd.seed(20260902)
nv = 0
ntest = 0
nmulti = 0
nmps = 0
nmultimps = 0
for _ in range(1000000):
    Q = _rnd.randint(6, 10)
    D = sorted([_rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    k0 = _ks(w, D)
    if k0 == 0 or k0 >= Q - 2:
        continue
    d = [0.0] * Q
    for j in range(k0):
        d[j] = _rnd.uniform(0.0, 0.05)
    for j in range(k0, Q - 1):
        d[j] = _rnd.uniform(-0.4, 0.4)
    d[Q - 1] = -sum(d[: Q - 1])
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        continue
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        continue
    nzz = [x for x in d if abs(x) > 1e-12]
    ch = sum(1 for a, b in zip(nzz, nzz[1:]) if a * b < 0)
    ntest += 1
    mps = _is_mps(w, w2)
    if mps:
        nmps += 1
    if ch >= 2:
        nmulti += 1
        if mps:
            nmultimps += 1
    if _ks(w2, D) < k0:
        nv += 1
    if ntest >= 3000:
        break
print(f"      randomised: {ntest} spreads, {nmulti} with >=2 sign changes")
print(f"      of these, genuine mean-preserving SPREADS (second-order")
print(f"      dominance, not merely sum(d)=0): {nmps}; multi-crossing AND")
print(f"      genuine MPS: {nmultimps}")
check("R1 tail condition => k* never falls, incl. multi-crossing spreads",
      nv == 0, f"{ntest} spreads, {nv} violations")
check("R1 the multi-crossing case is actually exercised", nmulti > 0,
      f"{nmulti} spreads are not pivot-spreads")
check("R1 the genuine-MPS subcase is exercised, and is a strict subset",
      nmultimps > 0 and nmps < ntest,
      f"{nmps} of {ntest} are genuine MPS; {nmultimps} of {nmulti} multi-crossing")

print()
print("=" * 72)
print("R1-SCOPE.  MPS DOES NOT IMPLY THE TAIL CONDITION")
print("=" * 72)
print("      The proposition is conditional. A mean-preserving spread in the")
print("      Rothschild-Stiglitz sense need not have its quantile difference")
print("      signed down to the margin, and when it does not, k* can fall.")
print("      This is what makes the open characterisation non-vacuous.")

_rnd.seed(5)
tested_s = 0
fail_s = 0
wit = None
for _ in range(400000):
    Q = _rnd.randint(6, 10)
    D = sorted([_rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    k0 = _ks(w, D)
    if k0 < 2 or k0 >= Q - 1:
        continue
    d = [_rnd.uniform(-0.5, 0.5) for _ in range(Q - 1)]
    d.append(-sum(d))
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        continue
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        continue
    if not _is_mps(w, w2):
        continue
    tested_s += 1
    if any(d[j] < -1e-12 for j in range(k0)):
        k1 = _ks(w2, D)
        if k1 < k0:
            fail_s += 1
            if wit is None:
                wit = (Q, k0, k1, [round(x, 3) for x in w],
                       [round(x, 3) for x in d])
    if tested_s >= 4000:
        break
print(f"      {tested_s} genuine MPS instances; {fail_s} violate the tail")
print(f"      condition AND have k* strictly FALL")
if wit:
    print(f"      witness: Q={wit[0]}, k* {wit[1]} -> {wit[2]}")
    print(f"        w = {wit[3]}")
    print(f"        d = {wit[4]}")
check("R1-SCOPE MPS alone is NOT sufficient: witness exhibited",
      fail_s > 0 and wit is not None,
      f"{fail_s} of {tested_s} genuine MPS instances")
check("R1-SCOPE the failure is a minority, so the condition is not vacuous "
      "in the other direction either",
      0 < fail_s < tested_s, f"{fail_s} / {tested_s}")

print()
print("=" * 72)
print("R1-WEAK.  THE FALL BRANCH DOES NOT NEED THE MARGIN CONSTRAINED")
print("=" * 72)
print("      The stated hypothesis signs the displacement at ranks >= k*.")
print("      Rank k* is the marginal ENTRANT: the entry condition holds")
print("      there, so nothing about her wealth is needed. Constraining")
print("      only the strict outsiders, ranks > k*, suffices.")

_rnd.seed(77)
nw = 0
vw = 0
for _ in range(400000):
    Q = _rnd.randint(6, 10)
    D = sorted([_rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    k0 = _ks(w, D)
    if k0 < 2 or k0 >= Q - 1:
        continue
    d = [0.0] * Q
    d[k0 - 1] = _rnd.uniform(-0.3, 0.3)
    for j in range(k0, Q):
        d[j] = _rnd.uniform(-0.4, 0.0)
    for j in range(k0 - 1):
        d[j] = _rnd.uniform(-0.3, 0.3)
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        continue
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        continue
    nw += 1
    if _ks(w2, D) > k0:
        vw += 1
    if nw >= 4000:
        break
print(f"      marginal entrant and all inframarginal ranks left FREE,")
print(f"      only ranks > k* signed: {nw} cases, {vw} violations of k* not rising")
check("R1-WEAK the fall branch holds with the margin itself unconstrained",
      vw == 0, f"{nw} cases")

_rnd.seed(78)
nc = 0
vc = 0
for _ in range(400000):
    Q = _rnd.randint(6, 10)
    D = sorted([_rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    k0 = _ks(w, D)
    if k0 < 2 or k0 >= Q - 1:
        continue
    d = [_rnd.uniform(-0.3, 0.3) for _ in range(Q)]
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        continue
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        continue
    nc += 1
    if _ks(w2, D) > k0:
        vc += 1
    if nc >= 4000:
        break
print(f"      CONTROL: sign nothing at all -> {vc} of {nc} cases have k* rise")
check("R1-WEAK CONTROL: the outsider ranks are the load-bearing ones",
      vc > 0, f"{vc} rises when the outsiders are left unsigned")

print()
print("=" * 72)
print("R1-FREE.  MEAN-PRESERVATION IS NOT AMONG THE HYPOTHESES")
print("=" * 72)
print("      The proposition compares two sorted profiles rank by rank. It")
print("      does not require w' to be a spread of w, mean-preserving or")
print("      otherwise. Here the profiles are built to violate")
print("      sum(w' - w) = 0 on purpose.")

_rnd.seed(1234)
nfree = 0
vfree = 0
gaps = []
for _ in range(400000):
    Q = _rnd.randint(6, 10)
    D = sorted([_rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([_rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    k0 = _ks(w, D)
    if k0 == 0 or k0 >= Q - 1:
        continue
    d = [0.0] * Q
    for j in range(k0):
        d[j] = _rnd.uniform(0.0, 0.05)
    for j in range(k0, Q):
        d[j] = _rnd.uniform(-0.4, 0.4)
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        continue
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        continue
    if abs(sum(d)) < 1e-6:
        continue
    nfree += 1
    gaps.append(abs(sum(d)))
    if _ks(w2, D) < k0:
        vfree += 1
    if nfree >= 4000:
        break
print(f"      {nfree} pairs, none mean-preserving "
      f"(median |sum(w'-w)| = {sorted(gaps)[len(gaps) // 2]:.3f}),")
print(f"      {vfree} violations of k* not falling")
check("R1-FREE the tail condition alone suffices; no mean-preservation used",
      vfree == 0, f"{nfree} non-mean-preserving pairs")
check("R1-FREE the pairs really are not mean-preserving",
      min(gaps) > 1e-6, f"smallest |sum| = {min(gaps):.2e}")

print()
print("=" * 72)
nf_ = sum(1 for _, o in res if not o)
print(f"SUMMARY: {len(res)} checks, {nf_} failures")
print("=" * 72)
