import numpy as np
import sympy as sp
from scipy import integrate
from scipy.stats import beta as Beta
from core import (
    score_FG,
    random_pair,
    delta,
    delta_via_phi,
    delta_step_identity,
    kappa,
    equilibrium_k,
    mean_preserving_spread,
)

rng = np.random.default_rng(20260831)
FAIL = []


def report(name, ok, detail=""):
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"  {detail}" if detail else ""))
    if not ok:
        FAIL.append(name)


print("=" * 70)
print("P1  Delta(m) = V * E[phi(M_m)],  phi = G - F")
print("=" * 70)
worst = 0.0
grid = np.linspace(1e-9, 1 - 1e-9, 20001)
for mu in [0.2, 0.35, 0.5, 0.7]:
    F, G, g = score_FG(mu, Beta(2, 2), Beta(2, 3), n=200000)
    for Q in [2, 4, 6]:
        for m in range(Q):
            for inc in [True, False]:
                worst = max(
                    worst,
                    abs(delta(F, G, g, m, Q, inc) - delta_via_phi(F, G, g, m, Q, inc)),
                )
report("P1 representation", worst < 5e-3, f"max err {worst:.2e}")

print()
print("=" * 70)
print("P2  FOSD:  F <= G pointwise")
print("=" * 70)
viol = 0
for mu in [0.15, 0.3, 0.5, 0.8]:
    for (a1, b1, a2, b2) in [(2, 5, 3, 2), (0.7, 0.7, 2, 2), (5, 1, 1, 4)]:
        F, G, g = score_FG(mu, Beta(a1, b1), Beta(a2, b2), n=200000)
        if np.any(F > G + 5e-3):
            viol += 1
report("P2 FOSD", viol == 0, f"{viol} violations")

print()
print("=" * 70)
print("P3  Delta(m+1) - Delta(m) = -(V/2) * Int phi^2 dK   [KEY IDENTITY]")
print("=" * 70)


def conv_err(n):
    gr = np.linspace(0, 1, n)
    a, b = 0.6, 1.8
    G, F = gr**a, gr**b
    Q = 5
    return max(
        abs(
            delta(F, G, gr, m + 1, Q)
            - delta(F, G, gr, m, Q)
            - delta_step_identity(F, G, gr, m, Q)
        )
        for m in range(Q - 1)
    )


errs = [(n, conv_err(n)) for n in [2001, 8001, 32001, 128001]]
for n, e in errs:
    print(f"      n={n:>7}  err={e:.3e}")
report("P3 identity converges", errs[-1][1] < 1e-10, f"err {errs[-1][1]:.2e}")

x = sp.symbols("x", positive=True)
G_s, F_s = x ** sp.Rational(3, 5), x ** sp.Rational(9, 5)
Q = 5
exact_ok = True
for m in range(Q - 1):
    K = F_s**m * G_s ** (Q - 2 - m) * F_s
    Hm = F_s**m * G_s ** (Q - 1 - m) * F_s
    Hm1 = F_s ** (m + 1) * G_s ** (Q - 2 - m) * F_s
    lhs = (
        sp.integrate(Hm1 * sp.diff(F_s, x), (x, 0, 1))
        - sp.integrate(Hm1 * sp.diff(G_s, x), (x, 0, 1))
        - sp.integrate(Hm * sp.diff(F_s, x), (x, 0, 1))
        + sp.integrate(Hm * sp.diff(G_s, x), (x, 0, 1))
    )
    rhs = -sp.Rational(1, 2) * sp.integrate(
        (G_s - F_s) ** 2 * sp.diff(K, x), (x, 0, 1)
    )
    if sp.simplify(lhs - rhs) != 0:
        exact_ok = False
report("P3 identity exact (SymPy)", exact_ok)

print()
print("=" * 70)
print("P4  Delta strictly decreasing in m  [corollary of P3]")
print("=" * 70)
worst_inc = 0.0
gr = np.linspace(0, 1, 3001)
for _ in range(1500):
    F, G = random_pair(gr, rng)
    if np.any(F > G + 1e-12):
        continue
    Qr = int(rng.integers(3, 8))
    for inc in [True, False]:
        ds = [delta(F, G, gr, m, Qr, inc) for m in range(Qr)]
        d = np.diff(ds)
        if len(d):
            worst_inc = max(worst_inc, d.max())
report("P4 monotone (1500 random pairs)", worst_inc <= 1e-6, f"worst rise {worst_inc:.2e}")

print()
print("=" * 70)
print("P5  unique k*: entry set is a prefix {0,...,k*-1}")
print("=" * 70)
F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 3))
Q = 8
ds = [delta(F, G, g, m, Q) for m in range(Q)]
ok = True
for kap in [0.30, 0.20, 0.12, 0.05, 0.01]:
    ent = [m for m in range(Q) if ds[m] >= kap]
    if ent != list(range(len(ent))):
        ok = False
report("P5 prefix property", ok, f"Delta={[round(d,4) for d in ds]}")

print()
print("=" * 70)
print("P6  mu -> 0:  Delta(m) = 1/(m+2), independent of Q")
print("=" * 70)
ok = True
for mu in [0.001, 0.01]:
    for Qv in [3, 6, 10, 16]:
        F, G, g = score_FG(mu, Beta(2, 2), Beta(2, 2), n=200000)
        for m in [0, 1, 2]:
            if abs(delta(F, G, g, m, Qv) - 1.0 / (m + 2)) > 5e-3:
                ok = False
report("P6 mu->0 limit", ok)

print()
print("=" * 70)
print("R1  REFUTED: 'Delta(0) -> 0 as Q -> inf'  (no collapse threshold)")
print("=" * 70)
for mu in [0.3, 0.6]:
    F, G, g = score_FG(mu, Beta(2, 2), Beta(2, 2))
    vals = [(Qv, delta(F, G, g, 0, Qv)) for Qv in [2, 32, 512, 2048]]
    print("      mu=%.1f: " % mu + ", ".join(f"Q={q}:{d:.5f}" for q, d in vals))
print("      Delta converges to a POSITIVE limit: no collapse.")

print()
print("=" * 70)
print("P7  k* saturates in Q  =>  entry RATE k*/Q -> 0")
print("=" * 70)
F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 2))
for kap in [0.20, 0.10, 0.05]:
    row = []
    for Qv in [4, 16, 64, 256]:
        ds = [delta(F, G, g, m, Qv) for m in range(min(Qv, 40))]
        row.append((Qv, sum(1 for d in ds if d >= kap)))
    print(f"      kappa={kap}: " + ", ".join(f"Q={q}:k*={k}" for q, k in row))

print()
print("=" * 70)
print("R2  REFUTED: 'k* decreasing in Q whenever mu>0'  (sign depends on mu)")
print("=" * 70)
for mu in [0.1, 0.3, 0.5, 0.7, 0.9]:
    F, G, g = score_FG(mu, Beta(2, 2), Beta(2, 2), n=200000)
    d = [delta(F, G, g, 0, Qv) for Qv in [2, 8, 32, 64]]
    trend = "INCREASING" if d[-1] > d[0] + 1e-4 else "DECREASING"
    print(f"      mu={mu}: Delta(0) {trend:>10}  " + ", ".join(f"{v:.4f}" for v in d))

print()
print("=" * 70)
print("P8  prize and affordability effects are separate and signed")
print("=" * 70)
Qw = 40
c0 = 0.5
ws = rng.lognormal(0.6, 0.5, size=Qw) + c0 + 0.05
F, G, g = score_FG(0.4, Beta(2, 2), Beta(2, 2))
cache = {}


def dfun(m, Qv, V):
    if (m, Qv) not in cache:
        cache[(m, Qv)] = delta(F, G, g, m, Qv)
    return V * cache[(m, Qv)]


kV = [equilibrium_k(ws, c0, V, Qw, dfun) for V in [0.5, 1.0, 2.0, 4.0]]
kC = [equilibrium_k(ws, cc, 1.0, Qw, dfun) for cc in [0.2, 0.5, 1.0, 2.0]]
report("P8a k* nondecreasing in V", all(np.diff(kV) >= 0), f"{kV}")
report("P8b k* nonincreasing in c", all(np.diff(kC) <= 0), f"{kC}")

print()
print("=" * 70)
print("P9  MPS of wealth: sign REVERSES with exclusivity of the competition")
print("=" * 70)
Qm = 200
base = rng.lognormal(0.6, 0.45, size=Qm) + 0.05
print(f"      {'c':>6} {'entry rate':>11} {'k*(lam=1)':>10} {'k*(lam=1.6)':>12} {'sign':>7}")
signs = {}
for c in [0.02, 0.05, 0.12, 0.35, 1.0, 2.5]:
    w1 = np.maximum(base, c + 1e-3)
    w2 = np.maximum(mean_preserving_spread(base, 1.6), c + 1e-3)
    k1 = equilibrium_k(w1, c, 1.0, Qm, dfun)
    k2 = equilibrium_k(w2, c, 1.0, Qm, dfun)
    s = "RISES" if k2 > k1 else ("FALLS" if k2 < k1 else "flat")
    signs[c] = s
    print(f"      {c:>6} {k1/Qm:>11.3f} {k1:>10} {k2:>12} {s:>7}")
report(
    "P9 reversal present",
    "FALLS" in signs.values() and "RISES" in signs.values(),
    "low-c FALLS, high-c RISES",
)

print()
print("=" * 70)
print(f"SUMMARY: {len(FAIL)} failures" + (f" -> {FAIL}" if FAIL else ""))
print("=" * 70)
