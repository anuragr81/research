"""A1: is anonymity load-bearing, and what happens when it fails?

`prop:anon` says Delta is a function of the entrant COUNT alone, not of which
agents entered.  Everything downstream of it -- count invariance (P5-inv), the
endogenous-margin proposition, the tail condition, and assortative selection --
is conditional on it.  Until this suite existed, nothing varied the assumption:
`verify_equilibria.py` tests anonymity with a SYNTHETIC gain schedule
(`gain_anon` vs `gain_id`), which shows the algebra needs anonymity but does not
show whether the model's own primitives deliver it, nor what breaks when they
do not.

Here the primitives are the model's.  Scores are sigma_i = mu*r_i + (1-mu)*s_i*e_i
with r_i ~ U[0,1] wealth-independent, and ability tilted against wealth by
theta:  P(s_i <= x) = x^{a_i} on [0,1], a_i = exp(theta * z_i), where z_i runs
from +1 at the richest rank to -1 at the poorest.  theta = 0 restores the
model's assumption exactly (all a_i = 1, so s_i ~ U[0,1] independent of wealth);
theta > 0 makes the rich abler, theta < 0 the poor abler.

Delta is computed by DETERMINISTIC QUADRATURE, not Monte Carlo.  This matters:
the Nash conditions are decided by comparing kappa_i against Delta_i, so a noisy
Delta flips near-ties at random and manufactures spurious multiplicity.  The
earlier ad hoc probe was Monte Carlo and its rate was therefore indicative
only.  A1-1 below bounds the quadrature error so every reported violation can
be checked against it.
"""
import itertools
import numpy as np

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


# ----------------------------------------------------------------------
# model-induced primitives
# ----------------------------------------------------------------------

def entrant_cdf(a, mu, tgrid, nu):
    """CDF of mu*r + (1-mu)*s with r ~ U[0,1] and P(s <= x) = x^a on [0,1]."""
    u = np.linspace(0.0, 1.0, nu)
    X = (tgrid[:, None] - mu * u[None, :]) / (1.0 - mu)
    np.clip(X, 0.0, 1.0, out=X)
    return np.trapezoid(X**a, u, axis=1)


def outsider_cdf(mu, tgrid):
    """CDF of mu*r: the outsider draws no ability component."""
    return np.clip(tgrid / mu, 0.0, 1.0)


def build_tables(Q, theta, mu, ngrid=2001, nu=2001):
    """P[S][i] = Pr(i wins) when the entrant set is S.  Rank 0 is richest.

    The ability exponents depend only on RANK, so the win-probability table is
    a function of (Q, theta, mu) alone and is reused across wealth draws --
    wealth enters only through kappa.
    """
    t = np.linspace(0.0, 1.0, ngrid)
    z = np.linspace(1.0, -1.0, Q) if Q > 1 else np.array([0.0])
    a = np.exp(theta * z)
    ent = np.array([entrant_cdf(ai, mu, t, nu) for ai in a])
    out = outsider_cdf(mu, t)
    inc = entrant_cdf(1.0, mu, t, nu)          # incumbent enters, neutral ability

    P = {}
    for r in range(Q + 1):
        for S in itertools.combinations(range(Q), r):
            S = frozenset(S)
            cdfs = [ent[i] if i in S else out for i in range(Q)]
            probs = np.empty(Q)
            for i in range(Q):
                prod = inc.copy()
                for j in range(Q):
                    if j != i:
                        prod = prod * cdfs[j]
                probs[i] = np.trapezoid(np.gradient(cdfs[i], t) * prod, t)
            P[S] = probs
    return P


def delta_table(P, Q):
    """D[(i, T)] = gain to i from entering when the OTHER entrants are T."""
    D = {}
    for r in range(Q + 1):
        for T in itertools.combinations(range(Q), r):
            T = frozenset(T)
            for i in range(Q):
                if i not in T:
                    D[(i, T)] = P[T | {i}][i] - P[T][i]
    return D


def kappa(w, c, gamma=2.0):
    f = lambda x: x ** (1 - gamma) / (1 - gamma)
    return f(w) - f(np.maximum(w - c, 1e-12))


def nash_sets(D, kap, Q):
    """Every pure-strategy entrant set, with the tightest decision margin."""
    eqs, margins = [], []
    for r in range(Q + 1):
        for S in itertools.combinations(range(Q), r):
            S = frozenset(S)
            ok, mgn = True, np.inf
            for i in range(Q):
                d = D[(i, S - {i})] if i in S else D[(i, S)]
                mgn = min(mgn, abs(kap[i] - d))
                if (kap[i] <= d) != (i in S):
                    ok = False
                    break
            if ok:
                eqs.append(S)
                margins.append(mgn)
    return eqs, margins


MU, C, Q4 = 0.5, 1.0, 4

print("=" * 72)
print("A1-1  QUADRATURE ERROR BOUND  (so 'violation' can be distinguished")
print("      from 'numerical noise' -- the gap the Monte Carlo probe left)")
print("=" * 72)
prev, prev_ng, deltas = None, None, []
for ng in [501, 1001, 2001, 4001]:
    P = build_tables(Q4, -0.9, MU, ngrid=ng, nu=ng)
    D = delta_table(P, Q4)
    vec = np.array([D[k] for k in sorted(D, key=lambda x: (x[0], sorted(x[1])))])
    if prev is not None:
        d = np.abs(vec - prev).max()
        deltas.append((prev_ng, ng, d))
        print(f"      ngrid {prev_ng:>5} -> {ng:>5}:   max |dDelta| = {d:.3e}")
    prev, prev_ng = vec, ng
EPS = deltas[-1][2]
ratios = [deltas[i][2] / deltas[i + 1][2] for i in range(len(deltas) - 1)]
print(f"      refinement ratios {['%.1f' % r for r in ratios]}  (steady contraction;")
print("      below the trapezoid's O(h^2) because clipping puts a kink in the")
print("      integrand -- the bound is what is used, not the rate)")
print(f"      => residual error at ngrid=2001 is bounded by EPS = {EPS:.3e}")
check("A1-1 Delta table converges, error bounded", EPS < 1e-5,
      f"EPS = {EPS:.2e}; every violation below is reported with its margin")

print()
print("=" * 72)
print("A1-2  ANONYMITY HOLDS AT theta = 0, ON THE MODEL'S OWN PRIMITIVES")
print("=" * 72)
P0 = build_tables(Q4, 0.0, MU)
D0 = delta_table(P0, Q4)
by_size = {}
for (i, T), v in D0.items():
    by_size.setdefault(len(T), []).append(v)
spread0 = max(max(v) - min(v) for v in by_size.values())
print(f"      max spread of Delta within a fixed count class: {spread0:.3e}")
print("      (Delta_i(T) compared across every identity i and every T of the")
print("       same size; anonymity says this spread is zero)")
check("A1-2 Delta depends on the COUNT alone when draws are wealth-independent",
      spread0 < 1e-12,
      "prop:anon verified on induced primitives, not assumed")

print()
print("=" * 72)
print("A1-3  CONTROL: anonymity FAILS once ability correlates with wealth")
print("=" * 72)
rows = []
allbroken = True
for th in [0.3, 0.6, 0.9, -0.3, -0.6, -0.9]:
    D = delta_table(build_tables(Q4, th, MU), Q4)
    bs = {}
    for (i, T), v in D.items():
        bs.setdefault(len(T), []).append(v)
    sp = max(max(v) - min(v) for v in bs.values())
    rows.append(f"theta={th:+.1f}: {sp:.2e}")
    if sp <= 1e-6:
        allbroken = False
print("      max spread of Delta within a count class:")
print("      " + "; ".join(rows))
check("A1-3 CONTROL: the test can detect identity-dependence", allbroken,
      "so A1-2 is not vacuous -- it would have failed had anonymity failed")

print()
print("=" * 72)
print("A1-4  COUNT INVARIANCE ACROSS THE TILT")
print("=" * 72)
print("      Enumerating every pure-strategy entrant set, per wealth draw.")
print("      'count mult' = equilibria of DIFFERENT sizes coexist (breaks")
print("      P5-inv).  'id mult' = same size, different members (Finding 1,")
print("      expected and harmless).")
TRIALS = 4000
print(f"      Q=4, mu={MU}, c={C}, CRRA gamma=2, {TRIALS} wealth draws per theta")
print()
print(f"      {'theta':>7} {'inst':>6} {'countMult':>10} {'idMult':>8} {'min margin':>12}")
summary = {}
for th in [0.0, 1.5, 0.9, 0.6, 0.3, -0.3, -0.6, -0.9, -1.5]:
    D = delta_table(build_tables(Q4, th, MU), Q4)
    rng = np.random.default_rng(20260902)
    inst = cmult = idmult = 0
    worst = []
    for _ in range(TRIALS):
        w = np.sort(rng.uniform(C + 0.15, 8.0, Q4))[::-1]
        eqs, mgns = nash_sets(D, kappa(w, C), Q4)
        if not eqs:
            continue
        inst += 1
        if len(eqs) > 1:
            idmult += 1
        if len({len(S) for S in eqs}) > 1:
            cmult += 1
            worst.append(min(mgns))
    summary[th] = (inst, cmult, idmult, min(worst) if worst else None)
    mm = f"{min(worst):.2e}" if worst else "-"
    print(f"      {th:>+7.1f} {inst:>6} {cmult:>10} {idmult:>8} {mm:>12}")

pos_bad = sum(summary[t][1] for t in [0.0, 0.3, 0.6, 0.9, 1.5])
neg_bad = sum(summary[t][1] for t in [-0.3, -0.6, -0.9, -1.5])
neg_margins = [summary[t][3] for t in [-0.3, -0.6, -0.9, -1.5] if summary[t][3]]

print()
check("A1-4a count invariance holds at theta = 0 (the model's assumption)",
      summary[0.0][1] == 0, f"{summary[0.0][0]} instances, 0 with mixed sizes")
check("A1-4b count invariance holds for theta > 0 (ability rising in wealth)",
      pos_bad == 0,
      "SUPPORTED, NOT ESTABLISHED: 0 violations is weak evidence FOR a "
      "universal claim (see PROOFS.tex tiers, tier N)")
check("A1-4c CONTROL: it FAILS for theta < 0 (ability falling in wealth)",
      neg_bad > 0,
      f"{neg_bad} instances with equilibria of different sizes; a single "
      f"genuine violation refutes, so this direction IS established")
if neg_margins:
    m = min(neg_margins)
    print(f"      tightest violating margin {m:.2e} vs quadrature error "
          f"{EPS:.2e}  ->  {m / EPS:.0f}x")
    check("A1-4d the violations are genuine, not quadrature noise",
          m > 100 * EPS, "margin exceeds the error bound by >100x")

print()
print("=" * 72)
print("A1-5  AN EXPLICIT WITNESS  (a fact, not a rate)")
print("=" * 72)
D = delta_table(build_tables(Q4, -1.5, MU), Q4)
rng = np.random.default_rng(7)
found = None
for _ in range(60000):
    w = np.sort(rng.uniform(C + 0.15, 8.0, Q4))[::-1]
    eqs, mgns = nash_sets(D, kappa(w, C), Q4)
    if len({len(S) for S in eqs}) > 1 and min(mgns) > 1000 * EPS:
        found = (w, eqs, min(mgns))
        break
if found:
    w, eqs, mgn = found
    print(f"      theta = -1.5, mu = {MU}, c = {C}, CRRA gamma = 2")
    print(f"      wealths (richest first): {np.array2string(w, precision=6)}")
    print(f"      kappa:                   {np.array2string(kappa(w, C), precision=6)}")
    print(f"      equilibria: {sorted([tuple(sorted(S)) for S in eqs], key=len)}")
    print(f"      sizes: {sorted({len(S) for S in eqs})}   margin {mgn:.2e} "
          f"({mgn / EPS:.0f}x the quadrature error)")
check("A1-5 an explicit two-size witness exists and is reproducible",
      found is not None,
      "count invariance is not merely 'rare' under a negative tilt -- it is "
      "false, with a displayed counterexample")

print()
print("      WHAT THIS DOES AND DOES NOT SETTLE.  Established: anonymity is")
print("      delivered by wealth-independent draws (A1-2), it is genuinely")
print("      load-bearing (A1-3), and count invariance FAILS when ability runs")
print("      against wealth (A1-4c, A1-5) -- a negative result, which sampling")
print("      can establish because one violation refutes.  NOT established:")
print("      that theta >= 0 is sufficient.  A1-4b is 0 violations in a finite")
print("      sample, which is tier N evidence for a universal claim and is")
print("      therefore SUPPORT, not proof.  Nothing currently claimed in")
print("      PROOFS.tex is threatened: the model assumes wealth-independent")
print("      draws and so sits at theta = 0 by construction.  What changes is")
print("      that the assumption now has a demonstrated boundary rather than a")
print("      caveat sentence.")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"ANONYMITY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
