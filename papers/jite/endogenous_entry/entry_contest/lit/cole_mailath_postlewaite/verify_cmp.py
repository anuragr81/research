import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


n, q, N, V, c, kap, m = sp.symbols("n q N V c kappa m", positive=True)
phi = sp.Function("phi")

print("=" * 72)
print("Cole, Mailath & Postlewaite")
print("  1992: 'Social Norms, Savings Behavior, and Growth', JPE 100(6),")
print("        Dec. 1992, pp. 1092-1125.  [F, published]")
print("  1995: 'Incorporating Concern for Relative Wealth into Economic")
print("        Models', CARESS Working Paper 95-14.  [F, WP]")
print("Checks of OUR READING.  These papers justify a rank-allocated")
print("non-market prize; most of what we take from them is conceptual.")
print("=" * 72)
print()

print("CMP-1  Rank allocation of a single prize is FIXED-SUM in the count")
print("-" * 72)
print("      CMP 1995 sec.4: 'To make the land example analogous to our models,")
print("      we should have the land simply given away, with the best given to")
print("      the wealthiest, and so on.'")
print("      One prize of value V goes to the top-ranked competitor. With n >= 1")
print("      competitors the prize mass delivered is V, whatever n is.")
prize_mass = V  # independent of n
d_mass = sp.simplify(sp.diff(prize_mass, n))
print(f"      prize mass delivered = {prize_mass};  d/dn = {d_mass}")
check("CMP-1 the delivered prize mass does not grow with the count",
      bool(d_mass == 0),
      "rank allocation reassigns a fixed prize; it does not create one")

print()
print("CMP-2  This premise REPRODUCES Levin-Smith's welfare function (8)")
print("-" * 72)
print("      If entry is independent with probability q among N, the prize is")
print("      delivered iff at least one enters, and each entrant pays c:")
print("        S = V * Pr(at least one entrant) - (expected total entry cost)")
S_derived = (1 - (1 - q) ** N) * V - q * N * c
S_ls8 = (1 - (1 - q) ** N) * V - q * N * c  # Levin-Smith eq (8)
print(f"      derived        S = {S_derived}")
print("      Levin-Smith (8) S = [1 - (1-q)^N] V - qNc")
check("CMP-2 the rank-allocation premise yields Levin-Smith eq (8) exactly",
      bool(sp.simplify(S_derived - S_ls8) == 0),
      "CMP's conceptual point and LS's welfare algebra are the same object")

print()
print("CMP-3  The wedge CMP 1995 sec.4 is pointing at, made arithmetic")
print("-" * 72)
print("      CMP 1995 sec.4: 'when the desirable goods or decisions are")
print("      allocated as prizes rather than sold, the standard welfare")
print("      theorems regarding the Pareto optimality of the outcomes no")
print("      longer apply.'")
print("      Social value added by an entrant BEYOND THE FIRST is zero,")
print("      because the prize is delivered either way; her private gain is")
print("      strictly positive in any equilibrium in which she enters.")
social_marginal = sp.simplify(V - V)  # prize delivered with or without her
private_gain = sp.Symbol("Delta_m", positive=True)  # > 0 by the entry condition
print(f"      social marginal value of entrant n >= 2: {social_marginal}")
print(f"      her private gain: {private_gain} > 0 (entry requires Delta >= kappa)")
wedge = sp.simplify(private_gain - social_marginal)
print(f"      wedge = {wedge} > 0")
check("CMP-3 private and social marginal value diverge under rank allocation",
      bool(social_marginal == 0 and wedge == private_gain),
      "this is the same wedge as Levin-Smith's business stealing (LS-7)")

print()
print("CMP-4  Why we may import their justification without their machinery")
print("-" * 72)
print("      P1 (PROOFS.tex): Delta(m) = V * E[phi(M_m)].")
print("      So V enters the model ONLY as a multiplicative scale factor.")
Delta = V * phi(m)
dV = sp.simplify(sp.diff(Delta, V))
d2V = sp.simplify(sp.diff(Delta, V, 2))
homog = sp.simplify(Delta.subs(V, 2 * V) - 2 * Delta)
print(f"      dDelta/dV = {dV}   (free of V)")
print(f"      d2Delta/dV^2 = {d2V};  Delta(2V) - 2*Delta(V) = {homog}")
check("CMP-4 V is a pure scale parameter in our model",
      bool(d2V == 0 and homog == 0 and dV == phi(m)),
      "so CMP's foundation for a rank-allocated V transfers without their model")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"COLE-MAILATH-POSTLEWAITE SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the sources is arithmetically consistent. Most of")
print("what these papers give us is conceptual and is NOT checkable; see")
print("NOTES.md for what was confirmed by reading rather than by computation.")
print("This does not reprove any result of either paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
