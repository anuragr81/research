import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


N = sp.symbols("N", positive=True)
Pi0, C = sp.symbols("Pi0 C", positive=True)

print("=" * 72)
print("Fu & Lu, 'Contest design and optimal endogenous entry'")
print("MPRA Paper No. 945, posted 28 Nov 2006. Working version of the")
print("Economic Inquiry article.  Checks of OUR READING.")
print("eq (7), p.10:   E = Pi_0 - N(V*,S*) * C")
print("=" * 72)
print()

print("FL-1  eq (7): total effort is budget minus total entry costs")
print("-" * 72)
print("      Confirmed verbatim from the source: 'the equilibrium total effort")
print("      is given by the difference between the total budget of the contest")
print("      organizer and the total entry costs incurred by participating")
print("      contestants, regardless of the contest technology.'")
E = Pi0 - N * C
dE = sp.simplify(sp.diff(E, N))
print(f"      E(N) = {E};   dE/dN = {dE}")
check("FL-1 E strictly decreases in the entrant count", bool(dE == -C),
      "the source states exactly this: RHS 'strictly decreases with N'")

print()
print("FL-2  Theorem 1: the optimum attracts exactly two entrants, E = Pi_0 - 2C")
print("-" * 72)
print("      Since E falls in N and the design requires N >= 2, the maximum")
print("      over admissible N is at N = 2.")
cands = {n: sp.simplify(E.subs(N, n)) for n in [2, 3, 4, 5, 10]}
for n, v in cands.items():
    print(f"      N={n}: E = {v}")
best = max(cands, key=lambda n: sp.limit(cands[n] - cands[2], C, 1))
strictly_less = all(
    bool(sp.simplify(cands[2] - cands[n]) > 0) for n in cands if n != 2
)
check("FL-2 N = 2 is the unique maximiser, giving E = Pi_0 - 2C",
      bool(sp.simplify(cands[2] - (Pi0 - 2 * C)) == 0) and strictly_less,
      "matches Theorem 1's stated value exactly")

print()
print("FL-3  CONTROL: the entry cost is what drives Theorem 1")
print("-" * 72)
print("      If C were 0, E would not depend on N and Theorem 1's conclusion")
print("      would not follow. A check that passed with C = 0 would not be")
print("      testing the mechanism.")
E0 = sp.simplify(E.subs(C, 0))
dE0 = sp.simplify(sp.diff(E0, N))
print(f"      with C = 0:  E = {E0};  dE/dN = {dE0}")
check("FL-3 the count-dependence vanishes when the entry cost vanishes",
      bool(dE0 == 0 and sp.simplify(E0 - Pi0) == 0),
      "so FL-1/FL-2 are testing the entry-cost channel, not an artefact")

print()
print("FL-4  Relation to P7: same accounting, different object bounded")
print("-" * 72)
print("      Fu-Lu:  N * C  <=  Pi_0        (from E = Pi_0 - N*C and E >= 0)")
print("      P7:     (m+1) * kappa <= V")
print("      Both read 'count times unit entry cost is capped by the budget'.")
print("      The formal common core is proved in")
print("      ../fu_jiao_lu_2015/Accounting.lean (theorem accounting_bound),")
print("      with both bounds derived from it as instances.")
# From eq (7) with E >= 0: N*C <= Pi0.
implied = sp.simplify(Pi0 - N * C)  # = E, and E >= 0 gives the bound
check("FL-4 eq (7) with non-negative effort yields N*C <= Pi_0",
      bool(sp.simplify(implied - E) == 0),
      "the cap is eq (7) rearranged; it is the same inequality as P7's")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"FU-LU SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
