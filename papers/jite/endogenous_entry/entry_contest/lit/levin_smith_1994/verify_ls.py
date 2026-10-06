import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


v = sp.symbols("v", positive=True)
q = sp.symbols("q", nonnegative=True)
V, c = sp.symbols("V c", positive=True)
Nn = sp.symbols("Nn", integer=True, positive=True)

print("=" * 72)
print("Levin & Smith (1994), 'Equilibrium in Auctions with Entry'")
print("American Economic Review 84(3), June 1994, pp. 585-599.")
print("Checks of OUR READING.  Read from the JSTOR scan (no text layer).")
print("=" * 72)
print()


def Vn(n, F):
    """Expected value of the item to the highest of n bidders (footnote 16)."""
    f = sp.diff(F, v)
    return sp.simplify(sp.integrate(v * n * f * F ** (n - 1), (v, 0, 1)))


def Wn(n, F):
    """Expected payment: expected value of the second-order statistic."""
    f = sp.diff(F, v)
    return sp.simplify(
        sp.integrate(v * n * (n - 1) * f * (1 - F) * F ** (n - 2), (v, 0, 1))
    )


print("LS-1  eq (18):  V_n - W_n = n (V_n - V_{n-1})")
print("-" * 72)
print("      Derived in footnote 16 from W_n = n V_{n-1} - (n-1) V_n.")
fams = [("F=v", v), ("F=v^2", v**2), ("F=v^3", v**3), ("F=sqrt(v)", sp.sqrt(v))]
allok = True
rows = []
for name, F in fams:
    ok_all = True
    for n in range(2, 6):
        lhs = sp.simplify(Vn(n, F) - Wn(n, F))
        rhs = sp.simplify(n * (Vn(n, F) - Vn(n - 1, F)))
        if sp.simplify(lhs - rhs) != 0:
            ok_all = False
    rows.append(f"{name}:{'ok' if ok_all else 'MISMATCH'}")
    if not ok_all:
        allok = False
print("      " + "  ".join(rows) + "   (n = 2..5)")
check("LS-1 eq (18) holds exactly", allok, "computed from the order statistics")

print()
print("LS-2  Proposition 6: private gain equals social gain in IPV")
print("-" * 72)
print("      p.593: 'the social gain is simply (V_n - V_{n-1} - c), whereas the")
print("      individual bidder's gain is (V_n - W_n)/n - c. Due to (18), the two")
print("      always coincide.'")
allok = True
for name, F in fams:
    for n in range(2, 6):
        social = sp.simplify(Vn(n, F) - Vn(n - 1, F) - c)
        private = sp.simplify((Vn(n, F) - Wn(n, F)) / n - c)
        if sp.simplify(social - private) != 0:
            allok = False
check("LS-2 social and private gains coincide exactly", allok,
      "this is why free entry is OPTIMAL in IPV (Prop 6), not excessive")

print()
print("LS-3  eq (9) is the SOCIAL PLANNER's first-order condition")
print("-" * 72)
print("      eq (8), p.590:  S(q,R^s,e) = [1 - (1-q)^N] V - qNc")
print("      The paper differentiates and sets to zero to obtain eq (9).")
S = (1 - (1 - q) ** Nn) * V - q * Nn * c
d2S = sp.simplify(sp.diff(S, q, 2))
# SymPy will not reduce a symbolic exponent here, so verify per concrete N.
allok = True
rows = []
for Nv in [2, 3, 4, 5, 8, 12]:
    Sv = (1 - (1 - q) ** Nv) * V - q * Nv * c
    dSv = sp.simplify(sp.diff(Sv, q))
    target = Nv * ((1 - q) ** (Nv - 1) * V - c)
    ok = sp.simplify(sp.expand(dSv - target)) == 0
    rows.append(f"N={Nv}:{'ok' if ok else 'MISMATCH'}")
    if not ok:
        allok = False
print(f"      dS/dq = N[(1-q)^(N-1) V - c]:  " + "  ".join(rows))
print("      Setting this to zero gives eq (9) exactly.")
check("LS-3 eq (9) is exactly dS/dq = 0", allok,
      "so (9) characterises the socially optimal q^s, NOT an entry equilibrium")

print()
print("LS-4  Concavity, and what 'excessive entry' means formally")
print("-" * 72)
print("      The paper states d^2S/dq^2 < 0. With S concave and dS/dq = 0 at")
print("      q^s, any equilibrium entry ABOVE q^s sits where dS/dq < 0, so")
print("      welfare is strictly falling in entry there. That is the formal")
print("      content of Prop 3's 'entry would be excessive'.")
allok = True
rows = []
for Nv in [2, 3, 5, 10]:
    d2 = sp.simplify(d2S.subs(Nn, Nv))
    neg = sp.simplify(d2.subs({V: 1, q: sp.Rational(1, 3)})) < 0
    rows.append(f"N={Nv}:{bool(neg)}")
    if not neg:
        allok = False
print("      d^2S/dq^2 < 0 at q=1/3, V=1: " + " ".join(rows))
check("LS-4 S is strictly concave in q", allok,
      "excessive entry = equilibrium q above the peak of a concave S")

print()
print("LS-5  Proposition 8's algebra:  q^s declines in N, no-entry prob rises")
print("-" * 72)
print("      p.595: 'From (9), (1-q^s_N)^{N-1} = c/V; thus q^s_N declines with")
print("      N and (1-q^s_N)^N = (1-q^s_N)(c/V) must increase with N.'")
ratio = sp.Rational(1, 4)  # c/V
rows = []
allok = True
prev_q = None
prev_noentry = None
for Nv in [2, 3, 4, 6, 10]:
    # (1-q)^(N-1) = c/V solves exactly: q^s = 1 - (c/V)^(1/(N-1))
    sol = sp.simplify(1 - ratio ** sp.Rational(1, Nv - 1))
    resid = sp.simplify((1 - sol) ** (Nv - 1) - ratio)
    if resid != 0:
        allok = False
    noentry = sp.simplify((1 - sol) ** Nv)
    if prev_q is not None:
        if not bool(sp.simplify(sol - prev_q) < 0):
            allok = False
        if not bool(sp.simplify(noentry - prev_noentry) > 0):
            allok = False
    prev_q, prev_noentry = sol, noentry
    rows.append(f"N={Nv}: q^s={float(sol):.4f} P(no entry)={float(noentry):.4f}")
for row in rows:
    print("      " + row)
check("LS-5 q^s falls and P(no entry) rises with N", allok,
      "c/V = 1/4; matches the stated monotonicities")

print()
print("LS-6  Fu-Jiao-Lu Definition 1 has the same ALGEBRA, different meaning")
print("-" * 72)
print("      FJL Def 1 (p.396): q_0 solves (1-q)^{M-1} V - Delta = 0.")
print("      LS eq (9):         (1-q^s)^{N-1} V = c.")
Delta = sp.symbols("Delta", positive=True)
fjl = (1 - q) ** (Nn - 1) * V - Delta
ls9 = (1 - q) ** (Nn - 1) * V - c
same = sp.simplify(fjl.subs(Delta, c) - ls9) == 0
print(f"      identical after Delta <-> c: {same}")
print("      BUT the derivations differ, and so does the meaning:")
print("        FJL: a LOWER bound on equilibrium entry, from the deviation")
print("             payoff of bidding zero and winning only if alone.")
print("        LS : the SOCIALLY OPTIMAL entry probability, from dS/dq = 0.")
print("      In LS, free-entry equilibrium in CV auctions EXCEEDS q^s")
print("      (Prop 3), so reading (9) as an equilibrium condition inverts")
print("      the welfare conclusion.  See NOTES.md.")
check("LS-6 the two equations coincide algebraically", bool(same),
      "algebraic coincidence only; the economic roles are opposite")

print()
print("LS-7  The CV/IPV branch criterion: does V_n vary with n?")
print("-" * 72)
print("      p.596: 'In CV auctions, social gains are zero (and therefore")
print("      smaller than social costs) for all n >= 2.'  With a prize fixed")
print("      at V, V_n = V for every n, so the social gain from the marginal")
print("      entrant is V_n - V_{n-1} = 0 while the social cost is c > 0.")
print("      This is what puts entry_contest in the CV branch: by construction")
print("      (V exogenous and fixed), not by resemblance.")
Wsym = sp.symbols("W", positive=True)
# Fixed prize: V_n = V for all n.
social_cv = sp.simplify(V - V)  # V_n - V_{n-1}
eq18_rhs_cv = sp.simplify(3 * social_cv)  # n(V_n - V_{n-1}) at n=3
eq18_lhs_cv = sp.simplify(V - Wsym)  # V_n - W_n
wedge = sp.simplify(eq18_lhs_cv - eq18_rhs_cv)
print(f"      V_n - V_(n-1) = {social_cv};  n(V_n - V_(n-1)) = {eq18_rhs_cv}")
print(f"      V_n - W_n = {eq18_lhs_cv};  so eq (18) fails by {wedge}")
# Contrast: in IPV (V_n strictly increasing) eq (18) held exactly in LS-1.
ipv_holds = True
for name, F in fams:
    if sp.simplify(Vn(3, F) - Vn(2, F)) <= 0:
        ipv_holds = False
check("LS-7 eq (18) fails under a fixed prize, holds under IPV",
      bool(social_cv == 0 and wedge == V - Wsym and ipv_holds),
      "so Prop 3 governs a fixed-prize contest and Prop 6 does not")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"LEVIN-SMITH SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, EXCEPT")
print("as recorded in NOTES.md. This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
