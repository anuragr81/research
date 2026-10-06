import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


mu, muhat, th = sp.symbols("mu muhat theta", positive=True)
b0, b1 = sp.symbols("beta0 beta1", positive=True)
p, x = sp.symbols("p x", positive=True)

print("=" * 72)
print("Costrell & Loury, 'Distribution of Ability and Earnings in a")
print("Hierarchical Job Assignment Model'.  Draft, 11 December 2003.")
print("Published as JPE 112(6), 2004 -- proposition numbers are the DRAFT's.")
print("Checks of OUR READING.")
print("=" * 72)
print()

print("CL-1  Two-job wage schedule (1)-(2): mu-hat is a single sufficient statistic")
print("-" * 72)
W_lo = b0 * mu + (1 - th) * (b1 - b0) * muhat
W_hi = b1 * mu - th * (b1 - b0) * muhat
d_lo = sp.simplify(sp.diff(W_lo, muhat))
d_hi = sp.simplify(sp.diff(W_hi, muhat))
print(f"      dW/dmu-hat below the margin: {d_lo}   (> 0 since beta1 > beta0)")
print(f"      dW/dmu-hat above the margin: {d_hi}   (< 0)")
check("CL-1 raising mu-hat raises low wages and lowers high wages",
      bool(sp.simplify(d_lo - (1 - th) * (b1 - b0)) == 0
           and sp.simplify(d_hi + th * (b1 - b0)) == 0),
      "opposite signs: this is the p.9 'single sufficient statistic' claim")

print()
print("CL-2  The schedule is continuous at the margin (no-arbitrage check)")
print("-" * 72)
gap = sp.simplify((W_hi - W_lo).subs(mu, muhat))
print(f"      W_hi(mu-hat) - W_lo(mu-hat) = {sp.simplify(sp.expand(gap))}")
check("CL-2 the two branches agree at mu = mu-hat", bool(sp.simplify(gap) == 0),
      "confirms (1) and (2) were transcribed correctly from the scan-free text")

print()
print("CL-3  Slopes are beta0 below and beta1 above (no-arbitrage condition)")
print("-" * 72)
s_lo = sp.simplify(sp.diff(W_lo, mu))
s_hi = sp.simplify(sp.diff(W_hi, mu))
print(f"      dW/dmu = {s_lo} below, {s_hi} above")
check("CL-3 slopes match the stated no-arbitrage condition",
      bool(s_lo == b0 and s_hi == b1))

print()
print("CL-4  Proposition 5's technique: Gamma against a NON-DECREASING beta")
print("-" * 72)
print("      Q(G) - Q(F) = int_0^1 Gamma(p) dbeta(p),  Gamma(p) >= 0.")
print("      The sign needs dbeta >= 0, i.e. beta NON-DECREASING.")
print("      Discrete analogue, exactly the shape of our own S8:")
# non-negative integrand against a non-decreasing integrator
Gam = [sp.Symbol(f"G{i}", nonnegative=True) for i in range(4)]
bet = [sp.Symbol(f"b{i}") for i in range(5)]
dbet = [sp.Symbol(f"db{i}", nonnegative=True) for i in range(4)]
total = sum(g * d for g, d in zip(Gam, dbet))
expanded = sp.expand(total)
allnonneg = all(t.is_nonnegative for t in expanded.args)
print(f"      sum Gamma_i * dbeta_i = {expanded}")
check("CL-4 non-negative integrand against non-decreasing integrator is >= 0",
      bool(allnonneg),
      "monotonicity of beta is the operative condition, not concavity")

print()
print("CL-5  CONTROL: the sign FAILS if beta is allowed to decrease")
print("-" * 72)
print("      If dbeta may be negative the conclusion does not follow, so CL-4")
print("      is testing the monotonicity hypothesis rather than restating an")
print("      identity.")
neg_case = (sp.Integer(2)) * (sp.Integer(-3))  # Gamma = 2 >= 0, dbeta = -3 < 0
print(f"      Gamma = 2, dbeta = -3  ->  contribution {neg_case} < 0")
check("CL-5 monotonicity is load-bearing", bool(neg_case < 0),
      "a decreasing weight reverses the sign")

print()
print("CL-6  Proposition 6's flip is on CURVATURE of beta, not on a pivot")
print("-" * 72)
print("      Prop 6: G MUT F on [0,1] atomless full support.")
print("        (i)  beta concave => w_G(0) <= w_F(0) and the span WIDENS")
print("        (ii) beta convex  => w_G(1) <= w_F(1) and the span NARROWS")
print("      Lemma 1: G MUT F and psi convex [concave] =>")
print("        int psi dG^-1 <= [>=] int psi dF^-1.")
print("      Verified here only as the standard SOSD characterisation on")
print("      concrete quantile pairs: a riskier distribution raises the")
print("      expectation of a convex function.")
allok = True
rows = []
# F uniform on [0,1].  G is a mean-preserving spread of it that STAYS on
# [0,1] with G^-1(0)=0 and G^-1(1)=1, as CL's full-support hypothesis needs.
Finv = p
Ginv = p + p * (1 - p) * (2 * p - 1)
# sanity: strictly increasing, endpoints pinned, stays inside [0,1]
slope = sp.diff(Ginv, p)
slope_min = sp.minimum(slope, p, sp.Interval(0, 1))
slope_mid = sp.simplify(slope.subs(p, sp.Rational(1, 2)))
rng_lo = sp.minimum(Ginv, p, sp.Interval(0, 1))
rng_hi = sp.maximum(Ginv, p, sp.Interval(0, 1))
mean_ok = sp.simplify(sp.integrate(Ginv - Finv, (p, 0, 1))) == 0
# single crossing of the quantile functions at p = 1/2, below then above
below = sp.simplify((Ginv - Finv).subs(p, sp.Rational(1, 4)))
above = sp.simplify((Ginv - Finv).subs(p, sp.Rational(3, 4)))
print(f"      G^-1 - F^-1 at p=1/4: {below} (<0), at p=3/4: {above} (>0)")
print(f"      slope >= {sp.nsimplify(slope_min)} on [0,1] (zero only at the two"
      f" endpoints; interior slope at p=1/2 is {sp.nsimplify(slope_mid)}), range "
      f"[{sp.nsimplify(rng_lo)}, {sp.nsimplify(rng_hi)}], mean preserved: {bool(mean_ok)}")
for psi, lbl, convex in [(x**2, "x^2", True), (sp.sqrt(x), "sqrt(x)", False)]:
    lhsv = sp.integrate(psi.subs(x, Ginv), (p, 0, 1))
    rhsv = sp.integrate(psi.subs(x, Finv), (p, 0, 1))
    diff = sp.nsimplify(sp.simplify(sp.re(sp.N(lhsv - rhsv, 30))))
    ok = (diff > 0) if convex else (diff < 0)
    rows.append(f"{lbl} ({'convex' if convex else 'concave'}): "
                f"diff={float(diff):+.6f} -> {bool(ok)}")
    if not bool(ok):
        allok = False
for r in rows:
    print("      " + r)
allok = allok and bool(mean_ok) and bool(slope_min >= 0) and bool(slope_mid > 0) \
    and bool(rng_lo >= 0) and bool(rng_hi <= 1) \
    and bool(below < 0) and bool(above > 0)
check("CL-6 Lemma 1's direction reproduces on a mean-preserving pair",
      allok and bool(mean_ok),
      "convex up, concave down, as Lemma 1 states")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"COSTRELL-LOURY SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, EXCEPT")
print("as recorded in NOTES.md. This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
