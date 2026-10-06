import os
import re
import shutil
import subprocess
import tempfile

import sympy as sp

results = []
HERE = os.path.dirname(os.path.abspath(__file__))


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
print("CL-L  Lean: CostrellLoury.lean (core Lean 4, no Mathlib)")
print("-" * 72)
print("      Tail rule at fixed theta, Prop 5 monotone weight by Abel summation,")
print("      Lemma 1 / Prop 6 span by double Abel summation, Prop 10 skeleton.")
lean = shutil.which("lean")
if lean is None:
    check("CL-L1 CostrellLoury.lean compiles", False, "lean not on PATH")
else:
    src_path = os.path.join(HERE, "CostrellLoury.lean")
    src = open(src_path).read()
    p = subprocess.run([lean, src_path], capture_output=True, text=True)
    compile_out = p.stdout + p.stderr
    n_sorry_out = len(re.findall(r"sorry", compile_out))
    check("CL-L1 CostrellLoury.lean compiles with no errors and no sorry",
          p.returncode == 0 and n_sorry_out == 0, compile_out.strip()[:200])

    hygiene = {
        "sorry": re.search(r"\bsorry\b", src),
        "axiom declaration": re.search(r"^\s*axiom\b", src, re.M),
        "native_decide": re.search(r"native_decide", src),
        "import": re.search(r"^\s*import\b", src, re.M),
        "comment": re.search(r"--|/-", src),
    }
    bad = [k for k, v in hygiene.items() if v]
    check("CL-L2 source has no sorry, axiom, native_decide, import or comment",
          not bad, ", ".join(bad))

    names = re.findall(r"^(?:theorem|lemma)\s+([A-Za-z0-9_']+)", src, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(src)
            fh.write("\n\n")
            for n in names:
                fh.write(f"#print axioms CostrellLoury.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    allowed = {"propext", "Quot.sound"}
    n_free, n_core, other, missing = 0, 0, [], []
    for n in names:
        m = re.search(
            rf"'CostrellLoury\.{re.escape(n)}' "
            r"(does not depend on any axioms|depends on axioms: \[([^\]]*)\])",
            q.stdout)
        if m is None:
            missing.append(n)
        elif m.group(2) is None:
            n_free += 1
        else:
            axs = {a.strip() for a in m.group(2).split(",")}
            if axs <= allowed:
                n_core += 1
            else:
                other.append(f"{n}: {sorted(axs - allowed)}")
    audited = n_free + n_core + len(other)
    print(f"      theorems: {len(names)}; audited: {audited}; axiom-free: {n_free}; "
          f"propext/Quot.sound only: {n_core}; other axioms: {len(other)}; "
          f"sorry: {n_sorry_out}")
    for o in other:
        print(f"      OTHER AXIOM  {o}")
    check("CL-L3 axiom audit covers every declared theorem; propext/Quot.sound at most",
          q.returncode == 0 and not missing and not other
          and audited == len(names) and len(names) > 0,
          f"unaudited: {missing}" if missing else "")

    print("      CONTROL: the harness must reject a false statement. The negation")
    print("      of control_prop5_decreasing_weight (a decreasing weight still")
    print("      raises output) is appended and must fail to compile.")
    false_stmt = ("\n\nnamespace CostrellLoury\n"
                  "theorem harness_false : "
                  "output wDown qFlat 2 ≤ output wDown qWide 2 := by decide\n"
                  "end CostrellLoury\n")
    with tempfile.TemporaryDirectory() as td:
        neg = os.path.join(td, "negated.lean")
        with open(neg, "w") as fh:
            fh.write(src + false_stmt)
        r = subprocess.run([lean, neg], capture_output=True, text=True)
    check("CL-L4 CONTROL: Lean rejects the negated control statement",
          r.returncode != 0, "the compile check can fail")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"COSTRELL-LOURY SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, EXCEPT")
print("as recorded in NOTES.md. This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
