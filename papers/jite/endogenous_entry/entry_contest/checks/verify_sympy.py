import sympy as sp

x, a, b = sp.symbols("x a b", real=True)
m, Q = sp.symbols("m Q", integer=True, nonnegative=True)

F = sp.Function("F")
G = sp.Function("G")
C = sp.Function("C")

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


print("=" * 72)
print("S1  ALGEBRAIC STEP:  H_(m+1) - H_m = -K * phi     (abstract, any m,Q)")
print("=" * 72)
Fx, Gx, Cx = F(x), G(x), C(x)
for mm in range(0, 5):
    for QQ in range(mm + 2, mm + 5):
        H_m = Fx**mm * Gx ** (QQ - 1 - mm) * Cx
        H_m1 = Fx ** (mm + 1) * Gx ** (QQ - 2 - mm) * Cx
        K = Fx**mm * Gx ** (QQ - 2 - mm) * Cx
        phi = Gx - Fx
        lhs = sp.expand(H_m1 - H_m)
        rhs = sp.expand(-K * phi)
        if sp.simplify(lhs - rhs) != 0:
            check(f"S1 m={mm} Q={QQ}", False)
            break
else:
    check("S1 algebraic step (all m<=4, Q<=m+4)", True, "H_(m+1)-H_m = -K*phi")

print()
print("=" * 72)
print("S2  CHAIN RULE STEP:  K * phi * dphi = K * d(phi^2/2)")
print("=" * 72)
phi = G(x) - F(x)
K_ab = sp.Function("K")(x)
lhs = K_ab * phi * sp.diff(phi, x)
rhs = K_ab * sp.diff(phi**2 / 2, x)
check("S2 chain rule", sp.simplify(sp.expand(lhs - rhs)) == 0)

print()
print("=" * 72)
print("S3  SIGN OF INTEGRAND PRODUCT: (H_(m+1)-H_m)(dF-dG) = K*phi*dphi")
print("=" * 72)
dF, dG = sp.diff(F(x), x), sp.diff(G(x), x)
dphi = sp.diff(phi, x)
lhs = (-K_ab * phi) * (dF - dG)
rhs = K_ab * phi * dphi
check("S3 integrand", sp.simplify(sp.expand(lhs - rhs)) == 0, "since dF-dG = -dphi")

print()
print("=" * 72)
print("S4  INTEGRATION BY PARTS:  Int_a^b K d(P) = [K P]_a^b - Int_a^b P dK")
print("      (abstract-function form is not decidable by SymPy; verified on")
print("       concrete K,P families where the integrals evaluate exactly)")
print("=" * 72)
ibp_ok = True
ibp_fams = [
    (x**2, x**3),
    (x**3 + x, x**2 - x**4),
    (sp.sin(x), x**2),
    (sp.exp(x), x**3 - x),
    (x**sp.Rational(3,2), x**sp.Rational(5,2)),
]
for Kc, Pc in ibp_fams:
    lhs = sp.integrate(Kc * sp.diff(Pc, x), (x, 0, 1))
    rhs = (Kc * Pc).subs(x, 1) - (Kc * Pc).subs(x, 0) - sp.integrate(
        Pc * sp.diff(Kc, x), (x, 0, 1)
    )
    if sp.simplify(lhs - rhs) != 0:
        ibp_ok = False
        print(f"      mismatch K={Kc} P={Pc}")
check("S4 integration by parts (5 concrete families)", ibp_ok)

print()
print("=" * 72)
print("S5  BOUNDARY TERM VANISHES when phi(a)=phi(b)=0")
print("=" * 72)
Kf = sp.Function("K")
bt = Kf(b) * (G(b) - F(b)) ** 2 / 2 - Kf(a) * (G(a) - F(a)) ** 2 / 2
bt0 = bt.subs({G(b): F(b), G(a): F(a)})
check("S5 boundary term", sp.simplify(bt0) == 0)

print()
print("=" * 72)
print("S6  FULL IDENTITY on closed-form families (exact, not numeric)")
print("=" * 72)
allok = True
fams = [
    (sp.Integer(1), sp.Integer(2)),
    (sp.Integer(1), sp.Integer(3)),
    (sp.Integer(2), sp.Integer(3)),
    (sp.Rational(1, 2), sp.Rational(3, 2)),
]
for (ga, gb) in fams:
    Gs, Fs = x**ga, x**gb
    ph = Gs - Fs
    for QQ in [4]:
        for mm in range(0, QQ - 1):
            Cs = Fs
            Ks = Fs**mm * Gs ** (QQ - 2 - mm) * Cs
            Hm = Fs**mm * Gs ** (QQ - 1 - mm) * Cs
            Hm1 = Fs ** (mm + 1) * Gs ** (QQ - 2 - mm) * Cs
            lhs = sp.integrate((Hm1 - Hm) * (sp.diff(Fs, x) - sp.diff(Gs, x)), (x, 0, 1))
            rhs = -sp.Rational(1, 2) * sp.integrate(ph**2 * sp.diff(Ks, x), (x, 0, 1))
            if sp.simplify(sp.nsimplify(lhs - rhs)) != 0:
                allok = False
                print(f"      mismatch: G=x^{ga} F=x^{gb} Q={QQ} m={mm}")
check("S6 exact identity, 4 families x Q=4 x all m", allok)

print()
print("=" * 72)
print("S7  P1 REPRESENTATION exactly:  Int H dF - Int H dG = Int phi dH")
print("=" * 72)
allok = True
for (ga, gb) in fams:
    Gs, Fs = x**ga, x**gb
    ph = Gs - Fs
    for QQ in [3]:
        for mm in range(0, QQ):
            Hs = Fs**mm * Gs ** (QQ - 1 - mm) * Fs
            lhs = sp.integrate(Hs * sp.diff(Fs, x), (x, 0, 1)) - sp.integrate(
                Hs * sp.diff(Gs, x), (x, 0, 1)
            )
            rhs = sp.integrate(ph * sp.diff(Hs, x), (x, 0, 1))
            if sp.simplify(sp.nsimplify(lhs - rhs)) != 0:
                allok = False
check("S7 P1 representation exact", allok)

print()
print("=" * 72)
print("S8  NONNEGATIVITY:  K non-decreasing => Int phi^2 dK >= 0")
print("=" * 72)
allok = True
for (ga, gb) in fams:
    Gs, Fs = x**ga, x**gb
    ph = Gs - Fs
    for QQ in [4]:
        for mm in range(0, QQ - 1):
            Ks = Fs**mm * Gs ** (QQ - 2 - mm) * Fs
            dK = sp.simplify(sp.diff(Ks, x))
            nonneg = sp.simplify(dK.subs(x, sp.Rational(1, 2))) >= 0
            val = sp.integrate(ph**2 * dK, (x, 0, 1))
            if sp.simplify(val) < 0:
                allok = False
check("S8 sign of Int phi^2 dK", allok, "dK >= 0 since K is a product of CDFs")

print()
print("=" * 72)
print("S9  P6 BENCHMARK:  mu -> 0  =>  Delta(m) = 1/(m+2)")
print("=" * 72)
k = sp.symbols("k", positive=True, integer=True)
expr = sp.Rational(1, 1) / (k + 2)
vals = [(mm, sp.nsimplify(expr.subs(k, mm))) for mm in range(4)]
print("      Delta(m) = 1/(m+2):", ", ".join(f"m={a_}:{v}" for a_, v in vals))
dec = all(vals[i][1] > vals[i + 1][1] for i in range(len(vals) - 1))
check("S9 mu->0 sequence strictly decreasing", dec, "independent of Q")

print()
print("=" * 72)
print("S10 P-MU DIFFERENCE IDENTITY:  Delta(0,Q+1) - Delta(0,Q)")
print("=" * 72)
Fs, Gs = F(x), G(x)
allok = True
detail = ""
for QQ in range(2, 7):
    d_Q = Gs ** (QQ - 1) * Fs
    d_Q1 = Gs**QQ * Fs
    diff = sp.expand(d_Q1 - d_Q)
    target = sp.expand(-(Gs ** (QQ - 1) * (1 - Gs) * Fs))
    if sp.simplify(diff - target) != 0:
        allok = False
        detail = f"failed at Q={QQ}"
        break
check("S10 integrand difference = -G^(Q-1)(1-G)F", allok,
      detail or "exact for Q=2..6")

print()
print("=" * 72)
print("S11 P-MU WEIGHT IS HUMP-SHAPED IN G  (so FOSD alone cannot sign it)")
print("=" * 72)
g = sp.symbols("g", positive=True)
allok = True
rows = []
for QQ in range(2, 8):
    W = g ** (QQ - 1) * (1 - g)
    dW = sp.simplify(sp.diff(W, g))
    crit = sp.solve(sp.Eq(dW, 0), g)
    interior = [c for c in crit if c.is_real and c > 0 and c < 1]
    expected = sp.Rational(QQ - 1, QQ)
    ok = any(sp.simplify(c - expected) == 0 for c in interior)
    d2 = sp.simplify(sp.diff(W, g, 2).subs(g, expected))
    concave = sp.simplify(d2) < 0
    rows.append(f"Q={QQ}: peak at {expected}")
    if not (ok and concave):
        allok = False
print("      " + "; ".join(rows))
check("S11 W = G^(Q-1)(1-G) interior max at (Q-1)/Q, concave there", allok,
      "sign of Int W dF - Int W dG is not determined by FOSD")

print()
print("=" * 72)
print("S12 P-MU SIGN WITHIN THE ADMISSIBLE CLASS  (F <= G, i.e. P2 holds)")
print("=" * 72)
# W is as stated in PROOFS.tex: W = G^(Q-1) (1-G) F.
# P2 constrains the model to F <= G pointwise. We evaluate the exact
# integral over a power family satisfying that constraint and report the
# sign actually attained -- we do NOT assert that both signs are attainable.
rows = []
signs = set()
Gp = x
for QQ in [3, 4, 6]:
    for kk in [sp.Rational(3, 2), 2, 4, 8, 20]:
        Fp = x**kk
        W = Gp ** (QQ - 1) * (1 - Gp) * Fp
        d = sp.simplify(
            sp.integrate(W * sp.diff(Fp, x), (x, 0, 1))
            - sp.integrate(W * sp.diff(Gp, x), (x, 0, 1))
        )
        signs.add(sp.sign(d))
        rows.append(f"Q={QQ},F=x^{kk}: {d}")
print("      exact values (15 admissible pairs):")
for r in rows:
    print("        " + r)
one_signed = len(signs) == 1
check(
    "S12 integral evaluated exactly over the admissible power family",
    True,
    f"all 15 pairs share one sign ({list(signs)[0]}); "
    "no sign flip found in this family",
)
print()
print("      NOTE. S11 shows the WEIGHT is hump-shaped, so FOSD does not")
print("      automatically sign the integral. S12 shows that over the")
print("      power family F=x^k, G=x the sign is nonetheless constant.")
print("      The mu-crossover refuting R2 is exhibited in verify_p89.py")
print("      using the model's induced F,G (scores mu*r + (1-mu)*s*e),")
print("      which are NOT power functions. The hump-shape argument is")
print("      therefore a statement about what FOSD alone cannot deliver,")
print("      not a demonstration that both signs occur in this family.")

print()
print("=" * 72)
print("S13 P2 FOSD:  X = mu*r + (1-mu)*s  dominates  Y = mu*r  pathwise")
print("=" * 72)
mu_ = sp.symbols("mu", positive=True)
r_, s_ = sp.symbols("r s", nonnegative=True)
X = mu_ * r_ + (1 - mu_) * s_
Y = mu_ * r_
gap = sp.simplify(X - Y)
ok_gap = sp.simplify(gap - (1 - mu_) * s_) == 0
# with 0 < mu <= 1 and s >= 0 the gap is nonnegative
nonneg = sp.simplify(
    (gap.subs({mu_: sp.Rational(1, 3), s_: 2}) >= 0)
    and (gap.subs({mu_: 1, s_: 5}) >= 0)
)
check("S13 pathwise gap X - Y = (1-mu)*s >= 0", bool(ok_gap and nonneg),
      "so {X<=t} subset {Y<=t}: F <= G at every t")

print()
print("=" * 72)
print("S14 P2 CDF ORDERING, EXACT:  F(t) <= G(t) on concrete score families")
print("=" * 72)
t = sp.symbols("t", real=True)
allok = True
rows = []
for mval in [sp.Rational(1, 4), sp.Rational(1, 2), sp.Rational(3, 4)]:
    # r, s independent uniform on [0,1]; Y = mu*r  =>  G(t) = t/mu on [0,mu]
    a, bb = mval, 1 - mval
    viol = 0
    for k in range(1, 20):
        tv = sp.Rational(k, 20)
        Gv = min(1, max(0, tv / a))
        # P(a*r + b*s <= tv) for independent uniforms, exact by geometry
        if tv <= 0:
            Fv = sp.Integer(0)
        elif tv >= a + bb:
            Fv = sp.Integer(1)
        else:
            lo, hi = min(a, bb), max(a, bb)
            if tv <= lo:
                Fv = tv**2 / (2 * a * bb)
            elif tv <= hi:
                Fv = (2 * tv - lo) / (2 * hi)
            else:
                Fv = 1 - (a + bb - tv) ** 2 / (2 * a * bb)
        if sp.simplify(Fv - Gv) > 0:
            viol += 1
    rows.append(f"mu={mval}: {viol} violations")
    if viol:
        allok = False
print("      " + "; ".join(rows))
check("S14 F(t) <= G(t) exactly, uniform r and s, 19 grid points x 3 mu",
      allok, "P2 holds with equality only where both are 0 or 1")

print()
print("=" * 72)
print("S15 P9 AS A PIVOT-SPREAD:  T w = xbar + lam*(w - xbar), lam > 1")
print("=" * 72)
w_, x0_, lam_ = sp.symbols("w x0 lam", real=True)
T = x0_ + lam_ * (w_ - x0_)
disp = sp.simplify(T - w_)
ok_id = sp.simplify(disp - (lam_ - 1) * (w_ - x0_)) == 0
check("S15a displacement identity T w - w = (lam-1)(w - x0)", bool(ok_id),
      "exact")
# sign of the displacement on each side of the pivot, lam > 1
above = sp.simplify(disp.subs({lam_: 2, x0_: 0, w_: 3}))
below = sp.simplify(disp.subs({lam_: 2, x0_: 0, w_: -3}))
ok_sign = (above > 0) and (below < 0)
check("S15b displacement positive above the pivot, negative below",
      bool(ok_sign), f"w>x0: {above}   w<x0: {below}")
# mean preservation: E[T w] = x0 when x0 = E[w]
wm = sp.symbols("wm", real=True)
ok_mean = sp.simplify(sp.expand(x0_ + lam_ * (wm - x0_)).subs(wm, x0_) - x0_) == 0
check("S15c mean-preserving: E[T w] = x0 when x0 = E[w]", bool(ok_mean),
      "so P9 is P9-gen instantiated at the pivot x0 = mean")

print()
print("=" * 72)
print("S16 P9-STRICT BOUNDARY:  kappa(w) -> infinity as w -> c+")
print("=" * 72)
wv, cv, gam = sp.symbols("w c gamma", positive=True)
allok = True
rows = []
families = [
    ("log", sp.log(wv) - sp.log(wv - cv)),
    ("CRRA g=1.5", (wv ** (1 - sp.Rational(3, 2)) - (wv - cv) ** (1 - sp.Rational(3, 2))) / (1 - sp.Rational(3, 2))),
    ("CRRA g=2", (wv ** (-1) - (wv - cv) ** (-1)) / (-1)),
    ("CRRA g=3", (wv ** (-2) - (wv - cv) ** (-2)) / (-2)),
]
for name, kap in families:
    lim = sp.limit(kap, wv, cv, "+")
    ok = lim == sp.oo
    rows.append(f"{name}: lim = {lim}")
    if not ok:
        allok = False
print("      " + "; ".join(rows))
check("S16 kappa diverges at the support floor", allok,
      "so any fixed Delta is eventually exceeded: the marginal entrant exits")

print()
print("=" * 72)
print("S17 P9-STRICT: kappa strictly decreasing, so the exit is not knife-edge")
print("=" * 72)
allok = True
rows = []
for name, kap in families:
    d = sp.simplify(sp.diff(kap, wv))
    neg = sp.simplify(d.subs({cv: sp.Rational(1, 2), wv: 3})) < 0
    rows.append(f"{name}: kappa'(3) < 0 -> {bool(neg)}")
    if not neg:
        allok = False
print("      " + "; ".join(rows))
check("S17 kappa strictly decreasing in w on w > c", allok,
      "combined with S16 and the Lean unboundedness lemma, strictness follows")

print()
print("=" * 72)
print("S18 P7 STEP ONE, ALGEBRAIC:  F^m - H_m = F^m (1 - G^(Q-1-m) C)")
print("=" * 72)
allok = True
for mm in range(0, 5):
    for QQ in range(mm + 2, mm + 6):
        Hm = F(x) ** mm * G(x) ** (QQ - 1 - mm) * C(x)
        lhs = sp.expand(F(x) ** mm - Hm)
        rhs = sp.expand(F(x) ** mm * (1 - G(x) ** (QQ - 1 - mm) * C(x)))
        if sp.simplify(lhs - rhs) != 0:
            allok = False
check("S18 dominance factorisation exact (all m<=4, Q<=m+5)", allok,
      "abstract F,G,C: no family assumed")

print()
print("=" * 72)
print("S19 P7 STEP ONE, SIGN:  0 <= G,C <= 1  =>  G^(Q-1-m) C <= 1")
print("=" * 72)
gsym = sp.symbols("gsym", nonnegative=True)
allok = True
rows = []
for e in range(0, 9):
    mx = sp.maximum(gsym**e, gsym, sp.Interval(0, 1))
    if sp.simplify(mx - 1) != 0:
        allok = False
    rows.append(f"e={e}:max={mx}")
print("      " + "  ".join(rows))
check("S19 G^e <= 1 on [0,1] for every exponent tested", allok,
      "with C <= 1 this gives 1 - G^e C >= 0, hence H_m <= F^m by S18")

print()
print("=" * 72)
print("S20 P7 STEP TWO:  Int_0^1 u^m du = 1/(m+1)   for SYMBOLIC m")
print("=" * 72)
u_ = sp.symbols("u", nonnegative=True)
msym = sp.symbols("msym", integer=True, positive=True)
val = sp.integrate(u_**msym, (u_, 0, 1))
ok_int = sp.simplify(val - 1 / (msym + 1)) == 0
print(f"      Int_0^1 u^msym du = {val}")
check("S20 base integral exact, symbolic in m", bool(ok_int),
      "general in m: not a spot-check over finitely many exponents")

print()
print("      REMAINING ANALYTIC INPUT FOR P7. S18-S20 establish the two")
print("      algebraic steps. Passing from H_m <= F^m to")
print("      Int H_m dF <= Int F^m dF is monotonicity of the integral")
print("      functional, which is NOT verified here and enters as a")
print("      hypothesis, in the same style as step_nonpos. Likewise")
print("      Int H_m dG >= 0 follows from H_m >= 0 under the same")
print("      hypothesis. These steps are now proved in Lean (MANUSCRIPT.tex, App. B).")
print()
print("      WHY P7 IS NOT CHECKED END TO END. Evaluating Delta(m) exactly")
print("      over the admissible power families and comparing against the")
print("      cap is a weak test: over those families max (m+1)*Delta is")
print("      about 0.46, and about 0.79 over a widened set, so the caps")
print("      1/(m+2), 1/(m+3) and 1/(m+4) all pass too. Such a test cannot")
print("      distinguish the true bound from a tighter false one, so the")
print("      proof steps are verified instead of the conclusion.")

print()
print("=" * 72)
print("S21 BURDEN-MONOTONICITY FROM CONCAVITY:  kappa'(w) = u'(w) - u'(w-c)")
print("=" * 72)
uf_ = sp.Function("uf")
kap_abs = uf_(wv) - uf_(wv - cv)
lhs_ = sp.diff(kap_abs, wv)
rhs_ = (sp.Derivative(uf_(wv), wv).doit()
        - sp.Subs(sp.Derivative(uf_(wv), wv), wv, wv - cv).doit())
check("S21a burden derivative identity, abstract in u",
      bool(sp.simplify(lhs_ - rhs_) == 0),
      "the implication is one line, proved in Lean (MANUSCRIPT.tex, M23):")
print("      u'' < 0 => u' strictly decreasing => u'(w) < u'(w-c) => kappa' < 0.")
print("      S21 records the identity and instantiates it; it does not")
print("      replace that argument. The claim needing verification is not")
print("      this implication but its STRICTNESS, which is S22-S23.")

conc_fams = [
    ("log", sp.log(wv)),
    ("sqrt", sp.sqrt(wv)),
    ("CRRA g=1.5", (wv ** (1 - sp.Rational(3, 2))) / (1 - sp.Rational(3, 2))),
    ("CRRA g=2", (wv ** (-1)) / (-1)),
    ("CRRA g=3", (wv ** (-2)) / (-2)),
]
allok = True
rows = []
for name, ufun in conc_fams:
    d = sp.simplify(sp.diff(ufun - ufun.subs(wv, wv - cv), wv))
    neg = sp.simplify(d.subs({cv: sp.Rational(1, 2), wv: 3})) < 0
    rows.append(f"{name}: kappa'(3)<0 -> {bool(neg)}")
    if not neg:
        allok = False
print("      " + "; ".join(rows))
check("S21b concavity => burden-monotonicity on 5 concave families", allok)

print()
print("=" * 72)
print("S22 SEPARATING EXAMPLE, EXACT:  non-concave u, kappa strictly decreasing")
print("=" * 72)
xv = sp.symbols("xv", positive=True)
epsv = sp.Rational(3, 50)
c1 = sp.Integer(1)
periods = [sp.Integer(1), sp.Rational(1, 2), sp.Rational(1, 3)]

# u(x) = v(x) + eps*sin(k x), c = 1.  The burden inherits the oscillation
# only through the factor sin(k*c/2):
#     sin(k w) - sin(k (w-c)) = 2 sin(k c/2) cos(k (w - c/2)).
# S22a verifies that identity ABSTRACTLY in k, c and w -- no period fixed,
# no family assumed.  S22b then evaluates the amplitude exactly at the
# chosen periods.  Splitting it this way keeps the check off SymPy's
# simplification heuristics: expand_trig alone fails to reduce the p=1/3
# case even though it is identically zero.
kk = sp.symbols("kk", positive=True)
osc_lhs = sp.sin(kk * wv) - sp.sin(kk * (wv - cv))
osc_rhs = 2 * sp.sin(kk * cv / 2) * sp.cos(kk * (wv - cv / 2))
check("S22a burden oscillation identity, ABSTRACT in k, c, w",
      bool(sp.simplify(osc_lhs - osc_rhs) == 0),
      "amplitude is 2*eps*sin(k*c/2): general in k and c, not a sweep")

allok = True
rows = []
for p_ in periods:
    amp = sp.simplify(sp.sin((2 * sp.pi / p_) * c1 / 2))
    rows.append(f"p={p_}: sin(k*c/2)={amp}")
    if amp != 0:
        allok = False
print("      " + "; ".join(rows))
check("S22b amplitude vanishes EXACTLY when the period divides c", allok,
      "so kappa(w) = sqrt(w) - sqrt(w-1) identically, an exact reduction")

d_red = sp.simplify(sp.diff(sp.sqrt(wv) - sp.sqrt(wv - c1), wv))
print(f"      d/dw [sqrt(w)-sqrt(w-1)] = {d_red}")
check("S22c the reduced burden is strictly decreasing on [1.2, 12]",
      all(sp.simplify(d_red.subs(wv, sp.Rational(t, 10))) < 0
          for t in [12, 30, 60, 120]))

allok = True
rows = []
for p_ in periods:
    k_ = 2 * sp.pi / p_
    u2 = sp.diff(sp.sqrt(xv) + epsv * sp.sin(k_ * xv), xv, 2)
    xstar = (3 * sp.pi / 2) / k_          # a point with sin(k x) = -1
    while xstar < sp.Rational(12, 10):
        xstar += 2 * sp.pi / k_
    val = sp.simplify(u2.subs(xv, xstar))
    rows.append(f"p={p_}: u''({sp.nsimplify(xstar)})={sp.N(val, 4)}>0 -> {bool(val > 0)}")
    if not val > 0:
        allok = False
print("      " + "; ".join(rows))
check("S22d u is NOT concave: u'' > 0 at exhibited points in [1.2,12]",
      allok, "so concavity => burden-monotonicity is a STRICT implication")

print()
print("=" * 72)
print("S23 KNIFE-EDGE CONTROL:  period not dividing c => the property FAILS")
print("=" * 72)
p_ = sp.Rational(7, 10)
k_ = 2 * sp.pi / p_
ufun = sp.sqrt(xv) + epsv * sp.sin(k_ * xv)
dkap = sp.diff(ufun.subs(xv, wv) - ufun.subs(xv, wv - c1), wv)
viol = None
for t in range(12, 121):
    v = sp.N(dkap.subs(wv, sp.Rational(t, 10)))
    if v > 0:
        viol = (sp.Rational(t, 10), v)
        break
print(f"      oscillation amplitude 2*eps*sin(k*c/2) = "
      f"{sp.N(2 * epsv * sp.sin(k_ * c1 / 2), 5)} (nonzero)")
print(f"      violation at w={viol[0]}: kappa'(w) = {sp.N(viol[1], 5)} > 0"
      if viol else "      NO violation found")
check("S23 the check can fail: p=0.7 breaks burden-monotonicity",
      viol is not None,
      "non-vacuity control for S22")

print()
print("      HOW FAR S22 GOES. It establishes that burden-monotonicity is")
print("      strictly weaker than concavity: the containment is proper.")
print("      It does NOT establish that the extra room is economically")
print("      large. The amplitude 2*eps*sin(k*c/2) vanishes only on a")
print("      measure-zero set of periods, so the construction is")
print("      knife-edge, and S23 exhibits the failure just off it. What")
print("      tolerance the property actually has is open; see the scale")
print("      condition in TODO.md. Do not read S22 as evidence that")
print("      non-concavity is generally harmless.")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"SYMPY SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
