import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


w, c, y, a, b, g_ = sp.symbols("w c y a b gamma", positive=True)

print("=" * 72)
print("Lazear & Rosen (1981), 'Rank-Order Tournaments as Optimum Labor")
print("Contracts', Journal of Political Economy 89(5), pp. 841-864.")
print("Checks of OUR READING.  The question is whether the 1981 precedent")
print("for wealth-based self-selection runs through the SAME channel as ours.")
print("=" * 72)
print()

A = lambda u, v: sp.simplify(-sp.diff(u, v, 2) / sp.diff(u, v))  # absolute risk aversion

print("LR-1  OUR channel: kappa(w) = u(w) - u(w-c) falls in w under CONCAVITY alone")
print("-" * 72)
print("      kappa'(w) = u'(w) - u'(w-c) < 0 whenever u'' < 0, since u' is then")
print("      decreasing and w > w-c.  No condition on u''' is used.")
utils = [
    ("log w", sp.log(w)),
    ("sqrt w", sp.sqrt(w)),
    ("CRRA g=2", -1 / w),
    ("CARA a=1", -sp.exp(-w)),
    ("quadratic w - w^2/8", w - w**2 / 8),
]
allok = True
rows = []
for name, u in utils:
    kap = u - u.subs(w, w - c)
    dk = sp.simplify(sp.diff(kap, w))
    # evaluate at a concrete admissible point
    val = sp.simplify(dk.subs({w: 3, c: sp.Rational(1, 2)}))
    u2 = sp.simplify(sp.diff(u, w, 2).subs(w, 3))
    ok = bool(val < 0) and bool(u2 < 0)
    rows.append(f"{name}: kappa'={sp.nsimplify(val)} u''={sp.nsimplify(u2)} -> {ok}")
    if not ok:
        allok = False
for r in rows:
    print("      " + r)
check("LR-1 concavity alone makes kappa strictly decreasing", allok,
      "holds for every concave u tested, including one with u''' = 0")

print()
print("LR-2  THEIR channel: DARA, which cannot be had without u'''")
print("-" * 72)
print("      A(w) = -u''/u';  DARA means A'(w) < 0.")
u_f = sp.Function("u")
Aw = -sp.diff(u_f(w), w, 2) / sp.diff(u_f(w), w)
dA = sp.simplify(sp.diff(Aw, w))
num = sp.simplify(sp.numer(sp.together(dA)))
print(f"      A'(w) numerator (up to sign) = {sp.factor(num)}")
print("      so A' < 0 requires (u'')^2 < u''' u'.  With u''' = 0 the left side")
print("      is strictly positive, so A' > 0: IARA, never DARA.")
check("LR-2 DARA is a third-derivative condition",
      bool(sp.simplify(num - (sp.diff(u_f(w), w, 2) ** 2
                              - sp.diff(u_f(w), w, 3) * sp.diff(u_f(w), w))) == 0),
      "u''' enters essentially; concavity alone cannot deliver DARA")

print()
print("LR-3  DECISIVE: a utility where OUR channel works and THEIRS fails")
print("-" * 72)
print("      Quadratic utility is concave (u'' < 0) but has u''' = 0, hence")
print("      INCREASING absolute risk aversion.  So wealth-sorting via kappa")
print("      operates while wealth-sorting via DARA does not.")
u_q = w - w**2 / 8  # concave and increasing on w < 4
kap_q = sp.simplify(u_q - u_q.subs(w, w - c))
dkap_q = sp.simplify(sp.diff(kap_q, w))
u3_q = sp.simplify(sp.diff(u_q, w, 3))
A_q = A(u_q, w)
dA_q = sp.simplify(sp.diff(A_q, w))
print(f"      u = w - w^2/8:  u'' = {sp.diff(u_q, w, 2)},  u''' = {u3_q}")
print(f"      kappa'(w) = {dkap_q}   (< 0 for any c > 0: OUR channel works)")
print(f"      A(w) = {A_q},  A'(w) = {sp.simplify(dA_q)}")
print(f"      A'(3) = {sp.nsimplify(dA_q.subs(w, 3))} > 0  =>  IARA, so DARA FAILS")
check("LR-3 the two channels are genuinely different",
      bool(u3_q == 0 and dkap_q < 0 and dA_q.subs(w, 3) > 0),
      "concavity gives our sorting; it does not give theirs")

print()
print("LR-4  Their table 1 utility: U = a*y^a, 'constant relative but")
print("      declining absolute risk aversion, s(y) = (1-a)/y'")
print("-" * 72)
U = a * y**a
A_U = sp.simplify(A(U, y))
R_U = sp.simplify(y * A_U)
dA_U = sp.simplify(sp.diff(A_U, y))
print(f"      A(y) = {A_U};  relative risk aversion y*A(y) = {R_U}")
print(f"      dA/dy = {dA_U}  (< 0 for 0 < a < 1: declining absolute)")
check("LR-4 s(y) = (1-a)/y, constant relative, declining absolute",
      bool(sp.simplify(A_U - (1 - a) / y) == 0 and sp.simplify(R_U - (1 - a)) == 0),
      "matches the paper's stated s(y) exactly")

print()
print("LR-5  Table 1 internal consistency (transcription check)")
print("-" * 72)
print("      The table reports s(y0) = .005 at y0 = 100 and s(y0) = .020 at")
print("      y0 = 25, with a = .5 per the table note.")
s_of = lambda y0: sp.Rational(1, 2) / y0
s100, s25 = s_of(100), s_of(25)
print(f"      (1-a)/y0 at a=1/2:  y0=100 -> {s100} ; y0=25 -> {s25}")
# the reported comparison at sigma^2 = 1
rich_contest, rich_piece = sp.Rational(5012295, 10**6), sp.Rational(5012100, 10**6)
poor_contest, poor_piece = sp.Rational(2523437, 10**6), sp.Rational(2524237, 10**6)
rich_prefers_contest = rich_contest > rich_piece
poor_prefers_piece = poor_piece > poor_contest
print(f"      sigma^2=1, y0=100: E(U*)={float(rich_contest)} > E(U)={float(rich_piece)}"
      f"  -> rich prefer the contest: {bool(rich_prefers_contest)}")
print(f"      sigma^2=1, y0=25 : E(U)={float(poor_piece)} > E(U*)={float(poor_contest)}"
      f"  -> poor prefer the piece rate: {bool(poor_prefers_piece)}")
check("LR-5 the table's s values and its sorting pattern check out",
      bool(s100 == sp.Rational(1, 200) and s25 == sp.Rational(1, 50)
           and rich_prefers_contest and poor_prefers_piece),
      "self-selection by wealth, exactly as the text describes")

print()
print("LR-6  Section IV handicap algebra: h* = delta-mu/2 and zero sum")
print("-" * 72)
print("      (32): y_a(h) ~ V*g*(delta-mu/2 - h), y_b the same with sign")
print("      reversed, so y_a(h) + y_b(h) = 0 for all admissible h.")
V, gg, dmu, h = sp.symbols("V g dmu h", positive=True)
ya = V * gg * (dmu / 2 - h)
yb = -ya
hstar = sp.solve(sp.Eq(ya, 0), h)[0]
print(f"      y_a(h) = {ya};  y_a + y_b = {sp.simplify(ya + yb)}")
print(f"      y_a(h) = 0 at h* = {hstar}")
print(f"      dy_a/dh = {sp.diff(ya, h)}  (< 0: gain to a falls in the handicap)")
check("LR-6 competitive handicap is delta-mu/2, gains are zero-sum",
      bool(sp.simplify(hstar - dmu / 2) == 0
           and sp.simplify(ya + yb) == 0
           and sp.diff(ya, h) < 0),
      "matches the paper: h < h* makes a's prefer mixed contests")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"LAZEAR-ROSEN SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
