import math
import os
import re
import shutil
import subprocess
import tempfile

import mpmath
import sympy as sp

HERE = os.path.dirname(os.path.abspath(__file__))
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
print("LR-6  Section IV handicap: (30) reduces to (32), y_a(h) = V*(dmu/2 - h)")
print("-" * 72)
print("      Built from the paper's primitives, not from (32) itself: P = 1/2 +")
print("      g*(dmu - h) (p.862), spread V/g from (29), mixed zero profit W1+W2 =")
print("      V*(mu_a + mu_b) (p.862), own-league zero profit from (7).")
V, gg, ma, mb, h = sp.symbols("V g mu_a mu_b h", positive=True)
dmu = ma - mb
P = sp.Rational(1, 2) + gg * (dmu - h)
dW = V / gg
S = V * (ma + mb)
W1v, W2v = (S + dW) / 2, (S - dW) / 2
ya = sp.simplify(P * W1v + (1 - P) * W2v - (2 * V * ma) / 2)
yb = sp.simplify((1 - P) * W1v + P * W2v - (2 * V * mb) / 2)
hstar = sp.solve(sp.Eq(ya, 0), h)[0]
print(f"      y_a(h) = {sp.factor(ya)};  y_a + y_b = {sp.simplify(ya + yb)}")
print(f"      dy_a/dg = {sp.simplify(sp.diff(ya, gg))};  dy_a/dh = {sp.diff(ya, h)};  h* = {hstar}")
with_g = V * gg * (dmu / 2 - h)
print(f"      control: y_a - V*g*(dmu/2 - h) = {sp.factor(sp.simplify(ya - with_g))}  (nonzero)")
check("LR-6 (30) gives (32) with no factor g; zero sum; h* = dmu/2",
      bool(sp.simplify(ya - V * (dmu / 2 - h)) == 0
           and sp.simplify(ya + yb) == 0
           and sp.simplify(sp.diff(ya, gg)) == 0
           and sp.simplify(hstar - dmu / 2) == 0
           and sp.simplify(sp.diff(ya, h) + V) == 0
           and sp.simplify(ya - with_g) != 0),
      "p.862 (32); the earlier V*g*(...) form is rejected by the control")

print()
print("LR-7  Table 1 against the paper's own approximations (16), (17), (24), (25)")
print("-" * 72)
print("      V = 1, C = mu^2/2, C'' = 1 (table note), x = s*sigma^2.  To second order")
print("      E(U) = U(mean) - (s/2) U'(mean) var, so the certainty-equivalent net")
print("      income is mean - (s/2) var.")
x, s_, sig2 = sp.symbols("x s sigma2", positive=True)
mu_p = 1 / (1 + s_ * sig2)
mu_c = 1 / (1 + sp.pi * s_ * sig2)
var_p = sig2 / (1 + s_ * sig2) ** 2
var_c = sp.pi * sig2 / (1 + sp.pi * s_ * sig2) ** 2
ce_p = mu_p - mu_p ** 2 / 2 - s_ / 2 * var_p
ce_c = mu_c - mu_c ** 2 / 2 - s_ / 2 * var_c
gap = sp.simplify(ce_p - ce_c)
vgap = sp.expand((1 + sp.pi * x) ** 2 - sp.pi * (1 + x) ** 2)
print(f"      CE_piece - mu/2 = {sp.simplify(ce_p - mu_p / 2)};  CE_contest - mu*/2 = {sp.simplify(ce_c - mu_c / 2)}")
print(f"      CE_piece - CE_contest = {sp.factor(gap)}  (> 0 for s, sigma^2 > 0)")
print(f"      (1+pi x)^2 - pi (1+x)^2 = {sp.factor(vgap)}  so var_c > var_p iff pi x^2 < 1")
check("LR-7a under (16), (17), (24), (25) the piece rate is preferred at every s, sigma^2",
      bool(sp.simplify(ce_p - mu_p / 2) == 0 and sp.simplify(ce_c - mu_c / 2) == 0
           and sp.simplify(gap - (sp.pi - 1) * s_ * sig2
                           / (2 * (1 + s_ * sig2) * (1 + sp.pi * s_ * sig2))) == 0
           and sp.simplify(vgap - (sp.pi - 1) * (sp.pi * x ** 2 - 1)) == 0),
      "CE gap = (mu - mu*)/2 = (pi-1)x / 2(1+x)(1+pi x)")

alpha = 0.5
U = lambda yy: alpha * yy ** alpha
U2 = lambda yy: alpha * alpha * (alpha - 1) * yy ** (alpha - 2)
table = {
    100: [(0.1, 9995, 9984, 5012155, 5012465), (0.5, 9975, 9922, 5012150, 5012445),
          (1, 9950, 9846, 5012100, 5012295), (3, 9852, 9552, 5011940, 5011925),
          (6, 9710, 9142, 5011800, 5011415), (12, 9436, 8420, 5011420, 5010515)],
    25: [(0.1, 9980, 9938, 2524665, 2524725), (0.2, 9960, 9878, 2524616, 2524575),
         (1, 9807, 9419, 2524237, 2523437), (12, 8094, 5741, 2519930, 2514282)],
}
trunc_ok, eus_close, eu_gaps, reversed_ = 0, 0, [], 0
exact_gaps, risk_terms, exact_beats_contest = [], [], 0
for y0, rows in table.items():
    for sg, mu_t, mus_t, eu_t, eus_t in rows:
        m = ms = 1.0
        for _ in range(200):
            ybar = y0 + m - m * m / 2
            m = 1 / (1 + (1 - alpha) / ybar * sg)
            ybar_c = y0 + ms - ms * ms / 2
            ms = 1 / (1 + math.pi * (1 - alpha) / ybar_c * sg)
        eu = U(ybar) + 0.5 * U2(ybar) * (m * m * sg)
        sd = m * math.sqrt(sg)
        z_lo = max(-12.0, -0.999 * ybar / sd)
        exact = mpmath.quad(lambda z: U(ybar + sd * z) * mpmath.npdf(z), [z_lo, 0, 12])
        exact_gaps.append(float(exact) - eu_t / 1e6)
        exact_beats_contest += int(float(exact) > eus_t / 1e6)
        risk_terms.append(abs(0.5 * U2(ybar) * (m * m * sg)))
        eus = U(ybar_c) + 0.5 * U2(ybar_c) * (math.pi * sg * ms * ms)
        trunc_ok += int(math.floor(m * 1e4) == mu_t) + int(math.floor(ms * 1e4) == mus_t)
        eus_close += int(abs(eus - eus_t / 1e6) <= 5e-6)
        eu_gaps.append(eu - eu_t / 1e6)
        reversed_ += int(eu > eus_t / 1e6)
        print(f"      y0={y0:>3} s2={sg:<4} E(U) recomputed - printed = {eu - eu_t / 1e6:+.6f};"
              f"  E(U*) recomputed - printed = {eus - eus_t / 1e6:+.7f}")
n_rows = sum(len(r) for r in table.values())
print(f"      mu, mu* equal to recomputed values truncated to 4 places: {trunc_ok} of {2 * n_rows}")
print(f"      E(U*) within 5e-6 of recomputed: {eus_close} of {n_rows}")
print(f"      E(U) printed below recomputed by {min(eu_gaps):.6f} to {max(eu_gaps):.6f} in every row")
print(f"      recomputed E(U) exceeds printed E(U*) in {reversed_} of {n_rows} rows")
print(f"      exact normal expectation at the paper's approximate piece-rate contract")
print(f"      (r = mu, I = mu(1 - mu)) exceeds printed E(U) by {min(exact_gaps):.6f} to {max(exact_gaps):.6f};")
print(f"      and exceeds printed E(U*) in {exact_beats_contest} of {n_rows} rows;"
      f" second-order risk term at s2 = .1, y0 = 100 is {risk_terms[0]:.1e}")
check("LR-7b mu, mu*, E(U*) columns reproduce; E(U) column sits below its own formula",
      trunc_ok == 2 * n_rows and eus_close >= 8
      and all(1.5e-4 <= g_ <= 4e-4 for g_ in eu_gaps) and reversed_ == n_rows
      and all(g_ >= 1.2e-4 for g_ in exact_gaps) and exact_beats_contest == n_rows,
      "fails if the printed E(U) column reproduces from (16), (17) and footnote 8")

print()
print("LR-L  Lean: DARA sign algebra, quadratic separator, section IV handicap, table 1")
print("-" * 72)
ALLOWED = {"propext", "Quot.sound"}
src = os.path.join(HERE, "LazearRosen.lean")
text = open(src).read()
lean = shutil.which("lean")
if lean is None:
    check("LR-L LazearRosen.lean compiles", False, "lean not on PATH")
else:
    p = subprocess.run([lean, src], capture_output=True, text=True)
    out = p.stdout + p.stderr
    check("LR-L LazearRosen.lean compiles with no errors or warnings",
          p.returncode == 0 and out.strip() == "", out.strip()[:200])
    n_sorry = len(re.findall(r"\bsorry\b", text)) + out.count("sorry")
    banned = re.findall(r"^\s*(?:axiom|import)\b|native_decide|--|/-", text, re.M)
    check("LR-L no sorry, user axiom, native_decide, import or comment",
          n_sorry == 0 and not banned, f"sorry: {n_sorry}; banned: {banned}")
    names = re.findall(r"^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", text, re.M)
    n_keywords = len(re.findall(r"\b(?:theorem|lemma)\b", text))
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(text)
            fh.write("\n")
            for n in names:
                fh.write(f"#print axioms LazearRosen.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    found = {}
    for m_ in re.finditer(r"'LazearRosen\.([A-Za-z0-9_']+)' (?:depends on axioms: \[([^\]]*)\]"
                          r"|does not depend on any axioms)", q.stdout):
        found[m_.group(1)] = {a.strip() for a in (m_.group(2) or "").split(",") if a.strip()}
    audited = [n for n in names if n in found]
    used = set().union(*found.values()) if found else set()
    bad = {n: sorted(ax - ALLOWED) for n, ax in found.items() if ax - ALLOWED}
    print(f"      theorems: {len(names)}; audited: {len(audited)}; sorry: {n_sorry}; "
          f"axioms used: {sorted(used) if used else 'none'}")
    check("LR-L axiom audit covers every declared theorem",
          q.returncode == 0 and len(names) > 0 and len(set(names)) == len(names)
          and n_keywords == len(names) and len(audited) == len(names),
          f"{len(audited)} of {len(names)} names audited; {n_keywords} theorem/lemma keywords")
    check("LR-L every theorem uses at most propext and Quot.sound", not bad,
          f"outside the allowed set: {bad}" if bad else "no Classical.choice, no sorryAx")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"LAZEAR-ROSEN SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
