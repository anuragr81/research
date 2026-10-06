import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


w, a, b, gam, m, x = sp.symbols("w a b gamma m x", positive=True)

A_of = lambda u: sp.simplify(-sp.diff(u, w, 2) / sp.diff(u, w))
P_of = lambda u: sp.simplify(-sp.diff(u, w, 3) / sp.diff(u, w, 2))

print("=" * 72)
print("Schroyen & Treich, 'The Power of Money: Wealth Effects in Contests'")
print("TSE WP 16-699, manuscript 11 April 2016.  Checks of OUR READING.")
print("Theorem 3 / eq (A.16):   2A(1 - m^2) > P,   A = -u''/u',  P = -u'''/u''")
print("=" * 72)
print()

print("ST-2  Arrow-Pratt definitions reproduce A = P under CARA")
print("-" * 72)
u_cara = -sp.exp(-a * w)
A_cara, P_cara = A_of(u_cara), P_of(u_cara)
print(f"      CARA u=-exp(-a w):  A = {A_cara},  P = {P_cara}")
check("ST-2 A and P well-formed; CARA gives A = P = a",
      bool(sp.simplify(A_cara - a) == 0 and sp.simplify(P_cara - a) == 0))

print()
print("ST-3  Quadratic utility:  P = 0, condition reduces to m < 1")
print("-" * 72)
u_quad = w - b * w**2
A_quad, P_quad = A_of(u_quad), P_of(u_quad)
print(f"      quadratic u=w-b w^2:  A = {A_quad},  P = {P_quad}")
# 2A(1-m^2) > 0 with A > 0  <=>  1 - m^2 > 0  <=>  m < 1  (m > 0)
cond_quad = sp.simplify(2 * A_quad * (1 - m**2) - P_quad)
red_quad = [sp.simplify(r) for r in sp.solve(sp.Eq(cond_quad, 0), m)]
print(f"      2A(1-m^2) - P = {sp.factor(cond_quad)};  boundary at m = {red_quad}")
check("ST-3 quadratic reduces to m < 1",
      bool(P_quad == 0 and any(sp.simplify(r - 1) == 0 for r in red_quad)),
      "P = 0 so the boundary is exactly m = 1")

print()
print("ST-4  CARA:  A = P, condition reduces to m < 2^(-1/2)")
print("-" * 72)
cond_cara = sp.simplify(2 * A_cara * (1 - m**2) - P_cara)
sols = [sp.simplify(r) for r in sp.solve(sp.Eq(cond_cara, 0), m)]
target = sp.Pow(2, sp.Rational(-1, 2))
print(f"      2A(1-m^2) - P = {sp.factor(cond_cara)};  boundary at m = {sols}")
print(f"      target 2^(-1/2) = {sp.nsimplify(target)} = {float(target):.6f}")
check("ST-4 CARA reduces to m < 2^(-1/2) ~ .707",
      any(sp.simplify(r - target) == 0 for r in sols),
      f"boundary {float(target):.6f}")

print()
print("ST-5  CRRA:  condition reduces to gamma*(1/2 - m^2) > 1/2")
print("-" * 72)
u_crra = w ** (1 - gam) / (1 - gam)
A_crra, P_crra = A_of(u_crra), P_of(u_crra)
print(f"      CRRA:  A = {A_crra},  P = {P_crra}")
lhs = sp.simplify(2 * A_crra * (1 - m**2) - P_crra)
tgt = sp.simplify(gam * (sp.Rational(1, 2) - m**2) - sp.Rational(1, 2))
# the two inequalities agree iff lhs is a positive multiple of tgt
ratio = sp.simplify(sp.cancel(lhs / tgt))
print(f"      [2A(1-m^2) - P] / [gamma(1/2 - m^2) - 1/2] = {ratio}")
check("ST-5 CRRA reduces to gamma(1/2 - m^2) > 1/2",
      bool(sp.simplify(ratio - 2 / w) == 0),
      "ratio is 2/w > 0, so the two inequalities have the same truth value")

print()
print("ST-8  Multiplying (A.16) by (w-x) gives relative RA and relative prudence")
print("-" * 72)
rel = sp.symbols("relscale", positive=True)
lhs_abs = 2 * A_crra * (1 - m**2) - P_crra
lhs_rel = sp.simplify(sp.expand(lhs_abs * rel))
ok_rel = sp.simplify(sp.cancel(lhs_rel / lhs_abs) - rel) == 0
print(f"      scaling by a positive factor preserves the sign: {ok_rel}")
check("ST-8 relative-risk restatement is sign-preserving", bool(ok_rel),
      "A*(w-x) and P*(w-x) are relative risk aversion and relative prudence")

print()
print("ST-12a  The condition GENUINELY depends on the third derivative")
print("-" * 72)
print("      If two utilities with the SAME A but different P could not give")
print("      opposite signs, 'no third derivative needed' would be an empty")
print("      distinction and our separation from Theorem 3 would be verbal.")
# CARA with a=1 at any w:  A = 1, P = 1
# log utility (CRRA gamma=1) at w=1:  A = 1/w = 1, P = 2/w = 2
A1, P1 = sp.Integer(1), sp.Integer(1)
A2, P2 = sp.Integer(1), sp.Integer(2)
mstar = sp.Rational(1, 2)
s1 = sp.simplify(2 * A1 * (1 - mstar**2) - P1)
s2 = sp.simplify(2 * A2 * (1 - mstar**2) - P2)
A_log = sp.simplify(A_of(sp.log(w)).subs(w, 1))
P_log = sp.simplify(P_of(sp.log(w)).subs(w, 1))
print(f"      CARA a=1:      A = {A1}, P = {P1}")
print(f"      log at w=1:    A = {A_log}, P = {P_log}   (derived, not asserted)")
print(f"      at m = 1/2:    CARA sign = {s1} (> 0),  log sign = {s2} (< 0)")
check("ST-12a same A, different P, opposite signs at a common m",
      bool(A_log == A1 and P_log == P2 and s1 > 0 and s2 < 0),
      "third derivative is load-bearing in (A.16); P9-gen needs none")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"SCHROYEN-TREICH SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
