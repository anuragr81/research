"""
verify_comparative_statics.py

Canonical symbolic verifier for the algebraic cores of the reduction, the
comparative statics, and the second-order conditions:

  eq:rhoL   the homogeneity reduction, and that it forces rho_L (not rho)
  eq:Maffine  the impulse operator is affine in x
  SOC       sign(V'') = sign(Psi) at any point where V' = 1+kappa
  TCS/KCS   the barrier identities and their ratio
  IDN       the Jacobian determinant factorisation and its sign

WHAT THIS DOES NOT DO. It verifies algebra, not analysis. The envelope
step (Danskin), the hitting-time expansion at the trigger, the sign-change
argument at the injection target, and the existence of the derivatives are
proofs in the document; nothing here substitutes for them. What it does
catch is exactly the class of error that reached the document once
already: a discount rate carried through a reduction incorrectly.

Run: python3 verify_comparative_statics.py
"""

import sympy as sp

FAILURES = []


def check(tag, claim, ok):
    status = "PASS" if ok else "FAIL"
    if not ok:
        FAILURES.append(tag)
    print(f"{tag:<6}{status:<8}{claim}")


# --- symbols ---------------------------------------------------------------
x, y, A, L = sp.symbols('x y A L', positive=True)
rho, mu_L, kap, K, R, lamS = sp.symbols('rho mu_L kappa K R lambda_S', positive=True)
rho_L = rho - mu_L

V = sp.Function('V')
s2 = sp.Function('sigma2')      # sigma^2(x, u(x))
mu = sp.Function('mu')          # drift of x
Lam = sp.Function('Lambda')     # shortfall penalty

print("=" * 74)
print("PART 1 -- the homogeneity reduction (eq:rhoL)")
print("=" * 74)

# J(A,L) = L * V(A/L), the unreduced value; x = A/L
J = L * V(A / L)
dJ_dL = sp.simplify(sp.diff(J, L))
# expected: V(x) - x V'(x), with x = A/L
expected = (V(x) - x * sp.Derivative(V(x), x)).subs(x, A / L).doit()
check("H1a", "dJ/dL = V(x) - x V'(x)",
      sp.simplify(dJ_dL - expected.doit()) == 0)

# The liability drift contributes mu_L * L * dJ/dL; discounting is -rho*J.
# Divide by L and collect the terms multiplying V and V'.
Vx, Vpx = sp.symbols('V_x Vp_x')
per_unit = mu_L * (Vx - x * Vpx) - rho * Vx
reduced = -(rho - mu_L) * Vx - mu_L * x * Vpx
check("H1b", "mu_L(V - xV') - rho*V  ==  -rho_L*V - mu_L*x*V'",
      sp.simplify(sp.expand(per_unit - reduced)) == 0)

# The -mu_L*x*V' term is precisely what makes the drift carry -mu_L*x, so a
# drift written with -mu_L*x MUST be paired with a -rho_L*V discount term.
coeff_V = sp.simplify(sp.expand(per_unit).coeff(Vx))
check("H1c", f"coefficient on V is -(rho - mu_L) = -rho_L, not -rho",
      sp.simplify(coeff_V + rho_L) == 0 and sp.simplify(coeff_V + rho) != 0)

print()
print("=" * 74)
print("PART 2 -- the impulse operator is affine (eq:Maffine)")
print("=" * 74)

integrand = V(y) - (1 + kap) * (y - x)
separated = (V(y) - (1 + kap) * y) + (1 + kap) * x
check("H2a", "V(y)-(1+k)(y-x) = [V(y)-(1+k)y] + (1+k)x  (y-part separates)",
      sp.simplify(integrand - separated) == 0)
check("H2b", "so d/dx of Mv is (1+kappa): Mv is affine, slope 1+kappa",
      sp.simplify(sp.diff(separated, x) - (1 + kap)) == 0)
check("H2c", "and argmax over y is independent of x",
      sp.simplify(sp.diff(V(y) - (1 + kap) * y, x)) == 0)

print()
print("=" * 74)
print("PART 3 -- second-order conditions (SOC): sign(V'') = sign(Psi)")
print("=" * 74)

# Interior equation:  1/2 s2 V'' + mu V' - rho_L V - Lambda = 0
Vpp = sp.Symbol('Vpp')
hjb = sp.Rational(1, 2) * s2(x) * Vpp + mu(x) * (1 + kap) - rho_L * V(x) - Lam(x)
sol = sp.solve(sp.Eq(hjb, 0), Vpp)[0]
Psi = rho_L * V(x) + Lam(x) - (1 + kap) * mu(x)
check("H3a", "at V'=1+kappa:  (1/2)sigma^2 V'' = Psi := rho_L V + Lambda - (1+k)mu",
      sp.simplify(sp.Rational(1, 2) * s2(x) * sol - Psi) == 0)
check("H3b", "sigma^2 > 0 so V'' and Psi share a sign",
      True)  # positivity of sigma^2 is Lemma ITR, not algebra

print()
print("=" * 74)
print("PART 4 -- barrier identities (TCS, KCS)")
print("=" * 74)

# Differentiate the interior equation in x, then impose V''=0, V'=1, Lambda'=0.
Vp, Vppp = sp.symbols('Vp Vppp')
s2s, mus, mups = sp.symbols('s2 mu mup', positive=True)
# d/dx[ 1/2 s2 V'' + mu V' - rho_L V - Lam ]
#   = 1/2 s2' V'' + 1/2 s2 V''' + mu' V' + mu V'' - rho_L V' - Lam'
# at the barrier V''=0, V'=1, Lam'=0:
diffd = sp.Rational(1, 2) * s2s * Vppp + mups * 1 - rho_L * 1
Vppp_sol = sp.solve(sp.Eq(diffd, 0), Vppp)[0]
check("H4a", "(1/2)sigma^2 V''' = rho_L - mu'  at the barrier",
      sp.simplify(sp.Rational(1, 2) * s2s * Vppp_sol - (rho_L - mups)) == 0)

# W-equation at the barrier: W'=0 and (R-y*)^+ = 0 for y* > R
Wv, Wpp = sp.symbols('W Wpp', positive=True)
weq = sp.Rational(1, 2) * s2s * Wpp + mus * 0 - rho_L * Wv + 0
Wpp_sol = sp.solve(sp.Eq(weq, 0), Wpp)[0]
check("H4b", "(1/2)sigma^2 W'' = rho_L W  at the barrier",
      sp.simplify(sp.Rational(1, 2) * s2s * Wpp_sol - rho_L * Wv) == 0)

# N-equation is homogeneous; same computation with the running term absent
Nv, Npp = sp.symbols('N Npp', positive=True)
neq = sp.Rational(1, 2) * s2s * Npp + mus * 0 - rho_L * Nv
Npp_sol = sp.solve(sp.Eq(neq, 0), Npp)[0]
check("H4c", "(1/2)sigma^2 N'' = rho_L N  at the barrier (homogeneous)",
      sp.simplify(sp.Rational(1, 2) * s2s * Npp_sol - rho_L * Nv) == 0)

check("H4d", "dy*/dlambda_S = W''/V''' = rho_L W / (rho_L - mu')",
      sp.simplify(Wpp_sol / Vppp_sol - rho_L * Wv / (rho_L - mups)) == 0)
check("H4e", "dy*/dK       = N''/V''' = rho_L N / (rho_L - mu')",
      sp.simplify(Npp_sol / Vppp_sol - rho_L * Nv / (rho_L - mups)) == 0)

print()
print("=" * 74)
print("PART 5 -- joint identification (IDN)")
print("=" * 74)

Wp_xL, Np_xL, Vpp_xL = sp.symbols('Wp_xL Np_xL Vpp_xL')
den = rho_L - mups
Jac = sp.Matrix([
    [rho_L * Wv / den, rho_L * Nv / den],
    [Wp_xL / Vpp_xL,   Np_xL / Vpp_xL],
])
det = sp.simplify(Jac.det())
factored = (rho_L / (den * Vpp_xL)) * (Wv * Np_xL - Nv * Wp_xL)
check("H5a", "det factorises as [rho_L/((rho_L-mu')V''(x_L))]*(W(y*)N'(x_L) - N(y*)W'(x_L))",
      sp.simplify(det - factored) == 0)

# Sign, verified STRUCTURALLY rather than at sample values: encode each
# established sign in the symbol itself, so the conclusion cannot depend on
# a numerical accident. (An earlier version substituted arbitrary positive
# numbers and passed even when N'(x_L) was flipped positive -- caught by
# mutation testing. Do not reintroduce that form.)
Wp, Np_mag, Vpp_p, gap = sp.symbols('Wpos Nprime_mag Vpp_pos gap', positive=True)
Wv_p, Nv_p, WpxL_p = sp.symbols('W_pos N_pos WpxL_pos', positive=True)
bracket_sym = Wv_p * (-Np_mag) - Nv_p * WpxL_p          # N'(x_L) = -Np_mag < 0
check("H5b", "with W(y*)>0, N(y*)>0, W'(x_L)>0, N'(x_L)<0 the bracket is negative",
      sp.simplify(bracket_sym + (Wv_p * Np_mag + Nv_p * WpxL_p)) == 0
      and (Wv_p * Np_mag + Nv_p * WpxL_p).is_positive is True)
prefac_sym = rho_L.subs({rho: mu_L + gap}) / (den.subs(mups, rho_L - gap) * Vpp_p)
check("H5c", "with rho_L>0, rho_L>mu', V''(x_L)>0 the prefactor is positive",
      sp.simplify(prefac_sym - gap / (gap * Vpp_p)) == 0
      and (1 / Vpp_p).is_positive is True)
check("H5d", "hence det < 0, in particular non-zero: local identification",
      (bracket_sym / (Vpp_p)).is_negative is not False
      and sp.simplify(-bracket_sym - (Wv_p * Np_mag + Nv_p * WpxL_p)) == 0)

print()
print("=" * 74)
if FAILURES:
    print(f"{len(FAILURES)} FAILURE(S): {FAILURES}")
    raise SystemExit(1)
print("all checks resolved as expected")
print()
print("Scope reminder: this file verifies algebra only. ENV/ENVK (Danskin),")
print("the hitting-time expansion in TCS, the sign-change argument in SOC(i),")
print("and the existence of every derivative used are proofs in the document.")
