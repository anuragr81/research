"""
Arrow (1973), "The Theory of Discrimination" (Princeton IR Section WP 30A, 1971
pagination) -- the algebra of Sections 1, 2 and 4.

Checks, exactly (sympy):
  (1) eqs. (2)-(4): w_W - w_B = d_B - d_W  [Lean: wage_gap];
  (2) eqs. (5)-(6): pi - pi0 = d_W W + d_B B  [Lean: profit_change];
  (3) eq. (7): for U = V(pi, B/W), d_W W + d_B B = 0 with d_i = -U_i/U_pi
      [Lean: ratio_utility_euler, eq7_of_ratio_utility];
  (4) p. 8: W/L = d_B/(w_W-w_B), B/L = -d_W/(w_W-w_B)  [Lean: segregation_shares];
  (5) eq. (14) from (13), and w_W > w_B when p_B < p_W  [Lean: eq14, wage_differential];
  (6) the stability condition (p. 31).  Two groups with population weights
      n_W, n_B; MP_S = M(P), w_U = U(P) with P = n_W p_W + n_B p_B the skilled
      supply; w_i = M(P) - r/p_i by (13); dynamics dp_i/dt = S(w_i - w_U) - p_i.
      At a symmetric equilibrium the Jacobian has the antisymmetric eigenvector
      (n_B, -n_W) with eigenvalue S'(v) r/p^2 - 1, which is < 0 iff
      E (MP_S - w_S)/(w_S - w_U) < 1 -- Arrow's condition; the other eigenvalue
      adds S'(v)(M'(P) - U'(P))(n_W + n_B), negative under diminishing returns
      (M' < 0, U' > 0), so Arrow's condition is necessary and sufficient for
      local stability  [Lean: desired_hasDerivAt, stability_iff];
  (7) the explicit instance: S(v) = v/(v+2), MP_S - w_U = 4, r = 1 has
      self-confirming shares exactly {1/2, 1/3}; criterion values 1/2 and 2
      [Lean: example_two_fixed_points, example_only_fixed_points,
      example_discriminatory, example_stability].
Exit status 0 iff every check passes.
"""
import sys
import sympy as sp

ok = True


def check(name, cond):
    global ok
    cond = bool(cond)
    print(f"[{'PASS' if cond else 'FAIL'}] {name}")
    ok = ok and cond


MP, wW, wB, dW, dB, W, B, L = sp.symbols('MP w_W w_B d_W d_B W B L', real=True)

# (1)
sol = sp.solve([sp.Eq(MP, wB + dB), sp.Eq(MP, wW + dW)], [wW, wB], dict=True)[0]
check("eq.(4): w_W - w_B = d_B - d_W", sp.simplify(sol[wW] - sol[wB] - (dB - dW)) == 0)

# (2)
f = sp.Function('f')
profit = f(W + B) - (MP - dW) * W - (MP - dB) * B
check("eqs.(5)-(6): pi - pi0 = d_W W + d_B B",
      sp.simplify(profit - (f(W + B) - MP * (W + B)) - (dW * W + dB * B)) == 0)

# (3)
pi = sp.symbols('pi', real=True)
Wp, Bp = sp.symbols('W_p B_p', positive=True)
V = sp.Function('V')
U = V(pi, Bp / Wp)
Upi, UB, UW = sp.diff(U, pi), sp.diff(U, Bp), sp.diff(U, Wp)
check("eq.(7): d_W W + d_B B = 0 for ratio-dependent utility",
      sp.simplify((-UW / Upi) * Wp + (-UB / Upi) * Bp) == 0)

# (4)
s4 = sp.solve([sp.Eq(wW - wB, dB - dW), sp.Eq(dW * W + dB * B, 0), sp.Eq(W + B, L)],
              [W, B, wW], dict=True)[0]
check("p. 8: W/L = d_B/(w_W-w_B)",
      sp.simplify(s4[W] / L - dB / (s4[wW] - wB)) == 0)
check("p. 8: B/L = -d_W/(w_W-w_B)",
      sp.simplify(s4[B] / L + dW / (s4[wW] - wB)) == 0)

# (5)
MPS, pW, pB, r = sp.symbols('MP_S p_W p_B r', positive=True)
wWs = sp.solve(sp.Eq((MPS - wW) * pW, (MPS - wB) * pB), wW)[0]
qq = pB / pW
check("eq.(14): w_W = q w_B + (1-q) MP_S",
      sp.simplify(wWs - (qq * wB + (1 - qq) * MPS)) == 0)
check("w_W - w_B = (1-q)(MP_S - w_B)",
      sp.simplify(wWs - wB - (1 - qq) * (MPS - wB)) == 0)

# (6) stability.  Generic smooth S, M, U are represented by second-order Taylor
# polynomials around the symmetric point, so the Jacobian there involves exactly
# S'(v) = S1, M'(P) = M1, U'(P) = U1 (and nothing is lost: only first
# derivatives enter a Jacobian).
nW, nB, p = sp.symbols('n_W n_B p', positive=True)
xW, xB = sp.symbols('x_W x_B', positive=True)
S1, S2, M1, M2, U1, U2, m0, u0 = sp.symbols('S1 S2 M1 M2 U1 U2 m0 u0', real=True)
P0 = (nW + nB) * p
v0 = m0 - r / p - u0
Mf = lambda P: m0 + M1 * (P - P0) + M2 * (P - P0) ** 2
Uf = lambda P: u0 + U1 * (P - P0) + U2 * (P - P0) ** 2
Sf = lambda x: p + S1 * (x - v0) + S2 * (x - v0) ** 2   # S(v0) = p: symmetric equilibrium
P = nW * xW + nB * xB
rhs = sp.Matrix([Sf(Mf(P) - r / xW - Uf(P)) - xW,
                 Sf(Mf(P) - r / xB - Uf(P)) - xB])
check("the symmetric point is a rest point", sp.simplify(rhs.subs({xW: p, xB: p})) == sp.zeros(2, 1))
J = sp.simplify(rhs.jacobian([xW, xB]).subs({xW: p, xB: p}))
anti = sp.Matrix([nB, -nW])
lam_anti = S1 * r / p ** 2 - 1
check("Jacobian: (n_B, -n_W) is an eigenvector with eigenvalue S'(v) r/p^2 - 1",
      sp.simplify(J * anti - lam_anti * anti) == sp.zeros(2, 1))
lam_sym = lam_anti + S1 * (M1 - U1) * (nW + nB)
check("other eigenvalue = S'(v) r/p^2 - 1 + S'(v)(M'-U')(n_W+n_B)",
      sp.simplify(J.trace() - lam_anti - lam_sym) == 0
      and sp.simplify(J.det() - lam_anti * lam_sym) == 0)
# Arrow's form of the antisymmetric condition
Sv, vv, wS, wU = sp.symbols('Sv v w_S w_U', positive=True)
E = S1 * vv / Sv
crit = E * (MPS - wS) / (wS - wU)
crit_at = crit.subs({Sv: p, wS: MPS - r / p, vv: MPS - r / p - wU})
check("p. 31: E(MP_S - w_S)/(w_S - w_U) equals S'(v) r / p^2 at S(v) = p",
      sp.simplify(crit_at - S1 * r / p ** 2) == 0)

# (7) the explicit instance
pp_ = sp.symbols('pp', positive=True)
S = lambda x: x / (x + 2)
vfun = 4 - 1 / pp_
roots = sp.solve(sp.Eq(S(vfun), pp_), pp_)
check("instance: self-confirming shares are exactly {1/2, 1/3}",
      set(roots) == {sp.Rational(1, 2), sp.Rational(1, 3)})
Sprime = sp.diff(S(sp.Symbol('x')), sp.Symbol('x'))
critv = lambda pv: (Sprime.subs(sp.Symbol('x'), vfun.subs(pp_, pv)) * vfun.subs(pp_, pv)
                    / S(vfun.subs(pp_, pv))) * (1 / pv) / vfun.subs(pp_, pv)
check("instance: criterion 1/2 at p = 1/2 (stable), 2 at p = 1/3 (unstable)",
      sp.nsimplify(critv(sp.Rational(1, 2))) == sp.Rational(1, 2)
      and sp.nsimplify(critv(sp.Rational(1, 3))) == 2)
check("instance: w_W - w_B = r/p_B - r/p_W = 1 > 0",
      (1 / sp.Rational(1, 3)) - (1 / sp.Rational(1, 2)) == 1)

print("ALL PASS" if ok else "SOME CHECKS FAILED")
sys.exit(0 if ok else 1)
