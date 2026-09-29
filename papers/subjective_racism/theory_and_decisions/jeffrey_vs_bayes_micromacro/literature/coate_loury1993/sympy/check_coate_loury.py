"""
Coate & Loury (1993), AER 83(5), 1220-1240 -- Bayes rule, standards,
self-confirming beliefs, and the Section II.B example.

Checks, exactly (sympy, rationals):
  (1) eq. (1): pi fq/(pi fq + (1-pi) fu) = 1/{1 + [(1-pi)/pi] phi}  [Lean: posterior_eq_odds];
      assignment rule r >= [(1-pi)/pi] phi  <=>  pi >= phi/(r+phi)
      [Lean: assign_iff, assign_iff_threshold];
  (2) the stereotype of p. 1221 as a believed covariance:
      Cov(1_W, 1_q) = lam(1-lam)(pi_w - pi_b)  [Lean: cov_group_qualified];
  (3) the smooth instance: from f_q = 2(1+t)/3, f_u = 2(2-t)/3 derive
      phi = (2-t)/(1+t), F_u - F_q = 2s(1-s)/3; with omega = 3, uniform costs, r = 1:
      EE(s) = (2-s)/3, WW(s) = 2s(1-s); intersections s in {1/2, 2/3} giving
      self-confirming beliefs {1/2, 4/9}; local stability by the slope criterion of
      p. 1226 (|EE'| > |WW'|): 1/2 stable, 4/9 unstable  [Lean: smooth_example];
  (4) Example II.B: p_q, p_u from theta_q, theta_u; beta at the liberal and
      conservative standards equals omega(1-p_u), omega(1-p_q); pihat = middle term
      of eq. (6)  [Lean: pihat_eq6]; eq. (7) alpha; eq. (8) roots {pi_l, 1-pi_l}
      [Lean: alpha_eq7, eq8_solutions]; footnote 20's lambda-hat
      [Lean: lambdaHat_iff];
  (5) Proposition 3 dynamics (9): slopes (1-pi_l)/pi_l < 1 at 1 - pi_l and
      pi_l/(1-pi_l) > 1 at pi_l; 60 exact iterations from pi_c approach 1 - pi_l
      [Lean: colorBlind_unstable, patronSeq_tendsto];
  (6) footnote 21: with p_u = .2, p_q = .3, r = 2/3 the patronization region is
      exactly 0.5/(lam - 0.2) < omega < 5/7 (nonempty iff lam > 0.9), and the
      stereotype worsens iff omega > 2/3  [Lean: fn21_instance for one point].
Exit status 0 iff every check passes.
"""
import sys
import sympy as sp

ok = True
R = sp.Rational


def check(name, cond):
    global ok
    cond = bool(cond)
    print(f"[{'PASS' if cond else 'FAIL'}] {name}")
    ok = ok and cond


pi, fq, fu, r, phi = sp.symbols('pi f_q f_u r phi', positive=True)

# (1)
xi = pi * fq / (pi * fq + (1 - pi) * fu)
check("eq.(1): odds form", sp.simplify(xi - 1 / (1 + (1 - pi) / pi * fu / fq)) == 0)
xq, xu = sp.symbols('x_q x_u', positive=True)
payoff = xi * xq - (1 - xi) * xu
# sign(payoff) = sign(pi fq xq - (1-pi) fu xu) since the denominator is positive
check("assignment payoff numerator",
      sp.simplify(payoff * (pi * fq + (1 - pi) * fu) - (pi * fq * xq - (1 - pi) * fu * xu)) == 0)
check("r >= [(1-pi)/pi] phi  <=>  pi >= phi/(r+phi): boundary coincides",
      sp.simplify(((1 - pi) / pi * phi - r).subs(pi, phi / (r + phi))) == 0)

# (2)
lam, pb, pw = sp.symbols('lam pi_b pi_w', real=True)
cov = lam * pw - lam * (lam * pw + (1 - lam) * pb)
check("p.1221: Cov(1_W, 1_q) = lam(1-lam)(pi_w - pi_b)",
      sp.expand(cov - lam * (1 - lam) * (pw - pb)) == 0)

# (3) smooth instance
t, s = sp.symbols('t s', real=True)
fq_ = 2 * (1 + t) / 3
fu_ = 2 * (2 - t) / 3
check("densities integrate to 1",
      sp.integrate(fq_, (t, 0, 1)) == 1 and sp.integrate(fu_, (t, 0, 1)) == 1)
phi_ = sp.simplify(fu_ / fq_)
check("phi = (2-t)/(1+t), strictly decreasing",
      sp.simplify(phi_ - (2 - t) / (1 + t)) == 0
      and sp.simplify(sp.diff(phi_, t) + 3 / (1 + t) ** 2) == 0)
Fq = sp.integrate(fq_, (t, 0, s))
Fu = sp.integrate(fu_, (t, 0, s))
omega = 3
beta = sp.expand(omega * (Fu - Fq))
check("beta(s) = 2 s (1-s) (<= 1/2, so G(beta) = beta under uniform costs)",
      sp.expand(beta - 2 * s * (1 - s)) == 0)
EE = sp.simplify(phi_.subs(t, s) / (1 + phi_.subs(t, s)))
WW = beta
check("EE(s) = (2-s)/3", sp.simplify(EE - (2 - s) / 3) == 0)
sols = sp.solve(sp.Eq(EE, WW), s)
check("EE = WW at s in {1/2, 2/3}", set(sols) == {R(1, 2), R(2, 3)})
beliefs = {sp.simplify(EE.subs(s, x)) for x in sols}
check("self-confirming beliefs {1/2, 4/9} (plus the zero belief)", beliefs == {R(1, 2), R(4, 9)})
dEE, dWW = sp.diff(EE, s), sp.diff(WW, s)
check("p.1226 stability: 1/2 stable (|EE'| = 1/3 > |WW'| = 0)",
      abs(dEE.subs(s, R(1, 2))) > abs(dWW.subs(s, R(1, 2))))
check("p.1226 stability: 4/9 unstable (|EE'| = 1/3 < |WW'| = 2/3)",
      abs(dEE.subs(s, R(2, 3))) < abs(dWW.subs(s, R(2, 3))))
check("Prop. 1 hypothesis at s0 = 7/12: WW > EE",
      WW.subs(s, R(7, 12)) > EE.subs(s, R(7, 12)))
# the adjustment process pi_{t+1} = G(beta(s*(pi_t))) near each root
check("s*(pi) = 2 - 3 pi inverts EE", sp.simplify(EE.subs(s, 2 - 3 * pi) - pi) == 0)
step = lambda p_: WW.subs(s, 2 - 3 * p_)
x = R(49, 100)
for _ in range(6):   # WW'(1/2) = 0: quadratic convergence
    x = step(x)
check("adjustment from 0.49 converges toward 1/2 (6 exact steps, error < 1e-6)",
      abs(x - R(1, 2)) < R(1, 10 ** 6))
x = R(43, 100)   # just below 4/9: moves away, toward the zero belief
for _ in range(5):
    x = step(x)
check("adjustment from 0.43 moves away from 4/9 (downward)", x < R(43, 100))

# (4) Example II.B
thq, thu, w_ = sp.symbols('theta_q theta_u omega', positive=True)
pq = (thu - thq) / (1 - thq)
pu = (thu - thq) / thu
Fq_u = lambda z: sp.Piecewise((0, z <= thq), ((z - thq) / (1 - thq), True))
Fu_u = lambda z: sp.Piecewise((z / thu, z <= thu), (1, True))
# theta_q < theta_u: F_q(theta_q) = 0, F_u(theta_q) = theta_q/theta_u,
# F_u(theta_u) = 1, F_q(theta_u) = (theta_u - theta_q)/(1 - theta_q)
check("the CDF branches used below (concrete theta_q = 1/4, theta_u = 3/5)",
      Fq_u(thq).subs({thq: R(1, 4), thu: R(3, 5)}) == 0
      and Fu_u(thq).subs({thq: R(1, 4), thu: R(3, 5)}) == R(5, 12)
      and Fu_u(thu).subs({thq: R(1, 4), thu: R(3, 5)}) == 1
      and Fq_u(thu).subs({thq: R(1, 4), thu: R(3, 5)}) == R(7, 15))
beta_lib = w_ * (thq / thu - 0)
beta_con = w_ * (1 - (thu - thq) / (1 - thq))
check("beta(theta_q) = omega(1-p_u) = pi_l", sp.simplify(beta_lib - w_ * (1 - pu)) == 0)
check("beta(theta_u) = omega(1-p_q) = pi_c", sp.simplify(beta_con - w_ * (1 - pq)) == 0)
phiII = sp.simplify((1 / thu) / (1 / (1 - thq)))
check("phi on the unclear range = (1-theta_q)/theta_u = p_u/p_q",
      sp.simplify(phiII - pu / pq) == 0)
Pq, Pu = sp.symbols('p_q p_u', positive=True)
pihat = (Pu / Pq) / (xq / xu + Pu / Pq)
check("eq.(6) middle term = pihat", sp.simplify(pihat - xu * Pu / (xq * Pq + xu * Pu)) == 0)
pl, al = sp.symbols('pi_l alpha', real=True)
alpha_sol = sp.solve(sp.Eq(pl + (1 - pl) * Pu, pb + (1 - pb) * (Pu + (1 - Pu) * al)), al)[0]
check("eq.(7): alpha = (pi_l - pi_b)/(1 - pi_b)", sp.simplify(alpha_sol - (pl - pb) / (1 - pb)) == 0)
check("B investment under patronization = (1-alpha) pi_l",
      sp.simplify(w_ * (1 - al) * (1 - Pu) - (1 - al) * (w_ * (1 - Pu))) == 0)
roots8 = sp.solve(sp.Eq(pb, (1 - pl) / (1 - pb) * pl), pb)
check("eq.(8): roots {pi_l, 1 - pi_l}", set(roots8) == {pl, 1 - pl})
xil, lamv = sp.symbols('xi_l lam', positive=True)
lhs = lamv / (1 - lamv) * (xil * xq - (1 - xil) * xu) - xu
check("fn 20: boundary at lam = 1/(xi_l (1 + r))",
      sp.simplify(lhs.subs(lamv, 1 / (xil * (1 + xq / xu)))) == 0)

# (5) dynamics (9)
xx = sp.symbols('x', real=True)
stepf = (1 - pl) * pl / (1 - xx)
d = sp.diff(stepf, xx)
check("slope at 1 - pi_l is (1-pi_l)/pi_l", sp.simplify(d.subs(xx, 1 - pl) - (1 - pl) / pl) == 0)
check("slope at pi_l is pi_l/(1-pi_l)", sp.simplify(d.subs(xx, pl) - pl / (1 - pl)) == 0)
PL, PC = R(14, 25), R(49, 100)
x = PC
for _ in range(60):
    x = (1 - PL) * PL / (1 - x)
check("60 exact iterations from pi_c = .49 reach 1 - pi_l = .44 within 1e-6",
      abs(x - (1 - PL)) < R(1, 10 ** 6))

# (6) footnote 21
om = sp.symbols('omega', positive=True)
pu21, pq21, r21 = R(1, 5), R(3, 10), R(2, 3)
pihat21 = (pu21 / pq21) / (r21 + pu21 / pq21)
pil = om * (1 - pu21)
pic = om * (1 - pq21)
check("fn 21: pihat = 1/2", pihat21 == R(1, 2))
# eq. (6) is 7w/10 < 1/2 < 4w/5, i.e. 5/8 < w < 5/7
check("fn 21: eq.(6) <=> 5/8 < omega < 5/7",
      sp.solve(sp.Eq(pic, pihat21), om) == [R(5, 7)] and sp.solve(sp.Eq(pil, pihat21), om) == [R(5, 8)])
xi21 = pil * pq21 / (pil * pq21 + (1 - pil) * pu21)
lam_hat = sp.simplify(1 / (xi21 * (1 + r21)))
# lam > lam_hat  <=>  omega > 0.5/(lam - 0.2)
L = sp.symbols('L', positive=True)
bound = sp.solve(sp.Eq(lam_hat, L), om)
check("fn 21: lam > lamhat <=> omega > 0.5/(lam - 0.2)",
      len(bound) == 1 and sp.simplify(bound[0] - R(1, 2) / (L - R(1, 5))) == 0
      and sp.diff(lam_hat, om).subs(om, R(7, 10)) < 0)
check("fn 21: region nonempty iff lam > 0.9 (0.5/(lam-0.2) = 5/7 at lam = 0.9)",
      sp.solve(sp.Eq(R(1, 2) / (L - R(1, 5)), R(5, 7)), L) == [R(9, 10)])
check("fn 21: 0.5/(lam-0.2) >= 5/8 for lam <= 1, so eq.(6)'s lower bound is implied",
      R(1, 2) / (1 - R(1, 5)) == R(5, 8))
check("fn 21: pi_l > 1/2 <=> omega > 5/8 (implied)", sp.solve(sp.Eq(pil, R(1, 2)), om) == [R(5, 8)])
check("fn 21: stereotype worsens (pi_l + pi_c > 1) <=> omega > 2/3",
      sp.solve(sp.Eq(pil + pic, 1), om) == [R(2, 3)])

print("ALL PASS" if ok else "SOME CHECKS FAILED")
sys.exit(0 if ok else 1)
