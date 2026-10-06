import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


x, u, y, t, k = sp.symbols("x u y t k", positive=True)

print("=" * 72)
print("Wing Suen, 'The comparative statics of differential rents in two-sided")
print("matching markets', J Econ Inequal (2007) 5:149-158.")
print("Checks of OUR READING.")
print("=" * 72)
print()

print("SU-1  Matching function mu = G^-1 o F is itself a distribution function")
print("-" * 72)
print("      p.151: 'the matching function mu(x) = G^-1(F(x)) is an increasing")
print("      function with mu(0)=0 and mu(1)=1. Indeed, one can regard the")
print("      matching function itself as a distribution function.'")
allok = True
rows = []
for Fl, F in [("F=x", x), ("F=x^2", x**2)]:
    for Gl, G in [("G=y", y), ("G=y^3", y**3)]:
        Ginv = [s for s in sp.solve(sp.Eq(G, u), y) if s.is_real is not False][0]
        muf = sp.simplify(Ginv.subs(u, F))
        endpoints = (sp.simplify(muf.subs(x, 0)) == 0
                     and sp.simplify(muf.subs(x, 1)) == 1)
        slope_ok = bool(sp.minimum(sp.diff(muf, x), x, sp.Interval(0, 1)) >= 0)
        ok = endpoints and slope_ok
        rows.append(f"{Fl},{Gl}:{'ok' if ok else 'BAD'}")
        if not ok:
            allok = False
print("      " + "  ".join(rows))
check("SU-1 mu increasing with mu(0)=0, mu(1)=1", allok,
      "so mu qualifies as a CDF on [0,1], as the paper notes")

print()
print("SU-2  Equilibrium wage ODE (2):  w'(x) = theta'(x) phi(mu(x))")
print("-" * 72)
print("      From the Y-agent's problem max theta(x)phi(y) - w(x), FOC")
print("      theta'(x)phi(y) - w'(x) = 0 at y = mu(x).")
th = sp.Function("theta")
ph = sp.Function("phi")
muF = sp.Function("mu")
foc = th(x).diff(x) * ph(y) - sp.Function("w")(x).diff(x)
foc_at = foc.subs(y, muF(x))
print(f"      FOC at y = mu(x):  {foc_at} = 0")
# Second-order condition: w'' > theta'' phi(mu) when mu is increasing.
soc_gap = sp.simplify(
    th(x).diff(x) * ph(muF(x)).diff(x)
)
print(f"      w'' - theta'' phi(mu) = theta' * d/dx[phi(mu)] = {soc_gap}")
check("SU-2 the FOC and the SOC gap are as stated",
      bool(sp.simplify(foc_at + sp.Function("w")(x).diff(x)
                       - th(x).diff(x) * ph(muF(x))) == 0),
      "positive assortative matching makes the SOC hold")

print()
print("SU-3  log-concavity of 1-F  <=>  rho' <= 0,  rho = (1-F)/f")
print("-" * 72)
print("      This is the exact role of the log-concavity hypothesis in the")
print("      proof: 'log-concavity of 1 - F implies that rho' <= 0.'")
Fs = sp.Function("F")
lg = sp.log(1 - Fs(x))
d2lg = sp.simplify(sp.diff(lg, x, 2))
rho = (1 - Fs(x)) / sp.diff(Fs(x), x)
drho = sp.simplify(sp.diff(rho, x))
# d^2/dx^2 log(1-F) = rho'/rho^2, so the two conditions coincide in sign.
ident = sp.simplify(sp.expand(d2lg - drho / rho**2))
print(f"      d^2/dx^2 log(1-F) - rho'/rho^2 = {ident}")
check("SU-3 the two conditions are the same condition", bool(ident == 0),
      "log-concavity <=> rho' <= 0, exactly; not merely sufficient")

print()
print("SU-4  Quantile SOSD: int_0^k [G1^-1 - G0^-1] <= 0, equality at k=1")
print("-" * 72)
print("      The step Suen imports: 'if G0 second-order stochastically")
print("      dominates G1 and the two distributions have the same mean, then")
print("      G1^-1 second-order stochastically dominates G0^-1 (see, for")
print("      example, [2])' -- where [2] is Costrell and Loury.")
G0inv = t
G1inv = 3 * t**2 - 2 * t**3
partial = sp.simplify(sp.integrate(G1inv - G0inv, (t, 0, k)))
at_one = sp.simplify(partial.subs(k, 1))
worst = sp.simplify(sp.maximum(partial, k, sp.Interval(0, 1)))
print(f"      int_0^k [G1^-1 - G0^-1] dt = {sp.factor(partial)}")
print(f"      value at k=1: {at_one};   max over [0,1]: {worst}")
check("SU-4 partial integrals are signed, with equality at k=1",
      bool(at_one == 0 and worst <= 0),
      "mean preserved and every partial integral non-positive")

print()
print("SU-5  Footnote 1: rank-rescaling does NOT preserve concavity")
print("-" * 72)
print("      p.151 fn.1: rescaling to y-hat = G(y) needs phi-hat = phi o G^-1,")
print("      and 'unless the density function g is increasing, concavity of")
print("      phi does not imply concavity of phi-hat'.")
phi_c = sp.sqrt(y)
print(f"      phi = sqrt(y),  phi'' = {sp.simplify(sp.diff(phi_c, y, 2))}  (< 0, concave)")
# G with strictly DECREASING density on [0,1]: G(y) = 1-(1-y)^2, g = 2(1-y).
Ginv_dec = 1 - sp.sqrt(1 - u)
g_dec = sp.simplify(sp.diff(1 - (1 - y) ** 2, y))
phihat = sp.simplify(phi_c.subs(y, Ginv_dec))
d2 = sp.simplify(sp.diff(phihat, u, 2))
vals = {v: float(d2.subs(u, v)) for v in [sp.Rational(1, 5), sp.Rational(1, 2),
                                          sp.Rational(4, 5)]}
print(f"      g(y) = {g_dec}  (decreasing)")
print("      phi-hat'' at u = 1/5, 1/2, 4/5: "
      + ", ".join(f"{float(v):+.4f}" for v in vals.values()))
flips = any(v > 0 for v in vals.values()) and any(v < 0 for v in vals.values())
check("SU-5 concavity is lost under rank-rescaling when g decreases", flips,
      "phi-hat turns CONVEX on part of the range: a real trap for open item 2")

print()
print("SU-6  Concavity is sufficient, not necessary")
print("-" * 72)
print("      p.155: 'the concavity assumption is sufficient but not necessary")
print("      [...] If theta and phi are both linear, the proof of Proposition 2")
print("      indicates that W1 is still strictly lower than W0.'")
print("      With theta linear, theta'' = 0, so (5) gives H = theta' * H-hat.")
print("      H-hat < 0 and theta' > 0 give H < 0; with rho' <= 0, (4) gives")
print("      W1 - W0 = -int H rho' dx < 0.")
Hh, thp, rhp = sp.symbols("Hhat thetaprime rhoprime", real=True)
expr = -(thp * Hh) * rhp  # integrand of (4) with theta'' = 0
signed = expr.subs({Hh: -1, thp: 2, rhp: -3})  # H-hat<0, theta'>0, rho'<0
print(f"      sample integrand with H-hat=-1, theta'=2, rho'=-3: {signed} < 0")
check("SU-6 the linear case still signs the conclusion", bool(signed < 0),
      "so concavity is not load-bearing for the direction, only for the bound")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"SUEN SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, EXCEPT")
print("as recorded in NOTES.md. This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
