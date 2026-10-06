import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


x, u, z = sp.symbols("x u z", positive=True)
a = sp.symbols("a", positive=True)
k, Q, n = sp.symbols("k Q n", positive=True)

print("=" * 72)
print("Ryvkin & Drugov (2020), 'The shape of luck and competition in")
print("winner-take-all tournaments', Theoretical Economics 15, 1587-1626.")
print("Checks of OUR READING, and of the P-MU correspondence the handover")
print("asks for ('a computation, not a read').")
print("=" * 72)
print()

print("RD-1  b_k = E[f(X_(k-1:k-1))],  their eq (3) and (9)")
print("-" * 72)
print("      (3): b_k = (k-1) Int F(x)^(k-2) f(x) dF(x)")
print("      (9): b_k = Int f(x) dF(x)^(k-1) = Int f(x) f_(k-1:k-1)(x) dx")
F = sp.Function("F")
pdf_max = sp.diff(F(x) ** (k - 1), x)  # pdf of the max of k-1 iid draws
expanded = sp.simplify(pdf_max - (k - 1) * F(x) ** (k - 2) * sp.diff(F(x), x))
print(f"      d/dx F^(k-1) - (k-1)F^(k-2) f = {expanded}")
check("RD-1 the two forms of b_k agree; b_k is E[f(max of k-1 draws)]",
      bool(expanded == 0),
      "confirms the survey's 'b_k = E[f(X_(k-1:k-1))]'")

print()
print("RD-2  Their worked example, derived independently")
print("-" * 72)
print("      Type I generalized logistic: F(x) = 1/(1+exp(-x))^a.")
print("      Changing variable to u = F gives f = a*u*(1 - u^(1/a)).")
f_u = a * u * (1 - u ** (1 / a))
bk = sp.simplify((k - 1) * sp.integrate(u ** (k - 2) * f_u, (u, 0, 1)))
stated = a * (k - 1) / (k * (a * k + 1))
diff_stated = sp.simplify(bk - stated)
d_bk = sp.simplify(bk.subs(k, k + 1) - bk)
ratio = sp.simplify(sp.cancel(d_bk / (1 + a - a * k * (k - 1))))
print(f"      derived b_k = {bk}")
print(f"      RD state    = a(k-1)/[k(ak+1)];  difference = {diff_stated}")
print(f"      b_(k+1) - b_k over (1+a-ak(k-1)) = {ratio}   (positive)")
check("RD-2 the example reproduces exactly, including the difference sign",
      bool(diff_stated == 0 and sp.simplify(ratio.subs({a: 1, k: 3})) > 0),
      "b_(k+1)-b_k proportional to 1+a-ak(k-1), as stated")

print()
print("RD-3  Their log-supermodular kernel:  -H_theta = F^(k-1) - F^k")
print("-" * 72)
print("      p.1597: 'the role of H(z,theta) is played by F(x)^(k-1) [...]")
print("      It is easy to see that -H_theta = F^(k-1) - F^k is log")
print("      supermodular.'")
kern = z ** (k - 1) - z**k
kern_fact = sp.factor(kern)
peak = sp.solve(sp.Eq(sp.diff(kern, z), 0), z)
peak = [sp.simplify(s) for s in peak if s != 0]
print(f"      -H_theta = {kern_fact};  interior peak at z = {peak}")
check("RD-3 the kernel is z^(k-1)(1-z), peaking at (k-1)/k",
      bool(sp.simplify(kern_fact - z ** (k - 1) * (1 - z)) == 0
           and any(sp.simplify(s - (k - 1) / k) == 0 for s in peak)),
      "hump-shaped in the CDF value, with an interior maximum")

print()
print("RD-4  THE CORRESPONDENCE with P-MU's weight")
print("-" * 72)
print("      Our P-MU (PROOFS.tex): Delta(0,Q+1) - Delta(0,Q) =")
print("        -Int W (dF - dG),   W = G^(Q-1)(1-G) F.")
print("      S11 established W is hump-shaped in G with max at (Q-1)/Q.")
print("      RD's kernel is F^(k-1)(1-F), max at (k-1)/k.")
ourk = z ** (Q - 1) * (1 - z)
our_peak = [sp.simplify(s) for s in sp.solve(sp.Eq(sp.diff(ourk, z), 0), z) if s != 0]
print(f"      our kernel G^(Q-1)(1-G): peak at G = {our_peak}")
print("      Same functional form under G <-> F and Q <-> k.  Our extra")
print("      factor F is the incumbent's CDF and does not involve Q.")
check("RD-4 P-MU's weight contains exactly RD's kernel",
      bool(any(sp.simplify(s - (Q - 1) / Q) == 0 for s in our_peak)
           and sp.simplify(ourk.subs(Q, k) - kern) == 0),
      "S11's hump at (Q-1)/Q IS RD's log-supermodular kernel")

print()
print("RD-5  Log-supermodularity carries over to our kernel")
print("-" * 72)
print("      Log supermodularity of phi(z,n) means d^2 log phi / dz dn >= 0.")
logk = (n - 1) * sp.log(z) + sp.log(1 - z)
cross = sp.simplify(sp.diff(logk, z, 1, n, 1))
print(f"      log[z^(n-1)(1-z)] cross-partial = {cross}  (> 0 on 0 < z < 1)")
print("      Our extra factor F does not involve Q, so it adds nothing to the")
print("      cross-partial: log-supermodularity in (G,Q) is inherited.")
check("RD-5 our weight is log-supermodular in (G, Q)",
      bool(sp.simplify(cross - 1 / z) == 0),
      "the FIRST of RD's two hypotheses transfers")

print()
print("RD-6  BUT the second hypothesis does NOT line up")
print("-" * 72)
print("      RD need u' single crossing +- , with u = f the (unimodal) noise")
print("      pdf.  Karlin's variation-diminishing argument then makes the")
print("      integral unimodal in k.")
print("      Our integrand plays the role of u': it is F*(f - g) = -F*phi',")
print("      where phi = G - F >= 0 vanishes at both endpoints (P2 and the")
print("      regularity in Sec. Primitives).")
Fs, Gs = x**2, x
phi = sp.simplify(Gs - Fs)
dphi = sp.simplify(sp.diff(phi, x))
integrand = sp.simplify(sp.diff(Fs, x) - sp.diff(Gs, x))  # f - g = -phi'
lo, hi = sp.Rational(1, 4), sp.Rational(3, 4)
print(f"      example F=x^2, G=x on [0,1]: phi = {phi}, phi' = {dphi}")
print(f"      phi'(1/4) = {dphi.subs(x, lo)} > 0,  phi'(3/4) = {dphi.subs(x, hi)} < 0"
      "   -> phi' crosses +-")
print(f"      f - g = {integrand}: at 1/4 = {integrand.subs(x, lo)} < 0, "
      f"at 3/4 = {integrand.subs(x, hi)} > 0   -> crosses -+")
opposite = bool(dphi.subs(x, lo) > 0 and dphi.subs(x, hi) < 0
                and integrand.subs(x, lo) < 0 and integrand.subs(x, hi) > 0)
check("RD-6 our integrand single-crosses -+, RD's needs +-", opposite,
      "the orientation is reversed, so Karlin's conclusion does NOT transfer as-is")

print()
print("RD-7  Their aggregate-effort object uses the hazard rate")
print("-" * 72)
print("      p.1593: with quadratic cost, aggregate effort is E(h(X_(k-1:k))),")
print("      where h is the failure (hazard) rate of noise and X_(k-1:k) is the")
print("      SECOND-highest of k draws -- a different order statistic from the")
print("      one in b_k.")
print("      So the survey's 'hazard-rate result' is the AGGREGATE-effort")
print("      statement, while b_k = E[f(X_(k-1:k-1))] is the INDIVIDUAL one.")
print("      P-MU concerns Delta(0), an individual gain, so b_k is the right")
print("      comparator -- as the survey says.")
Fv = sp.Function("F")(x)
fv = sp.diff(Fv, x)
pdf_max_km1 = sp.simplify((k - 1) * Fv ** (k - 2) * fv)          # max of k-1 draws
pdf_second_k = sp.simplify(k * (k - 1) * Fv ** (k - 2) * (1 - Fv) * fv)  # 2nd of k
gap = sp.simplify(pdf_second_k - pdf_max_km1)
ratio = sp.simplify(sp.cancel(pdf_second_k / pdf_max_km1))
print(f"      pdf of max of (k-1) draws   : {pdf_max_km1}")
print(f"      pdf of 2nd-highest of k     : {pdf_second_k}")
print(f"      ratio = {ratio}   (not 1, so the order statistics differ)")
check("RD-7 the two objects are distinct and the survey picks the right one",
      bool(sp.simplify(ratio - k * (1 - Fv)) == 0 and gap != 0),
      "individual: b_k = E[f(X_(k-1:k-1))]; aggregate: E[h(X_(k-1:k))]")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"RYVKIN-DRUGOV SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, and the")
print("P-MU correspondence is now computed rather than conjectured.")
print("This does not reprove any result of the paper, and RD-6 records why")
print("the correspondence does NOT yet yield a transferable theorem.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
