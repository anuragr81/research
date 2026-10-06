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
check("RD-6 on F=x^2, G=x: phi' crosses +- and f-g crosses -+", opposite,
      "arithmetic only; which of the two plays u' is decided in RD-8")

print()
print("RD-7  Their aggregate-effort object uses the hazard rate")
print("-" * 72)
print("      p.1589 and p.1602 (eq 11): with quadratic cost, aggregate effort is E(h(X_(k-1:k))),")
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
print("RD-8  Which function plays u' in the P-MU difference")
print("-" * 72)
print("      RD p.1597: gamma_theta = -Int u' H_theta dz = Int u' * (-H_theta) dz,")
print("      with the kernel -H_theta >= 0 log supermodular and u' required +-.")
print("      PROOFS.tex prop:PMU: D(Q) = Delta(0,Q+1) - Delta(0,Q)")
print("        = -Int G^(Q-1)(1-G) F (f-g) dx = Int [G^(Q-1)(1-G)] * [F (g-f)] dx.")
print("      So the function in the role of u' is F(g-f) = F phi' = -F(f-g),")
print("      not F(f-g). Witness pair with a sign change in Q:")
print("      F = x^4, G = 1-(1-x)^5 on [0,1] (F <= G, phi = G-F hump-shaped).")
xx = sp.symbols("xx", real=True)
Fw = xx**4
Gw = 1 - (1 - xx) ** 5
phiw = sp.expand(Gw - Fw)
dphiw = sp.diff(phiw, xx)
u_role = sp.expand(-Fw * (sp.diff(Fw, xx) - sp.diff(Gw, xx)))
roots = [complex(r) for r in sp.Poly(dphiw, xx).nroots()]
interior = sorted(r.real for r in roots if abs(r.imag) < 1e-12 and 0 < r.real < 1)
grid = [sp.Rational(i, 400) for i in range(1, 400)]
phi_nonneg = all(phiw.subs(xx, t) >= 0 for t in grid)
r0 = sp.nsimplify(interior[0], rational=True) if len(interior) == 1 else None
u_pm = (r0 is not None
        and u_role.subs(xx, r0 / 2) > 0
        and u_role.subs(xx, (r0 + 1) / 2) < 0)
D_vals = []
for Qv in range(1, 13):
    integrand_w = sp.expand(Gw ** (Qv - 1) * (1 - Gw) * Fw * (sp.diff(Fw, xx) - sp.diff(Gw, xx)))
    D_vals.append(-sp.integrate(integrand_w, (xx, 0, 1)))
signs = []
for v in D_vals:
    s = "+" if v > 0 else "-"
    if not signs or signs[-1] != s:
        signs.append(s)
pattern = "".join(signs)
print(f"      u-role = -F(f-g) equals F*phi': {sp.simplify(u_role - Fw * dphiw) == 0}")
print(f"      phi >= 0 on grid: {phi_nonneg}; interior critical points of phi: "
      f"{[round(t, 4) for t in interior]}")
print(f"      u-role crosses +- about that point: {u_pm}")
print(f"      D(1..4) = {[str(v) for v in D_vals[:4]]}")
print(f"      sign pattern of D(Q), Q=1..12: {pattern}   (RD's orientation is +-)")
check("RD-8 the u'-role function crosses +- and D(Q) single-crosses +- in Q",
      bool(sp.simplify(u_role - Fw * dphiw) == 0 and phi_nonneg and len(interior) == 1
           and u_pm and pattern == "+-"),
      "Delta(0,Q) rises then falls here: an interior MAXIMUM, not a minimum")

print()
print("RD-L  Lean: kernel TP2, peak, single-crossing, discrete Karlin step")
print("-" * 72)
ALLOWED_AXIOMS = {"propext", "Quot.sound"}
lean = shutil.which("lean")
if lean is None and os.path.exists(os.path.expanduser("~/.elan/bin/lean")):
    lean = os.path.expanduser("~/.elan/bin/lean")
src = os.path.join(HERE, "RyvkinDrugov.lean")
if lean is None:
    check("RD-L RyvkinDrugov.lean compiles", False, "lean not found on PATH or in ~/.elan/bin")
elif not os.path.exists(src):
    check("RD-L RyvkinDrugov.lean compiles", False, "RyvkinDrugov.lean missing")
else:
    src_text = open(src).read()
    hygiene = []
    if re.search(r"\bsorry\b", src_text):
        hygiene.append("sorry")
    if re.search(r"^\s*(?:private\s+)?axiom\b", src_text, re.M):
        hygiene.append("axiom declaration")
    if "native_decide" in src_text:
        hygiene.append("native_decide")
    if "--" in src_text or "/-" in src_text:
        hygiene.append("comment")
    check("RD-L source has no sorry, axiom, native_decide or comment", not hygiene,
          ", ".join(hygiene))
    p = subprocess.run([lean, src], capture_output=True, text=True, timeout=900)
    out = p.stdout + p.stderr
    check("RD-L RyvkinDrugov.lean compiles with no errors and no sorry",
          p.returncode == 0 and "sorry" not in out,
          f"returncode {p.returncode}; " + out.strip()[:300])
    names = re.findall(r"^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", src_text, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(src_text)
            fh.write("\n")
            for nm in names:
                fh.write(f"#print axioms RyvkinDrugov.{nm}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True, timeout=900)
        aout = q.stdout + q.stderr
    axioms_of = {}
    for nm in names:
        m = re.search(r"'RyvkinDrugov\." + re.escape(nm)
                      + r"' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])",
                      aout)
        if m:
            axioms_of[nm] = ([] if m.group(2) is None
                             else [a.strip() for a in m.group(2).split(",") if a.strip()])
    audited = len(axioms_of)
    free = sum(1 for v in axioms_of.values() if not v)
    core_only = sum(1 for v in axioms_of.values() if v and set(v) <= ALLOWED_AXIOMS)
    other = {nm: v for nm, v in axioms_of.items() if not set(v) <= ALLOWED_AXIOMS}
    n_sorry = len(re.findall(r"declaration uses .sorry.", out))
    print(f"      theorems: {len(names)}; audited: {audited}; axiom-free: {free}; "
          f"propext/Quot.sound only: {core_only}; other axioms: {len(other)}; "
          f"sorry: {n_sorry}")
    for nm, v in other.items():
        print(f"      NON-CORE AXIOMS in {nm}: {v}")
    check("RD-L axiom audit covers every declared theorem",
          q.returncode == 0 and audited == len(names) and len(names) > 0,
          f"{audited} of {len(names)}")
    check("RD-L no theorem depends on an axiom beyond propext / Quot.sound", not other,
          "allowed: none, propext, Quot.sound")
    mutations = [
        ("peak of z^(k-1)(1-z) moved off (k-1)/k",
         "theorem peak_k3 : PeakAt 36 3 24", "theorem peak_k3 : PeakAt 36 3 23"),
        ("logistic b_3 = b_4 at a = 1/6 instead of 1/(k^2-k-1) = 1/5",
         "bLogNum 1 3 * bLogDen 1 5 4 = bLogNum 1 4 * bLogDen 1 5 3",
         "bLogNum 1 3 * bLogDen 1 6 4 = bLogNum 1 4 * bLogDen 1 6 3"),
        ("the correctly signed P-MU difference claimed NOT to cross +-",
         "theorem plus_sum_not_scpm : ¬ SCpmFrom 1 (fun Q => sumTo 2 (fun i => Kint 4 Gex i Q * wex i))",
         "theorem plus_sum_not_scpm : ¬ SCpmFrom 1 (Dpmu 2 4 Gex wex)"),
        ("Gumbel b_k claimed increasing",
         "fracLt (bGumbelNum (k + 1)) (bGumbelDen (k + 1)) (bGumbelNum k) (bGumbelDen k)",
         "fracLt (bGumbelNum k) (bGumbelDen k) (bGumbelNum (k + 1)) (bGumbelDen (k + 1))"),
    ]
    rejected = 0
    with tempfile.TemporaryDirectory() as td:
        for desc, before, after in mutations:
            if before not in src_text:
                print(f"      mutation target missing: {desc}")
                continue
            mpath = os.path.join(td, "mutant.lean")
            with open(mpath, "w") as fh:
                fh.write(src_text.replace(before, after, 1))
            r = subprocess.run([lean, mpath], capture_output=True, text=True, timeout=900)
            ok_rej = r.returncode != 0
            rejected += ok_rej
            print(f"      mutant [{'rejected' if ok_rej else 'ACCEPTED'}]: {desc}")
    check("RD-L every mutated statement is rejected by Lean",
          rejected == len(mutations),
          f"{rejected} of {len(mutations)} mutants rejected")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"RYVKIN-DRUGOV SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, and the")
print("P-MU correspondence is now computed rather than conjectured.")
print("This does not reprove any result of the paper. RD-8 and the Lean block")
print("record that the u'-role function in P-MU crosses +-, RD's orientation.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
