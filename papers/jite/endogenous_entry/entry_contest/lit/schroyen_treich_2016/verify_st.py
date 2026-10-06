import os
import re
import shutil
import subprocess
import tempfile

import sympy as sp

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = os.path.dirname(os.path.abspath(__file__))

w, a, b, gam, m, x = sp.symbols("w a b rho m x", positive=True)

A_of = lambda u: sp.simplify(-sp.diff(u, w, 2) / sp.diff(u, w))
P_of = lambda u: sp.simplify(-sp.diff(u, w, 3) / sp.diff(u, w, 2))

print("=" * 72)
print("Schroyen & Treich, 'The Power of Money: Wealth Effects in Contests'")
print("TSE WP 16-699 (cover dated September 2016) wrapping the GEB revision")
print("manuscript dated April 11, 2016.  Checks of OUR READING.")
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
print("ST-5  CRRA:  condition reduces to rho*(1/2 - m^2) > 1/2")
print("      (the paper writes the CRRA coefficient as rho; LITERATURE.tex writes gamma)")
print("-" * 72)
u_crra = w ** (1 - gam) / (1 - gam)
A_crra, P_crra = A_of(u_crra), P_of(u_crra)
print(f"      CRRA:  A = {A_crra},  P = {P_crra}")
lhs = sp.simplify(2 * A_crra * (1 - m**2) - P_crra)
tgt = sp.simplify(gam * (sp.Rational(1, 2) - m**2) - sp.Rational(1, 2))
ratio = sp.simplify(sp.cancel(lhs / tgt))
print(f"      [2A(1-m^2) - P] / [rho(1/2 - m^2) - 1/2] = {ratio}")
check("ST-5 CRRA reduces to rho(1/2 - m^2) > 1/2",
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
print("ST-AP  Derivative triples fed to the Lean file are the true derivatives")
print("-" * 72)
z, al, rho = sp.symbols("z alpha rho", positive=True)
d3 = lambda u: [sp.diff(u, z, i) for i in (1, 2, 3)]
cara_t = d3(-sp.exp(-al * z) / al)
sc = sp.exp(-al * z)
ok_cara = all(sp.simplify(t - e) == 0 for t, e in zip(cara_t, [sc, -al * sc, al**2 * sc]))
crra_t = d3(z ** (1 - rho) / (1 - rho))
ok_crra = all(sp.simplify(t * z ** (rho + 2) - e) == 0
              for t, e in zip(crra_t, [z**2, -rho * z, rho * (rho + 1)]))
log_t = [t.subs(z, 1) for t in d3(sp.log(z))]
quad_t = d3(z - b * z**2)
ok_log = log_t == [1, -1, 2]
ok_quad = quad_t[2] == 0
bad_t = [t.subs(z, 1) for t in d3(sp.sqrt(z))]
ctrl = bad_t != [1, -1, 2]
print(f"      CARA (s, -alpha s, alpha^2 s): {ok_cara};  CRRA z^(rho+2)*(u',u'',u''') = "
      f"(z^2, -rho z, rho(rho+1)): {ok_crra}")
print(f"      log at z=1 = (1,-1,2): {ok_log};  quadratic u''' = 0: {ok_quad};  "
      f"control sqrt at z=1 = {bad_t} rejected: {ctrl}")
check("ST-AP cara_AP, crra_AP, separator_derivs, quadratic_reduction hypotheses",
      bool(ok_cara and ok_crra and ok_log and ok_quad and ctrl))

print()
print("ST-2b  Appendix A.2: CSF derivatives at the symmetric equilibrium (p.31)")
print("-" * 72)
xa, xb, wa, r = sp.symbols("x_a x_b w_a r", positive=True)
kk, nn = sp.symbols("k n", positive=True)
pcsf = xa**m / (xa**m + xb**m)
SE = {xa: x, xb: x, wa: w}
pd = {k_: sp.simplify(sp.diff(pcsf, *a_).subs(SE)) for k_, a_ in dict(
    p1=(xa,), p11=(xa, 2), p111=(xa, 3), p112=(xa, 2, xb), p122=(xa, xb, 2), p12=(xa, xb)).items()}
paper = dict(p1=m / (4 * x), p11=-m / (4 * x**2),
             p111=m / (2 * x**3) - m**3 / (8 * x**3),
             p122=-m**3 / (8 * x**3), p112=m**3 / (8 * x**3), p12=0)
ok_a2 = all(sp.simplify(pd[k_] - paper[k_]) == 0 for k_ in paper)
ok_rem = sp.simplify(3 * pd["p112"] - pd["p111"] - m / (2 * x**3) * (m**2 - 1)) == 0
mk = kk / nn
c1, c2, c3 = 8 * nn**3 * x, 8 * nn**3 * x**2, 8 * nn**3 * x**3
scaled = [sp.simplify((c1 * pd["p1"]).subs(m, mk) - 2 * kk * nn**2),
          sp.simplify((c2 * pd["p11"]).subs(m, mk) + 2 * kk * nn**2),
          sp.simplify((c3 * pd["p111"]).subs(m, mk) - (4 * kk * nn**2 - kk**3)),
          sp.simplify((c3 * pd["p112"]).subs(m, mk) - kk**3),
          sp.simplify(pd["p122"] + pd["p112"]),
          sp.simplify(c1 * c3 - c2 * c2)]
ok_sc = all(s_ == 0 for s_ in scaled)
wrong = sp.simplify(pd["p111"] - m / (2 * x**3) - m**3 / (8 * x**3)) != 0
print(f"      p1, p11, p111, p112, p122, p12 match p.31: {ok_a2};  3p112 - p111 = "
      f"(m/(2x^3))(m^2-1): {ok_rem}")
print(f"      scaled hypotheses of thm3_from_A2 / thm3_end_to_end hold: {ok_sc};  "
      f"control p111 with +m^3/8 rejected: {wrong}")
check("ST-2b A.2 values and the Lean scaling S = 8n^3 x^j p", bool(ok_a2 and ok_rem and ok_sc and wrong))

print()
print("ST-2c  Proof of Theorem 3 (A.7, pp.43-44) recomputed from the privilege FOC")
print("-" * 72)
U1, U2, U3 = sp.symbols("U1 U2 U3", real=True)
z0 = w - x
up = lambda zz: U1 + U2 * (zz - z0) + U3 * (zz - z0) ** 2 / 2
h = -up(wa - xa) + sp.diff(pcsf, xa) * r
hx = sp.diff(h, xa)
f_b = -sp.diff(h, xb) / hx
f_w = -sp.diff(h, wa) / hx


def second(i, j, fi, fj):
    return -(sp.diff(h, i, j) + sp.diff(h, i, xa) * fj + sp.diff(h, xa, j) * fi
             + sp.diff(h, xa, 2) * fi * fj) / hx


ev = lambda e: sp.simplify(e.subs(SE))
h1 = ev(hx)
fb, fw = ev(f_b), ev(f_w)
fbb, fbw, fww = ev(second(xb, xb, f_b, f_b)), ev(second(xb, wa, f_b, f_w)), ev(second(wa, wa, f_w, f_w))
lean_hyps = [sp.simplify(fw * h1 - U2),
             sp.simplify(fbb * h1 + r * pd["p122"]),
             sp.simplify(fbw * h1**2 + r * U2 * pd["p112"]),
             sp.simplify(fww * h1**3 + r * (-(U3 * pd["p11"] ** 2 * r) + U2**2 * pd["p111"]))]
ok_hyp = all(e == 0 for e in lean_hyps) and fb == 0
paper_f11 = (-r / h1**3) * (-U3 * pd["p11"] ** 2 * r + U2**2 * pd["p111"])
paper_f22 = (-r / h1**3) * pd["p122"] * h1**2
swap = (sp.simplify(paper_f11 - fww) == 0 and sp.simplify(paper_f22 - fbb) == 0
        and sp.simplify(paper_f11 - fbb) != 0)
h11_true = ev(sp.diff(h, xa, 2))
h11_typo = sp.simplify(h11_true - (pd["p111"] * r - U3)) == 0 and \
    sp.simplify(h11_true - (-pd["p111"] * r - U3)) != 0
QF = fw**2 * fbb - 2 * (1 + fb) * fw * fbw + (1 + fb) ** 2 * fww
foc = sp.solve(sp.Eq(ev(h), 0), r)[0]
QFf = sp.simplify(QF.subs(r, foc))
cond = 2 * (-U2 / U1) * (1 - m**2) - (-U3 / U2)
rat = sp.factor(sp.simplify(QFf / cond))
sgn = sp.simplify(rat.subs({U2: -sp.Symbol("s2", positive=True), U1: sp.Symbol("s1", positive=True)}))
ok_thm3 = (not rat.has(U3)) and bool(sgn.is_positive)
ctrl3 = sp.simplify(QFf / (2 * (-U2 / U1) * (1 + m**2) - (-U3 / U2))).has(U3)
mid = (-U2 / U1) * (pd["p111"] - 3 * pd["p112"]) - (-U3 / U2) * pd["p11"] ** 2 / pd["p1"]
ok_mid = sp.simplify(mid / cond - m / (4 * x**3)) == 0
print(f"      Lean hypotheses hf2, hf11, hf12, hf22 (true labels) hold, f1 = 0: {ok_hyp}")
print(f"      paper's p.43 'f11' is d2f/dwa2 and 'f22' is d2f/dxb2 (labels swapped "
      f"against (A.9)): {swap}")
print(f"      true h11 = p111 r - u''' (p.43 prints -p111 r - u'''): {h11_typo}")
print(f"      (A.9) numerator / [2A(1-m^2) - P] = {rat}  (free of u''', positive)")
print(f"      A(p111-3p112) - P p11^2/p1 = (m/(4x^3)) [2A(1-m^2) - P]: {ok_mid};  "
      f"control 2A(1+m^2) - P not proportional: {ctrl3}")
check("ST-2c Theorem 3 follows from (A.9) at the symmetric privilege equilibrium",
      bool(ok_hyp and ok_thm3 and ok_mid and ctrl3))
check("ST-2c source typos located (p.43 f11/f22 labels swapped, h11 sign)", bool(swap and h11_typo))

print()
print("ST-2d  (A.10): willingness to pay Pi/u' has slope (Pi/u')A, curvature (Pi/u')A(2A-P)")
print("-" * 72)
uu = sp.Function("u")
Pi = sp.Symbol("Pi", positive=True)
wtp = Pi / sp.diff(uu(w), w)
Aw = -sp.diff(uu(w), w, 2) / sp.diff(uu(w), w)
Pw = -sp.diff(uu(w), w, 3) / sp.diff(uu(w), w, 2)
ok_d1 = sp.simplify(sp.diff(wtp, w) - wtp * Aw) == 0
ok_d2 = sp.simplify(sp.diff(wtp, w, 2) - wtp * Aw * (2 * Aw - Pw)) == 0
ctrl_d2 = sp.simplify(sp.diff(wtp, w, 2) - wtp * Aw * (2 * Aw + Pw)) != 0
print(f"      slope: {ok_d1};  curvature: {ok_d2};  control A(2A+P) rejected: {ctrl_d2}")
check("ST-2d (A.10) is the hypothesis of wtp_concave_mps_negative", bool(ok_d1 and ok_d2 and ctrl_d2))

print()
print("ST-10  Rent-seeking FOC: CARA makes H'_RS = -alpha H_RS, so H'_RS = 0 at H_RS = 0")
print("-" * 72)
u_c = -sp.exp(-al * z) / al
okV = sp.simplify(sp.diff(u_c, z) + al * u_c) == 0
okW = sp.simplify(sp.diff(u_c, z, 2) + al * sp.diff(u_c, z)) == 0
K = sp.Symbol("K", positive=True)
zl, zh = sp.symbols("z_l z_h", positive=True)
H = K * (u_c.subs(z, zh) - u_c.subs(z, zl)) - (sp.diff(u_c, z).subs(z, zh) + sp.diff(u_c, z).subs(z, zl)) / 2
dH = K * (sp.diff(u_c, z).subs(z, zh) - sp.diff(u_c, z).subs(z, zl)) \
    - (sp.diff(u_c, z, 2).subs(z, zh) + sp.diff(u_c, z, 2).subs(z, zl)) / 2
ok_id = sp.simplify(dH + al * H) == 0
u2r = -1 / z
vals = [(u2r * 4).subs(z, 1), (u2r * 4).subs(z, 2),
        (sp.diff(u2r, z) * 4).subs(z, 1), (sp.diff(u2r, z) * 4).subs(z, 2),
        (sp.diff(u2r, z, 2) * 4).subs(z, 1), (sp.diff(u2r, z, 2) * 4).subs(z, 2)]
Kc = sp.Rational(5, 4)
Hc = Kc * (u2r.subs(z, 2) - u2r.subs(z, 1)) - (sp.diff(u2r, z).subs(z, 2) + sp.diff(u2r, z).subs(z, 1)) / 2
dHc = Kc * (sp.diff(u2r, z).subs(z, 2) - sp.diff(u2r, z).subs(z, 1)) \
    - (sp.diff(u2r, z, 2).subs(z, 2) + sp.diff(u2r, z, 2).subs(z, 1)) / 2
ok_ctrl = vals == [-4, -2, 4, 1, -8, -1] and Hc == 0 and dHc != 0
print(f"      u' = -alpha u: {okV};  u'' = -alpha u': {okW};  H'_RS + alpha H_RS = 0: {ok_id}")
print(f"      control CRRA rho=2, z = 1, 2, K = 5/4: 4*(u,u',u'') = {vals}, H_RS = {Hc}, "
      f"H'_RS = {dHc}")
check("ST-10 CARA cancellation identity and the CRRA control values", bool(okV and okW and ok_id and ok_ctrl))

print()
print("ST-L  Lean: SchroyenTreich.lean (core Lean 4, no Mathlib)")
print("-" * 72)
NAME_RE = re.compile(
    r"^\s*(?:@\[[^\]]*\]\s*)?(?:private\s+|protected\s+)?(?:theorem|lemma)\s+([A-Za-z0-9_']+)", re.M)
AX_RE = re.compile(
    r"'([A-Za-z0-9_'.]+)' (?:depends on axioms: \[([^\]]*)\]|does not depend on any axioms)")
ALLOWED = {"propext", "Quot.sound"}


def banned_tokens(text):
    out = [t for t in ("sorry", "native_decide") if re.search(rf"\b{t}\b", text)]
    if re.search(r"^\s*axiom\s", text, re.M):
        out.append("axiom")
    return out


def audit(lean, text, ns):
    names = NAME_RE.findall(text)
    with tempfile.TemporaryDirectory() as td:
        path = os.path.join(td, "audit.lean")
        with open(path, "w") as fh:
            fh.write(text + "\n")
            for n in names:
                fh.write(f"#print axioms {ns}.{n}\n")
        q = subprocess.run([lean, path], capture_output=True, text=True)
    found = {}
    for mm in AX_RE.finditer(q.stdout):
        full = mm.group(1)
        if full.startswith(ns + "."):
            found[full[len(ns) + 1:]] = [s.strip() for s in mm.group(2).split(",")] if mm.group(2) else []
    return names, found, q.returncode


lean = shutil.which("lean")
if lean is None:
    check("ST-L lean on PATH", False, "lean not on PATH")
else:
    src = os.path.join(HERE, "SchroyenTreich.lean")
    text = open(src).read()
    p = subprocess.run([lean, src], capture_output=True, text=True)
    out = (p.stdout + p.stderr).strip()
    check("ST-L SchroyenTreich.lean compiles with no errors", p.returncode == 0, out[:300])
    bt = banned_tokens(text)
    check("ST-L no sorry, no native_decide, no user axiom",
          not bt and "declaration uses 'sorry'" not in out, ", ".join(bt))
    names, found, rc = audit(lean, text, "SchroyenTreich")
    audited = [n for n in names if n in found]
    bad = {n: [x_ for x_ in found[n] if x_ not in ALLOWED] for n in audited}
    bad = {n: v for n, v in bad.items() if v}
    free = sum(1 for n in audited if not found[n])
    digit_names = [n for n in names if re.search(r"\d", n)]
    print(f"      theorems: {len(names)}; audited: {len(audited)}; axiom-free: {free}; "
          f"propext/Quot.sound only: {len(audited) - free - len(bad)}; disallowed: {len(bad)}; "
          f"sorry: 0")
    print(f"      digit-containing names audited: {len([n for n in digit_names if n in found])} "
          f"of {len(digit_names)}")
    check("ST-L axiom audit covers every declared theorem",
          rc == 0 and len(names) > 0 and len(audited) == len(names) and len(found) == len(names))
    check("ST-L every theorem uses no axioms or only propext/Quot.sound", not bad,
          "; ".join(f"{n}: {v}" for n, v in bad.items()))
    ctrl_src = ("namespace AuditControl\n"
                "theorem em_ctrl (q : Prop) : q ∨ ¬ q := Classical.em q\n"
                "theorem ok_ctrl_2 : (1 : Nat) + 1 = 2 := rfl\n"
                "end AuditControl\n")
    cn, cf, _ = audit(lean, ctrl_src, "AuditControl")
    ctrl_ok = (cn == ["em_ctrl", "ok_ctrl_2"] and "Classical.choice" in cf.get("em_ctrl", [])
               and cf.get("ok_ctrl_2") == []
               and banned_tokens("theorem t : False := by sorry") == ["sorry"])
    check("ST-L audit control: Classical.choice and sorry are detected", ctrl_ok,
          "the audit can fail")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"SCHROYEN-TREICH SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent, and the")
print("Theorem 3 structure, separator and CARA cancellation are machine-checked in Lean.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
