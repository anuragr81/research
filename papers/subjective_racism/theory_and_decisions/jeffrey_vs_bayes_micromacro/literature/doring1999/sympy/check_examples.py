"""
Doring (1999), "Why Bayesian Psychology Is Incomplete", Phil. Sci. 66
(Proceedings), S379-S389 -- every numeric example, in exact rationals.

Checks:
  (1) S382 displayed computation p'(AB) = 0.99 * 0.05/0.55 = 0.09;
  (2) Figure 1 (S383): all four tables of both sequences, exactly, and their
      rounding to the printed percentages;
  (3) S383 text: A-notB is "one fifth" of notA-notB (exactly 19/99), and
      P(A|notB) = "1/6" vs "5/6" (exactly 19/118 vs 99/118);
  (4) the A-marginal after each sequence, and the S383 third step (P(B) -> .01);
  (5) "by playing with the numbers ... as close to 1 as you please" (S383):
      the closed form 1 - 4n/(n^2+2n-1) of the Lean family `fam`, and its limit;
  (6) Figure 2 (S385): the one-step merged Jeffrey update gives 49/49/1/1;
  (7) Dempster's orthogonal sum (S385) and the S386 claim "to within 1/1000 of
      a percentage point" (exact gap 1/10100, about 1/100 of a point);
  (8) Figure 3 (S388): both dampened sequences, exactly, and their rounding
      (the printed 42.2 should round to 42.3: exact value 391/925 = 42.27%);
  (9) Field's odds-factor rule (S386) commutes on Doring's two cues.
Exit status 0 iff every check passes.
"""
import sympy as sp

R = sp.Rational
CELLS = ("AB", "nAB", "AnB", "nAnB")
ok = True


def check(name, cond):
    global ok
    print(("PASS  " if cond else "FAIL  ") + name)
    ok = ok and bool(cond)


def jeff_single(p, cell, t):
    """Jeffrey on the two-cell partition {cell, rest}: cell gets t."""
    rest = 1 - p[cell]
    return {k: (t if k == cell else v * (1 - t) / rest) for k, v in p.items()}


def jeff_partition(p, blocks, vals):
    """Jeffrey on an arbitrary partition given as a list of cell tuples."""
    out = {}
    for blk, v in zip(blocks, vals):
        m = sum(p[c] for c in blk)
        for c in blk:
            out[c] = v * p[c] / m
    return out


def pct(x, nd=0):
    return round(float(100 * x), nd)


prior = {"AB": R(1, 20), "nAB": R(1, 20), "AnB": R(9, 20), "nAnB": R(9, 20)}
eps = R(1, 100)

# (1)
check("S382: p'(AB) = 0.99*0.05/0.55 = 0.09", R(99, 100) * R(5, 100) / R(55, 100) == R(9, 100))

# (2) Figure 1
s1a = jeff_single(prior, "nAnB", eps)              # A v B -> .99
s1b = jeff_single(s1a, "AnB", eps)                 # notA v B -> .99
s2a = jeff_single(prior, "AnB", eps)
s2b = jeff_single(s2a, "nAnB", eps)
check("Fig.1 row1 middle = 9,9,81,1 (exact)", [s1a[c] for c in CELLS] == [R(9, 100), R(9, 100), R(81, 100), R(1, 100)])
check("Fig.1 row1 right  = 891/1900,891/1900,1/100,99/1900",
      [s1b[c] for c in CELLS] == [R(891, 1900), R(891, 1900), R(1, 100), R(99, 1900)])
check("Fig.1 row2 middle = 9,9,1,81 (exact)", [s2a[c] for c in CELLS] == [R(9, 100), R(9, 100), R(1, 100), R(81, 100)])
check("Fig.1 row2 right  = mirror image",
      [s2b[c] for c in CELLS] == [R(891, 1900), R(891, 1900), R(99, 1900), R(1, 100)])
check("Fig.1 printed percentages 47,47,1,5 / 47,47,5,1",
      [pct(s1b[c]) for c in CELLS] == [47, 47, 1, 5] and [pct(s2b[c]) for c in CELLS] == [47, 47, 5, 1])

# (3)
ratio = s1b["AnB"] / s1b["nAnB"]
c1 = s1b["AnB"] / (s1b["AnB"] + s1b["nAnB"])
c2 = s2b["AnB"] / (s2b["AnB"] + s2b["nAnB"])
print(f"      A-notB / notA-notB = {ratio} = {float(ratio):.4f} ('one fifth')")
print(f"      P(A|notB): seq1 = {c1} = {float(c1):.4f}, seq2 = {c2} = {float(c2):.4f} ('1/6', '5/6')")
check("ratio exactly 19/99", ratio == R(19, 99))
check("P(A|notB) exactly 19/118 and 99/118", c1 == R(19, 118) and c2 == R(99, 118))
check("'1/6' and '5/6' are within 1/100 of the exact values", abs(c1 - R(1, 6)) < R(1, 100) and abs(c2 - R(5, 6)) < R(1, 100))

# (4)
mA1 = s1b["AB"] + s1b["AnB"]
mA2 = s2b["AB"] + s2b["AnB"]
check("P(A) after the two orders: 91/190 vs 99/190", mA1 == R(91, 190) and mA2 == R(99, 190))
t1 = jeff_partition(s1b, [("AB", "nAB"), ("AnB", "nAnB")], [eps, 1 - eps])
t2 = jeff_partition(s2b, [("AB", "nAB"), ("AnB", "nAnB")], [eps, 1 - eps])
a1, a2 = t1["AB"] + t1["AnB"], t2["AB"] + t2["AnB"]
print(f"      third step (P(B) -> .01): P(A) = {a1} = {float(a1):.4f} vs {a2} = {float(a2):.4f}")
check("S383 third step with the paper's numbers: 97/590 vs 493/590", a1 == R(97, 590) and a2 == R(493, 590))

# (5) the family of the Lean file
n = sp.symbols("n", positive=True)
fam = {"AB": 1 / (2 * (2 * n + 1)), "nAB": 1 / (2 * (2 * n + 1)), "AnB": n / (2 * n + 1), "nAnB": n / (2 * n + 1)}
check("fam(9/2) is the paper's prior", all(sp.simplify(fam[c].subs(n, R(9, 2)) - prior[c]) == 0 for c in CELLS))
f1 = jeff_single(jeff_single(fam, "nAnB", 1 / n), "AnB", 1 / n)
f2 = jeff_single(jeff_single(fam, "AnB", 1 / n), "nAnB", 1 / n)
g = sp.simplify(f2["AnB"] / (f2["AnB"] + f2["nAnB"]) - f1["AnB"] / (f1["AnB"] + f1["nAnB"]))
check("family gap = 1 - 4n/(n^2+2n-1)", sp.simplify(g - (1 - 4 * n / (n ** 2 + 2 * n - 1))) == 0)
check("family gap -> 1 as n -> oo", sp.limit(g, n, sp.oo) == 1)

# (6) Figure 2
merged = jeff_partition(prior, [("AB", "nAB"), ("AnB",), ("nAnB",)], [R(98, 100), R(1, 100), R(1, 100)])
check("Fig.2 = 49,49,1,1 exactly", [merged[c] for c in CELLS] == [R(49, 100), R(49, 100), R(1, 100), R(1, 100)])

# (7) Dempster
m_single = R(1, 100) * R(99, 100) / (1 - R(1, 100) ** 2)
m_B = R(99, 100) ** 2 / (1 - R(1, 100) ** 2)
check("orthogonal sum: m(notA notB) = m(A notB) = 1/101, m(B) = 99/101",
      m_single == R(1, 101) and m_B == R(99, 101) and 2 * m_single + m_B == 1)
d = jeff_partition(prior, [("AB", "nAB"), ("AnB",), ("nAnB",)], [m_B, m_single, m_single])
gaps = [abs(d[c] - merged[c]) for c in CELLS]
print(f"      Dempster result {[d[c] for c in CELLS]}; gaps {gaps}")
check("Dempster-Jeffrey result = 99/202,99/202,1/101,1/101", [d[c] for c in CELLS] == [R(99, 202), R(99, 202), R(1, 101), R(1, 101)])
check("gap is exactly 1/10100 in every cell", all(gp == R(1, 10100) for gp in gaps))
check("S386 'within 1/1000 of a percentage point' (1e-5) is NOT met; within 1/100 (1e-4) is",
      all(gp > R(1, 100000) for gp in gaps) and all(gp < R(1, 10000) for gp in gaps))

# (8) Figure 3


def damp_seq(w):
    a = jeff_single(prior, "nAnB", w * prior["nAnB"] + (1 - w) * eps)
    b = jeff_single(a, "AnB", w * a["AnB"] + (1 - w) * eps)
    return a, b


a, b = damp_seq(R(1, 10))
check("Fig.3 row1 middle = 43/500,43/500,387/500,27/500 (8.6,8.6,77.4,5.4)",
      [a[c] for c in CELLS] == [R(43, 500), R(43, 500), R(387, 500), R(27, 500)])
check("Fig.3 row1 right = 24553/70625,...,54/625,15417/70625",
      [b[c] for c in CELLS] == [R(24553, 70625), R(24553, 70625), R(54, 625), R(15417, 70625)])
check("Fig.3 row1 printed 34.8,34.8,8.6,21.8", [pct(b[c], 1) for c in CELLS] == [34.8, 34.8, 8.6, 21.8])
a, b = damp_seq(R(1, 2))
check("Fig.3 row2 middle = 7,7,63,23 (exact)", [a[c] for c in CELLS] == [R(7, 100), R(7, 100), R(63, 100), R(23, 100)])
check("Fig.3 row2 right = 119/925,119/925,8/25,391/925",
      [b[c] for c in CELLS] == [R(119, 925), R(119, 925), R(8, 25), R(391, 925)])
check("Fig.3 row2 rounds to 12.9,12.9,32,42.3", [pct(b[c], 1) for c in CELLS] == [12.9, 12.9, 32.0, 42.3])
print(f"      notA-notB = {b['nAnB']} = {float(100 * b['nAnB']):.3f}%: printed 42.2, correct rounding 42.3")
check("printed 42.2 is off by less than 0.1 point (rounding slip in Fig.3)",
      abs(100 * b["nAnB"] - R(422, 10)) < R(1, 10))

# (9) Field's rule commutes on Doring's cues (symbolic factors, symbolic prior)
p = dict(zip(CELLS, sp.symbols("pAB pnAB pAnB pnAnB", positive=True)))
r1, r2 = sp.symbols("r1 r2", positive=True)


def field(p, cell, r):
    w = {k: (r * v if k == cell else v) for k, v in p.items()}
    Z = sum(w.values())
    return {k: v / Z for k, v in w.items()}


fa = field(field(p, "nAnB", r1), "AnB", r2)
fb = field(field(p, "AnB", r2), "nAnB", r1)
check("Field's rule (S386) commutes on the two cues", all(sp.simplify(fa[c] - fb[c]) == 0 for c in CELLS))
q = field(p, "nAnB", r1)
odds_new = q["nAnB"] / (1 - q["nAnB"])
odds_old = p["nAnB"] / (p["AB"] + p["nAB"] + p["AnB"])
check("Field's factor is the odds ratio o_new/o_old (S386)", sp.simplify(odds_new / odds_old - r1) == 0)

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
raise SystemExit(0 if ok else 1)
