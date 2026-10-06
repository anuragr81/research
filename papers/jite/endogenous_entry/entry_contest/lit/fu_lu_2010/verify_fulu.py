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
N = sp.symbols("N", positive=True)
Gamma0, C = sp.symbols("Gamma0 C", positive=True)

print("=" * 72)
print("Fu & Lu, 'Contest design and optimal endogenous entry'")
print("MPRA Paper No. 945, posted 28 Nov 2006. Working version of the")
print("Economic Inquiry article.  Checks of OUR READING.")
print("eq (7), p.10:   E = Gamma_0 - N(V*,S*) C   (the budget is Gamma_0)")
print("=" * 72)
print()

print("FL-1  eq (7): total effort is budget minus total entry costs")
print("-" * 72)
print("      p.10: 'in the optimally designed contest, the equilibrium total")
print("      effort is given by the difference between the total budget of the")
print("      contest organizer and the total entry costs incurred by")
print("      participating contestants, regardless of the contest technology.'")
E = Gamma0 - N * C
dE = sp.simplify(sp.diff(E, N))
print(f"      E(N) = {E};   dE/dN = {dE}")
check("FL-1 E strictly decreases in the entrant count", bool(dE == -C),
      "p.10: the RHS 'strictly decreases with N(V*,S*)'")

print()
print("FL-2  Theorem 1: the optimum attracts exactly two entrants, E = Gamma_0 - 2C")
print("-" * 72)
print("      Since E falls in N and the count is at least 2, the maximum over")
print("      admissible N is at N = 2.  Control: N = 3 must NOT be a maximiser.")
cands = {n: sp.simplify(E.subs(N, n)) for n in [2, 3, 4, 5, 10]}
for n, v in cands.items():
    print(f"      N={n}: E = {v}")


def is_maximiser(m):
    return all(bool(sp.simplify(cands[m] - cands[n]) >= 0) for n in cands)


two_wins = is_maximiser(2) and all(
    bool(sp.simplify(cands[2] - cands[n]) > 0) for n in cands if n != 2
)
three_loses = not is_maximiser(3)
check("FL-2 N = 2 is the unique maximiser, giving E = Gamma_0 - 2C",
      bool(sp.simplify(cands[2] - (Gamma0 - 2 * C)) == 0) and two_wins and three_loses,
      "matches Theorem 1's stated value; the N = 3 control is rejected")

print()
print("FL-3  CONTROL: the entry cost is what drives Theorem 1")
print("-" * 72)
print("      Section 4.2 (p.13): with C = 0 'the optimal number of participating")
print("      contestants would not necessarily be two'.  A check that passed")
print("      with C = 0 would not be testing the mechanism.")
E0 = sp.simplify(E.subs(C, 0))
dE0 = sp.simplify(sp.diff(E0, N))
print(f"      with C = 0:  E = {E0};  dE/dN = {dE0}")
check("FL-3 the count-dependence vanishes when the entry cost vanishes",
      bool(dE0 == 0 and sp.simplify(E0 - Gamma0) == 0),
      "matches Theorem 3's 'same total amount of effort, Gamma_0'")

print()
print("Tullock instance used by FL-4..FL-6:  f(x) = x, so H(x) = f/f' = x and")
print("H^{-1}(x) = x.  Equations (3), (4), Lemma 2 and Definition 1 (5) are")
print("implemented as printed; nothing is fitted to the conclusion.")
G0v, Cv, Mv = sp.Integer(10), sp.Integer(1), 6


def effort(n, V):
    return sp.Integer(0) if n == 1 else sp.Rational(1, 1) * V / n * (1 - sp.Rational(1, n))


def payoff(n, V, S, Cc):
    return V + S - Cc if n == 1 else V / n - effort(n, V) + S - Cc


def lemma2_count(V, S, Cc, M):
    if payoff(1, V, S, Cc) < 0:
        return 0
    return max(n for n in range(1, M + 1) if payoff(n, V, S, Cc) >= 0)


def feasible(V, S, n, G0):
    return 0 <= V <= G0 - n * S


print()
print("FL-4  Theorem 2 optimum in the Tullock instance, Gamma_0 = 10, C = 1")
print("-" * 72)
Vstar = 4 * (G0v / 2 - Cv)
Sstar = G0v / 2 - 2 * (G0v / 2 - Cv)
nstar = lemma2_count(Vstar, Sstar, Cv, Mv)
estar = effort(nstar, Vstar)
Estar = nstar * estar
pistar = payoff(nstar, Vstar, Sstar, Cv)
print(f"      V* = 4H(Gamma_0/2 - C) = {Vstar};  S* = Gamma_0/2 - 2H(Gamma_0/2 - C) = {Sstar}")
print(f"      N(V*,S*) = {nstar};  e = {estar};  pi = {pistar};  E = {Estar}")
print(f"      N*C = {nstar * Cv} <= Gamma_0 = {G0v}, slack = {G0v - nstar * Cv}")
check("FL-4 the optimum satisfies Lemmas 4, 5 and eq (7), with N*C <= Gamma_0 exact up to E",
      nstar == 2 and pistar == 0 and Vstar == G0v - nstar * Sstar
      and Estar == G0v - nstar * Cv and G0v - nstar * Cv == Estar
      and (Vstar, Sstar, Cv, estar, pistar, Estar) == (16, -3, 1, 4, 0, 8),
      "these are the numbers in eq7_hypotheses_satisfiable")

print()
print("FL-5  Fixed rules: the Lean control witnesses are genuine Fu-Lu contests")
print("-" * 72)
print("      Each witness is a feasible (V,S) at its Lemma-2 count, under the")
print("      Tullock technology, at which eq (7) FAILS.")
witnesses = [
    ("eq7_fails_without_lemma4", 8, 8, 0, 1, 2, 1, 4),
    ("eq7_fails_without_lemma5", 10, 4, 0, 1, 1, 0, 2),
]
allok = True
for name, G0w, Vw, Sw, Cw, ew, piw, Ew in witnesses:
    Vw, Sw, G0w, Cw = map(sp.Integer, (Vw, Sw, G0w, Cw))
    n = lemma2_count(Vw, Sw, Cw, Mv)
    e = effort(n, Vw)
    p = payoff(n, Vw, Sw, Cw)
    Et = n * e
    ok = (n == 2 and e == ew and p == piw and Et == Ew and feasible(Vw, Sw, n, G0w)
          and Et < G0w - n * Cw and Et <= G0w - n * Cw)
    print(f"      {name}: Gamma_0={G0w}, (V,S)=({Vw},{Sw}), N={n}, e={e}, pi={p}, "
          f"E={Et} < Gamma_0 - N C = {G0w - n * Cw}: {ok}")
    allok = allok and ok
check("FL-5 both witnesses are feasible Lemma-2 equilibria where eq (7) fails",
      allok, "eq (7) needs Lemmas 4 and 5; it is not a fixed-rules identity")

print()
print("FL-6  Grid over (V,S) in the Tullock instance, Gamma_0 = 10, C = 1, M = 6")
print("-" * 72)
print("      Every feasible contest must satisfy E <= Gamma_0 - N C")
print("      (fixed_rules_cap); equality only where pi = 0 and V = Gamma_0 - N S")
print("      (eq7_iff_lemma4_and_lemma5); max E = 8, attained only at (16,-3).")
n_feas = n_infeas = n_strict = 0
cap_ok = iff_ok = True
bestE, argbest = None, []
eq_points: list = []
for vi in range(0, 41):
    V = sp.Rational(vi, 2)
    for si in range(-12, 13):
        S = sp.Rational(si, 2)
        n = lemma2_count(V, S, Cv, Mv)
        if not feasible(V, S, n, G0v):
            n_infeas += 1
            continue
        n_feas += 1
        Et = n * effort(n, V) if n >= 1 else sp.Integer(0)
        if n >= 1:
            if Et > G0v - n * Cv:
                cap_ok = False
            lemmas = payoff(n, V, S, Cv) == 0 and V == G0v - n * S
            if (Et == G0v - n * Cv) != lemmas:
                iff_ok = False
            if Et < G0v - n * Cv:
                n_strict += 1
            else:
                eq_points.append((V, S, n, Et))
        if bestE is None or Et > bestE:
            bestE, argbest = Et, [(V, S, n)]
        elif Et == bestE:
            argbest.append((V, S, n))
print(f"      feasible {n_feas}, infeasible {n_infeas}, strict-slack {n_strict}")
print(f"      max E = {bestE} at {argbest}")
print(f"      eq (7) holds with equality at (V,S,N,E): {eq_points}")
check("FL-6 fixed-rules cap holds, equality iff Lemmas 4 and 5, Theorem 1 optimum unique",
      cap_ok and iff_ok and bestE == 8 and argbest == [(16, -3, 2)]
      and n_feas > 0 and n_infeas > 0 and n_strict > 0
      and (16, -3, 2, 8) in eq_points and any(p[2] != 2 for p in eq_points),
      "non-vacuous: some contests infeasible, some with strict slack; "
      "eq (7) also holds at a non-optimal contest with N != 2")

print()
print("FL-L  Lean: FuLu.lean (eq (7), Theorem 1, fixed rules, sequential entry)")
print("-" * 72)
lean = shutil.which("lean") or (
    os.path.expanduser("~/.elan/bin/lean")
    if os.path.exists(os.path.expanduser("~/.elan/bin/lean")) else None)
if lean is None:
    check("FL-L FuLu.lean compiles", False, "lean not found on PATH or in ~/.elan/bin")
else:
    ver = subprocess.run([lean, "--version"], capture_output=True, text=True).stdout.strip()
    print(f"      {ver}")
    src = os.path.join(HERE, "FuLu.lean")
    text = open(src).read()
    p = subprocess.run([lean, src], capture_output=True, text=True)
    out_all = p.stdout + p.stderr
    has_sorry = "sorry" in text or "declaration uses 'sorry'" in out_all
    hygiene = {
        "sorry": has_sorry,
        "native_decide": "native_decide" in text,
        "user axiom": re.search(r"^\s*axiom\b", text, re.M) is not None,
        "comment": "--" in text or "/-" in text,
    }
    bad = [k for k, v in hygiene.items() if v]
    check("FL-L1 FuLu.lean compiles with no errors", p.returncode == 0,
          out_all.strip()[:200])
    check("FL-L2 no sorry, no native_decide, no user axiom, no comments",
          not bad, "found: " + ", ".join(bad) if bad else "")
    names = re.findall(r"^\s*(?:theorem|lemma)\s+([A-Za-z0-9_']+)", text, re.M)
    decls = re.findall(r"^\s*(?:theorem|lemma)\b", text, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(text)
            fh.write("\n\n")
            for n in names:
                fh.write(f"#print axioms FuLu.{n}\n")
        out = subprocess.run([lean, audit], capture_output=True, text=True).stdout
    allowed = {"propext", "Quot.sound"}
    axioms_of = {}
    for ln in out.splitlines():
        m = re.match(r"'FuLu\.([A-Za-z0-9_']+)' (does not depend on any axioms|depends on axioms: \[(.*)\])", ln)
        if m:
            axs = [] if m.group(3) is None else [a.strip() for a in m.group(3).split(",")]
            axioms_of[m.group(1)] = axs
    audited = sum(1 for n in names if n in axioms_of)
    free = sum(1 for n in names if axioms_of.get(n) == [])
    core = sum(1 for n in names if axioms_of.get(n) and set(axioms_of[n]) <= allowed)
    other = {n: [a for a in axioms_of.get(n, []) if a not in allowed] for n in names}
    other = {n: a for n, a in other.items() if a}
    print(f"      theorems: {len(names)}; audited: {audited}; sorry: {int(has_sorry)}")
    print(f"      axiom-free: {free}; propext/Quot.sound only: {core}; other: {len(other)}")
    for n, a in other.items():
        print(f"      OTHER AXIOMS in {n}: {a}")
    check("FL-L3 axiom audit covers every declared theorem, core axioms only",
          audited == len(names) == len(decls) and len(names) > 0 and not other,
          "allowed: none, propext, Quot.sound")

    print()
    print("      Cross-file: eq (7) as an instance of FuJiaoLu.accounting_bound")
    acc = os.path.join(HERE, "..", "fu_jiao_lu_2015", "Accounting.lean")
    bridge = (
        "\n\ntheorem fulu_eq7_is_accounting_instance\n"
        "    (N : Nat) (Γ₀ C e E : Int)\n"
        "    (he : 0 ≤ e)\n"
        "    (htotal : E = (N : Int) * e)\n"
        "    (h7 : E = Γ₀ - (N : Int) * C) :\n"
        "    (N : Int) * C ≤ Γ₀ :=\n"
        "  FuJiaoLu.accounting_bound (N : Int) C e Γ₀ Γ₀ (by omega) he\n"
        "    (Int.le_of_eq (FuLu.eq7_dissipation_exact N Γ₀ C e E htotal h7))\n"
        "    (Int.le_refl Γ₀)\n\n"
        "#print axioms fulu_eq7_is_accounting_instance\n"
    )
    with tempfile.TemporaryDirectory() as td:
        comb = os.path.join(td, "bridge.lean")
        with open(comb, "w") as fh:
            fh.write(open(acc).read())
            fh.write("\n\n")
            fh.write(text)
            fh.write(bridge)
        q = subprocess.run([lean, comb], capture_output=True, text=True)
    m = re.search(r"'fulu_eq7_is_accounting_instance' (does not depend on any axioms|depends on axioms: \[(.*)\])", q.stdout)
    bax = [] if (m is None or m.group(2) is None) else [a.strip() for a in m.group(2).split(",")]
    print(f"      bridge axioms: {bax if m else 'NOT AUDITED'}")
    check("FL-L4 eq (7) instantiates accounting_bound with rent = budget = Gamma_0",
          q.returncode == 0 and m is not None and set(bax) <= allowed,
          (q.stdout + q.stderr).strip()[:200] if q.returncode else
          "third instance beside FJL eq (2) and P7; proved once, not duplicated")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"FU-LU SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
