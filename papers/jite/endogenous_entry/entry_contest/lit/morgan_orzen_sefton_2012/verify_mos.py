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
x, X, P, F, w, n, N = sp.symbols("x X P F w n N", positive=True)

print("=" * 72)
print("Morgan, Orzen & Sefton (2012), 'Endogenous entry in contests',")
print("Economic Theory 51, pp. 435-463.")
print("Checks of OUR READING.")
print("=" * 72)
print()

print("MOS-1  Symmetric Tullock equilibrium: x_n* = (n-1)P/n^2, pi_n* = w + P/n^2")
print("-" * 72)
print("      p.441: 'expected earnings of a contestant who invests x_i when")
print("      total investment by all contestants is X is pi_i = w + x_i P/X - x_i'")
xi = sp.symbols("xi", positive=True)
others = (n - 1) * x
pi_i = w + xi * P / (xi + others) - xi
foc = sp.simplify(sp.diff(pi_i, xi).subs(xi, x))  # evaluate at the symmetric point
xstar = sp.solve(sp.Eq(foc, 0), x)
xstar = [sp.simplify(s) for s in xstar if s.is_positive is not False]
print(f"      FOC at the symmetric point: {sp.factor(foc)} = 0")
print(f"      x* = {xstar}")
pistar = sp.simplify(pi_i.subs(xi, xstar[0]).subs(x, xstar[0]))
print(f"      pi* = {sp.simplify(pistar)}")
check("MOS-1 the stated equilibrium is recovered from the payoff function",
      bool(any(sp.simplify(s - (n - 1) * P / n**2) == 0 for s in xstar)
           and sp.simplify(pistar - (w + P / n**2)) == 0),
      "both x_n* and pi_n* derived, not assumed")

print()
print("MOS-2  n* = floor(sqrt(P/F))")
print("-" * 72)
print("      p.441: 'n* is the largest integer such that P/n*^2 > F'.")
print("      P/n^2 > F  <=>  n^2 < P/F  <=>  n < sqrt(P/F).")
ineq = sp.simplify(P / n**2 - F)
solved = sp.solve(sp.Eq(P / n**2, F), n)
solved = [sp.simplify(s) for s in solved if s.is_positive is not False]
print(f"      P/n^2 = F  at  n = {solved}")
check("MOS-2 the entry threshold is sqrt(P/F)",
      bool(any(sp.simplify(s - sp.sqrt(P / F)) == 0 for s in solved)),
      "so the largest admissible integer is floor(sqrt(P/F))")

print()
print("MOS-3  Their Table 2 (p.445) investment predictions, reproduced exactly")
print("-" * 72)
print("      Design: w = 100, N = 6, F = 10; P = 50 (small), P = 200 (large).")
table = {
    50: [sp.Integer(0), sp.Rational(125, 10), sp.Rational(111, 10),
         sp.Rational(94, 10), sp.Rational(80, 10), sp.Rational(69, 10)],
    200: [sp.Integer(0), sp.Rational(500, 10), sp.Rational(444, 10),
          sp.Rational(375, 10), sp.Rational(320, 10), sp.Rational(278, 10)],
}
allok = True
for Pv, printed in table.items():
    row = []
    for k in range(1, 7):
        derived = sp.Rational(k - 1, k**2) * Pv
        ok = abs(float(derived - printed[k - 1])) < 0.05
        row.append(f"n={k}:{float(derived):.2f}")
        if not ok:
            allok = False
    print(f"      P={Pv}: " + "  ".join(row))
check("MOS-3 all twelve table entries match (n-1)P/n^2 to the printed precision",
      allok, "the published design table is reproducible from MOS-1")

print()
print("MOS-4  Their predicted entrant counts")
print("-" * 72)
preds = {}
for Pv, expect in [(50, 2), (200, 4)]:
    val = sp.sqrt(sp.Rational(Pv, 10))
    fl = sp.floor(val)
    preds[Pv] = fl
    print(f"      P={Pv}, F=10: sqrt(P/F) = {float(val):.4f}, floor = {fl}"
          f"   (paper says {expect})")
check("MOS-4 n* = 2 and 4 as printed",
      bool(preds[50] == 2 and preds[200] == 4),
      "matches Table 2's 'Entrants (n*)' column (p.445)")

print()
print("MOS-5  The observed deviations go in OPPOSITE directions")
print("-" * 72)
print("      Reported (rounds 26-50, second half):")
print("        small prize: 2.5 entrants against a prediction of 2  -> EXCESS")
print("        large prize: 3.7 entrants against a prediction of 4  -> SHORTFALL")
small_obs, large_obs = sp.Rational(25, 10), sp.Rational(37, 10)
small_gap = sp.simplify(small_obs - 2)
large_gap = sp.simplify(large_obs - 4)
print(f"      gaps: small {sp.nsimplify(small_gap)} (> 0), "
      f"large {sp.nsimplify(large_gap)} (< 0)")
check("MOS-5 the two treatments deviate in opposite directions",
      bool(small_gap > 0 and large_gap < 0),
      "excess entry with the small prize, under-entry with the large one")

print()
print("MOS-6  Identity indeterminacy with identical agents, made arithmetic")
print("-" * 72)
print("      p.441: 'since all of the players are identical in the model, the")
print("      identity of the players choosing to opt into the contest is not")
print("      uniquely determined.'")
print("      Any subset of size n* is an equilibrium entrant set, so the number")
print("      of pure-strategy configurations is C(N, n*).")
cfg = {(6, 2): sp.binomial(6, 2), (6, 4): sp.binomial(6, 4)}
for (Nv, nv), cnt in cfg.items():
    print(f"      N={Nv}, n*={nv}: C(N,n*) = {cnt} distinct entrant sets")
check("MOS-6 the entrant set is genuinely non-unique",
      bool(all(c > 1 for c in cfg.values())),
      "15 configurations in each treatment (no claim about P5 is checked here)")

print()
print("MOS-L  Lean: MorganOrzenSefton.lean (core Lean 4, no Mathlib)")
print("-" * 72)
print("      Contest stage (p.441), entry count and floor root (p.441), Table 2")
print("      (p.445), count pinned / identity unpinned (p.441), Proposition 1")
print("      structure (p.442), and controls.  Claim IDs MOS-L1..MOS-L8.")

LEAN_NS = "MorganOrzenSefton"
ALLOWED_AXIOMS = {"propext", "Quot.sound"}
NAME_RE = re.compile(r"^(?:theorem|lemma)\s+([A-Za-z0-9_']+)", re.M)
DEP_RE = re.compile(r"'([A-Za-z0-9_'.]+)' depends on axioms: \[([^\]]*)\]")
FREE_RE = re.compile(r"'([A-Za-z0-9_'.]+)' does not depend on any axioms")
BANNED = [
    ("sorry", r"\bsorry\b"),
    ("admit", r"\badmit\b"),
    ("native_decide", r"\bnative_decide\b"),
    ("user axiom", r"^\s*(?:private\s+|protected\s+)?axiom\s"),
    ("comment", r"--|/-"),
]
MUTANTS = [
    ("Table 2 entry 44.4 misread as 44.5",
     "(200, 3, 444)", "(200, 3, 445)"),
    ("design value floor(sqrt(200/10)) stated as 5",
     "theorem design_large : floorRoot (200 / 10) = 4",
     "theorem design_large : floorRoot (200 / 10) = 5"),
]


def run_lean(lean_bin, path):
    p = subprocess.run([lean_bin, path], capture_output=True, text=True, timeout=600)
    return p.returncode, p.stdout + p.stderr


def parse_axioms(out):
    found = {}
    for m in DEP_RE.finditer(out):
        if m.group(1).startswith(LEAN_NS + "."):
            found[m.group(1)[len(LEAN_NS) + 1:]] = {
                a.strip() for a in m.group(2).split(",") if a.strip()}
    for m in FREE_RE.finditer(out):
        if m.group(1).startswith(LEAN_NS + "."):
            found[m.group(1)[len(LEAN_NS) + 1:]] = set()
    return found


lean = shutil.which("lean")
src = os.path.join(HERE, "MorganOrzenSefton.lean")
if lean is None:
    check("MOS-L lean is on PATH", False, "lean not found, so no Lean check ran")
else:
    ver = subprocess.run([lean, "--version"], capture_output=True, text=True)
    print(f"      {ver.stdout.strip()}")
    text = open(src).read()
    hits = [label for label, rx in BANNED if re.search(rx, text, re.M)]
    hits += [] if (f"namespace {LEAN_NS}" in text and f"end {LEAN_NS}" in text) \
        else ["namespace"]
    check("MOS-L source hygiene: namespace, no sorry/admit/native_decide/axiom/comment",
          not hits, ", ".join(hits))

    rc, out = run_lean(lean, src)
    check("MOS-L MorganOrzenSefton.lean compiles, return code 0, no sorry",
          rc == 0 and "sorry" not in out, out.strip()[:300])

    names = NAME_RE.findall(text)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "Audit.lean")
        with open(audit, "w") as fh:
            fh.write(text + "\n")
            for nm in names:
                fh.write(f"#print axioms {LEAN_NS}.{nm}\n")
        arc, aout = run_lean(lean, audit)

        probe = os.path.join(td, "Probe.lean")
        with open(probe, "w") as fh:
            fh.write(f"namespace {LEAN_NS}\n"
                     "theorem probe_em (p : Prop) : p ∨ ¬ p := Classical.em p\n"
                     f"end {LEAN_NS}\n"
                     f"#print axioms {LEAN_NS}.probe_em\n")
        _, pout = run_lean(lean, probe)

        mutant_results = []
        for label, before, after in MUTANTS:
            mpath = os.path.join(td, "Mutant.lean")
            with open(mpath, "w") as fh:
                fh.write(text.replace(before, after))
            mrc, _ = run_lean(lean, mpath)
            mutant_results.append((label, before in text, mrc != 0))

    audited = parse_axioms(aout)
    bad = {nm: sorted(ax - ALLOWED_AXIOMS)
           for nm, ax in audited.items() if ax - ALLOWED_AXIOMS}
    n_free = sum(1 for ax in audited.values() if not ax)
    n_core = sum(1 for ax in audited.values() if ax and ax <= ALLOWED_AXIOMS)
    n_sorry = sum(1 for ax in audited.values() if "sorryAx" in ax)
    print(f"      theorems: {len(names)}; audited: {len(audited)}; sorry: {n_sorry}; "
          f"axiom-free: {n_free}; propext/Quot.sound only: {n_core}; other: {len(bad)}")
    check("MOS-L axiom audit covers every declared theorem",
          arc == 0 and len(names) > 0 and len(set(names)) == len(names)
          and set(audited) == set(names),
          f"declared {len(names)}, audited {len(audited)}")
    check("MOS-L no theorem depends on an axiom beyond propext / Quot.sound",
          not bad, "; ".join(f"{k}: {v}" for k, v in bad.items()))
    probe_ax = parse_axioms(pout).get("probe_em", set())
    check("MOS-L audit control: a Classical.em probe is flagged as disallowed",
          "Classical.choice" in probe_ax,
          "the audit parser can report an axiom outside the allowed set")
    for label, present, rejected in mutant_results:
        print(f"      mutant '{label}': target present {present}, rejected by lean {rejected}")
    check("MOS-L compile control: every mutated false claim is rejected by lean",
          all(present and rejected for _, present, rejected in mutant_results),
          f"{len(mutant_results)} mutants")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"MORGAN-ORZEN-SEFTON SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our READING of the source is arithmetically consistent.")
print("This does not reprove any result of the paper, and none of the")
print("experimental findings are re-analysed here.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
