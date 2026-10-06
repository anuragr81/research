import os
import re
import shutil
import subprocess
import tempfile

results = []


def check(name, ok, detail=""):
    results.append((name, bool(ok)))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = os.path.dirname(os.path.abspath(__file__))
NS = "Sen1973"
SRC = os.path.join(HERE, "Sen1973.lean")
ALLOWED = {"propext", "Quot.sound"}

print("=" * 72)
print("Sen (1973), Economica 40(159):241-259. Checks of OUR READING.")
print("=" * 72)

print("SEN-4  Prisoners' dilemma payoffs, p.249 (sentences as negative numbers)")
s = {("C", "C"): -10, ("C", "N"): 0, ("N", "C"): -20, ("N", "N"): -2}
dom = all(s[("N", o)] < s[("C", o)] for o in "CN")
check("SEN-4a confessing is strictly better against either action", dom)
check("SEN-4b mutual non-confession beats mutual confession for each", s[("C", "C")] < s[("N", "N")])
s_bad = dict(s)
s_bad[("N", "N")] = 0
check("SEN-4c control: raising the mutual non-confession payoff to 0 removes dominance",
      not all(s_bad[("N", o)] < s_bad[("C", o)] for o in "CN"))

print("SEN-5  Non-confession is worse for the chooser against either action, p.251")
check("SEN-5 neither outcome of non-confession is preferred to the matching outcome of confession",
      not (s[("C", "C")] < s[("N", "C")]) and not (s[("C", "N")] < s[("N", "N")]))

print("SEN-6  Maximising the other's sentence makes non-confession dominant, p.252")
other = {(me, o): s[(o, me)] for (me, o) in s}
check("SEN-6a under the other-regarding objective non-confession is strictly dominant",
      all(other[("C", o)] < other[("N", o)] for o in "CN"))
check("SEN-6b that play leaves each better off in own terms", s[("N", "N")] > s[("C", "C")])

print("SEN-L  Lean: weak axiom on pairs and triples forces transitivity; controls")
lean = shutil.which("lean")
if lean is None:
    check("SEN-L lean on PATH", False, "lean not on PATH")
else:
    text = open(SRC).read()
    p = subprocess.run([lean, SRC], capture_output=True, text=True)
    check("SEN-L Sen1973.lean compiles with no errors", p.returncode == 0,
          (p.stdout + p.stderr).strip()[:200])
    check("SEN-L no sorry in the source", re.search(r"\bsorry\b", text) is None)
    names = re.findall(r"^theorem\s+([A-Za-z0-9_']+)", text, re.M)
    with tempfile.TemporaryDirectory() as td:
        audit = os.path.join(td, "audit.lean")
        with open(audit, "w") as fh:
            fh.write(text)
            fh.write("\n")
            for n in names:
                fh.write(f"#print axioms {NS}.{n}\n")
        q = subprocess.run([lean, audit], capture_output=True, text=True)
    out = q.stdout + q.stderr
    used = {}
    for n in names:
        m = re.search(rf"'{NS}\.{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    free = sum(1 for n in used if not used[n])
    bad = {n: sorted(a - ALLOWED) for n, a in used.items() if a - ALLOWED}
    print(f"      theorems: {len(names)}; audited: {len(used)}; axiom-free: {free}; "
          f"on propext/Quot.sound only: {len(used) - free - len(bad)}")
    check("SEN-L axiom audit covers every declared theorem", len(names) > 0 and len(used) == len(names))
    check("SEN-L no axiom outside propext and Quot.sound", not bad, str(bad) if bad else "")

nf = sum(1 for _, ok in results if not ok)
print("=" * 72)
print(f"SEN 1973 SUMMARY: {len(results)} checks, {nf} failures")
print("Scope: our reading is arithmetically consistent with the source and its")
print("formal steps are machine-checked. This does not reprove the paper.")
print("=" * 72)
raise SystemExit(1 if nf else 0)
