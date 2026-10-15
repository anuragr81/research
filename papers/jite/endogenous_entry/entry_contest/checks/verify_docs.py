"""D: does the paper's prose agree with the bundle's own generated evidence?

Two defect classes have now recurred and neither was catchable by any existing
suite, because both live in English rather than in a number a check compares.

  F1 (2 Sep, morning)  PROOFS.tex cited 3000 randomised spreads where the
                       suite ran 758.  Caught by hand.
  Recurrence (2 Sep)   PROOFS.tex cited "thirty-one theorems, twenty ...
                       eleven" where the audit reported 52 / 36 / 16.  A
                       numeric audit missed it because the counts were spelled
                       out as words.
  Environment note     PROOFS.tex and EntryContest.lean asserted Mathlib was
                       unreachable.  verify.sh now measures that, and the
                       measurement contradicted the prose in two files while a
                       third paragraph of the same document corrected it.

This suite runs LAST, after VERIFICATION.md has been written, and compares the
paper against it.  It takes the generated file as argv[1].
"""
import pathlib
import re
import sys

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


HERE = pathlib.Path(__file__).resolve().parent.parent
ver_path = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "VERIFICATION.md"
ver = ver_path.read_text()
tex = (HERE / "MANUSCRIPT.tex").read_text()

print("=" * 72)
print("D1  LEAN COUNTS STATED IN MANUSCRIPT.tex vs THE GENERATED AUDIT")
print("=" * 72)
print("      PROOFS.tex, retired at pass 6, stated the core counts in a sentence")
print("      this check compared. The manuscript states no counts. Any count it")
print("      comes to state, in digits, must match the audit, and a count in")
print("      words is refused because words are how the 2 Sep error survived.")

COUNT_WORDS = r"(?:twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred)"


def stated_counts(text):
    digits = [int(x) for x in re.findall(r"\b(\d+)\s+(?:Lean\s+)?theorems\b", text)]
    words = re.findall(COUNT_WORDS + r"[- a-z]*\s+(?:Lean\s+)?theorems\b", text, re.I)
    return digits, words


m_tot = re.search(r"theorems:\s*(\d+);\s*audited:\s*(\d+)", ver)
m_thm = re.search(r"theorems declared:\s*(\d+)", ver)
if not m_tot or not m_thm:
    check("D1 the audit lines are present in the generated file", False,
          "cannot compare; did the Lean suites run?")
else:
    allowed = {int(m_tot.group(1)), int(m_thm.group(1)), int(m_tot.group(1)) + int(m_thm.group(1))}
    print(f"      generated: Mathlib {m_tot.group(1)}, core {m_thm.group(1)}")
    digits, words = stated_counts(tex)
    check("D1a every theorem count the manuscript states matches the audit",
          all(d in allowed for d in digits), f"stated {digits}, allowed {sorted(allowed)}")
    check("D1b no theorem count is spelled in words", not words, f"{words}")
    bad_d, _ = stated_counts("We prove 12345 theorems.")
    _, bad_w = stated_counts("We prove thirty-one theorems.")
    check("D1-control a wrong count and a count in words are both caught",
          any(d not in allowed for d in bad_d) and bool(bad_w))

print()
print("=" * 72)
print("D2  NO FILE ASSERTS AN ENVIRONMENT FACT THAT verify.sh MEASURES")
print("=" * 72)
print("      Mathlib's presence is probed on every run. Any file that instead")
print("      ASSERTS it is unavailable can contradict the measurement, which")
print("      is what happened on 2 Sep in two files at once. VERIFICATION.md")
print("      is excluded: it is generated, and the target here is authored prose.")
print("      A +/-2 line window is allowed, so a file may QUOTE the old claim")
print("      while correcting it nearby; only an uncorrected assertion fails.")

pat = re.compile(r"mathlib", re.I)
bad = re.compile(r"unreachable|unavailable|infeasible|not available|cannot be (built|reached)", re.I)
allow = re.compile(r"was false|used to record|previously|no longer|probe|corrected|defect|OFFENDER", re.I)
offenders = []
scanned = 0
for path in sorted(HERE.rglob("*")):
    if path.suffix not in {".tex", ".lean", ".md", ".py", ".sh"}:
        continue
    if path.name in {"verify.sh", "verify_docs.py", "VERIFICATION.md"}:
        continue
    if ".lake" in path.parts or "__pycache__" in path.parts:
        continue
    scanned += 1
    lines = path.read_text(errors="replace").splitlines()
    for n, line in enumerate(lines, 1):
        if not (pat.search(line) and bad.search(line)):
            continue
        window = " ".join(lines[max(0, n - 3):n + 2])
        if allow.search(window):
            continue
        offenders.append(f"{path.relative_to(HERE)}:{n}: {line.strip()[:70]}")
for o in offenders:
    print(f"      OFFENDER {o}")
check("D2 no file asserts Mathlib is unavailable", not offenders,
      f"{scanned} files scanned; assertions of a measured fact: {len(offenders)}")

print()
print("=" * 72)
print("D3  EVERY CHECK IN checks/ IS ACTUALLY INVOKED BY verify.sh")
print("=" * 72)

vsh = (HERE / "verify.sh").read_text()
scripts = sorted(p.name for p in (HERE / "checks").glob("verify_*.py"))
orphans = [s for s in scripts if s not in vsh]
print(f"      {len(scripts)} check scripts: {', '.join(scripts)}")
for o in orphans:
    print(f"      ORPHAN {o} is never run")
check("D3 no orphaned check scripts", not orphans,
      f"{len(scripts)} scripts, {len(orphans)} never invoked")

print()
print("=" * 72)
print("D4  ONE NAME PER OBJECT: MANUSCRIPT.tex AND MEASUREMENT_MAP.tex")
print("=" * 72)
print("      The Terminology appendix binds one English name to each object.")
print("      Synonyms it retires (payer, investor, talent, ability, base score,")
print("      the incumbent investing) must not reappear in authored prose. The")
print("      literature table keeps other papers' words, Lean identifiers are")
print("      names, and the sentence that states the rule is allowed to list them.")

BANNED = [r"\bpayers?\b", r"\bpaying class", r"\binvestors?\b", r"\bnon-investors?\b",
          r"\btalent\b", r"\bability\b", r"\bbase score", r"incumbent investing",
          r"investing incumbent", r"\bweight on talent"]
ALLOW = [r"ability_with_wealth", r"payerShare", r"\\lrow", r"\\citet", r"^%",
         r"weight on ability",            # Costrell and Loury's own object (C8)
         r"investment rate", r"investors are the agents",   # Fryer and Loury's objects (C6)
         r"between investors and", r"non-investors, but", r"investors' share is not",  # Coate and Loury's (C13)
         r"not a payer", r"not talent", r"investor\)"]     # the rule sentence itself


def lit_table_span(text):
    a = text.find(r"\lrow{L1}")
    b = text.find(r"\end{longtable}", a)
    return (a, b) if a >= 0 and b >= 0 else (-1, -1)


def offenders_in(text, lit_excluded):
    a, b = lit_table_span(text) if lit_excluded else (-1, -1)
    out = []
    pos = 0
    for i, line in enumerate(text.split("\n"), 1):
        start, pos = pos, pos + len(line) + 1
        if a <= start < b:
            continue
        if any(re.search(x, line) for x in ALLOW):
            continue
        hits = [x for x in BANNED if re.search(x, line, re.I)]
        if hits:
            out.append((i, line.strip()[:90]))
    return out


mp = (HERE / "MEASUREMENT_MAP.tex").read_text()
off_tex = offenders_in(tex, True)
off_map = offenders_in(mp, False)
for i, l in off_tex[:12]:
    print(f"      MANUSCRIPT.tex:{i}  {l}")
for i, l in off_map[:12]:
    print(f"      MEASUREMENT_MAP.tex:{i}  {l}")
check("D4a no retired synonym in MANUSCRIPT.tex outside the literature table",
      not off_tex, f"{len(off_tex)} lines")
check("D4b no retired synonym in MEASUREMENT_MAP.tex", not off_map, f"{len(off_map)} lines")

ta = tex.find(r"\section{Terminology}")
tb = tex.find(r"\section{Primitives and scope}", ta)
terms = re.findall(r"^([a-z][a-z' \-]+?) & \$", tex[ta:tb], re.M)
TERM_ALIASES = {"enter": ["enter", "entry"], "entrant, outsider": ["entrant", "outsider"],
                "entry count": ["k^*", "count"], "count invariance": ["count invariance", "every pure-strategy"],
                "identity multiplicity": ["identit"], "ordering in dispersion": ["dispers"],
                "sum of payoffs": ["payoffs"], "total entry cost": ["k^*c", "entry cost"],
                "selection": ["selection", "assortative"], "best rival score": ["best rival"],
                "first entrant's gain": ["first entrant", r"\Delta_Q(0)"], "dominance gap": ["wedge", "varphi"],
                "entry condition": ["entry condition"], "scored as an entrant": ["scored as an"]}
missing = []
for t in terms:
    keys = TERM_ALIASES.get(t, [t])
    if not any(k.lower() in mp.lower() for k in keys):
        missing.append(t)
print(f"      {len(terms)} terms in the Terminology table")
for t in missing:
    print(f"      MISSING from MEASUREMENT_MAP.tex: {t}")
check("D4c every Terminology term is used by name in MEASUREMENT_MAP.tex", not missing,
      f"{len(missing)} missing")
ctrl = offenders_in("the investor pays and the payer invests; talent is ability", False)
check("D4-control retired synonyms are caught", bool(ctrl))

print()
print("=" * 72)
print("D5  THE ADDENDA: TERMS, ROW REFERENCES AND LEAN NAMES")
print("=" * 72)
print("      WEIGHT_ADDENDUM.tex and STEADY_STATE_ADDENDUM.tex are starters for")
print("      extensions. Each uses the manuscript's terms, every row it cites must")
print("      exist in MANUSCRIPT.tex, and every Lean name it cites must be a theorem")
print("      of the Weight files, so a renumbered row cannot leave them stale.")
known = set(re.findall(r"\\[kmlc]row\{([KMLC]\d+)\}", tex))
we_src = "".join((HERE / "lean" / "mathlib" / f).read_text()
                 for f in ("WeightEndpoints.lean", "WeightSelection.lean"))
we_names = set(re.findall(r"^(?:theorem|lemma)\s+([^\s:(\[{]+)", we_src, re.M))
for fname, tag, min_lean in (("WEIGHT_ADDENDUM.tex", "W", 4), ("STEADY_STATE_ADDENDUM.tex", "S", 0)):
    wa = (HERE / fname).read_text()
    off_wa = offenders_in(wa, False)
    for i, l in off_wa[:12]:
        print(f"      {fname}:{i}  {l}")
    check(f"D5a-{tag} no retired synonym in {fname}", not off_wa, f"{len(off_wa)} lines")
    cited_rows = set(re.findall(r"\b([KMLC]\d+)\b", re.sub(r"^%.*$", "", wa, flags=re.M)))
    unknown = sorted(cited_rows - known)
    check(f"D5b-{tag} every row {fname} cites exists in MANUSCRIPT.tex", not unknown,
          f"{len(cited_rows)} cited" + (f", unknown {unknown}" if unknown else ""))
    wa_lean = sorted(set(n.replace("\\_", "_") for n in re.findall(r"\\texttt\{([a-z][A-Za-z0-9\\_']*)\}", wa)
                         if "\\_" in n))
    missing_we = [n for n in wa_lean if n not in we_names]
    check(f"D5c-{tag} every Lean name {fname} cites is a theorem of the Weight files",
          not missing_we and len(wa_lean) >= min_lean,
          f"{len(wa_lean)} cited" + (f", missing {missing_we}" if missing_we else ""))
check("D5-control an unknown row is caught", "M99" not in known)

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"DOCS SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
sys.exit(1 if nf else 0)
