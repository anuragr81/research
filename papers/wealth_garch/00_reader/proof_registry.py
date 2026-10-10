"""Registry of PROOFS_v2's numbered results and their canonical verifiers.

Layer 1 of the reader stack. Numbering comes from PROOFS_v2.aux, never from
prose. Each numbered result carries exactly one canonical verifier; verify()
checks that assignment against the aux and against the verifier files, and
reports coverage. It checks bookkeeping, not mathematics.
"""

import re
import sys
from pathlib import Path

BUNDLE_ROOT = Path(__file__).resolve().parent.parent
PROOFS_AUX = BUNDLE_ROOT / "00_document" / "PROOFS_v2.aux"
PROOFS_TEX = BUNDLE_ROOT / "00_document" / "PROOFS_v2.tex"

LEAN = "lean"
LEAN_PARTIAL = "lean_partial"  # algebraic core machine-checked in Lean; the
                               # analytic step it consumes is proved in the
                               # document. Scope per entry in LEAN_PARTIAL_SCOPE.
SYMBOLIC = "symbolic"
NUMERICAL = "numerical"
PROOF_IN_TEXT = "proof_in_text"

PROOF_GRADE_VERIFIERS = {LEAN, LEAN_PARTIAL, SYMBOLIC, PROOF_IN_TEXT}
EVIDENCE_GRADE_VERIFIERS = {NUMERICAL}

NUMBERED_KINDS = {"prop", "thm", "cor", "rem", "lem"}

CANONICAL_VERIFIER = {
    "prop:identity": (LEAN, "01_theory/lean_project/AsymCapital/RateBased.lean", ()),
    "prop:persistence": (LEAN, "01_theory/lean_project/AsymCapital/RateBased.lean", ()),
    "thm:lambda4": (SYMBOLIC, "01_theory/verify_rate_based.py", ("S1a", "S1b")),
    "prop:robust": (SYMBOLIC, "01_theory/verify_rate_based.py", ()),
    "prop:egarch": (SYMBOLIC, "01_theory/verify_rate_based.py", ()),
    "prop:sojourn": (SYMBOLIC, "01_theory/verify_rate_based.py", ()),
    "prop:nesting": (LEAN, "01_theory/lean_project/AsymCapital/QVI_Part1.lean", ("lambda_asymmetry_vanishes_at_one",)),
    "prop:concave-payoff": (LEAN, "01_theory/lean_project/AsymCapital/QVI_Part1.lean", ("neg_Lambda_concave",)),
    "prop:rrL": (SYMBOLIC, "01_theory/verify_state_map.py", ("M10",)),
    "prop:lowerbound": (PROOF_IN_TEXT, None, ()),
    "prop:statemap": (SYMBOLIC, "01_theory/verify_state_map.py", ("M1",)),
    "cor:boundary": (SYMBOLIC, "01_theory/verify_state_map.py", ("M9",)),
    "prop:satlimits": (SYMBOLIC, "01_theory/verify_saturation_limits.py", ("L1", "L2", "L3")),
    "rem:solver": (NUMERICAL, "02_numerical/verify_M_operator.m", ("V1", "V10")),
    "rem:capbinds": (NUMERICAL, "01_theory/verify_document_figures.py", ("F9", "F9b", "F9c")),
    "rem:compstat": (NUMERICAL, "01_theory/verify_document_figures.py", ("F2", "F3", "F4")),
    "rem:nonconcave": (NUMERICAL, "01_theory/verify_document_figures.py", ("F12",)),
    "rem:Ksens": (NUMERICAL, None, ()),
    "lem:reg": (PROOF_IN_TEXT, None, ()),
    "lem:envelope": (LEAN_PARTIAL, "01_theory/lean_project/AsymCapital/Envelope.lean", ("wode_of_envelope",)),
    "lem:bdr": (PROOF_IN_TEXT, None, ()),
    "prop:tcs": (SYMBOLIC, "01_theory/verify_comparative_statics.py", ("H4a", "H4b", "H4d")),
    "rem:tcsn": (NUMERICAL, "01_theory/verify_document_figures.py", ("F5",)),
    "lem:envelopeK": (LEAN_PARTIAL, "01_theory/lean_project/AsymCapital/Envelope.lean", ("node_of_envelope", "neumann_at_ystar", "trigger_jump")),
    "prop:kcs": (SYMBOLIC, "01_theory/verify_comparative_statics.py", ("H4c", "H4e")),
    "cor:idn": (SYMBOLIC, "01_theory/verify_comparative_statics.py", ("H5a", "H5b", "H5c", "H5d")),
    "rem:kcsn": (NUMERICAL, None, ()),
    "prop:smf": (LEAN, "01_theory/lean_project/AsymCapital/SmoothFit.lean", ("smooth_fit",)),
    "rem:smfn": (LEAN_PARTIAL, "01_theory/lean_project/AsymCapital/Envelope.lean", ("smf_permits",)),
    "prop:soc": (SYMBOLIC, "01_theory/verify_comparative_statics.py", ("H3a",)),
    "rem:socn": (NUMERICAL, "01_theory/verify_document_figures.py", ("F6", "F7", "F8")),
    "lem:itr": (PROOF_IN_TEXT, None, ()),
    "lem:fin": (LEAN_PARTIAL, "01_theory/lean_project/AsymCapital/ImpulseCount.lean", ("count_summable", "count_le", "impulse_sum_summable")),
    "prop:ver": (PROOF_IN_TEXT, None, ()),
    "rem:vern": (NUMERICAL, "01_theory/verify_smooth_fit_shooting.py", ("S2", "S3")),
}

# For every LEAN_PARTIAL entry: what the Lean declarations check, and what
# remains proved in the document. A lean_partial assignment without a scope
# line here is a verify() failure.
LEAN_PARTIAL_SCOPE = {
    "lem:envelope": (
        "Lean (wode_of_envelope): eq:Wode follows from the envelope identity "
        "eq:envelope and the differentiated interior equation, both taken as "
        "hypotheses. In text: the Danskin step establishing eq:envelope itself."
    ),
    "lem:envelopeK": (
        "Lean (node_of_envelope, neumann_at_ystar, trigger_jump): eq:Node and "
        "both boundary data follow from eq:envelopeK, V''(y*)=0, the FOC, "
        "smooth fit, and value matching, taken as hypotheses. In text: the "
        "Danskin step establishing eq:envelopeK itself."
    ),
    "rem:smfn": (
        "Lean (smf_permits): every curvature q <= 0 satisfies the operator "
        "inequality, given the intervention-region inequality Psi(x_L) >= 0. "
        "In text: that the admissible curvatures under smooth fit are exactly "
        "the q <= 0."
    ),
    "lem:fin": (
        "Lean (count_summable, count_le): bounded partial sums of the "
        "nonnegative discounted-impulse sequence make the count N summable "
        "with N <= B/K; impulse_sum_summable gives absolute convergence of "
        "the impulse sum VER cites. In text: that each impulse costs at least "
        "K and the strategy value is finite, which supplies the bound B."
    ),
}

IMPULSE_OPERATORS = {"M", "H"}
CONVERGENCE_CERTIFIED_OPERATORS = {"M"}
QUOTES_FIGURES_FROM_OPERATORS = {
    "rem:solver": {"M"},
    "rem:capbinds": {"M"},
    "rem:compstat": {"M", "H"},
    "rem:nonconcave": {"M"},
    "rem:Ksens": {"M"},
    "rem:tcsn": {"M"},
    "rem:kcsn": {"M"},
    "rem:socn": {"M"},
}


def numbered_results_from_aux():
    if not PROOFS_AUX.exists():
        raise FileNotFoundError(
            f"{PROOFS_AUX} not found; run pdflatex on PROOFS_v2.tex first"
        )
    text = PROOFS_AUX.read_text()
    found = {}
    for label, number, _page in re.findall(
        r"\\newlabel\{([^}]+)\}\{\{([^}]*)\}\{(\d+)\}", text
    ):
        kind = label.split(":")[0]
        if kind in NUMBERED_KINDS:
            if label in found and found[label] != number:
                raise ValueError(f"{label} carries two numbers in the aux")
            found[label] = number
    return found


def verifier_of(tag):
    return CANONICAL_VERIFIER[tag][0]


def verifier_location_of(tag):
    return CANONICAL_VERIFIER[tag][1]


def verifier_tags_of(tag):
    return CANONICAL_VERIFIER[tag][2]


def is_proof_grade(tag):
    return verifier_of(tag) in PROOF_GRADE_VERIFIERS


def is_evidence_grade(tag):
    return verifier_of(tag) in EVIDENCE_GRADE_VERIFIERS


def rests_on_uncertified_operator(tag):
    quoted = QUOTES_FIGURES_FROM_OPERATORS.get(tag, set())
    return quoted - CONVERGENCE_CERTIFIED_OPERATORS


def results_resting_on_uncertified_operators():
    return {
        tag: rests_on_uncertified_operator(tag)
        for tag in QUOTES_FIGURES_FROM_OPERATORS
        if rests_on_uncertified_operator(tag)
    }


def unassigned_numbered_results():
    return set(numbered_results_from_aux()) - set(CANONICAL_VERIFIER)


def assigned_but_absent_from_aux():
    return set(CANONICAL_VERIFIER) - set(numbered_results_from_aux())


def missing_verifier_files():
    absent = {}
    for tag, (kind, location, _tags) in CANONICAL_VERIFIER.items():
        if location is None:
            continue
        if not (BUNDLE_ROOT / location).exists():
            absent[tag] = location
    return absent


def missing_verifier_tags():
    absent = {}
    for tag, (kind, location, tags) in CANONICAL_VERIFIER.items():
        if location is None or not tags:
            continue
        path = BUNDLE_ROOT / location
        if not path.exists():
            continue
        body = path.read_text()
        for wanted in tags:
            if wanted not in body:
                absent.setdefault(tag, []).append(wanted)
    return absent


def verify(report=True):
    ok = True
    aux_results = numbered_results_from_aux()

    unassigned = unassigned_numbered_results()
    if unassigned:
        ok = False
        if report:
            print(f"FAIL numbered results with no canonical verifier: {sorted(unassigned)}")

    phantom = assigned_but_absent_from_aux()
    if phantom:
        ok = False
        if report:
            print(f"FAIL verifier assigned to a tag absent from the aux: {sorted(phantom)}")

    absent_files = missing_verifier_files()
    if absent_files:
        ok = False
        if report:
            print(f"FAIL verifier location does not exist: {absent_files}")

    absent_tags = missing_verifier_tags()
    if absent_tags:
        ok = False
        if report:
            print(f"FAIL verifier tag not found in its named file: {absent_tags}")

    for tag in CANONICAL_VERIFIER:
        if verifier_of(tag) not in PROOF_GRADE_VERIFIERS | EVIDENCE_GRADE_VERIFIERS:
            ok = False
            if report:
                print(f"FAIL unknown verifier kind for {tag}")

    partial_without_scope = {
        tag for tag in CANONICAL_VERIFIER
        if verifier_of(tag) == LEAN_PARTIAL and tag not in LEAN_PARTIAL_SCOPE
    }
    if partial_without_scope:
        ok = False
        if report:
            print(f"FAIL lean_partial entries with no recorded scope: {sorted(partial_without_scope)}")

    scope_without_partial = {
        tag for tag in LEAN_PARTIAL_SCOPE
        if tag not in CANONICAL_VERIFIER or verifier_of(tag) != LEAN_PARTIAL
    }
    if scope_without_partial:
        ok = False
        if report:
            print(f"FAIL scope recorded for a non-lean_partial tag: {sorted(scope_without_partial)}")

    for tag, operators in QUOTES_FIGURES_FROM_OPERATORS.items():
        if tag not in CANONICAL_VERIFIER:
            ok = False
            if report:
                print(f"FAIL operator record for an unregistered tag: {tag}")
        if operators - IMPULSE_OPERATORS:
            ok = False
            if report:
                print(f"FAIL unknown impulse operator named for {tag}")

    if report:
        print(f"registry: {len(aux_results)} numbered results, "
              f"{len(CANONICAL_VERIFIER)} canonical verifier assignments")
        by_kind = {}
        for tag in CANONICAL_VERIFIER:
            by_kind.setdefault(verifier_of(tag), []).append(tag)
        for kind in sorted(by_kind):
            grade = "proof" if kind in PROOF_GRADE_VERIFIERS else "evidence"
            print(f"  {kind:<14} {len(by_kind[kind]):>2}  ({grade}-grade)")
        exposed = results_resting_on_uncertified_operators()
        if exposed:
            print("REPORT results quoting figures from a convergence-uncertified "
                  "operator:")
            for tag, operators in sorted(exposed.items()):
                print(f"  {tag}: {sorted(operators)}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    sys.exit(0 if verify() else 1)
