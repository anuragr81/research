"""Ledger for EMPIRICAL_v2's claims.

Layer 2 of the reader stack. Each claim records what it asserts, over what
population, and what supports it; support that is a PROOFS_v2 result is
resolved through proof_registry, and support that is a data artefact is
checked to exist and, where the claim quotes figures, to match. verify()
checks internal consistency and scope discipline, not econometric validity.
"""

import csv
import re
import sys
from pathlib import Path

import proof_registry as registry

BUNDLE_ROOT = Path(__file__).resolve().parent.parent
EMPIRICAL_TEX = BUNDLE_ROOT / "00_document" / "EMPIRICAL_v2.tex"
CURVE_CSV = BUNDLE_ROOT / "03_empirical" / "results" / "lambda_V_curve.csv"

MEAN_REVERTING_SUBSAMPLE = "mean_reverting_subsample"
ALL_FETCHED_COUNTRIES = "all_fetched_countries"
SIMULATED_PATHS = "simulated_paths"
SOLVED_MODEL = "solved_model"

POPULATIONS = {
    MEAN_REVERTING_SUBSAMPLE,
    ALL_FETCHED_COUNTRIES,
    SIMULATED_PATHS,
    SOLVED_MODEL,
}

PROOF_SUPPORT = "proof_support"
SIMULATION_SUPPORT = "simulation_support"
PANEL_SUPPORT = "panel_support"
SOLVER_SUPPORT = "solver_support"
SPECIFICATION_ONLY = "specification_only"

SUPPORT_KINDS = {
    PROOF_SUPPORT,
    SIMULATION_SUPPORT,
    PANEL_SUPPORT,
    SOLVER_SUPPORT,
    SPECIFICATION_ONLY,
}

CLAIMS = {
    "theta_not_identified_from_routine_excursions": {
        "population": SIMULATED_PATHS,
        "support": SIMULATION_SUPPORT,
        "proof_tags": (),
        "artefacts": ("01_theory/verify_egarch_recovery.py",),
        "has_computed_result": True,
    },
    "theta_free_estimator_recovers_lambda_at_null": {
        "population": SIMULATED_PATHS,
        "support": SIMULATION_SUPPORT,
        "proof_tags": (),
        "artefacts": ("01_theory/verify_egarch_recovery.py",),
        "has_computed_result": True,
    },
    "null_bias_opposes_the_reported_direction": {
        "population": SIMULATED_PATHS,
        "support": SIMULATION_SUPPORT,
        "proof_tags": (),
        "artefacts": ("01_theory/verify_egarch_recovery.py",),
        "has_computed_result": True,
    },
    "saturation_ratio_is_a_constant_of_cap_geometry": {
        "population": SOLVED_MODEL,
        "support": PROOF_SUPPORT,
        "proof_tags": ("thm:lambda4", "prop:satlimits"),
        "artefacts": (),
        "has_computed_result": True,
    },
    "definitional_null_rejected_as_benchmark": {
        "population": SOLVED_MODEL,
        "support": PROOF_SUPPORT,
        "proof_tags": ("prop:satlimits",),
        "artefacts": (),
        "has_computed_result": True,
    },
    "measured_scale_null_is_the_data_benchmark": {
        "population": SOLVED_MODEL,
        "support": SOLVER_SUPPORT,
        "proof_tags": (),
        "artefacts": ("03_empirical/results/lambda_RN_measured.py",),
        "has_computed_result": True,
    },
    "estimator_bias_toward_one_is_asymptotic": {
        "population": SOLVED_MODEL,
        "support": SOLVER_SUPPORT,
        "proof_tags": (),
        "artefacts": ("03_empirical/design/verify_fullsample_reference.py",),
        "has_computed_result": True,
    },
    "estimator_null_minimum_anywhere_is_1_2347": {
        "population": SOLVED_MODEL,
        "support": SOLVER_SUPPORT,
        "proof_tags": ("rem:solver", "rem:compstat"),
        "artefacts": ("03_empirical/results/lambda_V_curve.csv",),
        "has_computed_result": True,
    },
    "panel_median_lies_below_the_null_under_every_convention": {
        "population": MEAN_REVERTING_SUBSAMPLE,
        "support": PANEL_SUPPORT,
        "proof_tags": (),
        "artefacts": ("03_empirical/design/expanding_mean_panel.py",),
        "has_computed_result": True,
    },
    "attrition_is_directional_not_random": {
        "population": ALL_FETCHED_COUNTRIES,
        "support": PANEL_SUPPORT,
        "proof_tags": (),
        "artefacts": ("03_empirical/design/expanding_mean_panel.py",),
        "has_computed_result": True,
    },
    "cross_country_dispersion_straddles_the_null": {
        "population": MEAN_REVERTING_SUBSAMPLE,
        "support": PANEL_SUPPORT,
        "proof_tags": (),
        "artefacts": ("03_empirical/design/expanding_mean_panel.py",),
        "has_computed_result": True,
    },
    "secondary_reference_analysis": {
        "population": ALL_FETCHED_COUNTRIES,
        "support": SPECIFICATION_ONLY,
        "proof_tags": (),
        "artefacts": (),
        "has_computed_result": False,
    },
}

POPULATION_REQUIRING_PANEL_SUPPORT = {MEAN_REVERTING_SUBSAMPLE, ALL_FETCHED_COUNTRIES}

QUOTED_NULL_MINIMUM_ANYWHERE = 1.2347          # population scale (theory)
QUOTED_MEASURED_NULL_MINIMUM_ANYWHERE = 1.152  # panel-estimator scale (data comparison)
QUOTED_MEASURED_RISK_NEUTRAL_MEDIAN = 1.334    # lambda_S = 1, convention (a)
QUOTED_RISK_NEUTRAL_COLUMN_BOUNDS = (1.31, 1.48)
QUOTED_PANEL_MEDIAN = 1.038
QUOTED_PANEL_N = 33
RISK_NEUTRAL_LAMBDA_S = "1.00"


def population_of(claim):
    return CLAIMS[claim]["population"]


def support_of(claim):
    return CLAIMS[claim]["support"]


def proof_tags_of(claim):
    return CLAIMS[claim]["proof_tags"]


def artefacts_of(claim):
    return CLAIMS[claim]["artefacts"]


def has_computed_result(claim):
    return CLAIMS[claim]["has_computed_result"]


def claims_asserted_over_all_countries():
    return {c for c in CLAIMS if population_of(c) == ALL_FETCHED_COUNTRIES}


def claims_restricted_to_the_surviving_subsample():
    return {c for c in CLAIMS if population_of(c) == MEAN_REVERTING_SUBSAMPLE}


def claims_with_specification_only_support():
    return {c for c in CLAIMS if support_of(c) == SPECIFICATION_ONLY}


def unresolved_proof_tags():
    absent = {}
    for claim in CLAIMS:
        for tag in proof_tags_of(claim):
            if tag not in registry.CANONICAL_VERIFIER:
                absent.setdefault(claim, []).append(tag)
    return absent


def claims_resting_on_evidence_grade_results():
    resting = {}
    for claim in CLAIMS:
        evidence = [t for t in proof_tags_of(claim) if registry.is_evidence_grade(t)]
        if evidence:
            resting[claim] = evidence
    return resting


def missing_artefacts():
    absent = {}
    for claim in CLAIMS:
        for artefact in artefacts_of(claim):
            if not (BUNDLE_ROOT / artefact).exists():
                absent.setdefault(claim, []).append(artefact)
    return absent


def curve_values_from_csv():
    if not CURVE_CSV.exists():
        return None
    with CURVE_CSV.open() as handle:
        return [float(row["lambda_RN"]) for row in csv.DictReader(handle)]


def document_quotes_minimum_matching_the_curve():
    values = curve_values_from_csv()
    if values is None:
        return None
    return abs(min(values) - QUOTED_NULL_MINIMUM_ANYWHERE) < 5e-5


def risk_neutral_column_from_csv():
    if not CURVE_CSV.exists():
        return None
    with CURVE_CSV.open() as handle:
        return [
            float(row["lambda_RN"])
            for row in csv.DictReader(handle)
            if row["lambda_S"] == RISK_NEUTRAL_LAMBDA_S
        ]


def document_quotes_risk_neutral_column_matching_the_curve():
    column = risk_neutral_column_from_csv()
    if not column:
        return None
    low, high = QUOTED_RISK_NEUTRAL_COLUMN_BOUNDS
    return round(min(column), 2) == low and round(max(column), 2) == high


def curve_table_in_document_matches_csv():
    values = curve_values_from_csv()
    if values is None or not EMPIRICAL_TEX.exists():
        return None
    text = EMPIRICAL_TEX.read_text()
    quoted = {float(x) for x in re.findall(r"\b1\.\d{4}\b", text)}
    quoted |= {float(x) for x in re.findall(r"\b0\.\d{4}\b", text)}
    from_csv = {round(v, 4) for v in values}
    return from_csv <= quoted


UNIVERSE_PROVENANCE_DOCUMENTED = True
UNIVERSE_DIAGNOSTIC_HAS_RUN = True
PANEL_RERUN_ON_CORRECTED_UNIVERSE_REFLECTED_IN_DOCUMENT = True


def country_universe_is_trustworthy():
    return UNIVERSE_PROVENANCE_DOCUMENTED and UNIVERSE_DIAGNOSTIC_HAS_RUN


def document_reflects_the_corrected_universe():
    return PANEL_RERUN_ON_CORRECTED_UNIVERSE_REFLECTED_IN_DOCUMENT


def verify(report=True):
    ok = True

    if not country_universe_is_trustworthy():
        ok = False
        if report:
            print("FAIL the panel's country universe (FINAL_77) has no "
                  "recorded provenance. Run "
                  "03_empirical/design/universe_diagnostic.py and either "
                  "replace FINAL_77 with a stated rule or document the "
                  "coverage gap in empirical_design_exclusions.tex, then "
                  "set UNIVERSE_PROVENANCE_DOCUMENTED and "
                  "UNIVERSE_DIAGNOSTIC_HAS_RUN to True here.")

    if not document_reflects_the_corrected_universe():
        ok = False
        if report:
            print("FAIL the country universe was replaced "
                  "(03_empirical/design/panel_universe.py, sovereign_universe(), "
                  "11 Aug 2026) but EMPIRICAL_v2's figures -- median 1.040, "
                  "n=26 of 77, the attrition table -- were computed under the "
                  "OLD FINAL_77 universe and are now stale. Rerun "
                  "expanding_mean_panel.py on the corrected universe, update "
                  "every figure in EMPIRICAL_v2 Section 3 that changes, then "
                  "set PANEL_RERUN_ON_CORRECTED_UNIVERSE_REFLECTED_IN_DOCUMENT "
                  "to True here.")

    if not registry.verify(report=False):
        ok = False
        if report:
            print("FAIL proof_registry.verify() is False; fix layer 1 first")

    for claim in CLAIMS:
        if population_of(claim) not in POPULATIONS:
            ok = False
            if report:
                print(f"FAIL unknown population for {claim}")
        if support_of(claim) not in SUPPORT_KINDS:
            ok = False
            if report:
                print(f"FAIL unknown support kind for {claim}")

    unresolved = unresolved_proof_tags()
    if unresolved:
        ok = False
        if report:
            print(f"FAIL claim cites a tag absent from the registry: {unresolved}")

    absent = missing_artefacts()
    if absent:
        ok = False
        if report:
            print(f"FAIL claim cites an artefact that does not exist: {absent}")

    for claim in CLAIMS:
        if population_of(claim) in POPULATION_REQUIRING_PANEL_SUPPORT:
            if support_of(claim) not in {PANEL_SUPPORT, SPECIFICATION_ONLY}:
                ok = False
                if report:
                    print(f"FAIL {claim} asserts over a country population "
                          f"without panel support")

    for claim in claims_with_specification_only_support():
        if has_computed_result(claim):
            ok = False
            if report:
                print(f"FAIL {claim} has specification-only support but claims "
                      f"a computed result")

    minimum_match = document_quotes_minimum_matching_the_curve()
    if minimum_match is False:
        ok = False
        if report:
            print("FAIL the smallest null value quoted in EMPIRICAL_v2 does not "
                  "match the minimum in lambda_V_curve.csv")
    elif minimum_match is None:
        if report:
            print("SKIP lambda_V_curve.csv absent; minimum cross-check not run")

    column_match = document_quotes_risk_neutral_column_matching_the_curve()
    if column_match is False:
        ok = False
        if report:
            print("FAIL the risk-neutral column bounds quoted in EMPIRICAL_v2 do "
                  "not match the lambda_S=1 rows of lambda_V_curve.csv")
    elif column_match is None:
        if report:
            print("SKIP risk-neutral column cross-check not run")

    table_match = curve_table_in_document_matches_csv()
    if table_match is False:
        ok = False
        if report:
            print("FAIL EMPIRICAL_v2's null table does not reproduce every value "
                  "in lambda_V_curve.csv")
    elif table_match is None:
        if report:
            print("SKIP null-table cross-check not run")

    if report:
        print(f"empirical ledger: {len(CLAIMS)} claims")
        print(f"  asserted over the surviving subsample only: "
              f"{len(claims_restricted_to_the_surviving_subsample())}")
        print(f"  asserted over all fetched countries: "
              f"{len(claims_asserted_over_all_countries())}")
        print(f"  specification-only, no computed result: "
              f"{len(claims_with_specification_only_support())}")
        resting = claims_resting_on_evidence_grade_results()
        if resting:
            print("REPORT claims resting on evidence-grade (numerical) results:")
            for claim, tags in sorted(resting.items()):
                print(f"  {claim}: {tags}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    sys.exit(0 if verify() else 1)
