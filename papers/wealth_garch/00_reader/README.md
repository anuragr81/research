# Reader layer

Four files. Two are prose for a human; two are checks that run.

## PITCH_AND_SUMMARY.md

The paper in two lengths. Re-synced to v2 on 11 Aug 2026; it states the
usable null as the estimator null (roughly 1.23--1.48), not the
saturation-limit constant, and it reports the panel result at the scope
EMPIRICAL_v2 supports -- a mean-reverting subsample, not banking systems in
general.

## proof_registry.py

Layer 1. Every numbered result in PROOFS_v2, its number read from
PROOFS_v2.aux rather than from prose, and its ONE canonical verifier: a
Lean proof where one exists, an exact symbolic check otherwise, a numerical
check where the claim is numerical. verify() checks that every numbered
result has exactly one verifier, that no verifier is assigned to a
nonexistent result, that every named verifier file exists, and that every
named verifier tag (S1a, M10, V1, ...) actually occurs in the file that
claims it.

It also reports which results quote figures from an impulse operator whose
sweep is not convergence-certified. At present that is Rem. 3, which quotes
H-operator figures; only the M sweep has been certified.

## ledger_empirical.py

Layer 2. EMPIRICAL_v2's claims, each recording what it asserts, over which
population, and on what support. verify() resolves every cited PROOFS_v2 tag
through the registry, checks every cited artefact exists, enforces that a
claim asserted over a country population has panel support, enforces that a
specification-only claim reports no computed result, and cross-checks the
null figures quoted in the document against lambda_V_curve.csv.

The scope guard is the substantive one: it is what stops the headline
result being restated over a population the panel does not cover.

## check_citations.py

Numbering is authoritative from PROOFS_v2.aux, never reconstructed from
prose. Three checks: a spelled-out citation names a number that exists for
that kind; a symbolic \ref names a label that exists; and where both forms
appear together, they agree. Run it after every edit to PROOFS_v2.tex.

It reports which documents cite by bare number only -- for those, an
existing number is not evidence the citation points at the intended result,
and the remaining check is by hand.

## Running them

    python3 00_reader/proof_registry.py
    python3 00_reader/ledger_empirical.py
    python3 00_reader/check_citations.py

All three are wired into 04_reproduce/run_all.sh. They need PROOFS_v2.aux,
so run pdflatex on PROOFS_v2.tex first; the aux is not shipped.

## What a green verify() does and does not mean

It means the recorded relations are mutually consistent: numbers match the
aux, verifiers exist, cited tags resolve, quoted figures match the data
files, and no claim is stated over a population its support does not reach.
It does not mean the mathematics is right -- that is what the Lean and
symbolic layers are for -- and it does not mean the encoding faithfully
represents the manuscript. A mis-encoding can pass. Where the ledger and
the document disagree, find out which is wrong before changing either.
