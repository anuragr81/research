# Handover: KSW empirical thread

Status as of 12 August 2026. Written to be picked up without this
conversation's history.

## What this thread is

One testable implication of the theory paper, code **KSW**: when a bank
recapitalises, the gap between the capital level that triggered the raise
and the level it raises to is governed by the fixed cost of raising
capital, not by the bank's risk preference. Two predictions: the gap
vanishes at zero fixed cost, and it scales as the cube root of the fixed
cost. See `EMPIRICAL_CLAIM.pdf` for the full statement, or
`EMPIRICAL_TEST_STARTER.md` for the longer version with data design.

## Where it actually stands

**Settled.** The data source works. FDIC BankFind Call Reports supply all
nine needed fields, coverage 1995-2024, 4,560-12,076 institutions per
quarter. `bank_level_feasibility.py` was run and returned GO.

**Settled, and it cost a correction.** `EQCDIV` and `EQCSTKRX` are
Schedule RI-A *year-to-date* fields, not quarterly flows. They must be
de-cumulated (Q1 flow = Q1 YTD; Qn flow = Qn YTD - Q(n-1) YTD, resetting
each January) before any event detection. `decumulate_ytd()` in
`threshold_gap_design.py` does this and returns `None`, not zero, for a
quarter whose predecessor is missing.

**OPEN, and this is the live question.** Running
`quarterly_event_rate.py` over 2015-2024 gave a quarterly issuance rate of
11.09%, against the feasibility probe's 6.6% *annual* rate. That ordering
is impossible for the same population -- a bank issuing in some quarter
issued that year. Dividends show the expected ordering (52.29% quarterly
vs 89.8% annual), so the problem is specific to issuance.

Three candidate causes, none yet confirmed:

1. **Routine issuance counted as recapitalisation.** `EQCSTKRX` includes
   employee stock plans, ESOPs, and dividend reinvestment programmes --
   small, regular, unrelated to hitting a capital trigger. Supporting
   evidence: 47 banks issued in 8+ of 40 quarters, which is a trickle,
   not a rare impulse control. No materiality threshold was applied; the
   script counted any nonzero flow.
2. **Negative flows counted as events.** The test was `!= 0`, so a
   downward YTD restatement scored as an issuance. This is a bug in
   `quarterly_event_rate.py`, not a data property.
3. **Sample mismatch.** The 6.6% came from a 1,000-bank slice in whatever
   order the API returned, not a random sample, so it may not be
   comparable to the population figure at all.

`issuance_diagnostic.py` was written to test (1) and (2) -- it reports the
sign breakdown, the size distribution of positive issuances relative to
assets, and how the event rate falls as a materiality threshold rises. It
has **not been run on real data**. Cause (3) is not testable by that
script and would need the probe re-run on a random sample.

## What to do next, in order

1. Run `issuance_diagnostic.py`. If the rate falls below 6.6% at a
   sensible threshold and the size distribution shows most mass at
   trivial fractions of assets, causes (1) and (2) explain the anomaly
   and the data is fine.
2. Pre-register the materiality threshold **from the shape of the size
   distribution**, not from whichever value makes the rate comfortable.
   If there is a visible gap between trivial and substantial issuances,
   that gap is the threshold; if it is a smooth continuum, say so and
   check robustness across two or three values.
3. Recount repeat issuers post-threshold. The current figure (2,951 banks
   with 2+ events, 2005-2024) certainly overstates it. Within-bank tests
   of Prediction 2 need this number, not the raw one.
4. Only then build the gap panel and test the predictions.

## Files

| File | State |
|---|---|
| `EMPIRICAL_CLAIM.tex` / `.pdf` | The claim, self-contained. Current. |
| `EMPIRICAL_TEST_STARTER.md` | Longer design doc. Current. |
| `bank_level_feasibility.py` | Run, GO. Done. |
| `quarterly_event_rate.py` | Run. **Has bug (2) above** -- counts negative flows as events. Fix before rerunning. |
| `issuance_diagnostic.py` | Written, self-tested, **not run on real data**. |
| `threshold_gap_design.py` | Design skeleton. Self-tested. Not run on real data. Materiality constants at top are placeholders pending step 2. |
| `PROOFS_v2.tex` / `.pdf` | The theory document. Codes indexed at the top. |
| `references.bib` | Includes the flotation-cost citations behind the cost-proxy caveat. |

## One caveat carried forward

The corporate-finance literature on equity issuance costs finds weak
evidence for a large fixed component in ordinary SEOs (Altinkilic-Hansen
2000; Buhner-Kaserer 2002 -- both in `references.bib`). Bank
regulatory-capital raises may differ, but that is a hypothesis. The
practical consequence: proxy for *variation* in cost structure (issuer
size, charter type, stress-period timing), not for a dollar-valued fixed
cost.
