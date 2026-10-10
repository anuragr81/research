# Empirical Test Starter Pack: The K-Sweep Prediction (KSW)

This document is written to be read on its own, in a conversation that
does not have the history of the modelling work behind it. It gives one
precisely stated, falsifiable empirical claim, the reason it was chosen
over the alternatives, the data and construction needed to test it, and
what is already built versus what remains.

## 1. The claim, stated precisely

The model (`PROOFS_v2.tex`, included in this bundle, code **KSW**) predicts
that when a bank recapitalises — raises fresh capital because its buffer
has fallen to a threshold — the capital level it raises *to* sits above
the level that triggered the raise, and the size of that gap is governed
entirely by the fixed component of the cost of raising capital, not by
the bank's risk preference. Three specific, falsifiable sub-predictions,
in decreasing order of how directly they were established in the model:

1. **The gap should vanish, not merely shrink, as the fixed cost goes to
   zero.** In the solved model, at zero fixed cost the trigger and the
   target coincide exactly (to machine precision — the largest deviation
   found was 2e-14). A bank facing negligible fixed issuance costs (a
   private placement to an existing large shareholder, a regulator-
   arranged capital injection with no underwriting) should show
   post-recapitalisation capital levels landing at or very near the level
   that triggered the raise, not systematically above it.

2. **The gap should scale with the cube root of the fixed cost.** This is
   the sharpest and most literature-grounded prediction. Classical
   impulse-control theory with a fixed transaction cost predicts a $K^{1/3}$
   scaling law for the width of the region between trigger and target; the
   solved model reproduces this (log-log slope 0.3845 against the
   theoretical 1/3, with the gap-to-$K^{1/3}$ ratio stable between 1.26 and
   1.35 across a fourfold range of cost values) *without having been fitted
   to do so*. This is not primarily a claim about this paper's novelty —
   the $1/3$ law is standard — but its reproduction here is what
   distinguishes "the solver is doing impulse control" from "the solver
   produces a gap that merely looks like one." Empirically, this predicts
   that if you can rank recapitalisation episodes by a proxy for the fixed
   cost involved, the gap should grow markedly more slowly than the cost
   itself — proportional to its cube root, not to the cost linearly.

3. **The gap should not depend on the size of the shortfall being
   corrected.** Because recapitalisation is triggered exactly when the
   buffer reaches the trigger level, and the target is a property of the
   solved policy rather than of how the bank got there, two recapitalisation
   events for the same bank under stable conditions should land at similar
   post-event capital ratios, largely independent of how much capital was
   injected. This is the weakest of the three claims to lean on for a first
   pass (see §6) but is a useful internal-consistency check on the data
   before testing 1–2.

**The paper-specific leg: invariance.** The cube-root law by itself is
generic impulse control — confirming it corroborates the chassis, not
this model specifically. What is specific to this paper is the *joint*
pattern: the gap is silent in the risk-attitude parameter while the
trigger level is loud in it. In the solved model, moving the preference
weight across its whole admissible range shifts the trigger from about
1.025 to 1.10 and the target from about 1.39 to 1.47 — both endpoints
move substantially — while the gap between them stays pinned at
0.365–0.37. Constant, not merely insensitive. This is the same
separation that delivers the paper's identification result (IDN): one
observable (the gap) loads on the fixed cost only, the other (the
trigger level) loads on both. Empirically: the gap should be flat in
whatever attitude proxies one would otherwise expect to matter
(franchise value, ownership structure, pre-crisis risk-taking) even as
the trigger level itself correlates with them. A purely
proportional-cost model predicts no gap at all; a preference-driven
recapitalisation story predicts the gap tracks the attitude proxies;
only the fixed-cost impulse mechanism predicts a sizeable, cost-graded,
attitude-invariant gap.

**The sharpest operational form of the cost leg: a size gradient.** If
the fixed cost of an equity raise is closer to a dollar amount while the
state variable is a ratio, the effective $K$ in ratio units scales as
one over assets, and the cube-root law turns that into a parameter-free
elasticity: the ratio-units gap should decline in log assets with slope
near $-1/3$. This needs no measurement of $K$ itself, which is what
makes it runnable.

**What would falsify it:** a robust, non-vanishing gap at near-zero fixed
cost; a gap that scales roughly linearly (not sub-linearly, and specifically
not as a cube root) with a fixed-cost proxy; a gap that tracks attitude
proxies as strongly as the trigger level does; or no detectable clustering
of post-event capital ratios at all (which would suggest the "target"
concept itself doesn't describe real recapitalisation behaviour, a more
basic failure than getting the scaling wrong).

**Where the model side stands.** Every numbered result in
`PROOFS_v2` carries a canonical verifier (its verification appendix):
the smooth-fit step behind the trigger/target structure is
Lean-verified, the identification determinant and the comparative
statics are exact symbolic checks, and the no-chattering summability
behind the impulse count has its algebraic core machine-checked. The
KSW model-side numbers have additionally been reproduced on an
independent cost grid the paper never used (log-log slope 0.3811,
R² = 0.998, gap-to-cube-root ratio inside the paper's 1.26–1.35 band).
None of that is empirical evidence — it is the model agreeing with
itself — but it means the predictions below are what the model actually
says, not artefacts of one solve.

## 2. Why this claim and not another

Three claims came up in the conversation that produced this model. KSW was
chosen over the other two for reasons worth carrying forward:

- **The "thresholds carry preference, volatility doesn't" claim (codes CSL
  + SCG)** is the paper's actual headline, and this question is no longer
  open — it has been checked, and the answer disfavours testing the
  headline claim directly, for a specific quantitative reason. SCG proves
  the *definitional* deficit-to-surplus variance ratio is a constant of cap
  geometry, with no dependence on the preference at all. A separate check —
  applying the estimator a real panel would use to simulated paths from the
  solved model — shows the *estimated* ratio does move with the preference,
  monotonically, but only by about 0.18–0.22 across the entire admissible
  range of the parameter, against a per-path standard deviation of
  0.16–0.20. The signal is roughly the size of its own noise. It is not
  that the estimated object is untested and might turn out to work; it has
  been tested, and it is weakly identified — usable only in aggregate
  across many institutions, not diagnostic from any single one. That is a
  positive reason to prefer a threshold-based test, not a gap to close
  before one is possible.
- **KSW** avoids that problem. Both sides of the prediction — the gap, and
  the fixed cost — are in-principle observable, and the sharpest version of
  the claim (the $1/3$ scaling) is a well-known, independently-derived
  benchmark from the impulse-control literature, not something invented for
  this model. That gives the test a real chance of being wrong in an
  informative way, which is what makes it worth running first.

## 3. The honest complication: fixed costs are not obviously large for equity issuance

Before designing the data pipeline, a caveat that changes how the proxy for
"fixed cost" should be built. The corporate-finance literature on seasoned
equity offering costs — mainly non-bank firms — finds mixed-to-negative
evidence for a large fixed component in flotation costs specifically.
Altinkilic and Hansen (2000, *Review of Financial Studies*) find spreads
are up to 85% variable cost with *rising* marginal cost, which is the
opposite of a classical fixed-plus-linear structure; a Swiss-market
replication finds no supporting evidence for a fixed component at all; a
German-market study finds fixed costs account for at most 14–24% of total
flotation costs. This is real, checked-today evidence and should not be
smoothed over.

Two things follow. First, this literature covers ordinary corporate SEOs,
not bank regulatory-capital raises specifically — bank issuances often
involve instruments (preferred stock, contingent convertibles), regulatory
pre-clearance, or in stress periods government-arranged injections, any of
which could plausibly carry a different, more genuinely fixed cost
structure than an ordinary SEO. That is a hypothesis, not yet evidence.
Second, and more usefully: this makes cross-sectional or cross-time
*variation* in cost structure (small vs. large issuers, ordinary-times vs.
crisis-period raises, private placements vs. public offerings) a better
starting proxy than trying to pin down a dollar-for-dollar fixed cost
directly, since the model's prediction is about how the gap responds to
variation in the fixed component, not about its absolute size.

## 4. Data source and what's already built

The right source, established in prior work on this project (see
`03_empirical/design/bank_level_feasibility.py` in this bundle), is **US
bank Call Reports via the FDIC BankFind API** — free, public, quarterly,
covering every FDIC-insured depository institution, with financial data
back to 1992 (Call Report detail to 2001). Two schedules matter:

- **Schedule RC-R** — regulatory capital ratios (the buffer/state variable).
- **Schedule RI-A** ("Changes in Bank Equity Capital") — carries both
  dividends declared and sale of stock / capital contributions in the same
  schedule, which is exactly the pair of events (payout, recapitalisation)
  the model is about.

`bank_level_feasibility.py` probes reachability, field availability
(explicitly, rather than letting an absent field become a silently-missing
column — FDIC field names change over time), coverage depth, and the
deciding feasibility question: are dividend and issuance events frequent
enough at all to identify anything.

**It has now been run, and the verdict is GO** (12 Aug 2026). All nine
wanted fields are present under their expected names. Coverage runs
1995–2024 (12,076 institutions at 1995-Q4, declining to 4,560 at
2024-Q4 — consolidation, not a data gap). At 2024-Q4, 89.8% of a
1,000-bank sample show nonzero dividends and 6.6% show nonzero stock
issuance: rare-but-present issuance is exactly the expected pattern for
an impulse control, and is the result the probe was designed to
distinguish from the failure case of zero observable events.

**One correction that follows immediately from the GO.** `EQCDIV` and
`EQCSTKRX` are Schedule RI-A *year-to-date* fields, not quarterly flows —
a Q3 value is the sum of Q1 through Q3, and a Q4 value is the entire
calendar year. The 89.8% / 6.6% figures above are therefore **annual**
rates read off a single Q4 snapshot, not quarterly rates. Raw YTD values
must be de-cumulated (Q1 flow = Q1 YTD; Qn flow = Qn YTD − Q(n−1) YTD,
resetting each January) before any event detection, or one annual event
becomes up to four quarterly events and lands in the wrong quarter.
`threshold_gap_design.py` now provides `decumulate_ytd()` for this, which
returns `None` — not zero — for any quarter whose predecessor is missing,
so coverage gaps cannot quietly become non-events.

## 5. What still needs to be built

A second script, `threshold_gap_design.py`, is included in this bundle as a
design skeleton — not yet run on real data. The feasibility probe has
confirmed the field names and coverage (§4); the remaining precondition is
the issuance-rate resolution in §6 step 2. It is written
to the same discipline as the rest of this project's empirical design work:
report what it finds rather than assume it, compute nothing that could be
mistaken for a result, and be dry-run-tested against synthetic data before
it ever touches real Call Report data. Concretely it should, once the
feasibility probe confirms the fields exist:

1. **Define a recapitalisation event**: a quarter in which RI-A's sale-of-
   stock/capital-contribution line is materially positive for a given
   institution. "Materially" needs a pre-registered threshold (a fraction
   of assets, or a fraction of the preceding quarter's capital) decided
   *before* looking at how the choice affects the result — this project's
   standing practice, and the reason the exclusion grounds in
   `03_empirical/design/empirical_design_exclusions.tex` (from the sister
   empirical-companion work) were written down before results were seen.
2. **Define a dividend event** analogously from RI-A's dividends-declared
   line.
3. **Compute the observed gap**: the capital ratio in the quarter
   immediately following a recapitalisation event, minus the capital ratio
   in the triggering quarter. This is the empirical analogue of the
   model's trigger-to-target distance.
4. **Build at least one fixed-cost proxy**, informed by §3's caveat rather
   than assuming a dollar figure: candidates are issuer size (assets or
   market cap, if available), public vs. private/mutual charter status, and
   whether the event falls in a stress period (2008–09, 2020) versus
   ordinary times, on the reasoning that external capital costs are
   well-documented to rise in stress periods even without a change in the
   underlying fixed-cost technology.
5. **Test predictions 1–2 from §1** — a within-bank check for whether the
   gap shrinks toward zero as the fixed-cost proxy shrinks, and a log-log
   regression of gap against the proxy, checking the slope against 1/3
   rather than against 1. This is a design, not a result: what the
   regression should report, and what would count as support versus
   disconfirmation, should be written down before the real data is pulled,
   exactly as prediction 3's role as a weaker sanity check (§1) was decided
   in this document rather than after seeing what the data does.

## 6. Suggested order of operations for the next conversation

1. ~~Run `bank_level_feasibility.py`~~ **DONE, verdict GO** — see §4.
2. **Resolve the issuance-rate contradiction — this is the live blocker.**
   The quarterly re-derivation has been run (`quarterly_event_rate.py`,
   2015–2024) and returned a quarterly issuance rate of 11.09% against
   the probe's 6.6% *annual* rate. That ordering is impossible for one
   population — a bank issuing in some quarter issued that year — while
   dividends order correctly (52.29% quarterly vs 89.8% annual), so the
   problem is specific to issuance. Candidate causes: routine issuance
   (ESOP/DRIP, employee plans) counted as recapitalisation because no
   materiality screen was applied; a counting bug scoring negative
   restatements as events; or a non-random probe slice.
   `issuance_diagnostic.py` (in this directory) was built and self-tested
   to adjudicate between them but has NOT been run on real data. Nothing
   downstream — the event definition, the panel, either prediction — is
   safe to build until this is resolved, because the event sample cannot
   rest on a count that is internally impossible. Note the RI-A issuance
   field is *net* of retirement and repurchase, so event identification
   must isolate gross raises or bound the netting.
3. Pre-register the event and proxy definitions from §5 in a design
   document, in the style of `empirical_design_exclusions.tex`, before
   pulling the full panel.
4. Build and dry-run `threshold_gap_design.py` against synthetic data with
   a known, injected gap-vs-cost relationship, the same way
   `panel_currency_union_check.py` elsewhere in this project was tested
   against an injected triplicate before being trusted on real data.
5. Only then run it on real Call Report data and look at predictions 1–2.
6. If the quarterly rate forces a RETHINK (events too rare): the
   fallback is not to abandon KSW but to widen the population — bank
   holding companies rather than individual charters, or a longer window —
   rather than loosening the pre-registered event definition after seeing
   that it produces too few events.

## 7. What this does not attempt

This starter pack deliberately does not build a data pipeline for the
CSL/SCG headline claim (§2). Not because the relevant question is
unresolved — it isn't, as §2 now states — but because the resolution is
unfavourable: an estimated variance ratio is weakly identified, usable
only in aggregate across many institutions, which is a much larger and
different data-collection problem than the bank-level event panel this
pack is built around. That remains a reasonable future direction, but as
an aggregate-panel exercise closer in kind to this project's earlier
FSI-panel work than to what follows here, not as an extension of it. It
also does not attempt the FSI-panel-style aggregate approach used
elsewhere in this project's empirical work: that approach was already
found, earlier in this project, to be structurally incapable of showing
recapitalisation events at all, since a country aggregate cannot reveal
individual institutions' payout and issuance decisions — which is the
deeper reason bank-level Call Report data was chosen here in the first
place, not merely a data-availability preference.

## 8. Bundle contents

- `PROOFS_v2.tex` / `PROOFS_v2.pdf` — the theory document. Codes referenced
  above (KSW, CSL, SCG, NST) are indexed at its start.
- `bank_level_feasibility.py` — run 12 Aug 2026, verdict GO (§4).
- `quarterly_event_rate.py` — run; its output is the issuance-rate
  contradiction that currently blocks the thread (§6 step 2).
- `issuance_diagnostic.py` — built and self-tested to adjudicate that
  contradiction; not yet run on real data.
- `threshold_gap_design.py` — design skeleton for the event/gap
  construction (§5), not yet run.
- `references.bib` — includes the Altinkilic-Hansen and related flotation-
  cost citations underlying §3's caveat.
