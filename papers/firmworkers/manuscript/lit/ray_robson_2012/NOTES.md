# Notes — Ray and Robson (2012), pass 1 (read in full)

Run `python3 lit/ray_robson_2012/verify_rr.py` from the bundle root. It checks
the pinned sha256 of the PDF, the first page, that every block carries its page
number in the running head, each quotation verbatim on its page, two controls,
and measures `lean/Lit/RayRobson2012.lean` (build root, build, no `sorry`,
every cited name declared, every declared theorem audited, allowed axioms
only, controls present).

## What was verified

Our reading of the status rule, of the Jensen argument for inefficiency, of the
mean-below-median step, of the first-order algebra in the proofs of
Propositions 4(ii) and 5, and of the footnote 18 example is consistent with the
source. In particular, total status is ½ for every distribution only when ties
are split evenly (`total_status_half_iff`), which is the content of footnote 10
and matches Robson (1992).

## What could not be verified

Propositions 1–3 (the reduced-form construction, the steady state, the turnpike
convergence and existence) and Proposition 4(i); their proofs are summarised in
the Appendix and given in the Supplemental Material, which was not read.

## Discrepancies and readings

RR-D1 (`u_c(c, 0)` for `u_c(a, 0)` in the proof of Proposition 5) and RR-D2
(Becker, Murphy and Werning listed with pages 282–301) in `CLAIMS.md`. Neither
affects a result.

## Findings for the firmworkers ledger

1. **A new novelty risk: reference points from the distribution.** RR-7 credits
   Genicot and Ray (2010, "Aspirations and Inequality", then a mimeo) with
   using the income distribution to create endogenous reference points with
   high marginal gains and losses around them. That is close to the
   sub-section's reference `R_h` set by the income norm of the own group. The
   published version, to our knowledge Genicot and Ray, *Econometrica* 85(2),
   2017 (bibliographic details unverified), must be read before any novelty
   claim for `R_h`. It is not on the Drive folder.
2. **Status here is consumption rank, again market-driven.** As in Robson and
   in BMW's rank case, status answers to market outcomes; none of the three has
   inherited standing fixing a reference. The firmworkers `h` remains the point
   of difference (decision of 11 Oct 2026).
3. **Gambles are fair and continuum-based**; RR state that the finite case needs
   a residual claimant (footnote 11, p.1509). The firmworkers swap lottery is
   finite and negative-sum net of entry costs.
4. **Entry with aggregate risk is left open.** Footnote 25 (RR-24) asks what
   happens when people choose to enter a risky activity or stay out and
   stayers' status depends on how many enter. The firmworkers pool of displaced
   entrants (M32) is a version of that question, with positions rather than
   status at stake.
5. **Promotion as a gamble.** RR read occupational choice and promotion within
   a gang as gambles (RR-10, RR-22), which is the reading of the firmworkers
   lottery route as a job-market gamble.

## Controls

| Check | Control |
|---|---|
| Quotations | RR-C1, RR-5 with "no assumption at all" replaced by "a strong assumption", absent; RR-C2, RR-7 looked up on p.1508, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (RR-18), a wrong page (RR-7 on p.1508), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | three `control_*` theorems: with `η = 0` a single agent's status is 1, not ½; without strict concavity in consumption the slope at `a` is not negative; with `δ = 0` the savings rule fails at `x = ½` |

## Relevance to the firmworkers ledger

Row L18. No model row rests on Ray and Robson yet. It bears on the novelty of
the reference-dependent sub-section and adds Genicot and Ray to the papers
needed for it.
