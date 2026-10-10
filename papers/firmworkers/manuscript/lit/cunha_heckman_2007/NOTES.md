# Notes — Cunha and Heckman (2007), pass 1 (working paper, read in full)

Run `python3 lit/cunha_heckman_2007/verify_cunha_heckman_2007.py` from the
bundle root. It checks the pinned sha256 of the cached working paper, the
cover, that page blocks line up with printed page numbers, each quotation
verbatim on its own page, two controls on the quotation check, and measures
`lean/Lit/CunhaHeckman2007.lean` (build root, build, no `sorry`, every cited
name declared, every declared theorem audited, allowed axioms only, controls
present).

## Version

The source read is NBER WP 12840 (January 2007). The ledger cites the *AER*
Papers and Proceedings version, which was not supplied. Page numbers and
wording may differ; the ledger row carries `version_read` and is shown as
unverified.

## What was verified

Our reading of Sections II–III is consistent with the source:

- (4) at `γ = 1/2` makes timing irrelevant.
- (5) makes early investment necessary and the even allocation optimal.
- With perfect substitutes and prices, invest early iff `γ > (1 − γ)(1 + r)`.
- (9) is the unique solution of the interior first-order condition, equal to
  `γ/((1 − γ)(1 + r))` at `φ = 0` and rising in `γ`.

## What could not be verified

- The first-order conditions are hypotheses of `optimal_ratio_of_foc` and
  `constrained_ratio_of_focs`.
- The p.15 result under a binding `b' ≥ 0` is stated without derivation.
- The general CES cross-partial for `φ < 1` is not formalised.
- Sections V–VI report results of other papers.

## Discrepancies

- CH-D1, p.18. The printed ratio under the third credit constraint does not
  follow from (8) with both constraints binding. The derived ratio has no
  `1 + r` and carries `β` multiplicatively,
  `[γβ/(1 − γ)]^{1/(1−φ)} (c₁/c₂)^{(1−σ)/(1−φ)}`. The two disagree by a factor
  of 4 at one admissible point (`printed_ratio_reading`). The qualitative
  statements on p.18 survive for the derived ratio under `β(1 + r) = 1`.
- CH-D2, p.23. "37%" for Table 1's 0.3755.

## Findings from the formalisation

1. Under Leontief, the even allocation is optimal for any interest rate
   `r > −1`, not only `r = 0`; the bound `min{I₁, I₂} ≤ E(1 + r)/(2 + r)` is
   attained at `I₁ = I₂` (`leontief_even_is_optimal`, `leontief_even_attains`).
2. At `φ = 1` the first-order condition does not pin down the ratio
   (`control_ratio_needs_phi_lt_one`).

## Controls

| Check | Control |
|---|---|
| Quotations | CH-C1, CH-11 with "early" replaced by "late", is absent; CH-C2, CH-7 looked up on p.12, is absent |
| Quotation suite | mutation-tested 10 Oct 2026; an altered word, a wrong page and an undeclared Lean name each fail |
| Lean | four `control_*` theorems (timing, invest early, ratio, constrained ratio) |

## Relevance to the firmworkers ledger

No model row rests on this paper yet. If the law of motion for `h` is taken
from technology (1), self-productivity and dynamic complementarity are the
properties that would carry over; the Leontief limit is the case where a low
early stock cannot be compensated. Whether the micro state includes `h` is the
author's decision (TODO.md).
