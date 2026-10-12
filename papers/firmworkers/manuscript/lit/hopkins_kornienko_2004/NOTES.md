# Notes — Hopkins and Kornienko (2004), pass 1 (AER article read in full)

Run `python3 lit/hopkins_kornienko_2004/verify_hk.py` from the bundle root. It
checks the pinned sha256 of the PDF and of the column and OCR renderings, the
cover, the page numbers, each quotation verbatim on its page in at least one of
four renderings, two controls, and measures
`lean/Lit/HopkinsKornienko2004.lean` (build root, build, no `sorry`, every cited
name declared, every declared theorem audited, allowed axioms only, controls
present).

## What was verified

Our reading of the status specification, of the cooperative allocation and the
tax formula for `U = xyS`, of Examples 1 and 2, and of the comparison behind
"misery loves company" at a fixed allocation is consistent with the source.

## What could not be verified

Proposition 1 (existence, uniqueness, monotonicity), Propositions 2–5 and the
corollaries, which rest on comparison arguments for the differential equation
(6); Figures 1 and 2.

## Discrepancies and readings

HK-D1 to HK-D5 in `CLAIMS.md`. None is a slip in the source: they concern
garbled displays in the renderings and the unstated link between `τ_A` and
`τ_x` in Example 2, which `example2_revenue_neutral_rate` proves.

## Rendering note

The JSTOR text layer merges the two columns line by line and puts some words in
the wrong column (a left-column "in" sits at x ≈ 480 on p.1099), so no single
rendering finds every quotation. `lit/bbox_columns.py` rebuilds columns from
the word boxes; the local OCR (`lit/ocr_pages.py columns`) reads the page
images. The suite accepts a quotation found on its own page in any rendering and
reports which; both controls must fail in every rendering.

## Findings for the firmworkers ledger

1. **Inequality and status spending.** The source asks "how adaptability
   related expenditures (or social capital investments) respond to rise in
   inequality". HK answer the analogue for status spending: more equality
   raises the middle class's spending (HK-21, HK-22), raises the poor's when
   status has no floor (HK-24) and lowers it when it has one (Proposition 4(ii),
   read, not quoted), and may lower the rich's (HK-6, HK-29); a richer society spends more and is worse off at every
   income (HK-2). Any firmworkers claim on inequality and investment (the macro
   comparative statics in `μ`, deferred to the macro sync) must be set against
   HK. Differences to state: HK's spending is pure waste on a positional good
   (HK-1, HK-15); firmworkers' upskilling is productive, and its positional part
   is a lottery over positions.
2. **Exogenous status removes the race.** When status is given rather than won
   (HK-16, HK-17), everyone chooses the cooperative level. Firmworkers' `h` is
   inherited, fixed within a working life and not bought (M28, decision of 11
   Oct 2026), so in firmworkers status acts through access (closure `σ`) rather
   than through a spending race. HK's benchmark supports the distinction; it
   does not threaten it.
3. **A within-group-only case in the literature.** HK cite Cooper et al. (2001),
   a growth model with status where agents "only interact with other agents of
   the same wealth" (HK-7). That is the full-closure limit of firmworkers'
   cross-group pool. Cooper, B., García-Peñalosa, C. and Funk, P. (2001),
   "Status Effects and Negative Utility Growth", *Economic Journal* 111(473),
   642–665, is a candidate novelty paper for closure in a growth setting;
   triage from its abstract first.
4. **Fixed income against fixed rank.** HK compare welfare at a fixed income,
   not a fixed rank (HK-5, HK-30), and leave fixed-rank comparisons open.
   Firmworkers' ladder makes fixed-rank comparisons the natural ones, so its
   welfare statements are not directly comparable with HK's.
5. **Rank against a group norm as the reference.** HK's status is rank in the
   whole distribution (HK-14); the sub-section's `R_h` is the income norm of the
   own status group. HK are the rank-based alternative to `R_h`, alongside
   Clark–Oswald (L25) for the level-based one.

## Controls

| Check | Control |
|---|---|
| Quotations | HK-C1, HK-2 with worse replaced by better, absent from every rendering; HK-C2, HK-21 looked up on p.1095, absent from every rendering |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (HK-22), a wrong page (HK-21 on p.1097), an undeclared Lean name, a wrong PDF pin and a wrong OCR pin each fail |
| Lean | three `control_*` theorems: with `γ = 1` a tie at the top costs nothing; with `a > 0` Example 1's tax is not 1; with `V = 0` the poor's comparison is not strict |

## Relevance to the firmworkers ledger

Row L28. No model row rests on HK. It is a novelty record for the source's
first question and a contrast for `h`; it points to Cooper et al. (2001).
