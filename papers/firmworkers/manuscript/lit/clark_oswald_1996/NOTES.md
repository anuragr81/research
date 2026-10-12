# Notes — Clark and Oswald (1996), pass 1 (authors' revision read in full)

Run `python3 lit/clark_oswald_1996/verify_co.py` from the bundle root. It
checks the pinned sha256 of the PDF, the title page, the page numbers, each
quotation verbatim on its page, two controls, and measures
`lean/Lit/ClarkOswald1996.lean` (build root, build, no `sorry`, every cited
name declared, every declared theorem audited, allowed axioms only, controls
present).

## What was verified

Our reading of the relative-income specification, of footnote 16 with the
Table 2 coefficients, of the magnitude on p.13 and of the satisfaction
distribution on p.8 is consistent with the source. The estimates themselves are
reported, not reproduced.

## What could not be verified

The ordered-probit estimates (no data here), the earnings equation, the
robustness checks with the NES data beyond their reading, and the education
results.

## Discrepancies and readings

CO-D1 and CO-D2 in `CLAIMS.md`.

## Findings for the firmworkers ledger

1. **A referent for `R_h`.** CO measure comparison income as the predicted pay
   of "'typical' employees of given characteristics" (CO-9), "the going rate of
   pay for someone like him or her" (CO-10), and find reported satisfaction
   falling in it (CO-12). That is the nearest measured object to the income norm
   of a worker's group. A measurement-map row for `R_h` could propose, as its
   observation, the group median of the wage distribution, with CO as the
   literature showing that a characteristic-based norm matters to workers.
2. **It does not reach choice.** CO's outcome is reported satisfaction treated as
   "proxy utility data" (CO-7). Under the author's decision of 10 Oct 2026 (Q4:
   `u` is not observed; utilities rationalise choices), satisfaction scores
   cannot measure `u` or its curvature. CO therefore support the referent (people
   compare their pay with that of comparable workers) but not the sub-section's
   claims about risk-taking below the reference; those need choice data.
3. **The reference group is not identified.** CO say co-workers' wages may matter
   most but cannot test it (CO-19), and their norm is a regression prediction
   over many characteristics, not a status group. Whether `h` is among the
   characteristics that define the comparison is not something CO establish.
4. **Equal and opposite.** With the Mincer `y*` the coefficients are equal and
   opposite (CO-13), a pure relative-income form (`relative_income_form`); with
   the NES `y**` they are not (CO-D2). The evidence for "only relative income
   matters" is therefore mixed within the paper.

## Controls

| Check | Control |
|---|---|
| Quotations | CO-C1, CO-12 with "negatively" replaced by "positively", absent; CO-C2, CO-9 looked up on p.11, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (CO-6), a wrong page (CO-9 on p.11), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | one `control_*` theorem: with the NES coefficient (−0.26) the index is not a function of `y/y*` alone |

## Relevance to the firmworkers ledger

Row L25. No model row rests on Clark and Oswald yet. It is the candidate
literature support for a measurement-map row for `R_h`, limited to the
referent.
