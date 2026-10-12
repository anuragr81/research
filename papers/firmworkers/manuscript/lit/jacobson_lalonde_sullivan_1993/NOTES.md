# Notes — Jacobson, LaLonde and Sullivan (1993), pass 1 (working paper read in full)

Run `python3 lit/jacobson_lalonde_sullivan_1993/verify_jls.py` from the bundle
root. It checks the pinned sha256 of the PDF, the cover and title page, the page
numbers of pp.1–36, the order of the table and figure pages, each quotation
verbatim on its page (footnotes whose text layer has no spaces are compared with
all spaces removed), three controls, and measures
`lean/Lit/JacobsonLaLondeSullivan1993.lean` (build root, build, no `sorry`,
every cited name declared, every declared theorem audited, allowed axioms only,
controls present).

## What was verified

Our reading of the loss definition, of the identification of models (2) and (3)
without the error term, of the shape and averaging of model (5'), of the p.18
argument for long-run effects, and of the arithmetic linking the text to Tables
1–3 is consistent with the source. The estimates themselves are reported, not
reproduced.

## What could not be verified

The estimates (no data), the dating of separations (Appendix B), the local
labour-market series (Appendix D), and anything read off Figures 1–5, whose
values are not in the text layer.

## Discrepancies and readings

JLS-D1 to JLS-D8 in `CLAIMS.md`. Two concern the source's own text: the average
drop of $2,219 on p.23 against −2179 in Table 2 (JLS-D1), and the title of
Figure 5 (JLS-D4). Neither is used.

## Findings for the firmworkers ledger

1. **Size and persistence.** Displaced high-tenure workers in mass layoffs lose
   about 25 per cent of pre-displacement earnings six years on, with little
   recovery after year three (JLS-20 to JLS-22), and 38 per cent if they leave
   manufacturing (JLS-37). The sample drops the roughly quarter of separators
   with no later Pennsylvania earnings; with them the loss would be about 15
   points larger (JLS-9, JLS-10).
2. **Bottom rung or proportional step (M32).** Under the bottom-rung reading
   decided on 12 Oct 2026, a displaced worker loses `x_i − x_0`, so the loss as a
   share, `1 − x_0/x_i`, rises with former income. JLS measure losses as shares
   of pre-displacement earnings and find them similar across sex and age
   (JLS-5, JLS-39). They have no breakdown by income level (JLS-7). The only
   income contrast available goes the other way: men lose more than women in
   dollars in the fifth year ($7,143 against $4,744), but less as a share of
   1979 earnings, about 26 against 32 per cent (`level_and_share_by_sex`; our
   arithmetic, mixing samples, JLS-D8). Holding industry and the other factors
   fixed, the sex gap nearly disappears. This is not a test of M32. It does mean
   that nothing in JLS supports a common fallback, and a loss proportional to
   former income (reading (b)) is closer to what they report. M32 stays
   `DECIDED`; this is flagged for the author only.
3. **The source manuscript's "greater absolute losses".** The sentence "workers
   in high-skill, high-income positions face greater absolute losses when
   displaced" is consistent with JLS in the weak sense that a roughly constant
   share implies absolute losses rising with income. It is also consistent with
   the larger losses in very large firms and unionised sectors (JLS-33,
   JLS-34). JLS do not report losses by income level, so the sentence cannot
   rest on them directly.
4. **No unemployment spell.** In JLS's sample the losses are largely lower pay
   for those who work, not nonemployment (JLS-F-1). That holds only because the
   sample requires earnings in every calendar year, a restriction that removed
   38 per cent of separators (JLS-45, JLS-46). The source's "no long-term
   unemployment" matches JLS's sample, not the whole population of the
   displaced.
5. **Losses begin before separation.** Earnings fall from about three years
   before the separation (JLS-23). M32 treats displacement as a discrete event.
   Measuring the downgrade from the year before separation would understate it
   by nearly half (JLS-25; `short_base_understates`).
6. **What is lost.** The loss persists for workers re-employed in the same
   4-digit industry (JLS-6, JLS-38), so the lost component is firm-specific
   rather than industry-specific (JLS-43), with rents and union premiums
   contributing (JLS-33). M32's "the skills of the lost job are worth nothing on
   the ladder" matches the firm-specific part only: JLS's workers keep about
   three quarters of their earnings.
7. **Who is displaced.** Before separation, separators earned 9 per cent less
   than stayers of the same age (JLS-11). M32 takes displacement as exogenous;
   in JLS it is selected on levels, and their fixed effects absorb that for
   their estimate (JLS-14). For the model this means the pool of displaced
   workers is drawn from lower in the distribution than the ladder as a whole.
8. **Measurement map.** Definition (1) with model (2) is a ready observation for
   the planned row for the downgrade: the downgrade is the displaced worker's
   earnings relative to the never-displaced counterfactual, estimated against
   stayers with fixed effects and a base at least three years before separation
   (`did_identifies_effect`). JLS's convention of reporting losses as a share of
   pre-displacement earnings is one candidate scale for the cost of M7, which
   M35 leaves open.

## Controls

| Check | Control |
|---|---|
| Quotations | JLS-C1, JLS-4 with 25 replaced by 50, absent; JLS-C2, JLS-4 looked up on p.4, absent; JLS-C3, footnote 30 reversed, absent with spaces removed |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (JLS-20), a wrong page (JLS-12 on p.11), an altered footnote quotation (JLS-F-1), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | three `control_*` theorems: the simple change misses `δ` when the time effects differ; the average loss misses `δ_0` without the weighted restriction; the long-run bound fails when true and spurious effects have opposite signs |

## Relevance to the firmworkers ledger

Row L26. No model row rests on JLS yet. It is the candidate literature support
for a measurement-map row for the downgrade. Findings 2 and 4 bear on M32 and on
the source's "no long-term unemployment".
