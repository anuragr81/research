# Aigner and Cain (1977), reconstruction

Dennis J. Aigner and Glen G. Cain, "Statistical Theories of Discrimination in
Labor Markets", *Industrial and Labor Relations Review* 30(2), January 1977,
pp. 175–187.

## Source read

- The JSTOR scan of the published article (stable URL 2522871, accessed by
  the uploader on 24 February 2012), 14 PDF pages with an OCR text layer.
  PDF page 1 is the JSTOR cover, and PDF page `n` is journal page `173 + n`
  for `n` from 2 to 14. Cached at
  `~/.cache/paper_a/sources/aigner_cain_1977.pdf` (472,376 bytes), sha256
  `41b43241…0a74`, and its extracted text `aigner_cain_1977.txt`, sha256
  `4a23b808…eea5`.
- The text of every page, PDF pages 1 to 14, was read on 16 October 2026, so
  the evidence tag is [F]. The extracted text sets the two columns side by
  side and garbles every equation, so PDF pages 3 and 8, which carry eqs. (1)
  to (8) and footnotes 15 to 18, were rendered at 110 dpi and read from the
  image (AC-D2).
- Pages in this record are PDF pages.

## The model, from its own primitives

1. **Measurement** (PDF p. 3, eq. 1). An employer sees a test score
   `y = q + u` of a worker's true skill `q`. `u` is normal, independent of `q`,
   with mean zero; `q` is normal with group mean `α` and constant variance.
2. **Prediction** (p. 3, eqs. 2 and 3). `q̂ = E(q|y) = (1 − γ) α + γ y` with
   `γ = Var(q)/(Var(q) + Var(u)) = r²`, the reliability, `0 < γ < 1`.
3. **Two groups** (p. 3, eqs. 5a, 5b). Whites and blacks may differ in `α`,
   `Var(q)` and `Var(u)`, so each has its own `γ`. The employer pays
   `q̂` from the group's own equation.
4. **Consistency of beliefs** (p. 4). The employer's subjective conditional
   expectation equals the market's, so the group means are estimated without
   bias (AC-Q2, AC-Q3). This is an assumption justified by competition and by
   Spence's statement of equilibrium, not a fixed point derived in the paper.
5. **Definitions** (pp. 4–5). Economic discrimination is unequal pay for
   equal productivity. Group discrimination is the average wage of a group
   not proportional to its average productivity. Within-group discrimination
   is inevitable when `y` is fallible.
6. **A Phelps model** (pp. 5–7). With equal means, equal `Var(u)` and a larger
   `Var(q^B)`, the black line is steeper (Fig. 1A). With a less reliable test
   for blacks it is flatter (Fig. 1B). In both, each worker is paid his
   expected productivity and the groups' mean wages are equal, so there is no
   group discrimination.
7. **The risk-aversion model** (pp. 7–9). If `q` enters profit linearly, the
   employer is risk neutral and the dispersion of `q` given `y` is costless
   (AC-Q4). With `U(q) = a − b e^{−cq}`, `b, c > 0` (eq. 7), the expected
   utility under the normal belief is `a − b e^{−c E(q|y) + (c²/2) Var(q|y)}`
   (eq. 8, footnote 17), and the employer maximises `E(q|y) − k Var(q|y)`,
   `k = c/2`, choosing the group of labour with the larger risk-discounted
   productivity (AC-Q6). `Var(q|y) = Var(q)(1 − γ)` (footnote 15)
   `= Var(q) Var(u)/(Var(q) + Var(u))` (footnote 18). With equal means and
   equal `Var(q)`, a less reliable test for blacks gives them the larger risk
   factor `R^B` and lower pay at `y = α` (Fig. 2, AC-Q9). The authors doubt
   the size of `R` (AC-Q10) for three reasons, including employers
   heterogeneous in risk aversion bidding up black wages (p. 10).
8. **Spence's signalling model** (pp. 10–11). Multiple equilibria can sustain
   different thresholds `y*` for the two groups, but the authors argue such
   arrangements are unstable once high-ability black workers can contract on
   a lower `y*`.
9. **Selection–truncation** (p. 11). Hiring is confined to the upper end of
   the `y` distribution. A group with less reliable tests has its predictions
   compressed toward its mean, so fewer of its members clear the threshold.
   The noisier group gets a lower average wage with no risk aversion and no
   bias by employers (AC-Q11, AC-Q12). Fig. 4 is the extreme case of perfect
   and zero reliability with the hiring floor at the mean.
10. **Unequal average abilities** (pp. 11–13). With `α^B < α^W` and equal
    slopes, blacks are paid less at every `y` and at every `q`
    (`E(q̂|q) = (1 − γ) α + γ q`), which the authors call premarket rather than
    labour-market discrimination. With perfect information (`γ = 1`) the
    difference disappears (AC-Q13).
11. **Conclusions** (pp. 13–14). Reliability differences combined with risk
    aversion (AC-Q14), or with truncation, give economic discrimination.

## Derivation chain of what Paper A1 uses

- Eq. (3) gives `0 < γ < 1` when both variances are positive (`gamma_mem`).
- Footnote 15 and footnote 18 are the same quantity (`condVar_symm`).
- Eq. (8) is the expectation of eq. (7) under a normal belief (`eq8`, from
  Mathlib's moment generating function of the normal law).
- The mean–variance criterion follows because eq. (8) is decreasing in its
  exponent for `b, c > 0` (`eq8_order`). The paper's own route through "its
  logarithm" is recorded as AC-D3.
- `Var(q|y)` rises with `Var(u)` when `Var(q) > 0` (`condVar_strictMono_Vu`),
  which gives Fig. 2's lower black pay at `y = α` (`fig2_black_paid_less`).
- With zero reliability the prediction is the group mean and the risk factor
  is `k Var(q)` (`zero_reliability`).
- The two conditions of the Fig. 2 result are each necessary, as the
  controls show. Without risk aversion there is no discount
  (`control_risk_neutral`), and without spread in `q` there is no discount
  (`control_no_spread`).

## What could not be reconstructed

- The selection–truncation model is stated in words and a figure, with no
  equation, and is not formalised. Its logic (compression of `q̂` toward the
  mean under a floor above the mean) is recorded in words only.
- The Spence discussion and the counter-example handout of footnote 24 are
  outside the paper and not formalised.
- The empirical magnitudes on p. 9 (a differential of perhaps 10 to 30
  percent) are the authors' estimate with no source given, and nothing here
  relies on them.
