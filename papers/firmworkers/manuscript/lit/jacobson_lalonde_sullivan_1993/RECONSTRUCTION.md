# Reconstruction of Jacobson, LaLonde and Sullivan, 1993

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| JLS | "Earnings Losses of Displaced Workers", Federal Reserve Bank of Chicago Working Paper WP-92-28, cover dated December 1992, title page "Revised November 1992"; unnumbered title page, pp.1–36 (text, appendix, references), then Tables 1–3 and Figures 1–5 on unnumbered pages; published as *American Economic Review* 83(4), 1993, pp.685–709 | FRASER scan with an ABBYY FineReader text layer, 47 PDF pages, Drive `frbchi_workingpaper_1992-28.pdf`, id `1cMYr1sFiYzgY7iW4XhuwRYnLE9jQ4cxu`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/jls_1992_wp.pdf`, sha256 `22595eed564fd4b8b53b2826992bb811a1e60bb23a3fc12dac422bba7a15363c` | [F], working paper; the AER pages are not read |

Text extracted with `pdftotext -layout` to `~/.cache/firmworkers/jls_1992_wp.txt`
(sha256 `6c694f37a7e7365f6b6d32c516fdaf94caaf24ec65161f16731f779fe2c89e87`).
Form-feed block 0 is the FRASER cover, block 1 the title page, and block `p + 1`
is printed page `p` for `p = 1, ..., 36`; each of those pages ends with its
number before the FRASER footer. The nine pages after p.36 carry no number; this
record numbers them 37–45 in order (37 Table 1, 38–39 Table 2, 40 Table 3, 41–45
Figures 1–5), so block `p + 1` is page `p` throughout. Several footnotes have no
spaces between words in the text layer (for example footnotes 30, 31, 34–49);
their quotations are checked with all spaces removed. Read in full on 12 Oct
2026, with footnotes, appendix and tables. Cited as the AER article with
`version_read`.

**Why this paper.** The empirical counterpart for the displacement of M32: how
large and how lasting the loss of a displaced worker is, whether it involves an
unemployment spell, and what is lost. The source manuscript states that "workers
in high-skill, high-income positions face greater absolute losses when
displaced" and assumes "no long-term unemployment"; JLS is the standard
measurement of both.

## Primitives (§2–3)

- Quarterly earnings 1974–1986 from Pennsylvania UI tax records for a 5 per cent
  sample of workers, merged with ES202 firm data (employment, location, 4-digit
  SIC). Workers with the same firm from at least 1974 to the end of 1979 (six or
  more years of tenure), born 1930–1959, with sex and age known, with some
  earnings in every calendar year.
- Separators are split by the firm's employment change: the mass-layoff sample
  (firm employment in the year after the departure 30 per cent or more below its
  late-1970s maximum) and the rest. Stayers remain through 1986.
- Loss, definition (1): `E(y_it | D_is = 1, I_{s-p}) - E(y_it | D_iv = 0 for all v, I_{s-p})`,
  the change in expected earnings if, `p` quarters before `s`, it were revealed
  that the worker would be displaced at `s` rather than never.
- Model (2): `y_it = α_i + γ_t + x_it β + Σ_{k ≥ -m} D^k_it δ_k + ε_it`, with
  `m = 20` quarters; model (3) adds worker-specific trends `ω_i t`; model (4)
  interacts the quarter effects with the 1979 firm; model (5) adds dip, drop
  and recovery interactions `F^1_it w_i φ_1 + F^2_it w_i φ_2 + F^3_it w_i φ_3`.

## Derivation chain

1. **Definition (p.10).** The loss is measured against a never-displaced
   counterfactual from a date before the events that lead to displacement, so
   pre-separation declines count as losses.
2. **Identification (pp.12–14).** Least squares with fixed effects; the
   displacement effects are unbiased whatever the relation of `α_i` to
   displacement. A simple before–after change is adequate only with constant
   time effects, no controls and an early enough base. Model (3) handles layoffs
   targeted at workers with slow earnings growth.
3. **Parsimony (pp.15–17).** Group differences lie along three dimensions; model
   (5) fixes the gap between two workers' losses at zero more than three years
   before separation, linear in the dip window, constant for six quarters,
   linear thereafter; with `Σ_j f_j φ_lj = 0` the average loss in quarter `k` is
   `δ_k`.
4. **Bias (pp.17–19).** Selection on a low error at the layoff date biases only
   `δ_0` with independent errors; with stationary errors the spurious effects
   are symmetric about separation, so small estimated effects more than three
   years before imply small spurious effects more than three years after; the
   mass-layoff sample limits the nonstationary case.
5. **Findings (§4).** Mass layoffs: earnings start to fall about three years
   before separation, are about $1,000 below expected the quarter before, and
   remain $1,600 below six years after, 25 per cent of pre-displacement earnings,
   with little recovery after year three; with trends the loss is larger.
   Non-mass layoffs recover in three to five years. Conditioning on the firm
   lowers the loss by about 20 per cent. Table 2: modest variation by sex and
   age once other factors are held fixed; larger losses in unionised sectors and
   very large firms; depressed regions and adverse cycles add up to a third.
   Table 3: re-employment in the old 4-digit industry does not remove the loss;
   leaving manufacturing raises it to 38 per cent.
6. **Conclusion (§5).** Losses are long-term, begin before separation, vary
   modestly, and are large even in similar firms; something specific to the
   employment relationship is lost.

## What was reconstructed

Model (2) without its error term: the difference-in-differences of a displaced
worker and a stayer identifies `δ_k` when the base quarter lies before the
window, independent of the fixed effects; a base inside the window subtracts
`δ` at the base and so understates the loss when earnings already dipped; the
simple change is adequate with constant time effects and controls, and not
otherwise. Model (3): with unequal trends the model-(2) contrast is off by
`(ω_i - ω_j)(t - b)`, overstating the loss when displaced workers have slower
trends; two base quarters before the window remove the trends. Model (5'): the
four-piece form of the loss, footnote 36's weights 4 and 50, the average loss
`δ_k` under the weighted restriction and its failure without it. The p.18
argument for long-run effects. The arithmetic of pp.15, 19, 22–24, 29 and 33
against Tables 1–3. See `CLAIMS.md`.

## What was not reconstructed

The estimates themselves (no data), the construction of separations from EIN
changes (Appendix B), the local labour-market variables (Appendix D), the
selection model the authors decline to estimate, and Figures 1–5, which have no
readable values in the text layer.
