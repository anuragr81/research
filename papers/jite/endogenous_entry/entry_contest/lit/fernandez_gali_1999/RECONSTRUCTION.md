# Reconstruction of Fernández and Galí, "To Each According to...? Markets, Tournaments, and the Matching Problem with Borrowing Constraints"

**Source read.** NBER Working Paper 5930 (February 1997), 46 pages of page images with no text
layer, downloaded from NBER on 14 October 2026 and read in full [F, WP version]. The published
version is *Review of Economic Studies* 66(4): 799-824 (1999), which is not used; proposition
numbers may differ. The PDF is cached at `~/.cache/entry_contest/fernandez_gali_1997_w5930.pdf`,
the rendered pages at `~/.cache/entry_contest/fg_pages/`, and the OCR text at
`~/.cache/entry_contest/fernandez_gali_1997.txt`. Pages below are PDF pages; the printed page is
the PDF page minus one.

**Why this paper.** It was the leading lead of the application search of 14 October 2026: a
rank-order allocation (a tournament) in which wealth matters, compared with a market.

## Model (pp.6-7, 11-12)

- A continuum of agents with ability `a` and wealth `w`, uniform and independent on the unit
  square, and a continuum of schools of quality `s`, uniform on [0, 1].
- Output `X(a, s)` rises in both arguments, with `X_ss ≤ 0` and `X_as > 0`; `X(0, s) = 0` and
  `X < 1`.
- Utility is consumption: income plus wealth minus expenditure, so it is linear.
- **Market:** each school has a price `P(s)`.
- **Tournament:** an agent spends `e` to emit a signal `V(a, e)`, rising in both arguments, with
  `V(a, 0) = 0`. Schools go by the rank of the signal. The expenditure is wasted, and `e_va ≤ 0`.
- **Borrowing constraints:** spending (the price or the signal expenditure) cannot exceed `w`.

## Results

1. **Efficiency (Section 2.1, Theorem 2.2).** Positively assortative matching is efficient. The
   proof uses the Levy-Paroush lemma and the bound `Φ(a, s) ≤ min(a, s)`.
2. **Perfect capital markets (Section 3).** Markets and tournaments give the same, efficient,
   allocation, and wealth has no effect. Tournaments waste the signal expenditure, so
   consumption is lower. The price is never below the tournament expenditure (eq. 3.17).
3. **Markets with borrowing constraints (Section 4.1).** The lowest ability in school `s` is
   `a(s) = (s − P)/(1 − P) < s` (eqs. 4.4-4.5), so the allocation is inefficient. Prices fall,
   and some constrained agents gain.
4. **Tournaments with borrowing constraints (Section 4.2).** At equal spending higher ability
   gets a higher signal, so higher ability is "less credit constrained".
5. **Comparison (Section 4.3).** The lowest ability in each school is higher under tournaments
   (Lemma 4.15), and output is higher (Theorem 4.18).
6. **Signal power.** Over the family `V^θ` with `V^θ_a/V^θ_e = θ V_a/V_e`, aggregate signalling
   expenditure tends to zero as `θ → ∞` (Lemma 4.19, read from the page image). The allocation
   tends to the efficient one (Theorem 4.20), and consumption then exceeds the market's
   (Corollary 4.21).

## What is formalised

See `lean/mathlib/FernandezGali.lean`:

- single crossing for markets and tournaments, with a control;
- the Fréchet bound, and the finite counterpart of Theorem 2.2 (assortative matching maximises
  output for supermodular `X`), with a control;
- market clearing, eqs. (4.4)-(4.5);
- eq. (3.17);
- the core of Lemma 4.2;
- the bridge to our model: with linear utility the utility cost of a fee is constant in wealth.

The continuum equilibria and the limits in `θ` are read from the paper, not formalised.
