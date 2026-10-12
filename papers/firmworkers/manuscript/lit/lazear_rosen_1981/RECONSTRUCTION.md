# Reconstruction of Lazear and Rosen, 1981

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| LR | "Rank-Order Tournaments as Optimum Labor Contracts", *Journal of Political Economy* 89(5), 1981, pp.841–864 | publisher PDF with a text layer, 24 PDF pages, Drive `1981-lazear.pdf`, id `1K2jyuShICvvy0On7b0fEku7GQDatN9uS`; cached `~/.cache/firmworkers/lazear_rosen_1981.pdf`, sha256 `c45beeca541d99db242a2cd8fb111e992292ec872735a38193727318a5d5dc3d` | [F], published version |

Text extracted with `pdftotext -layout` to `~/.cache/firmworkers/lazear_rosen_1981.txt`.
Form-feed block `i` is page `841 + i`. Pages 842–864 start with their number
(even pages) or end their running head with it (odd pages); the text layer
prints 860 and 861 as "86o" and "86i"; page 841 ends with its number. The text
layer drops many spaces between words ("self-selectthe"), so quotations are
checked with all spaces removed; displays with Greek letters are garbled and are
read from the page images, not quoted. Read in full on 12 Oct 2026, with
footnotes and Table 1. The NBER working paper version (Drive `w0401.pdf`) is
not read. The entry_contest record of the same paper
(`papers/jite/endogenous_entry/entry_contest/lit/lazear_rosen_1981`) served
entry_contest's own claims and used SymPy; it was consulted, not copied, and
nothing here rests on it.

**Why this paper.** Novelty for firmworkers in three places: (i) workers
self-selecting by wealth into a rank-order contest that pays a binomial income
(§III, *Income Distributions*), against the lottery-versus-downgrade sorting of
M34 and the source's claim that low-rank workers prefer the lottery; (ii)
contests between heterogeneous types, where low types want to join high leagues
and sorting needs nonprice rationing (§IV), against the closure parameter of
M29; (iii) the paper's own pointer to Friedman (1953) as the only earlier work
on lotteries in occupations under Friedman–Savage preferences.

## Primitives (§II)

- Lifetime output `q_j = μ_j + ε_j`; skill `μ` costs `C(μ)`, `C', C'' > 0`;
  `ε` i.i.d. with mean zero and variance `σ²`; product value `V`.
- Piece rate `r`; two-player tournament with prizes `W_1 > W_2`, winner the
  larger `q`; `g` the density of `ε_k − ε_j`.
- Risk-neutral firms, free entry (zero profit).

## Derivation chain

1. **Risk neutrality (§II).** Piece rate `r = V = C'(μ)`. Tournament: Nash
   investment `C'(μ) = (W_1 − W_2) g(0)` (6); zero profit `Vμ = (W_1 + W_2)/2`
   (7); optimal prizes give `V = C'(μ)`, so both schemes are efficient and
   equivalent (9). Prizes (10) are expected product plus or minus an entrance
   fee `V/2g(0)`: a fair winner-take-all gamble over the fees. Levels of the
   prizes govern entry; the spread governs investment. With normal errors the
   spread rises with `V` and `σ`.
2. **Risk aversion (§III).** Second-order approximations: piece rate investment
   (16) and income variance (17); tournament (24) and (25). Investment and
   expected income are lower in the contest at given `s`; the contest's income
   variance is smaller when `σ²` exceeds `1/(sC''√π)`. With CARA the piece
   rate wins for all `σ²`; with `U = a y^a` (DARA) Table 1 shows contests
   preferred at low `σ²`, more so at higher endowed income.
3. **Income distributions (p.855).** "While it is not possible to make a
   general argument based on an example", richer workers prefer contests and
   poorer ones piece rates, and "Individuals will self-select the payment scheme
   in accordance with their wealth." Friedman (1953) assigns jobs by endowed
   wealth through Friedman–Savage utility; here workers are risk averse
   everywhere and gambles can still be optimal.
4. **Error structure.** A common shock drops out of a rank comparison.
5. **Heterogeneous contestants (§IV).** With private types, revenue functions
   are displacements of one another, so everyone prefers the high league; mixed
   contests are inefficient unless half the players are of each type; sorting
   needs credentials or nonprice rationing. With known types, the competitive
   handicap is `h* = Δμ/2`, gains are zero-sum, and without handicaps a
   reverse-discrimination prize schedule clears the market.

## What was reconstructed

Section II: the prize structure from the zero-profit and spread conditions, the
fair-gamble reading, the equivalence with the piece rate, and the normal spread
`2Vσ√π` increasing in `σ`. Section III: the variances (17) and (25) from the
piece rate of footnote 8 and from the spread of footnote 9 with the binomial
variance of p.855; the contest's lower investment; the variance comparison with
threshold `x = 1/√π`; the certainty equivalents of both schemes under (16),
(17), (24), (25) with `V = 1` and `C = μ²/2`, which put the piece rate ahead at
every `s` and `σ²`; the derivatives of `U = a y^a` and `s(y) = (1 − a)/y`;
the Table 1 comparisons. Section IV: the displacement of the revenue functions
and the result that players do not self-sort under a slope bound on `G`; the
efficiency condition for mixed contests; the handicap algebra (29)–(32) with
the zero-sum property, the competitive handicap, the preference for mixed play
below it and the reverse-discrimination schedule. See `CLAIMS.md`.

## What was not reconstructed

The existence of a pure-strategy Nash equilibrium (footnote 2, assumed by the
authors), the derivation of the approximations (16), (17), (24), (25) from
expected utility, the CARA claim on p.854, the numbers of Table 1 (not
recomputed; see `NOTES.md`), the linearisation of the win probability used for
(32), and Figure 1.
