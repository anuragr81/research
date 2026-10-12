# Reconstruction of Hopkins and Kornienko, 2004

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| HK | "Running to Keep in the Same Place: Consumer Choice as a Game of Status", *American Economic Review* 94(4), 2004, pp.1085–1107 | JSTOR PDF with a text layer, 24 PDF pages (a JSTOR cover, then pp.1085–1107), Drive `hopkins_kornienko_status_game.pdf`, id `1rQSjDPY7xk4Hg0a0nuj3h3N5QAvKXcBV`; cached `~/.cache/firmworkers/hopkins_kornienko_2004.pdf`, sha256 `84fbe499eb3f53cc8e745788a7efb82e68e3f1f30e8cd7af58c2c97b884660cc` | [F], published version |

The text layer interleaves the two columns, and in places places a word in the
wrong column, so four renderings of each page are kept, all derived from the
pinned PDF (poppler 25.02.0):

| Rendering | File in `~/.cache/firmworkers/` | Made by |
|---|---|---|
| layout | `hopkins_kornienko_2004.txt` | `pdftotext -layout` |
| reading order | `hopkins_kornienko_2004.raw.txt` | `pdftotext` |
| columns | `hopkins_kornienko_2004.cols.txt`, sha256 `c7b43f8a8f504feef48118f21ce3c66f5ce65fec5fb4be641f2c5dc8d4dfbbbe` | `lit/bbox_columns.py`: word boxes from `pdftotext -bbox`, each word assigned to a column by the right column's line starts near its height |
| OCR | `hopkins_kornienko_2004.ocr.txt`, sha256 `5b5cd03ea5feec8317631c141308d4a5efc56b3b0a71e4e3c5011442e60b8472` | `pdftoppm -r 250 -png`, then `lit/ocr_pages.py columns` (rapidocr-onnxruntime 1.2.3, scratch virtual environment) |

In the first three, form-feed block `p − 1084` is page `p` (block 0 is the
JSTOR cover); the OCR file opens each page with a form feed, so its block
`p − 1083` is page `p`. A quotation counts as found when it is verbatim on its
own page in at least one rendering. Read in full on 12 Oct 2026, with footnotes
and the Appendix. The 2009 and 2010 papers by the same authors (Drive
`hopkins_kornienko_status_game_2.pdf`, `_3.pdf`) are not read here. The
entry_contest record (`papers/jite/endogenous_entry/entry_contest/lit/hopkins_kornienko`)
covers the 2009 and 2010 papers for entry_contest's own claims; it was consulted,
not copied.

**Why this paper.** Novelty for the source manuscript's first question, "how
adaptability related expenditures (or social capital investments) respond to
rise in inequality": HK derive how spending on status responds to the income
distribution, with status as rank. Also a contrast for `h`: HK's status is won
by spending, while firmworkers' `h` is inherited standing that spending cannot
buy (decision of 11 Oct 2026).

## Primitives (§I)

- A continuum of agents, identical except for income `z` drawn from `G` on
  `[z̲, z̄]`, private information.
- Visible positional good `x` at price `p` and nonpositional good `y`, budget
  `px + y ≤ z`.
- Status (1): `S(x, F) = γF(x) + (1 − γ)F⁻(x) + a`, `γ ∈ [0, 1)`, `a ≥ 0` a
  guaranteed minimum; utility (2): `U = V(x, y) S(x, F)`, the form of a
  first-price auction.

## Derivation chain

1. **Equilibrium (Lemma 1, Proposition 1).** The symmetric equilibrium is
   strictly increasing, so rank in consumption equals rank in income; it solves
   the differential equation (6) with boundary `x(z̲) = z̲/p` if `a = 0` and the
   cooperative level if `a > 0`; everyone spends more on `x` than the
   cooperative level, with no gain in status.
2. **Exogenous status.** If status were given as `a + G(z)`, everyone would
   choose the cooperative level.
3. **Welfare (Proposition 2, Corollary 1).** Under second-order stochastic
   dominance with a finite number of crossings, those below the first crossing
   are worse off in the more equal or richer society ("misery loves company");
   under first-order dominance utility falls at every income.
4. **Spending (Propositions 3, 4, Corollary 2).** A higher `a` lowers spending.
   Under the unimodal likelihood ratio order (more equal), the middle class
   spends more; with `a = 0` the poor spend more too, with `a > 0` they spend
   less; the rich may spend less.
5. **Taxes (§III, Proposition 5).** A consumption tax and subsidy implement the
   cooperative allocation; with more equality the middle class's rates rise;
   Examples 1 and 2 give constant and decreasing schedules.

## What was reconstructed

The tie penalty of (1); the comparison behind "misery loves company" at a
fixed allocation; for `U = xyS` the cooperative tangency, the reduction of the
tax formula (12) to `zg/(G + a)`, Example 1 (the equilibrium `2z/(3p)` solves
(6) with `a = 0`, exceeds the cooperative `z/(2p)`, leaves `z/3` for `y`;
`τ = 1`; the revenue-neutral 1/3 tax and subsidy), and Example 2 (`G_A` is a
distribution function with density `6z(1 − z)` and median 1/2; its integral
lies below the uniform's with equality at 1, so the uniform is a
mean-preserving spread; the density ratio is unimodal; `τ_A`, the revenue-neutral
rate `3(1 − z)/(6 − 5z)`, its end points and its decrease; status lower below
the median and higher above it). See `CLAIMS.md`.

## What was not reconstructed

Existence, uniqueness and monotonicity of the equilibrium (Proposition 1 and
Lemma A1), the welfare and spending comparisons of Propositions 2–5 and the
corollaries (they rest on comparison arguments for the differential equation
(6)), and Figures 1 and 2.
