# Hitsch, Hortaçsu and Ariely (2006), reconstruction

Günter J. Hitsch, Ali Hortaçsu and Dan Ariely, "What Makes You Click? — Mate
Preferences and Matching Outcomes in Online Dating", MIT Sloan Working Paper
4603-06, February 2006 (SSRN 895442). Cited as the working paper. The later
journal articles drawn from the same data are not read and nothing is
attributed to them (HHA-D5).

## Source read

- Cached at `~/.cache/paper_a/sources/hitsch_2006.pdf` (1,578,181 bytes),
  sha256 `f0def83b…bf40f`, with the text layer at
  `~/.cache/paper_a/sources/hitsch_2006.txt`, sha256 `4698b2a0…f6e294`. 63 PDF
  pages. The text has 64 form-feed segments. Segment `n − 1` is PDF page `n`,
  and segment 63 is empty. PDF page 1 is the MIT Sloan cover. From PDF page 2
  on the printed page number is the PDF page minus one (HHA-D1).
- Every page, 1 to 63, was read from the text on 16 October 2026, so the
  evidence tag is [F]. PDF page 25, where the extraction garbles eq. (5) and
  footnote 21, and PDF page 62, Figure 5.10, were also read from 110 and 80 dpi
  images rendered with `pdftoppm`. The figures on PDF pp. 54 to 63 are read
  from their captions and, for Figure 5.10, the image. Pages are cited as PDF
  pages.

## The data

About 22,000 users of one online dating site, 10,721 in Boston and 11,024 in
San Diego, observed for three and a half months in 2003 (PDF pp. 6–7). A
profile shows age, education, height, weight, income, ethnicity, religion and
more, and photos for about a quarter of users (HHA-Q2). 65 percent of men and
53 percent of women report income (PDF p. 22). The data record who browsed
whom and who sent a first-contact e-mail to whom.

## The model, from its own primitives

1. **Revealed preference** (PDF pp. 4, 12). A user who browses two profiles and
   writes to one prefers that one. Man `m` writes to woman `w` iff
   `U_M(m, w) ≥ v_M(m)` (eq. 1), a threshold rule (HHA-Q1).
2. **Search** (PDF pp. 12–14). Adachi's (2003) stationary search model with
   mutual acceptance, Bellman equations (2), and reservation utilities that act
   as person-specific prices.
3. **Stability** (PDF pp. 13–14). As search costs vanish, the equilibrium
   matches of the search model are the Gale–Shapley stable matches, eq. (3).
4. **Strategic behaviour** (PDF pp. 15–18). If writing is costly, an
   unattractive man may not write to an attractive woman. Figure 4.1 shows the
   probability of writing rises in the partner's looks whatever one's own looks,
   which the authors read as little strategic behaviour.
5. **Outcome regressions** (PDF pp. 18–24). Under assumption 1, that all men
   rank women alike and all women rank men alike (HHA-Q10), and assumption 2,
   equal sampling, the number of first contacts a user receives is monotone in
   the common utility index. Poisson regressions of first contacts on 304
   attributes. For a priori segments, such as education levels or ethnic
   groups, the count of contacts received from each segment is regressed
   separately.
6. **Discrete choice** (PDF pp. 24–28). Eq. (5),
   `U_M(X_m, X_w; θ_M) = x_w′β_M + (|x_w − x_m|′₊)^α γ⁺_M + (|x_w − x_m|′₋)^α γ⁻_M
   + Σ_{k,l} I{d_mk = 1 and d_wl = 1} ϑ^{kl}_M + ε_mw`. The level term is
   vertical, the two distance terms let the valuation depend on one's own
   attribute, and `ϑ^{kl}` is a preference of a man of category `k` (say white)
   for a woman of category `l` (say black). With person fixed effects and
   `α = 1` the model is not identified, because
   `x_w − |x_w − x_m|₊ + |x_w − x_m|₋ = x_m` (footnote 21). So the fixed
   effects logit uses `α = 2` (HHA-Q11), and a random effects probit uses
   `α = 1` with reservation values independent of covariates.
7. **Matching simulations** (PDF pp. 31–36). Preferences from the logit feed
   the Gale–Shapley algorithm, 50 draws of the shocks. Counterfactuals set the
   shock, the distance terms, the physical terms or the race terms to zero.

## Results and what each is derived from

- **Looks** dominate. They explain 18 percent of the variance in first contacts
  for men and 30 percent for women, against 7 and 4 percent for income
  (Table 5.2, PDF p. 45).
- **Income** is vertical. Men's first contacts rise with income above $50,000,
  by 34 to 151 percent, and women's barely move (PDF p. 22). In Table 5.3 the
  income level coefficient is 0.0068 for men and 0.0171 for women (logit), and
  the distance coefficients are printed as 0.0000 in the logit and are at most
  0.0007 in absolute value in the probit. The authors conclude own income
  "matters only slightly" (HHA-Q5).
- **Education** is homophilous in part. Segmenting by education, women prefer
  men of equivalent education (HHA-Q7), and in the discrete choice both sexes
  prefer a partner of similar education (HHA-Q6). The probit puts this on the
  distance terms, with men avoiding more educated women and women avoiding less
  educated men, and the logit puts more of it on levels (PDF p. 28).
- **Ethnicity**. 38 percent of women and 18 percent of men state a preference
  for their own ethnicity (PDF p. 23). In behaviour, every group contacts the
  other groups less, with the other attributes, income and education among
  them, held fixed (HHA-Q3, HHA-Q4). Users who state no preference also act on
  one (HHA-Q15). In the logit, white men's term for a black woman is −0.9584
  (s.e. 0.1226) and white women's for a black man −0.7159 (0.1565).
- **Trade-offs** (Tables 5.4 to 5.6). A black man needs $154,000 more income to
  be as successful with a white woman as a white man earning $62,500 (HHA-Q8).
  Women mostly cannot compensate with income (HHA-Q9), because men's marginal
  utility of income is flat between $100,000 and $200,000 and falls above
  $200,000 (PDF p. 28).
- **Sorting**. Gale–Shapley on the estimated preferences reproduces the
  correlations in observed online matches (Table 6.1) and, without the shock,
  those in Census marriages. Setting the distance terms to zero collapses the
  income and education correlations, so sorting is "largely driven by
  preference heterogeneity" (PDF p. 34). Same-race preferences generate less
  endogamy than offline marriages show (HHA-Q14), unless the shock is removed.

## What is formalised

`lean/mathlib/HitschEtAl2006.lean`, in the paper's notation. The table
"Coverage of the source's results" in `CLAIMS.md` lists every numbered item.

- Section 3. The threshold rule (eq. 1), the expected utility of a meeting, the
  Bellman equations (eq. 2) as a definition on a finite market, and
  Gale–Shapley stability for two men and two women.
- Section 5.1. Under assumptions 1 and 2 the contacts received rise with rank
  (`received_mono`), the incidence rate ratio, and the college premium with the
  inconsistency of HHA-D2.
- Section 5.2. Eq. (5) for one attribute, footnotes 20, 21 and 23, the logit
  probability, and the reduction to A1's form (`symmetric_quadratic`).
- Section 6.3. In the two by two case, vertical preferences and preferences for
  similarity both make assortative matching the only stable one
  (`sorting_observationally_equivalent`).
- The arithmetic of the trade-off tables, the random matching benchmark,
  Table 6.2's column sums and the BMI constant.

## What could not be reconstructed

- The estimates, from data that are not available. The full outcome
  regressions are "available from the authors" (PDF p. 20, n. 16).
- Adachi's (2003) results, that eq. (2) has a lattice of solutions and that its
  matches converge to the Gale–Shapley stable set as `ρ → 1` (eq. 3), and the
  men-optimal and women-optimal stable matchings. The source cites them and
  does not prove them.
- Eq. (5) in general. It is formalised for one attribute, without the
  categorical terms and the shock, and stability for two men and two women.
- The probit probability, which needs the normal distribution function and from
  which nothing is derived.
- Table 5.6 exactly. With the rounded logit coefficients and the distance
  terms left out, the compensation for a black man lies between $148,000 and
  $149,000, against the printed $154,000 (HHA-D3). Which model the trade-offs
  use is not stated, and the logit is our assumption.
- The sign convention of the distance terms in the women's equation, which the
  paper writes only for men. It is read symmetrically (HHA-D4).
