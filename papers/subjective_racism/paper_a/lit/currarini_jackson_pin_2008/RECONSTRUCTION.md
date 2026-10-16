# Currarini, Jackson and Pin (2008), reconstruction

Sergio Currarini, Matthew O. Jackson and Paolo Pin, "An Economic Model of
Friendship: Homophily, Minorities and Segregation", working paper, January
2007, revised 29 July 2008. Published in *Econometrica* in 2009, which is not
read (CJP-D1).

## Source read

- The revised working paper of 29 July 2008, 46 PDF pages, whose page numbers
  equal the printed ones. Cached at `~/.cache/paper_a/sources/cjp_2009_wp.pdf`,
  sha256 `6a46cd21…10dfb9`. Text layer at
  `~/.cache/paper_a/sources/cjp_2009_wp.txt`, sha256 `8c250720…30b0`, with a
  47th, empty page. The cache file name says 2009, and the title page says
  "January 2007, Revised: July 29, 2008".
- Read in full on 16 October 2026 from the text layer, every page, so the
  evidence tag is [F]. Page images at 110 dpi were read for PDF pages 18, 19,
  26 and 27. The plotted values of Figures 2 to 9 are not attributed.

## The model, from its own primitives

1. **Measures** (PDF pp. 5–7). `H_i = s_i/(s_i + d_i)`, relative homophily
   (`w_i > w_j` implies `H_i > H_j`), baseline homophily `H_i = w_i`,
   inbreeding homophily `H_i > w_i`, heterophily `H_i < w_i`, and Coleman's
   `IH_i = (H_i − w_i)/(1 − w_i)` (Definitions 1 to 6).
2. **Data** (PDF pp. 7–12). Add Health, 84 schools, 305 race-by-school
   observations. In the example school of Table 1, `IH` is 0.69 for whites
   and 0.76 for blacks (CJP-Q3). Across schools `H_i` rises with `w_i`
   (slope .99, t = 31), friends per capita rise with `w_i` (slope 1.9,
   t = 5.0, intercept 5.9), and `IH_i` is an inverted U in `w_i`
   (coefficients 2.1 on `w_i` and −2.2 on `w_i²`, or 2.1 with s.e. .07 on
   `w_i(1 − w_i)` without intercept), peaking near .8 for middle-sized groups
   (CJP-Q2, CJP-Q4).
3. **Preferences** (PDF pp. 12–14). Types `1, …, K`. Utility `U(s, d)` over
   numbers of same-type and different-type friends, the same for every type
   (CJP-Q5, CJP-Q6), strictly increasing, strictly concave in each argument,
   with overall diminishing returns, `U(as, ad) < aU(s, d)` for `a > 1`.
4. **Meetings** (PDF pp. 14–15). A continuum enters at rate `N_i` and pays
   `c` per unit of time in the matching. Every meeting becomes a friendship,
   so nobody is rejected (CJP-Q7), and the only choice is the time `t_i` to
   stay. A matching process maps stocks `M_i = N_i t_i` to meeting
   probabilities `q_ij`, balanced so that `M_i q_ij = M_j q_ji`. Unbiased
   matching is `q_ij = M_j/M`.
5. **Choice and equilibrium** (PDF pp. 16–17). Type `i` maximises
   `U(q_i t, (1 − q_i)t) − ct` (eq. 2), with first-order condition (3). A
   steady-state equilibrium is `(t_i, M_i, q_i)` with optimal `t_i`,
   `M_i = N_i t_i`, `q = F(M)` and balance. In equilibrium `H_i = q_i`.
6. **Example** (PDF pp. 17–19). `U = (s + γd)^α` gives
   `t_i = (α/c)^{1/(1−α)} (γ + (1 − γ)q_i)^{α/(1−α)}`, eq. (4) gives `q_i` in
   closed form at `α = 1/2`, and at `γ = 1/2`, `w = (2/3, 1/3)` the solution is
   `q₁ = √3 − 1`, `q₂ = 2 − √3`.
7. **Benchmarks** (PDF pp. 19–20). Type-neutral preferences give every type
   the same number of friends (Proposition 1, CJP-Q8). Unbiased matching
   cannot make every group homophilous (Proposition 2, CJP-Q9).
8. **Two types, unbiased matching** (PDF pp. 20–24). Equilibrium exists and
   one with relative homophily exists (Proposition 3), unique if
   `t(q)(1 − q)` is decreasing (Claim 1). Under same-type bias (Definition 7),
   the larger group forms more friendships, is inbreeding homophilous, and the
   smaller group is heterophilous (Proposition 4). Under strong same-type bias
   (Definition 8), `t_i` rises with `w_i` across equilibria (Proposition 5).
   Eq. (6) shows that one group's homophily is the other's heterophily.
9. **Biased matching** (PDF pp. 24–28). `q₁^β + q₂^β = 1` (eq. 7) with `β > 1`
   lets all groups meet their own type above their stock share. The bias is
   attributed to meeting through friends, clubs and activities (CJP-Q10),
   and the fit needs it largest for small groups (CJP-Q11). The grid fit
   gives `α = .2`, `β = 2`, `γ = .8`, `c = .04`. Meeting bias carries the
   inverted U and taste carries the friend counts (CJP-Q12). Increasing
   returns in meeting could replace the taste (CJP-Q13).
10. **Many types and welfare** (PDF pp. 28–34). Proposition 6 extends
    Proposition 4. Under homogeneous preferences a group's welfare is
    proportional to its friend count (Proposition 7), and aggregate welfare
    trades friend counts against homophily (Proposition 8). The welfare
    optimum depends sharply on preferences.

## Derivation chain of what Paper A1 uses

- `IH_i > 0` exactly when `H_i > w_i` (`IH_pos_iff`, `IH_neg_iff`), and in
  equilibrium `H_i = q_i` (`H_eq_q`).
- Proposition 1. Under type neutrality the objective `V(t) − ct` does not
  depend on `q`, so the optimal time is common (`prop1`).
- Proposition 2. Under unbiased matching `Σ q_i = Σ w_i = 1`, so not every
  `q_i` can exceed `w_i` (`prop2`). With two types this is eq. (6) (`eq6`).
- The Section 4.5 example (`t_half`, `example_45`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every numbered result, and the gaps are these.

- Existence with many types (Proposition 6), which uses Brouwer's theorem,
  and the random-matching foundation of Appendix A (Lemmas 2 and 3). The
  ordering part of Proposition 6 is formalised, and its last step holds only
  when the smaller type is at or above baseline (CJP-D8).
- The steady state is formalised for two types with unbiased matching. The
  general matching process `F` and the `K`-type steady state are not.
- Propositions 4 and 5 are stated through the marginal return
  `φ(q, t) = ∂/∂t U(q t, (1 − q) t)`, with diminishing returns as strict
  decrease of `φ` in `t`. Utility functions themselves are not built.
- The fit of Section 7.2, Figures 6 to 9 and footnote 47's equilibria, all
  numerical.
- The Hispanic value in the example school (CJP-D2).
- The second derivative in Appendix D, of which only the sign of the printed
  numerator is checked.
- The Supplementary Material on satiated preferences, which is not read.
