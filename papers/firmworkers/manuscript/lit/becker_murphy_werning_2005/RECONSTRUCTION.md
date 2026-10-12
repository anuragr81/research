# Reconstruction of Becker, Murphy and Werning, 2005

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| BMW | "The Equilibrium Distribution of Income and the Market for Status", authors' manuscript "Revised December 2003", 32 pages numbered 1–32; published as *Journal of Political Economy* 113(2), 2005, pp.282–310 | PDF with text layer (LaTeX, `bmw_home.dvi`), 32 pages, Drive `The Equilibrium Distribution of Income and the Mar.pdf`, id `1xqR6As4CQRLV6Ql8iNdB6Arno4oCQhH9`, uploaded 1 Sep 2026; cached `~/.cache/firmworkers/bmw_2005.pdf`, sha256 `62df91b717e95d67474a7b4d9caa032f21451cd639be5e637463eccc13cf87f9` | [F], authors' manuscript; the JPE version is not read |

Text extracted with `pdftotext -layout`; page `p` is form-feed block `p − 1`
and every block ends with its page number. Displays on p.23 were read from the
page image at 110 dpi. Read in full on 12 Oct 2026. The manuscript is cited as
the JPE article with `version_read` stating what was read (author's rule of
10 Oct 2026: working versions are acceptable unless the published version adds
claims). Pages here are the manuscript's.

**Why this paper.** The second precedent, with Robson (1992), for the
reference-dependent sub-section decided on 12 Oct 2026: status raises the
marginal utility of consumption, and that alone makes people with concave
utility of income take fair gambles. Needed for the novelty check. It also
states, as an empirical implication, the contrast between societies where
status is endowed by family background and those where it is bought, which
bears on the reading of `h` as inherited standing.

## Primitives (§2–§3, pp.5–11)

- Identical utility `u(c, s)` over consumption and status, `u_c > 0`,
  `u_cc < 0`, `u_s > 0`, and the crucial complementarity `u_cs > 0` (p.5).
- Status positions are in fixed supply (p.6), normalised uniform on `[0, 1]`
  for a continuum of agents (p.9).
- Two stages: fair lotteries over income, then a hedonic status market with a
  price schedule `P(s)`, `P(0) = 0`; or an implicit market in a worthless "social
  good" `z` that ranks people (§3.2).

## Derivation chain

1. **Two-person example (§2, pp.6–9).** The utility frontier has a convex kink
   where high status switches between A and B. Optimal lotteries equalise
   `u_c(c_h, s_h) = u_c(c_l, s_l)`; with `u_cs > 0` this gives `c_h > c_l`.
   With separability winners do not consume more; with substitutes they consume
   less (p.8).
2. **Explicit market (§3.1).** Positive sorting `s(y) = Φ(y)`; price ODE (2),
   `P′(s) = u_s/u_c`; normality of status gives single crossing.
3. **Implicit market (Proposition 1, p.14).** Any explicit-market equilibrium is
   replicated by `p_z r(s) = P(s)` with status assigned by rank in `z`.
4. **Fair lotteries (§4.1).** On the support of an individual's lottery,
   `v(y; P) = λ_i y + φ_i`, so `u_c` is constant there.
5. **Main result (Proposition 2, p.19).** The ∗-allocation holds `u_c(c∗(s), s) = λ`
   with `∫c∗ = ω̄^c`; `P∗` integrates (2); `y∗ = c∗ + P∗`. If `Φ∗` is a
   mean-preserving spread of the initial full-income distribution `F`, then ∗ is
   a competitive lottery equilibrium. Without lotteries, a compensating
   differential gives `v` a convex kink at the common income (p.21), so there
   are no mass points.
6. **Power law (§4.3, p.23).** `u = c^α s^β`: `c∗(s) = ω̄(1 + δ)s^δ`,
   `P∗(s) = (β/(αδ))ω̄(1 + δ)s^δ`, `δ = β/(1 − α)`, `κ = 1 + δ`.
7. **Planner (Proposition 3, p.24).** The utilitarian planner's first-order
   condition is the same, so `c^p = c∗`.
8. **Empirical implications (§6).** The same final distribution for a range of
   initial ones; redistribution undone by lotteries; open societies (status
   bought) show more income inequality than structured ones (status endowed);
   gambling through occupations and entrepreneurship rather than lotteries.
9. **Income rank (§7, Proposition 4).** With status as income rank the main
   result carries over, but winners lower others' ranks: real externalities, and
   the planner's and the market's distributions differ.

## What was reconstructed

Item 1 (all three cases), the convex kink of item 5, the linear-value
indifference of item 4 for finite fair lotteries, and item 6 (the constant
marginal utility, the resource constraint by integration, the price slope as a
derivative, the closed form of `y∗`, its inverse `F∗`, and the top of the
support). See `CLAIMS.md`.

## What was not reconstructed

Proposition 1 (the implicit market), the single-crossing argument, the
mean-preserving-spread step of Proposition 2, Proposition 3 beyond the shared
first-order condition, Proposition 4 and the rank-model derivative (7).
