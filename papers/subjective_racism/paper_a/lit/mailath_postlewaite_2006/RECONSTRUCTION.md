# Mailath and Postlewaite (2006), reconstruction

George J. Mailath and Andrew Postlewaite, "Social Assets", PIER Working Paper
06-003, Penn Institute for Economic Research, dated January 5, 2006 ("first
version February 7, 2002"). The cover calls it "Social Assets, Third
Version". Cited as the working paper. The published article (International
Economic Review, 2006) is not read (MP-D1).

## Source read

- `~/.cache/paper_a/sources/mailath_postlewaite_2006_wp06.pdf`, 41 PDF pages,
  sha256 `3ec1560c4956c2c0c2eb778a16236711812e87ca26a0e0aff96814ab3f7da7d9`.
  PDF page 1 is the PIER cover, PDF pages 2 to 38 are the paper (printed pages
  1 to 37, so the printed page is the PDF page minus 1), and PDF pages 39 to 41
  are an SSRN abstract page. PDF page 42 of the text file is empty.
- Text layer `~/.cache/paper_a/sources/mailath_postlewaite_2006_wp06.txt`,
  sha256 `8e71e59236fc19d0a2af2687942fca6e8b173cef5ca9be87645eea96ac760827`.
  The text layer renders ligatures and Greek letters as other symbols (MP-D3).
- Read in full from the text layer on 16 October 2026, every page. PDF pages
  7, 9, 11, 19 and 20 were also rendered at 110 dpi and read as images to
  check the equations. Evidence tag [F].
- The two earlier versions were also read in full on 16 October 2026, PIER WP
  04-025 (June 4, 2004, 38 PDF pages,
  `~/.cache/paper_a/sources/mailath_postlewaite_2006_wp.pdf`, sha256
  `2acab67a…c20ff`, page 20 also read as an image) and PIER WP 02-003
  (February 7, 2002, 39 PDF pages, `~/.cache/paper_a/novelty/mp_2002.pdf`,
  sha256 `6358be5b…6bf1f`). Their differences are recorded in MP-D9 and
  MP-D10. Nothing is quoted from them.

All page numbers below are PDF pages of the 06-003 file.

## The model, from its own primitives

1. **Agents** (p. 6). An infinite sequence of two-period lived generations,
   each a continuum of men and women. In the second period, men and women
   match, consume their joint income (a public good within the couple), and
   have two children. A couple's utility is `U` of its consumption plus `β`
   times the average utility of its children, so it depends on every later
   generation. `U` is concave.
2. **Attribute** (p. 6). A child acquires the attribute (`y`) or not (`n`)
   only from its parents, for sure if both have it, never if neither has it,
   with probability `ρ` if one has it, and both children alike. The attribute
   does not enter utility.
3. **Income** (p. 6). Income is `H` or `L`, drawn independently of the
   parents' incomes, with probability `1/2 + k` of `H` for a `y` agent and
   `1/2 − k` for an `n` agent, `k ≥ 0`. `k = 0` is the unproductive attribute.
   Normalisation `U(2L) = 0`, `U(2H) = 1`, `U(H + L) = u ∈ [1/2, 1)`.
4. **What is seen** (pp. 7–8). "An agent's income level and the
   presence/absence of the attribute together constitute that agent's
   characteristic" (MP-Q6). Matching "occurs after income is realized"
   (MP-Q7), and the matching conditions are written on the four
   characteristics `Hy`, `Hn`, `Ly`, `Ln`. Income is therefore known to a
   prospective partner at the time of matching. Nothing is inferred from the
   attribute. The paper says so in its discussion of Fang (2001): "there is no
   asymmetric information, hence no role for signalling" (MP-Q14).
5. **Stability and equilibrium** (p. 7). A matching is stable if no pair of
   agents can both strictly gain by matching, taking account of the
   consequences for descendants. No side payments. An equilibrium is a
   sequence `{μ^t, m^t}` in which the distribution of attributes `μ^t` is
   induced by the characteristics and matching of period `t − 1` and each
   period's matching is stable. The equilibrium is anonymous.
6. **Values** (p. 8). Average discounted values. `V_y`, `V_n` are the values
   of having and lacking the attribute before income is realised.

## Derivation chain of the main results

- **Assortative matching** (pp. 7–9). Under `Hy–Hy`, `Hn–Hn`, `Ly–Ly`,
  `Ln–Ln`, `V_y^A = 1/2 + k` and `V_n^A = 1/2 − k` (`assort_values`). An
  `Hn`, `Ly` pair blocks if and only if `1 − u < βk/(1 − β) < u` (3.1).
  Proposition 1 (`prop1`): assortative matching is stable if and only if
  `βk ≤ (1 − u)(1 − β)` or `u(1 − β) ≤ βk`. With `k = 0` it is always stable
  (`assort_stable_unproductive`).
- **Mixed matching** (pp. 9–10). `Hn` matches `Ly`, the others match their
  own characteristic. The value equations (3.2), (3.3) give (3.4),
  `V_y^M − V_n^M = (1 + 2k)(1 − β)/(2 − β(1 + 2k))` (`eq_3_4`). The two
  incentive constraints (3.5) and the `Ly` constraint combine into (3.6), and
  with (3.4) into Proposition 2 (3.7) (`prop2`).
- **Unproductive attribute** (pp. 10–11). With `k = 0` the right inequality
  of (3.7) holds for every `u ≥ 1/2` (`right_ineq_free`), and the left one is
  (3.8), `u ≥ (4 − 3β)/(2(2 − β))` (`eq_3_8`). Both matchings are then
  stable, and the attribute has value only in the mixed one.
- **Pareto ranking** (pp. 12, 32–33). Proposition 3: with `k = 0` the mixed
  matching weakly Pareto dominates the assortative one if and only if (3.8)
  holds. The appendix solves (3.2), (3.3) at `k = 0` (`values_k0`) and
  compares each characteristic's value (`prop3_Hn`, `prop3_Ln`). The welfare
  gain is insurance of descendants' consumption and transfer to them, which
  have no market. The variance of consumption is lower under the mixed
  matching (MP-Q4, MP-Q11).
- **Emergence and collapse** (pp. 13–17). A once-productive attribute can
  leave a mixed matching that remains stable (Section 3.4). Proposition 4:
  with decreasing relative risk aversion, a small per-period probability of a
  permanent income rise supports mixed matching until the rise and
  assortative matching after it. These are perturbation arguments around the
  Section 3 values and are not formalised.
- **General case** (pp. 17–21). The law of motion (5.1) for `μ_y`.
  Proposition 5 (rationing when `μ_y ≠ 1/2`), 6 (`ρ < 1/2`) and 7 (`ρ > 1/2`,
  unravelling). Our reading of the rationed difference and threshold (5.2)
  differs from the printed one (MP-D4, `rationed_diff`, `rationed_HnIC`).
- **Endogenous attribute** (pp. 21–28). Parents can buy probability `q` of
  the attribute at cost `c(q)`. Proposition 8 (existence of a steady state, by
  Brouwer) and Examples 1 and 2, in which more agents hold the productive
  attribute under the mixed matching. Not formalised.
- **Discussion** (pp. 28–32). A non-matching example (white-shoe lawyers),
  the contrast with Fang (2001), and the contrast with social capital
  sustained by sanctions.

## What each result is derived from

Every result rests on joint consumption (so everyone prefers a richer
partner, all else equal, MP-Q5), on the absence of any way to transfer
consumption or insure descendants, and on stability with no side payments.
The value of the attribute comes from the matching prospects of the children
who inherit it (MP-Q9), not from anything the attribute reveals.

## What could not be reconstructed

- Propositions 4, 6, 7 and 8 and Examples 1 and 2 are limit, continuity and
  fixed-point arguments. They are read, not formalised.
- The published version is not read. Whether MP-D4 is corrected there is
  unknown.
