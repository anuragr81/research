# Eeckhout (2006), reconstruction

Jan Eeckhout, "Minorities and Endogenous Segregation", *Review of Economic
Studies* 73(1), January 2006, pp. 31–53.

## Source read

- The published article, 23 PDF pages, PDF page `n` being journal page
  `n + 30`. Quotations cite PDF pages.
- Downloaded on 15 October 2026 from the author's site,
  `https://www.janeeckhout.com/wp-content/uploads/07.pdf` (recorded in
  `~/.cache/paper_a/sources/urls7.txt`). The
  file is the publisher's typeset PDF (TnQ Books and Journals, January 2006).
- Cached at `~/.cache/paper_a/sources/eeckhout_2006.pdf`, 185,106 bytes,
  sha256 `3ea91484…80bb`. The PDF has a text layer, extracted to
  `~/.cache/paper_a/sources/eeckhout_2006.txt`, sha256 `7eb8e21f…9095`, one
  form feed per page.
- Every page was read in full from the text on 16 October 2026. The text
  layer drops `Π`, `≠` and bars, so PDF pages 4, 10, 13, 14, 15, 20 and 22
  were also read as 110 dpi images. The evidence tag is [F].

## The model, from its own primitives

1. **Agents** (PDF pp. 3 to 4). A continuum of infinitely lived agents in
   discrete time, with a mass one of new agents born each period. Agents are
   identical except for a colour `c ∈ C` with population shares `π(c)`.
   Colour is payoff irrelevant, costlessly observable and cannot be hidden
   (EE-Q7, EE-Q8).
2. **Stage game** (PDF p. 4). Matched pairs play a symmetric prisoner's
   dilemma with payoffs `(1, 1)` for `(C, C)`, `(−l, 1 + g)` for `(C, D)` and
   `(0, 0)` for `(D, D)`, with `g, l > 0` and `g − l ≤ 1`. There is no role
   identification.
3. **Matching** (PDF p. 4). Singles are matched uniformly at random. After
   each period either partner can end the match, and the separated return to
   the pool. Matching is anonymous, so the only history a player has is that
   of the current match.
4. **Payoffs and equilibrium** (PDF p. 5). Normalised discounted payoffs
   with factor `δ < 1`, eq. (1). The equilibrium is a stationary perfect
   public equilibrium.
5. **What colour does** (PDF pp. 1, 3, 9). Colour is a public randomisation
   device. Agents condition play on whether the match is same-colour or
   mixed (EE-Q1, EE-Q9, EE-Q10). Any other public randomisation device would
   do the same job (EE-Q6). Nothing is inferred from colour.

## Derivation chain

- **Incubation** (PDF pp. 5 to 6). Playing `(D, D)` in the first period and
  `(C, C)` after gives `v_I = δ`, and it is an equilibrium when `g ≤ δ`, eq.
  (2). Formalised as `incubation_value`, `incubation_ic`, and for `k`
  periods (footnote 13) as `k_incubation_ic`.
- **Constrained efficiency** (PDF p. 6). No deviation in the cooperation
  phase needs `v(s) ≤ V̄ = 1 − g(1 − δ)/δ`, eq. (3). Formalised as `eq3`.
- **Proposition 1** (PDF p. 7, proof PDF pp. 17 to 19). Generically the best
  colour-blind equilibrium has `v(s*) < V̄`. The proof reduces constrained
  efficiency with interior mixing to the polynomial condition (A.5) and
  argues it fails off a lower-dimensional set. Lemma A1 (continuation
  probabilities are 0 or 1), Lemma A2 (`σ = 1` is not an equilibrium), the
  case `σ = 0` and the formula for interior `σ` are formalised as
  `lemmaA1`, `lemmaA2`, `prop1_incubation` and `sigma_interior`. The
  genericity step is not.
- **Proposition 2** (PDF p. 10, proof PDF pp. 19 to 20). With symmetric
  shares some segregation strategy Pareto dominates `s*`. In the incubation
  case, cooperating at once in a same-colour match gives
  `v(s^S) = π + (1 − π)δ > δ`, and the incentive constraint holds when
  `π ≤ 1 − g/δ`. Formalised as `prop2_incubation_pareto` and
  `prop2_incubation_ic`, and for any `v(s*) < 1` as `inclusive_pareto`.
  The interior-mixing case uses the implicit function theorem and is not
  formalised.
- **Proposition 3** (PDF p. 13, proof PDF p. 21). With shares other than
  one half, symmetric type-dependent strategies are constrained
  inefficient. The step is `(1 − 2Π)(v(s_i) − v(s_{−i})) = 0`. Formalised
  as `prop3_step`.
- **Proposition 4** (PDF p. 14, proof PDF p. 21). Minority bias, eqs. (4)
  and (5), raises aggregate welfare over symmetric segregation when
  `π > 1/(2 + g − l)`, subject to (A.10) and (A.11). Formalised as
  `prop4`, `prop4_identity`, `prop4_welfare`, `eqA10`, `eqA11` and
  `minority_bias_vb_gt_vw`.
- **Exclusive segregation** (PDF pp. 14 to 15, derivation PDF p. 22).
  Cooperate forever with one's own colour and defect and rematch otherwise.
  The white share among singles `Π` solves
  `Π²(1 − 2π) + 2πΠ − π = 0`, with positive root
  `(−π + √(π(1 − π)))/(1 − 2π)`. The majority's incentive constraint holds
  when `Π < Π_0 = 1 − g/((1 + g)δ)`, and the minority does worse than under
  incubation when `Π > Π_1 = 1/(1 + δ)`. The interval is non-empty when
  `g < δ²/(1 + δ − δ²)`. Formalised as `singles_quadratic`, `singles_root`,
  `majority_ic`, `minority_worse` and `interval_nonempty`. The bounds
  `1/2 < Π < π` and the dominance of inclusive over exclusive segregation
  for the majority are `singles_share_bounds` and
  `exclusive_below_inclusive`.
- **Exogenous randomisation** (PDF pp. 15 to 16). Cooperating below a
  threshold `q̄` of a uniform draw meets the constraint when
  `q̄ ≤ 1 − g/((1 + g)δ)`. Formalised as `public_randomization`.
- **Numbers** (PDF pp. 7, 8, 11, 12). `V̄ = 0·875`, the `l = 0·5` example
  (`σ = 0·3545`, `v(s*) = 0·8063`) and the `l = 1` row of Table 2 are
  formalised as `Vbar_example`, `example_l05`, `example_l05_numbers`,
  `table2_l1` and `table2_discrepancy`.

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every numbered result. The gaps are
these.

- **Proposition 1, the genericity step.** (A.4) and (A.5) reduce constrained
  efficiency to a polynomial condition, and the claim that its zero set is
  nowhere dense is not formalised.
- **Proposition 2, the interior case.** The implicit-function argument on
  (A.6) and (A.7), with signs the paper calls generic, is not formalised.
- **Tables 1 and 2.** Only the `l = 0·5` example of Table 1 and the `l = 1`
  row of Table 2 are recomputed. One entry of that row disagrees with its
  formula (EE-D8).
- **Age-dependent strategies.** The stationary measures (A.8) and (A.9), the
  value `0·8238` and §4.1.2 are not formalised.
- **`Π` increasing in `π`** (§4.2.3) is not formalised.
- **Heterogeneity.** The model has none beyond colour. The author names
  heterogeneity as in Burdett and Coles (1997) as an extension (EE-Q14).
  Burdett and Coles (1997) is not read.
