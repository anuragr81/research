# Claims about Hu, Immorlica and Vaughan (2019)

**Source.** Lily Hu, Nicole Immorlica and Jennifer Wortman Vaughan, "The Disparate Effects of
Strategic Manipulation", *Proceedings of the Conference on Fairness, Accountability, and
Transparency (FAT* '19)*, ACM (2019), 259-268. Read in full from arXiv:1808.08646v4
[F, arXiv version]. Cited in `refs.bib` as `HuImmorlicaVaughan2019`. Pages are PDF pages.

Every quotation is checked verbatim, on the page named, against the PDF's text layer in layout
and in reading order, ignoring case, punctuation and spaces. The PDF is pinned by sha256. In the
text layer `ℓ` prints as a backquote. The results we use are proved in
`lean/mathlib/HuImmorlicaVaughan.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HIV-1 | 1 | "we adapt models of strategic manipulation to capture dynamics that may arise in a setting of social inequality wherein candidate groups face different costs to manipulation" | The question |
| HIV-2 | 4 | "Manipulating from a feature vector x to y is always at least as costly for a member of group B as it is for a member of group A." | The cost condition (`CostCondition`) |
| HIV-3 | 5 | "candidates are only willing to manipulate their features if it flips their classification from 0 to 1 and if the cost of the manipulation is less than 1" | A binary gain and a cost threshold |
| HIV-4 | 5 | "we require that a learner publish a classifier that is not adaptive to different agents based on their group identities" | One rule for both groups, as our rule is anonymous |
| HIV-5 | 6 | "Since the same cost expenditure is valued more highly by the disadvantaged group than by the advantaged group, the function u is more convex for group B than for group A." | The cost condition from utility, our burden channel (`cost_condition_of_wealth`) |
| HIV-6 | 7 | "it mistakenly admits unqualified candidates from the group with lower costs and mistakenly excludes qualified candidates from the group with higher costs" | The one-sided errors (`ell_order`) |
| HIV-7 | 7 | "A learner who employs a classifier f based on a threshold strategy σ ∈ [σB, σA] only commits false positives errors on group A and false negatives" | Proposition 2 |
| HIV-8 | 8 | "When cost functions are affine, the learner is indifferent between all σ∗ ∈ [σB, σA]." | Proposition 3, affine case (`affine_gap_const`) |
| HIV-9 | 13 | "When subsidies are not allowed, the learner chooses a classifier with threshold σ∗ = σB ≈ 0.398 at equilibrium." | Example 1 (`example1_sigmaB`) |
| HIV-10 | 13 | "There exist cost functions cA and cB satisfying the cost conditions, learner distributions DA and DB, true classifiers with threshold τA and τB, population proportions pA and pB, and learner penalty parameters CF N , CF P , and λ, such that no candidate in either group has higher payoff" | Theorem 2, shown by example |
| HIV-11 | 17 | "x ≥ c−1 B (cB(σ) + 1) = `B(σ)." | Misprint: Definition 2 has `− 1` (HIV-D2) |
| HIV-12 | 27 | "Now the equilibrium learner classifier without subsidies is based on threshold σ∗ 1 = σB = 0.55," | Example 2 (`example2_numbers`) |
| HIV-13 | 27 | "`B(σ∗ 1) = 0.39" | Example 3 (`example3_numbers`) |
| HIV-14 | 27 | "C(σ∗ 0) = 0.183 → C(σ∗ prop, β∗ ) = 0.128" | Example 3's label slip (HIV-D3) |

## Readings recorded

| ID | Point | Our reading |
|---|---|---|
| HIV-D1 | HIV-2, HIV-5, HIV-6 | Group B faces a higher cost of raising the score, and the paper derives that from the same outlay weighing more in utility for the disadvantaged group: our burden-monotonicity, where the fee's utility cost falls with wealth (`cost_condition_of_wealth`). The object differs: the learner sets a threshold, not a fixed number of positions, manipulation is continuous, and the result is about the learner's errors by group, not a count of those who pay or their share. |
| HIV-D2 | HIV-11 | In the proofs of Propositions 1-2 (pp.17-18) `ℓ_m(σ)` is printed as `c_m⁻¹(c_m(σ) + 1)`, a point above `σ`; Definition 2 (eq. 7) has `c_m⁻¹(c_m(σ) − 1)`, the lowest feature that reaches `σ` at cost one (`ell_def_sign`). The argument uses the latter. |
| HIV-D3 | HIV-14 | Example 3 labels the no-subsidy penalty `C(σ∗₀)`, which the next page uses for the no-manipulation regime; the comparison it then states (`C(σ∗₁) > C(σ∗prop, β∗)`) shows the 0.183 belongs to `σ∗₁`. The penalties themselves are not recomputed here. |
