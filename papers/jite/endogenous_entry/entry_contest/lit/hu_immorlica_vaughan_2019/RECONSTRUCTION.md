# Reconstruction of Hu, Immorlica and Vaughan, "The Disparate Effects of Strategic Manipulation"

**Source read.** arXiv:1808.08646v4 (10 May 2019), 29 pages with a text layer, the version of
the FAT* 2019 paper (ACM, 259-268), read in full [F, arXiv version]. The PDF is cached at
`~/.cache/entry_contest/hu_immorlica_vaughan_2019.pdf`. Pages are PDF pages.

**Why this paper.** The search of 15 October 2026 placed it closest to our wealth channel: two
groups face different costs of raising the score a classifier reads.

## Model (pp.3-5)

- Candidates of group A (advantaged) and group B have features `x` and can raise them to
  `y ≥ x` at cost `c_m(y) − c_m(x)`.
- The cost condition: `c_A(y) − c_A(x) ≤ c_B(y) − c_B(x)` for `y ≥ x`. The paper derives it
  from a cost felt more heavily by group B (pp.5-6).
- A learner commits to a classifier that cannot depend on the group. A positive classification
  is worth one, so a candidate manipulates only if that flips her classification and costs less
  than one.
- True labels are thresholds `τ_B ≤ τ_A`. The learner minimises a weighted error rate. There is
  no fixed number of positions.

## Results

1. **One dimension (Propositions 1-3).** The undominated thresholds form `[σ_B, σ_A]`. Every
   one of them admits some unqualified A candidates and rejects some qualified B candidates.
   With proportional costs the curvature picks the end: concave gives `σ_B`, convex gives
   `σ_A`, affine leaves the learner indifferent.
2. **`d` dimensions with linear costs (Theorem 1, Proposition 4).** The same one-sided errors.
3. **Subsidies (Section 4, Theorems 2-3, Proposition 6).** A subsidy to group B's costs can
   leave both groups worse off, by letting the learner raise the threshold. A world with no
   manipulation can be preferred by all three parties. These are shown by numerical examples.

## What is formalised

See `lean/mathlib/HuImmorlicaVaughan.lean`:

- Proposition 1's order `σ_B ≤ σ_A`;
- Proposition 2's core `ℓ_A(σ) ≤ ℓ_B(σ)`, and the sign misprint in the appendix's `ℓ_m`;
- the affine case of Proposition 3;
- the numbers of Examples 1-3;
- the bridge to our burden-monotonicity, with a control.
