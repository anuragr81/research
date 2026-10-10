# Notes: Li, Yu and Zhang (2023)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF. Control: a fabricated quotation and a true quotation on the
  wrong page are both refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/LiYuZhang.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. Our reading of the root
  relations, the Merton limit and the $k$-dependence of the limits is
  consistent with the source.

## Not verified

- The verification theorem and the concavification principle (Theorem 3.1,
  Prop. 2.1). Read, not formalised.
- The numerical results of Section 4.

## Discrepancies

- LYZ-D1. Remark 4.1 attributes a dependence on $k$ that the formulas carry
  only when $\beta_1=\beta_2$.

## Controls

- `LiYuZhang.control_limitTerm_depends_on_k_when_equal`. The term does move
  with $k$ when $\beta_1=\beta_2$, so `limitTerm_free_of_k` needs
  $\beta_2\ne\beta_1$.
- `LiYuZhang.control_roots_need_distinct`. The sum rule needs distinct roots.
- `LiYuZhang.control_U_not_concave`. $U$ is not concave, which is why the
  paper needs a concave envelope.

## Consequences for this bundle

- C2. The paper does not contain the result of C2. It has no firm, no
  regulatory cap and no volatility limit of a capital ratio. It supplies the
  contrast: in an unconstrained loss-averse problem, preference parameters
  reach the large-wealth limit of the control (LYZ-Q1, subject to LYZ-D1).
  In C2 the binding cap removes $\lambda_S$ from the saturation limits.
- H5. The paper is the S-shaped case that needs a concave envelope
  (LYZ-Q3), against which PROOFS_v2's concave kinked-linear penalty is set.
