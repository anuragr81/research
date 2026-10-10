# Notes: Bayraktar, Chevalier, Ly Vath and Wang (2026)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF, after Unicode normalisation (NFKC) and whitespace collapse.
  Control: a fabricated quotation and a true quotation on the wrong page are
  both refused.
- The Lean results in `CLAIMS.md` are declared in `lean/mathlib/BCVW.lean`,
  which builds with no `sorry` and audits within propext, Classical.choice
  and Quot.sound. Our reading is consistent with the source on each of them.

## Not verified

- The numerical results of Section 4. The solver was not rerun.
- The scale-function step of Lemma 5.2 and the uniqueness constants of
  Prop. 3.2 (see `RECONSTRUCTION.md`).

## Discrepancies

- BCVW-D2 and BCVW-D6. p. 17 states the limit of the cap as $1/a_3$ without
  the condition $a_3\ge a_1$. The general limit on p. 10 is $1/\max(a_1,a_3)$.

## Controls

- `BCVW.control_nondegenerate_needs_abs_c_lt_one` shows the non-degeneracy of
  BCVW-D4 fails at $c=1$.
- `BCVW.control_limit_needs_a1_le_a3` shows the limit $1/a_3$ fails when
  $a_1>a_3$.

## Consequences for this bundle

- M2's crossing point $\bar x$ is the paper's switching point $\hat y$. M2
  and M3 are statements about the paper's cap.
- The paper evaluates no saturation limit of the volatility, works in the
  level coordinate $y$ only, and has no asymmetric penalty.
- Remark 3.1 already states the degeneracy at $y=1$ and the repelling
  condition $r>r_L$ (H2, H2b in `TODO.md`).
- The paper's recapitalisation threshold is exogenous, and it has no fixed
  issuance cost (H3, H4).
