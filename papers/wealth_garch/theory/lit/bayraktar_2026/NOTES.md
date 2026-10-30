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

- The numerical results of Section 4 other than Table 1. Table 1 was rerun
  by a port of their solver and by policy iteration (BCVW-D9 to D11).
- The scale-function step of Lemma 5.2 and the uniqueness constants of
  Prop. 3.2 (see `RECONSTRUCTION.md`).

## Discrepancies

- BCVW-D9 to D11, added 30 October 2026 from the solve of Question 3. The
  published Table 1 carries a time-step error of a few percent and the solver
  poses a problem with issuance at every state; converged, the bank issues
  before the threshold and v is not concave at the baseline. The directions
  of Table 1 survive.
- BCVW-D12. The constraint decomposition of Figure 4(a) holds in direction
  but its size rests on a ceiling at the edge of a finite value.

- BCVW-D2 and BCVW-D6. p. 17 states the limit of the cap as $1/a_3$ without
  the condition $a_3\ge a_1$. The general limit on p. 10 is $1/\max(a_1,a_3)$.

## Controls

- `BCVW.control_nondegenerate_needs_abs_c_lt_one` shows the non-degeneracy of
  BCVW-D4 fails at $c=1$.
- `BCVW.control_limit_needs_a1_le_a3` shows the limit $1/a_3$ fails when
  $a_1>a_3$.
- `BCVW.control_ceiling_needs_small_runoff` shows the ceiling result fails
  when the runoff $a_2$ is large.

## Consequences for this bundle

- Added 23 October 2026 (BCVW-Q15 to Q18, BCVW-D8). The authors' main
  banking case is no borrowing, under which the liquidity branch never binds.
  M3's surplus limit belongs to their unrestricted benchmark.

- M2's crossing point $\bar x$ is the paper's switching point $\hat y$. M2
  and M3 are statements about the paper's cap.
- The paper evaluates no saturation limit of the volatility, works in the
  level coordinate $y$ only, and has no asymmetric penalty.
- Remark 3.1 already states the degeneracy at $y=1$ and the repelling
  condition $r>r_L$ (H2, H2b in `TODO.md`).
- The paper's recapitalisation threshold is exogenous, and it has no fixed
  issuance cost (H3, H4).

## Added 23 October 2026, for the theory paper's literature ledgers

- BCVW-Q19 to BCVW-Q22 (pp. 12 to 13), for the drawing of the regulatory-cap
  approach in the theory paper: the thresholds of Theorem 3.1, the
  illustrative parameterisation, the correlated liability shock, and the cap
  not creating the thresholds.
