# Notes — Chiarella and Szidarovszky (2002), pass 1 (read in full)

Run `python3 lit/chiarella_szidarovszky_2002/verify_chiarella_szidarovszky.py`
from the repository root. The suite does four things:

- it checks the quotations verbatim, on the page named, against the cached text
  (the PDF is pinned by sha256);
- it pins the page images of pp.172, 173, 174 and 177 for the six quotations
  whose text layer is garbled;
- it runs a reversed-quote control;
- it measures `lean/mathlib/ChiarellaSzidarovszky.lean` (see `LEAN`).

## Verdict

This is not an entry paper, and it is not Corcoran and Karels (1985). The
number of agents is fixed, every agent competes, and neither costs nor
wealth decide who takes part. It bears on none of C5, C6 or C13, and it has no
row in `skeleton_who_pays.tex`. Corcoran and Karels is still unread.

## Findings from the formalisation

1. **Eq. (6) bounds every agent, not only the dominant one.** It gives
   `d_i + e_i < 0` for every agent (`d_add_e_neg`). Summed over a group this is
   `m_l d_l + γ_l < 0` (`group_bound`). The paper draws the bound only for the
   dominant agent, as eq. (18) on p.174.
2. **Example 2 does not satisfy eq. (6).** Group 2 has `d_2 = 1` and
   `γ_2 = -b = -0.75`, so `m_2 - 0.75 < 0` would be needed, and that is false
   for every `m_2 ≥ 1` (`ex2_printed_group2_fails`). No choice of `a > 0` and `b`
   with Example 2's `d_l` fixes this.
   Eq. (6) for group 2 needs `b > 1`. Eq. (18) for group 3 needs `γ < 1`. At a
   purely imaginary root, `γ = b + a(1 + α^2)/(4 + α^2) > b`. All three cannot
   hold together (`ex2_not_admissible`). The arithmetic of the example is
   right: the roots, `α`, `γ` and `dλ/dγ ≈ 0.468 + 0.719 i` are proved to the
   printed precision (`ex2_z_approx`, `ex2_alpha_bounds`, `ex2_gamma_approx`,
   `dLam_printed`). The real part of `dλ/dγ` is positive at every purely
   imaginary root of the family, not only at the computed one (`dLam_re_pos`).
3. **The claim that limit cycles are possible is not refuted, only its
   example.** One parameter set satisfies eq. (6), has a dominant agent, and
   its `D + a 1ᵀ` has the eigenvalues `1/100 ± 3i/8` (`wB_unstable_pair`). So an
   unstable complex pair does occur in games that meet the paper's conditions.
   We have not shown that such a pair crosses the imaginary axis along an
   admissible path, which is what a Hopf bifurcation needs.
4. **Theorem 2's stability conclusions need Theorem 1(ii)'s hypothesis**
   (`d_l < 0` whenever `m_l ≥ 2`), and Theorem 2 does not restate it. Here is
   a witness that satisfies eq. (6):
   - efforts `(1,1,1)`, speeds `(1/2,2,2)`, `c'' = (-1/27,-7/54,-7/54)`;
   - every `γ_l < 0`, `d_r = 1 > 0`, `d_1 = -1 < 0` and `g(0) = -7/2 < -1`.

   Theorem 2(ii) would call this equilibrium stable, but the Jacobian has the
   eigenvalue `1/27` (`wA_unstable`).
5. **The p.172 statement that `d_l` is not an eigenvalue when `m_l = 1` needs
   `γ_l ≠ 0`.** In the symmetric duopoly every `e_i` is zero
   (`evec_symmetric_duopoly`), and then each `d_i` is an eigenvalue
   (`eigen_of_e_zero`). Eq. (6) makes such an eigenvalue negative, so Theorem
   1's conclusions are unaffected.
6. **The branch `d_r ≥ 0` of Theorem 3(i) is empty,** since eq. (18) gives
   `d_{l*} < 0` (`thm3_i_branch_empty`).
7. **Example 1 is always stable in its pair.** With `α ≥ 0` and `γ > 0` both
   roots of its quadratic have negative real part (`ex1_roots_stable`), so its
   complex roots never cross the imaginary axis. When `α > 0`, group 1 has at
   least two agents at `d_1 = 0`, so the Jacobian also has the eigenvalue 0
   (`bigM_eigen_pair`). The display on p.175 computes `γ_1`, and its `s̄_1` is
   group 1's total effort, which the paper does not define (`ex1_gamma1`).
8. **Case A as drawn in Figure 1 is proved in full** for every `γ_l < 0`:
   - all roots of eq. (12) are real (`caseA_real`);
   - none lies above the largest `d_l` (`caseA_below_max`);
   - exactly one root lies between each pair of neighbouring `d_l`, and one
     below the smallest (`caseA_root_between`, `caseA_root_below`,
     `caseA_root_unique`).
