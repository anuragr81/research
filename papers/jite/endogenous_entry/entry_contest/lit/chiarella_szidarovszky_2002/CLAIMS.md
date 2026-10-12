# Claims about Chiarella and Szidarovszky (2002)

**Source.** Carl Chiarella and Ferenc Szidarovszky, "The Asymptotic Behavior of
Dynamic Rent-Seeking Games", *Computers and Mathematics with Applications* 43,
169-178 (2002). The file was uploaded by the author on Drive as
`The_asymptotic_behavior_of_dynamic_rent`. It was read in full [F] and checked
on page images [P]. Pages are the printed pages; the printed page is the PDF
page plus 168. Cited in `LITERATURE.tex` and in `refs.bib` as
`ChiarellaSzidarovszky2002`.

The text layer garbles the equations and letter-spaces some words. Rows marked
P below were read from page images, which `verify_chiarella_szidarovszky.py`
pins by sha256 but cannot match against text. Every other row is checked
verbatim, on the page named, against the extracted text. The results we use
are proved in `lean/mathlib/ChiarellaSzidarovszky.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CS-1 | 169 | "We show that in the absence of a dominant agent, all eigenvalues of the Jacobian are real." | The paper's first result (Case A) |
| CS-2 | 169 | "In the presence of a dominant agent, complex eigenvalues are possible." | Case B |
| CS-3 | 170 | "Assume that n agents compete for a rent, which will earn a unit profit for the agent who actually wins the rent." | The number of agents is fixed |
| CS-4 | 170 | "Okuguchi [11] has pointed out that rent-seeking games are mathematically equivalent to profit maximizing Cournot oligopolies with hyperbolic price functions." | The paper's lineage |
| CS-5 | 171 | "each agent adjusts its effort proportionally to its marginal profit" | Eq. (7), the adjustment process |
| CS-6 | 171 | "since for each i there is a strictly increasing cost function with given first and second derivatives at a single point." | Any c'' satisfying eq. (6) can be realised |
| CS-7 | 172 | "no conclusion can be given based only on analysis of the eigenvalues, since the system is nonlinear" | The boundary case of Theorem 1 |
| CS-8 | 173 | "we see that all roots are real" | Case A, Figure 1 |
| CS-9 | 174 | "Notice that no condition can be given on the sign of the derivative" | Case B: g need not be monotone |
| CS-10 | 175 | "therefore, either all roots are real, or" | Case B: at most one complex pair |
| CS-11 | 176 | "the cases of both real and complex roots are possible" | Example 1 |
| CS-12 | 176 | "We have seen in the above discussion that Case B may occur only in the case of a dominant agent" | Case B needs a dominant agent |
| CS-13 | 176 | "This inequality shows that at the equilibrium, agent p spends more effort than all other agents combined." | What "dominant" means |
| CS-14 | 176 | "This assumption is therefore not very realistic, since antitrust constraints usually prohibit this kind of behaviour in the long run." | The authors' own view of Case B |
| CS-15 | 177 | "is nonzero showing the existence of a limit cycle" | The conclusion of Example 2 |
| CS-16 | 177 | "We have established the possibility of limit cycle motion in the case of local instability." | The paper's claim about limit cycles |
| CS-17 | 178 | "Future research could focus on the specific nature of nonlinearities" | What is left open |
| CS-P1 | 172 | "If m_l > 1, then λ = d_l is an eigenvalue with multiplicity m_l − 1, and if m_l = 1, then the corresponding factor cancels, so in this case λ = d_l is not an eigenvalue (l = 1, 2, ..., r)." | The eigenvalues of eq. (11) |
| CS-P2 | 173 | "Assume that d_r > 0 and all d_i < 0 (i = 1, 2, ..., r − 1). If g(0) < −1, then the equilibrium is locally asymptotically stable, and if g(0) > −1, then the equilibrium is locally unstable." | Theorem 2(ii) |
| CS-P3 | 174 | "Assume first that d_r > d_l (l = 1, 2, ..., r − 1). If d_r ≥ 0, then the equilibrium is unstable." | Theorem 3(i) |
| CS-P4 | 177 | "Consider the case of r = 3, γ1 = −a, γ2 = −b (where a and b are positive parameters), γ3 = γ > 0 is the bifurcation variable, and d1 = −2, d2 = 1, d3 = −1." | Example 2's parameters |
| CS-P5 | 177 | "In order to satisfy relation (18), we have to guarantee that γ < 1" | Example 2 checks eq. (18) for group 3 only |
| CS-P6 | 177 | "which has the approximating value (0.468 + 0.719i) at λ = iα with the above values of α and γ." | The transversality number |

The equations used below were read from the page images: eqs. (1)-(12) on pp.170-172,
eqs. (15)-(19) on pp.172-174, Example 1 on pp.175-176, and eqs. (20)-(26) with
Example 2's numbers on pp.176-177.

## Results in Lean (`lean/mathlib/ChiarellaSzidarovszky.lean`, namespace `ChiarellaSzidarovszky2002`)

| Result | Lean | What is proved |
|---|---|---|
| The game, eqs. (1)-(6) | `hasDerivAt_win`, `hasDerivAt_marginal_own`, `hasDerivAt_marginal_rival`, `foc_iff`, `soc_iff` | the derivatives behind eqs. (3) and (4) and their equilibrium forms (5) and (6) |
| Realising c'' (p.171) | `costWitness_hasDerivAt`, `costWitness'_hasDerivAt`, `costWitness_best_reply` | for any c'' satisfying eq. (6) a cost with the given c' and c'' makes `x̄` a best reply over all efforts; that the cost can be strictly increasing is not shown |
| The Jacobian, eqs. (7)-(9) | `rivals_update_self`, `rivals_update_other`, `field_partial_own`, `field_partial_other`, `jac_eq` | the partial derivatives of eq. (7) are the printed entries, and the Jacobian is `(D + a 1ᵀ)/s̄^3` |
| Eigenvalues, eqs. (10)-(12) and Theorem 1(i) | `bigM_mulVec`, `bigM_eigen_of_sum_zero`, `bigM_eigen_pair`, `thm1_i`, `bigM_sub_eq`, `det_bigM_sub`, `eigen_iff_eq12`, `sum_by_group` | the determinant identity; off the poles, eigenvalues are the roots of eq. (12); grouping into `γ_l`; two agents with the same d give the eigenvalue d, so `d > 0` gives the Jacobian a positive real eigenvalue |
| What eq. (6) implies | `d_add_e_neg`, `group_bound`, `group_d_add_gamma_neg` | `d_i + e_i < 0` for every agent, hence `m_l d_l + γ_l < 0` for every group, and `d_l + γ_l < 0` when `γ_l ≤ 0` |
| Case B set-up, eq. (18) | `dominant_unique`, `gamma_nonpos`, `eq18`, `thm3_i_branch_empty` | at most one agent spends more than all others together; groups without it have `γ ≤ 0`; its group has `d < -γ < 0` |
| Case A, Theorem 2 and Figure 1 | `caseA_real`, `caseA_below_max`, `thm2_i`, `g_strictAnti`, `caseA_root_unique`, `thm2_ii`, `caseA_root_between`, `caseA_root_below`, `thm2_iii` | with every `γ_l ≤ 0` all roots of eq. (12) are real and below the largest d_l; with every `γ_l < 0`, g falls strictly between poles, there is exactly one root in each gap and one below the smallest d_l; Theorem 2(i)-(iii) for the roots |
| Eqs. (20)-(23) | `term_at_imag`, `eq22_eq23`, `eq23_case_i` | real and imaginary parts, eq. (22), eq. (23), and no purely imaginary root in Theorem 3(i)'s case |
| Theorem 2's hypothesis (not in the paper) | `wA_d`, `wA_e`, `wA_soc`, `wA_thm2_ii_hyps`, `wA_unstable` | efforts `(1,1,1)`, speeds `(1/2,2,2)`, `c'' = (-1/27,-7/54,-7/54)` satisfy eq. (6) and Theorem 2(ii)'s hypotheses with `g(0) = -7/2 < -1`, yet the Jacobian has the eigenvalue `1/27` |
| An unstable complex pair under eq. (6) (not in the paper) | `wB_d`, `wB_e`, `wB_soc`, `wB_k_pos`, `wB_charpoly`, `wB_unstable_pair` | efforts `(1,3,1)` with a dominant agent; eq. (6) holds and `D + a 1ᵀ` has the eigenvalues `-1` and `1/100 ± 3i/8` |
| Example 1 | `ex1_gamma1`, `ex1_iff`, `ex1_disc`, `ex1_both`, `ex1_roots_stable` | the printed quadratic and discriminant; real and complex roots both occur; with `α ≥ 0` and `γ > 0` both roots have negative real part |
| Example 2, algebra | `ex2_parts`, `ex2_eq25`, `ex2_eq26`, `ex2_gamma_eq`, `ex2_gamma_gt_b`, `ex2_gamma_lt_one_iff`, `ex2_printed_eq26`, `ex2_printed_gamma_lt_one` | eq. (24) at `λ = iα` is the printed pair of real equations; eqs. (25) and (26); `γ < 1` is the printed inequality, which holds for every `α^2 ≥ 0` at `a = 0.15`, `b = 0.75` |
| Example 2, numbers | `ex2_t_bounds`, `ex2_roots`, `ex2_z_approx`, `ex2_alpha_bounds`, `ex2_gamma_approx`, `ex2_root_exact` | `α^2 = (-73 ± 3√921)/40`, within 0.0005 of 0.451 and -4.101; `α` within 0.0005 of 0.672; `γ` within 0.0005 of 0.799; `λ = iα` solves eq. (24) exactly |
| Example 2, `dλ/dγ` | `dLam_eq_implicit`, `sq_term`, `div_parts`, `dLam_re`, `dLam_im`, `dLam_re_pos`, `dLam_printed` | the printed formula is `-(∂F/∂γ)/(∂F/∂λ)`; its real part is `10(z+1)(z+4)(3z^2+26z+53)/(9(9z^3+127z^2+547z+729))` with `z = α^2`, positive at every `α ≠ 0`; the value is within 0.0005 of `0.468 + 0.719i` |
| Example 2 and eq. (6) (not in the paper) | `ex2_not_admissible`, `ex2_printed_group2_fails` | eq. (6) for group 2 needs `b > m_2 ≥ 1`, eq. (18) for group 3 needs `γ < 1`, and at a purely imaginary root `γ > b`; no `a > 0`, b meet all three, and the printed `b = 0.75` fails the first |

The derivative of eq. (24) in `λ` is also proved (`F2_hasDerivAt`, not cited
in backticks because the suite's name check reads lower-case names only).

Not formalised: the step from the signs of the eigenvalues to local asymptotic
stability or instability of eq. (7) (linearised stability), and the Hopf
bifurcation theorem, neither of which is in Mathlib, so Theorems 1-3 are proved
as statements about eigenvalues; Theorem 3(ii)-(iv); and that the cost built
for p.171 can be strictly increasing.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CS-D1 | 172 | "then the corresponding factor cancels" | Only when `γ_l ≠ 0` (CS-P1). In the symmetric duopoly every e_i is zero (`evec_symmetric_duopoly`), and then each d_i is an eigenvalue with `m_l = 1` (`eigen_of_e_zero`). Eq. (6) makes such an eigenvalue negative (`group_d_add_gamma_neg` with `γ_l = 0`), so Theorem 1's conclusions are unaffected |
| CS-D2 | 173 | "then the equilibrium is locally asymptotically stable" | Theorem 2 does not restate Theorem 1(ii)'s hypothesis that `d_l < 0` whenever `m_l ≥ 2`, and its stability conclusions need it (CS-P2). Without it, Theorem 2(ii)'s hypotheses hold with `g(0) < -1` while the Jacobian has a positive eigenvalue (`wA_unstable`). In Theorem 2(i), `d_l = 0` with `m_l ≥ 2` gives the eigenvalue 0 (`bigM_eigen_pair`) |
| CS-D3 | 174 | "then the equilibrium is unstable" | The branch `d_r ≥ 0` of Theorem 3(i) is empty: eq. (18) gives `d_{l*} < 0` (`thm3_i_branch_empty`, CS-P3) |
| CS-D4 | 177 | "In order to satisfy relation (18)" | Eq. (18) is checked for group 3 only (CS-P5). Eq. (6) bounds every group, `m_l d_l + γ_l < 0` (`group_bound`), and group 2 (`d_2 = 1`, `γ_2 = -0.75`) fails it; no `a > 0`, b give Example 2's structure under eq. (6) (`ex2_not_admissible`). The arithmetic of the example is right (`dLam_printed`) |
