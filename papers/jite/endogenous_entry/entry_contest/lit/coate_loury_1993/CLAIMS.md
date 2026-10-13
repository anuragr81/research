# Claims about Coate and Loury (1993)

**Source.** Stephen Coate and Glenn C. Loury, "Will Affirmative-Action Policies
Eliminate Negative Stereotypes?", *American Economic Review* 83(5), 1220-1240
(December 1993), the JSTOR copy (stable URL 2117558), 22 PDF pages: a JSTOR cover
and then the AER pages. Read in full [F] and checked on page images [P]. Pages are
the AER pages; the printed page is the PDF page plus 1218. Cited in
`MANUSCRIPT.tex` (row L28) and in `refs.bib` as `CoateLoury1993`.

The pages are set in two columns, and the text layer garbles the equations.
`verify_coate_loury.py` extracts each column separately and checks every row not
marked P verbatim, on the page named, against the two columns read in turn (or,
for the abstract, against the full width of the page). Rows marked P were read
from page images, which the suite pins by sha256 but cannot match against text.
The results we use are proved in `lean/mathlib/CoateLoury.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CO-1 | 1220 | "even when identifiable groups are equally endowed ex ante, affirmative action can bring about a situation in which employers (correctly) perceive the groups to be unequally productive, ex post." | The main result |
| CO-2 | 1221 | "We propose a job-assignment model in which employers observe the group identity, but not the productivity, of their workers." | The setting |
| CO-3 | 1221 | "Lowering the standard may reduce investment incentives, however, because the favored workers see themselves as likely to succeed without acquiring the relevant skills." | Patronization; row L28 |
| CO-4 | 1223 | "Workers belong to one of two identifiable groups, B or W." | Two groups |
| CO-5 | 1224 | "We assume that workers are qualified to do task one only if they have made some costly ex ante investment." | A binary investment |
| CO-6 | 1224 | "Suppose for now that the cost distribution is the same for each group." | Identical cost laws in Sections I-III |
| CO-7 | 1225 | "The rational worker invests if the cost of doing so does not exceed the expected benefit." | The investors are those below a cost threshold |
| CO-8 | 1225 | "can occur whenever (3) has multiple solutions" | Discrimination needs several self-confirming beliefs |
| CO-9 | 1226 | "then there exist at least two nonzero solutions of (3)." | Proposition 1 |
| CO-10 | 1226 | "Hence all equilibria can be identified in Figure 2 by associating each group with an intersection of the EE and WW curves." | Equilibria as crossings |
| CO-11 | 1228 | "In this paper “affirmative action” refers to a policy requiring employers to achieve the same aggregate rate of assignment to task one for both groups." | The policy |
| CO-12 | 1228 | "Workers' behavior is not affected by the policy" | The WW curve is unchanged |
| CO-13 | 1229 | "then all equilibria under affirmative action entail homogeneous beliefs about the two groups." | Proposition 2 |
| CO-14 | 1229 | "The positive investment effect may outweigh the stricter rationing effect." | Why the assignment rate need not fall |
| CO-15 | 1230 | "is a rough measure of the informativeness of the noisy signal" | The likelihood ratio at zero in (5) |
| CO-16 | 1230 | "Then affirmative action will eliminate stereotypes." | The rule of thumb |
| CO-17 | 1230 | "A liberal policy amounts to choosing the standard" | Liberal and conservative standards |
| CO-18 | 1230 | "If employers follow a liberal policy, a worker who has invested is assigned to task one for sure, while a noninvestor is assigned with probability" | Those who do not invest still reach task one |
| CO-19 | 1231 | "employers patronize B's by making it easier for them to achieve the desirable assignment." | Patronization |
| CO-20 | 1231 | "In fact, their (correct) assessment of the average productivity of B's may actually worsen in this equilibrium." | Proposition 3 |
| CO-21 | 1232 | "Whenever an employer assigns a failing worker to task one, we say the employer is patronizing that worker." | Patronization defined |
| CO-22 | 1232 | "Thus, the only stable equilibrium is the patronizing one." | The limit of (9) |
| CO-23 | 1232 | "are small (a highly accurate test)" | Worsening is likelier with an accurate test |
| CO-24 | 1234 | "an equilibrium under affirmative action exists exhibiting negative stereotypes toward B's." | Proposition 4 |
| CO-25 | 1235 | "The major insight of this paper is that an equal-assignment constraint creates" | Section IV |
| CO-26 | 1235 | "suppose that, because of unequal educational opportunities (say), B's have higher investment costs than W's on average." | Unequal cost laws |
| CO-27 | 1236 | "B's are doing less well than W's, but the difference derives solely from their inferior endowments." | The group gap |
| CO-28 | 1236 | "using preferences to help the disadvantaged group necessarily causes the objective difference in productivity between the two groups to rise." | A marginal policy widens the gap |
| CO-29 | 1238 | "it is better to subsidize disadvantaged workers for achieving good jobs, than to subsidize employers for promoting them" | Subsidies |

## Read from page images

| ID | Page | Transcription | What we rely on |
|---|---|---|---|
| CO-P1 | 1229 | "(5) φ(s)/[φ(s) − 1] > η(β(s)) for all s ∈ [0, ŝ)" | The condition for the assignment rate to fall |
| CO-P2 | 1230 | "η(c) ≡ d[c·G(c)]/dc; either φ(0) < 2 or β(ŝ) ≤ μ implies (5); either φ(0) < 1 + e² ≈ 8.4 or β(ŝ) ≤ μ implies (5)" | The two cost examples |
| CO-P3 | 1230 | "ξ = 1/{1 + [(1 − π)/π]φ}. The employer will assign the worker to task one only if ξx_q ≥ (1 − ξ)x_u. This is equivalent to π ≥ φ/(r + φ) ≡ π̂" | The liberal threshold |
| CO-P4 | 1231 | "(6) ω(1 − p_q) < x_u p_u/[x_q p_q + x_u p_u] < ω(1 − p_u)" | The discriminatory equilibrium |
| CO-P5 | 1232 | "(7) π_ℓ + (1 − π_ℓ)p_u = π_b + (1 − π_b)[p_u + (1 − p_u)α(π_b)], which implies: α(π_b) = (π_ℓ − π_b)/(1 − π_b)" | The patronizing probability |
| CO-P6 | 1232 | "(8) π_b = [1 − α(π_b)]π_ℓ = [(1 − π_ℓ)/(1 − π_b)]π_ℓ" | The equilibrium beliefs about B's |
| CO-P7 | 1232 | "(9) π_b^{t+1} = [1 − α(π_b^t)]π_ℓ = [(1 − π_ℓ)/(1 − π_b^t)]π_ℓ, t = 0,1,2,...; π_b^0 = π_c" | The adjustment process |
| CO-P8 | 1232 | "if [λ/(1 − λ)][ξ_ℓ x_q − (1 − ξ_ℓ)x_u] > x_u [i.e., if λ > λ̂ ≡ 1/ξ_ℓ(1 + r)]. Note that π_ℓ > π̂ implies λ̂ < 1." | Footnote 20 |
| CO-P9 | 1232 | "p_u = 0.2, p_q = 0.3, and r = 2/3. Then if λ > 0.9, for values of ω such that 5/7 > ω > 0.5/[λ − 0.2], patronization of B's is the result of affirmative action. The negative stereotype about B's is made worse if, in addition, ω > 2/3." | Footnote 21 |
| CO-P10 | 1233 | "(11a) r_w(γ) ≡ [x_q − γ/λ]/[x_u + γ/λ] = [(1 − π_w)/π_w]φ(s_w); (11b) r_b(γ) ≡ [x_q + γ/(1 − λ)]/[x_u − γ/(1 − λ)] = [(1 − π_b)/π_b]φ(s_b)" | The first-order conditions |
| CO-P11 | 1234 | "let D(γ) ≡ ρ̂(s_b(γ)) − ρ̂(s_w(γ)). D(·) is differentiable for γ near 0, and D′(0) < 0" | The proof of Proposition 4 |

## Results in Lean (`lean/mathlib/CoateLoury.lean`, namespace `CoateLoury1993`)

| Result | Lean | What is proved |
|---|---|---|
| The employer's rule (p.1224, footnote 18) | `posterior_eq`, `assign_iff`, `dP_pos_iff`, `ee_iff` | the posterior (1); assignment exactly when r ≥ [(1-π)/π]φ; the sign of the derivative of the employer's payoff in the standard; the EE curve π = φ/(r+φ) |
| The benefit of investing (p.1225) | `beta_hasDerivAt`, `beta_deriv_pos_iff` | β rises exactly where φ > 1 |
| Proposition 1 (p.1226) | `prop1` | if WW lies above EE at one standard and below it on both sides, the curves cross twice at a positive belief |
| Proposition 2 (p.1229) | `prop2` | a strictly decreasing assignment rate forces equal standards and beliefs |
| Eq. (5) (pp.1229-1230) | `rhoHat_hasDerivAt`, `rhoHat_deriv_eq`, `rhoHat_deriv_neg_of_le_one`, `eq5_iff`, `lhs5_anti` | the derivative of the assignment rate is f_q(φ(η-1) - η); it is negative where φ ≤ 1, and where φ > 1 exactly when (5) holds; the left side of (5) rises as φ falls |
| The cost examples (p.1230) | `eta_uniform`, `uniform_sufficient`, `eta_exp`, `eta_exp_le`, `eta_exp_at_max`, `exp_threshold_iff` | η(c) = c/μ for uniform costs, and φ(0) < 2 or β(ŝ) ≤ μ gives (5); η(c) = (c/μ - 1)e^{-c/μ} + 1 ≤ 1 + e^{-2} for exponential costs, with equality at c = 2μ, and the left side of (5) exceeds 1 + e^{-2} exactly when φ(0) < 1 + e² |
| The uniform example (pp.1230-1231) | `beta_liberal`, `beta_conservative`, `psi_eq`, `liberal_iff`, `pihat_eq`, `six_beta_falls` | the benefits of investing are ω(1 - p_u) under a liberal and ω(1 - p_q) under a conservative employer; the likelihood ratio on the unclear band is p_u/p_q; the employer is liberal exactly when π ≥ π̂, with π̂ as in (6); under (6) β falls across the unclear band, so incentives peak at the liberal standard |
| Accuracy of the test (pp.1230-1232) | `unclear_band_mono`, `rates_fall_with_band` | widening the unclear band raises p_u and p_q, so both investment rates fall as the test becomes less accurate |
| Proposition 3 (pp.1231-1232) | `alpha_eq`, `eq8_iff`, `nine_eq`, `T_sub`, `iter_tendsto`, `T_hasDerivAt`, `patronizing_stable`, `colorblind_unstable`, `worsens_iff` | (7) gives α; (8) has exactly the solutions π_ℓ and 1 - π_ℓ; for π_ℓ > 1/2 the process (9) converges to 1 - π_ℓ from every initial belief in [0, π_ℓ), the slope of (9) is below one there and above one at π_ℓ; the stereotype worsens exactly when π_ℓ + π_c > 1 |
| Footnotes 20 and 21 (pp.1231-1232) | `lambda_hat_iff`, `lambda_hat_lt_one`, `fn21_pihat`, `fn21`, `fn21_nonempty`, `fn21_worse_iff` | patronizing a failing B beats demoting an unclear W exactly when λ > λ̂, and λ̂ < 1 when π_ℓ > π̂; with footnote 21's numbers π̂ = 1/2, its bounds on ω give every hypothesis of Proposition 3, the bounds are compatible when λ > 0.9, and the stereotype worsens exactly when ω > 2/3 |
| Section III (pp.1233-1234) | `rho_hasDerivAt`, `foc_w_iff`, `foc_b_iff`, `prop4_D_hasDerivAt`, `prop4_D_deriv_neg`, `prop4_root` | the first-order conditions (11a) and (11b); D'(0) = ρ̂'(s̄)(s_b'(0) - s_w'(0)) < 0; and D(0) = 0, D'(0) < 0 with D(γ') > 0 give a root in (0, γ') |

Not formalised: the existence of equilibria in Definitions 1 and 2, the
implicit-function step of Proposition 4 (the signs of s_b' and s_w' are
hypotheses), the local stability of solutions of (3) stated on p.1226, and the
graphical arguments of Section IV.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CO-D1 | 1232 | "The reader may find it helpful to experiment with some numerical examples." | Footnote 21: the text layer reads r = 3 where the page image reads r = 2/3 (CO-P9). With 2/3 the footnote's bounds are exactly the hypotheses of Proposition 3 (`fn21`). With 3, π̂ = 2/11, and no ω has both π_ℓ > 1/2 and π_c < π̂ |
| CO-D2 | 1232 | "The reader can easily verify that" | The convergence of (9) is asserted. It holds from every initial belief in [0, π_ℓ), by a contraction bound with factor (1 - π_ℓ)/(1 - max(π_b^0, 1 - π_ℓ)) (`iter_tendsto`) |
| CO-D3 | 1229 | "A simple calculation shows that" | The calculation gives ρ̂' = f_q(φ(η - 1) - η) (`rhoHat_deriv_eq`). (5) is the condition below ŝ; at and above ŝ the derivative is negative without it (`rhoHat_deriv_neg_of_le_one`), as the text says |

No misprint was found.

## Comparison with the model rows

The investment is binary and its cost is private, as with our fee, and those
who do not invest still compete. A non-investor reaches task one when the signal
clears the standard (CO-18), and under affirmative action a failing B can be
patronized (CO-21). The standard is set by employers who best-respond to their
beliefs. There is no fixed prize, no rank and no fixed number of places, and the
number assigned to task one moves with the standard.

- **Uniqueness.** The investment rate need not be unique (Proposition 1, CO-9),
  because the standard responds to the belief. With the rule fixed, as in our
  model, the count is pinned (M6).
- **Accuracy of the test.** In the uniform example, at a fixed employer policy,
  both investment rates fall as the test becomes less accurate
  (`rates_fall_with_band`). That is the direction of M28 if a less accurate test
  is read as a rule that weighs the bought component less. The paper states no
  comparative static in the accuracy of the test beyond the worsening condition
  (CO-23).
- **The standard.** Lowering the standard can lower investment (CO-3, CO-14).
  This has no counterpart in our model, where the rule is fixed and the fee is
  the investment.
- **Group gap.** With unequal cost laws (Section IV), B's invest less and do less
  well (CO-26, CO-27), the group gap of C10.
