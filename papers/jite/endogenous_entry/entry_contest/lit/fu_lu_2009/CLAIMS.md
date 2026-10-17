# Claims about Fu and Lu (2009)

**Source.** Qiang Fu and Jingfeng Lu, "Contest with Pre-Contest Investment",
the authors' manuscript of February 2009 (8 pages), uploaded by the author on
Drive as `Contest_invest0212.pdf`. It thanks the editor, Eric Maskin, so it is
the accepted version of *Economics Letters* 103(3), 142-145 (2009); the
published pages are not checked. Read in full [F] and checked on page images
[P]. Pages are the manuscript's pages (equal to the PDF pages). Cited in
`skeleton_who_pays.tex` (row L26) and in `refs.bib` as `FuLu2009`.

The text layer loses the Greek letters and renders the font's ligatures as
stray glyphs ("e¤ort" for effort); the suite maps those glyphs back. Rows
marked P were read from page images, which `verify_fu_lu.py` pins by sha256.
Every other row is checked verbatim, on the page named, against the text. The
results we use are proved in `lean/mathlib/FuLu.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FU-1 | 1 | "In a standard noisy contest, more competition (more contestants) leads to lower individual equilibrium effort." | The benchmark |
| FU-2 | 1 | "We show that when contestants can make pre-contest investment to enhance their competency, neither equilibrium investment nor individual effort is monotonic in the number of contestants." | The paper's claim |
| FU-3 | 2 | "Positive equilibrium investment emerges when the number of contestants is small, while zero investment occurs in equilibrium when the number of contestants is large." | The claim in the number of contestants |
| FU-4 | 3 | "There are K ≥ 2 ex ante identical contestants" | Identical contestants; everyone competes |
| FU-5 | 3 | "In the first stage, contestants simultaneously engage in technological investment" | A continuous investment before the contest |
| FU-6 | 4 | "we focus on the pure-strategy symmetric subgame equilibrium where all contestants choose the same level of investment" | The equilibria studied |
| FU-7 | 6 | "When the number of contestants is sufficiently large, they simply invest nothing in equilibrium even if contestants are allowed to make pre-contest investment, and a standard model is restored." | The large-K claim |
| FU-8 | 6 | "Both equilibrium investment I*(K) and equilibrium individual effort x*(K) increases when the number of contestants increases" | Corollary 1 |
| FU-9 | 7 | "However, it drops to zero when K reaches a large number, say, K ≥ 7 as implied by Proposition 1(ii) and 1(iii)." | Corollary 2 |
| FU-10 | 7 | "However, when N is large, it is always optimal to shortlist as contestants would not invest and lower individual effort always results with more contestants." | The shortlisting reading |
| FU-P1 | 3 | "we assume that the cost function c(I_k) takes a convenient form of c(I_k) = λ/(λ+I_k)." | The cost of effort |
| FU-P2 | 5 | "A symmetric subgame perfect equilibrium with zero investment, i.e., I*(K) = 0, exists if and only λ ≥ 2(K−1)²/K³." | Proposition 1(ii) |
| FU-P3 | 5 | "If K = 2, 3 and λ ∈ [2(K−1)²/K³ − 1/K², 2(K−1)²/K³), a unique symmetric equilibrium with positive equilibrium investment exists iff x*_k(I*(K), I*(K)) ≥ x*_k(0, I*(K))." | Proposition 1(iv) |
| FU-P4 | 6 | "We have dΦ/dI_k = (K−1)/[λK + I_k(K−1)]⁴ · λ(3 − 4K), which is negative for any K ≥ 2." | The step in the proof of Proposition 1(ii) |
| FU-P5 | 6 | "for K = 2, 3, when λ belongs to a small neighborhood of λ0 = 0.23, a unique symmetric equilibrium with positive investment exists." | Corollary 1 |

## Results in Lean (`lean/mathlib/FuLu.lean`, namespace `FuLu2009`)

| Result | Lean | What is proved |
|---|---|---|
| The second stage, eqs. (5)-(9) | `total_effort`, `effort_foc_k`, `effort_foc_other`, `stage_payoff`, `ratio_eq9` | eqs. (7)-(8) solve the first-order conditions (5)-(6); k earns the square of its winning probability; with `c(I) = λ/(λ+I)` this is eq. (9) |
| The investment stage, eqs. (11)-(14) | `hasDerivAt_pay`, `sym_foc_iff`, `payoff14`, `effort12` | eq. (13) is the derivative of eq. (9); at a symmetric profile it holds exactly at `λ + I* = 2(K-1)²/K³`, eq. (11); eqs. (12) and (14) |
| Proposition 1(iii) | `hasDerivAt_pay'`, `soc_at_symmetric` | the second derivative has the sign of `(3K-5)b - (2K-2)a`, which at the symmetric point is the sign of `K - 3` |
| The derivative of `Φ` (p.6) | `hasDerivAt_Phi`, `phi_deriv_printed_wrong` | the derivative is `2(K-1)³λ(λ(K-3) - 2I_k(K-1))/[λK+I_k(K-1)]⁴`, positive near `I_k = 0` for `K ≥ 4`; at `K = 5`, `λ = 3/10`, `I_k = 1/50` the printed value is negative and the true one positive |
| Proposition 1(ii) | `zero_inv_key`, `zero_inv_gain_le`, `zero_inv_sufficient`, `zero_inv_necessary`, `prop1ii_fails_K7` | investing nothing is a best reply to zero investment when `λ ≥ (K-1)(K+1)²/(4K³)`, and for `K > 3` not below it; at `K = 7`, `λ = 0.23` the printed condition holds and a deviation to `0.23` pays `(7/13)² - 0.23 > 1/49` |
| Proposition 1(iv) and Corollary 1 | `K3_zero_beats_candidate`, `cor1_condition_misstated`, `K2_candidate_beats_zero` | for `K = 3` investing nothing beats the candidate `I*` for every `λ` where `I* > 0`; the stated effort comparison holds at `λ = 0.23` while the payoff comparison fails; for `K = 2` the candidate beats investing nothing |

Not formalised: that for `K = 2` the candidate is a global best reply (the
paper's argument that the derivative is bell-shaped), and the step from a
positive second derivative at the symmetric point to a profitable deviation
for `K ≥ 4`.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| FU-D1 | 6 | "which is negative for any K ≥ 2" | The printed derivative of `Φ` (FU-P4) is wrong; the true one is positive near zero for `K ≥ 4` (`hasDerivAt_Phi`, `phi_deriv_printed_wrong`), so the proof of Proposition 1(ii) fails for `K ≥ 4` |
| FU-D2 | 5 | "equilibrium with zero investment, i.e., I*(K) = 0, exists if and only" | Proposition 1(ii) (FU-P2) holds for `K ≤ 3`. For `K ≥ 4` the exact condition is `λ ≥ (K-1)(K+1)²/(4K³)`, above the printed bound and tending to 1/4 (`zero_inv_sufficient`, `zero_inv_necessary`) |
| FU-D3 | 5 | "a unique symmetric equilibrium with positive equilibrium investment exists" | The condition in Proposition 1(iv) (FU-P3) compares efforts where payoffs are meant. With payoffs, `K = 3` has no symmetric equilibrium with positive investment for any `λ` (`K3_zero_beats_candidate`); with the stated efforts the condition holds at `λ = 0.23` (`cor1_condition_misstated`). Corollary 1 (FU-P5, FU-8) fails |
| FU-D4 | 7 | "However, it drops to zero when K reaches a large number" | At `λ` near 0.23 zero investment is not an equilibrium for any `K ≥ 4` (`zero_inv_necessary`, `prop1ii_fails_K7`), and positive investment is not either (Proposition 1(iii)), so Corollary 2 (FU-9) fails |
| FU-D5 | 7 | "By part (iii) of Proposition 1, we only need to check" | The condition is in part (iv) |

## Comparison with the model rows

Fu and Lu's contestants are identical, all compete, and choose a continuous
investment. Our payers choose a binary investment, those who do not pay still
compete, and they differ in the cost of paying. Their result concerns the size
of the intensive investment as the number of contestants grows. M17 and M18
concern the gain from a binary investment as the number of challengers grows.
So the paper is not a precedent for C7. Read within its own symmetric
pure-strategy equilibria, its claims of non-monotonicity do not hold.
