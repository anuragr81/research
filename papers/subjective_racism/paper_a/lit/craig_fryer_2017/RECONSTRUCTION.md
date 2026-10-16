# Craig and Fryer (2017), reconstruction

Ashley C. Craig and Roland G. Fryer, Jr, "Complementary Bias: A Model of
Two-Sided Statistical Discrimination", NBER Working Paper 23811, September
2017. 50 PDF pages.

## Source read

- Cached at `~/.cache/paper_a/sources/complementary_bias_w23811.pdf`
  (598,425 bytes), sha256 `86f10e79…d558707`, with the text layer extracted to
  `~/.cache/paper_a/sources/complementary_bias_w23811.txt`, sha256
  `696451d2…ca402`. The text has 51 form-feed segments. Segment `n − 1` is PDF
  page `n`. PDF pages 1 and 2 are the NBER cover and abstract, and from PDF
  page 3 on the printed page is the PDF page minus one (CF-D1). Segment 50 is
  empty.
- Every page, 1 to 50, was read from the text on 16 October 2026, so the
  evidence tag is [F]. PDF pages 10, 11 and 16, where the extraction drops the
  bars of `ϖ` and `χ̄` and garbles fractions, were also read from 110 dpi
  images rendered with `pdftoppm`. Nothing beyond these pages is attributed.

## The model, from its own primitives

1. **Players and groups** (PDF p. 8). Many employers, each matched with many
   workers. Workers belong to one of two identifiable groups `j ∈ {A, B}` with
   shares `λ_A`, `λ_B`.
2. **Types** (PDF p. 8). A worker's type is her cost `c` of investing in human
   capital, with distribution `G^W`. An employer's type is its cost `k_j` of a
   group `j` investment in its workplace, with distribution `G^E`. Investment is
   binary on both sides.
3. **Signals** (PDF p. 9). The employer sees `θ ∈ [0, 1]` about the worker's
   investment, with densities `f_q^W`, `f_u^W` and `φ = f_u^W/f_q^W`
   non-increasing. The worker sees `ψ ∈ [0, 1]` about the employer's group `j`
   investment, with `f_q^E`, `f_u^E` and `τ = f_u^E/f_q^E` non-increasing.
4. **Mutual consent** (PDF p. 9). The worker applies or not after seeing `ψ`.
   The employer then hires or not after seeing `θ`. A match needs both.
5. **Payoffs** (PDF pp. 9–10). A hired worker gets `ω_q` from an employer who
   invested and `−ω_u` from one who did not, less her cost `c` if she invested,
   which she pays whether hired or not. The employer gets `χ_q` from a
   qualified hire and `−χ_u` from an unqualified one, less `k_j` if it invested.
   The gain from a match does not depend on one's own investment
   (`match_gain_indep_own`). Appendix 8.3.1 makes this an explicit assumption
   (CF-Q10).
6. **Two-sided inference** (PDF pp. 10–11). The employer's prior that a group
   `j` worker invested is `π_j`, and its posterior is
   `κ(π_j, θ) = π_j f_q^W/(π_j f_q^W + (1 − π_j) f_u^W)`. It hires above
   `s*_j(π_j) = min{θ : χ_q/χ_u > ((1 − π_j)/π_j) φ(θ)}`. The worker's prior
   that an employer made her group's investment is `δ_j`, her posterior is
   `ξ(δ_j, ψ)` of the same form, and she applies above `t*_j(δ_j)`, the same
   rule with `ω_q, ω_u, τ` (`offer_iff`). Both priors are group specific, so
   each side treats the other side's group as a prior and the signal as
   individual evidence.
7. **Returns** (PDF p. 11). `β_W(s_j, δ_j) = [F_u^W(s_j) − F_q^W(s_j)] ϖ(δ_j)`
   (eq. 1) and `β_E(t_j, π_j | λ_j) = λ_j [F_u^E(t_j) − F_q^E(t_j)] χ̄(π_j)`
   (eq. 2).
8. **Equilibrium** (PDF p. 12). A pair `{π, δ}` with `π_j = G^W(β_W(s_j, δ_j))`
   (eq. 3) and `δ_j = G^E(β_E(t_j, π_j | λ_j))` (eq. 4). Each side's belief
   about the other group equals the investment rate that the other side's
   best response produces.

## Derivation chain of the main results

- **Nesting** (PDF p. 12). Fixing `δ` gives Coate and Loury (1993) with wage
  `ϖ(δ)`. Fixing `π` gives the same model with the roles reversed.
- **Zero equilibrium** (PDF p. 13). `π = δ = 0`, `s = t = 1` always solves
  (3) and (4).
- **Proposition 1** (PDF p. 13, proof PDF p. 33). The largest solutions
  `π̄*(δ)` and `δ̄*(π)` are increasing, and a crossing argument gives a
  non-zero solution, hence multiple equilibria.
- **Dynamics** (PDF pp. 14–17). A backward-looking adjustment process, its
  linearisation, and a stability condition. The zero equilibrium is locally
  stable.
- **Example** (Section 3.1, PDF pp. 16–18). Uniform costs and signals give
  piecewise linear returns, `β_W = 7δ/4 − 1/4` for `δ ≥ δ̂` and `3δ/4`
  otherwise, and the same in `π` for `β_E`, with `δ̂ = π̂ = 1/4`
  (`hats`, `betaW_primitives`, `betaE_primitives`). The equilibria are exactly
  `Z = (0, 0)`, `U = (1/3, 1/3)` and `S = (1, 1)` (`three_equilibria`). Holding
  firm investment at any positive rate removes the zero equilibrium
  (`control_one_sided_zero_fails`).
- **Group size** (PDF pp. 12, 20). `β_E` scales with `λ_j` and the firm cost
  does not, so a smaller group gets less firm investment, and no equilibrium
  with positive investment has equal employer beliefs when `λ_A ≠ λ_B`
  (Proposition 3, `control_minority_breaks_U` in the example). With costs
  proportional to hires the disadvantage disappears (PDF p. 47).
- **Policy** (Sections 4 and 5). Affirmative action, quotas and subsidies on
  one side can fail or harm (Propositions 2, 4, 5, 6, 9). Proposition 7 says
  an intervention must set both investment margins, or one investment margin
  and the other side's threshold. Two-sided investment insurance
  (Proposition 8) reaches equal beliefs immediately.
- **Empirical test** (Section 6, PDF pp. 27–32). With log-normal ability and
  wages equal to marginal product, the wage elasticity to ability is
  `σ²_a/(σ²_ε + σ²_a)`, the employer's shrinkage toward the group mean.
  Proposition 10 shows the switcher regression understates the black-white
  gap in that elasticity.

## What could not be reconstructed

- Propositions 1 to 10 in general. Only the posterior and threshold rule, the
  independence of the match gain from one's own investment, and the Section
  3.1 example are formalised. The example is a special case, and the general
  existence argument is not formalised.
- The stability analysis of Section 3.F and the wage-regression algebra of
  Section 6 and Proposition 10 are read but not checked.
