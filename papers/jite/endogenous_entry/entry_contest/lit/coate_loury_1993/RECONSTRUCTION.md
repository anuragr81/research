# Reconstruction of Coate and Loury, "Will Affirmative-Action Policies Eliminate Negative Stereotypes?"

**Source read.** *American Economic Review* 83(5), 1220-1240 (December 1993),
the JSTOR copy, read in full [F]. It is cached at
`~/.cache/entry_contest/coate_loury_1993.pdf`, with the formula pages rendered
in `~/.cache/entry_contest/coate_loury_pages/`.

**Why this paper.** The binary-investment search of 13 October 2026 found it as
a candidate for C12 and C13. In it a binary investment is read through a noisy
test, and those who do not invest can still be assigned to the good job.

## Model (Section I)

There are many identical employers and two groups of workers, B and W. A worker
becomes qualified for task one by investing at a private cost `c ~ G`, which has
the same law in both groups. The employer sees the group and a signal `θ ∈ [0,1]`
with densities `f_q` and `f_u`. Its likelihood ratio `φ = f_u/f_q` is
non-increasing. A qualified worker in task one earns the employer `x_q` and an
unqualified one costs `x_u`, with `r = x_q/x_u`; every worker gains `ω` from task one.

Given a belief `π` about a group, the employer uses the standard `s*(π)` of (2).
A worker facing the standard `s` gains `β(s) = ω(F_u(s) - F_q(s))` by investing,
so the fraction `G(β(s))` invests. An equilibrium is a pair of beliefs solving
(3), `π_i = G(β(s*(π_i)))`. A discriminatory equilibrium uses two different
solutions of (3).

## Results

1. **Proposition 1.** If WW lies above EE at some standard, (3) has at least two
   non-zero solutions, and the equilibria are Pareto-ranked.
2. **Proposition 2.** Affirmative action requires equal assignment rates
   `ρ(s_b, π_b) = ρ(s_w, π_w)`. If `ρ̂(s) = ρ(s, G(β(s)))` is decreasing on
   `[0,1]`, every equilibrium under the policy has equal beliefs. Condition
   (5) gives this.
3. **The uniform example (Section II.B).** Signals are uniform on `[θ_q, 1]` and
   `[0, θ_u]`, and costs are uniform on `[0, 1]`. A liberal employer gives an
   unclear result the benefit of the doubt, and investment is then
   `π_ℓ = ω(1 - p_u)`. A conservative employer does not, and investment is
   `π_c = ω(1 - p_q)`. Condition (6) gives a discriminatory equilibrium
   `(π_c, π_ℓ)`.
4. **Proposition 3.** If `π_ℓ > π̂ > π_c`, `π_ℓ > 1/2` and W's are numerous
   enough, employers comply with the policy by patronizing B's: they assign
   failing B's with the probability `α` of (7). The only stable belief about B's
   is then `1 - π_ℓ`, which is below `π_c` when `π_ℓ + π_c > 1`.
5. **Proposition 4 (Section III).** With smooth distributions, if `ρ̂' > 0` at
   some standard below `ŝ`, an open set of parameters has a patronizing
   equilibrium under affirmative action.
6. **Section IV.** With unequal cost laws, a marginal policy can widen the gap in
   investment. A subsidy to B workers for reaching task one can break a
   stereotype that a subsidy to employers cannot.

## What could not be reconstructed

Nothing material. The formulas were read from page images (CLAIMS rows P),
because the text layer garbles them. In footnote 21 it also misreads `r = 2/3` as
`r = 3` (CO-D1).
