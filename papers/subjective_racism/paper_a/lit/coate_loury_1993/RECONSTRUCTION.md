# Coate and Loury (1993), reconstruction

Stephen Coate and Glenn C. Loury, "Will Affirmative-Action Policies Eliminate
Negative Stereotypes?", *American Economic Review* 83(5), December 1993,
pp. 1220–1240.

## Source read

- JSTOR PDF of the published article, 22 pages (a JSTOR cover page and pages
  1220 to 1240), downloaded on 15 October 2026 from the author's Drive folder
  `temp`, file `Coate&Loury_AER1993.pdf` (Drive id
  `1pdgu3oHukYffnqmbPHhM0aB2eN75laWK`, 562,446 bytes).
- Cached at `~/.cache/paper_a/coate_loury_1993.pdf`, sha256
  `a3389b46…259405`. Text layer extracted with `pdftotext` to
  `~/.cache/paper_a/coate_loury_1993.txt`, sha256 `857009c7…d251`.
- Read in full [F], every page, on 15 October 2026. The text layer garbles
  Greek letters and fractions (π, φ, θ, ξ), so equations (1), (2), (3), (6),
  (7), (8) and (9), Proposition 1 and Proposition 3 were read from page
  images rendered at 110 dpi (pages 1224, 1225, 1226, 1231, 1232).

## The model, from its own primitives

1. **Agents and timing** (p. 1223, Figure 1). Many identical employers, a
   larger population of workers in two identifiable groups, B and W, with
   `λ` the fraction of W's. Nature draws each worker's group and investment
   cost `c`, distributed `G` and the same for both groups. Workers decide
   whether to invest. They are matched with employers, a signal
   `θ ∈ [0,1]` is observed, and employers assign each worker to task zero or
   task one.
2. **Payoffs** (p. 1223). A worker assigned to task one gets `ω`. An employer
   gains `x_q` from a qualified worker in task one and loses `x_u` from an
   unqualified one, with `r = x_q/x_u`. Only investors are qualified.
3. **Signal** (p. 1223). Densities `f_q`, `f_u`, likelihood ratio
   `φ(θ) = f_u(θ)/f_q(θ)`, assumed nonincreasing.
4. **Employer's inference** (p. 1224, eq. 1). With prior `π` that a group
   member is qualified, Bayes' rule gives
   `ξ(π,θ) = π f_q / [π f_q + (1−π) f_u] = 1/{1 + [(1−π)/π] φ(θ)}`.
5. **Employer's decision** (p. 1224, eq. 2). Assign to task one iff
   `r ≥ [(1−π)/π] φ(θ)`, so a threshold standard `s*(π)`, decreasing in `π`.
6. **Worker's decision** (p. 1225). Facing standard `s`, investing raises the
   chance of task one by `F_u(s) − F_q(s)`, so the benefit is
   `β(s) = ω[F_u(s) − F_q(s)]`, single-peaked, `β(0) = β(1) = 0`. The
   investing fraction is `G(β(s))`.
7. **Equilibrium** (p. 1225, Definition 1, eq. 3). Beliefs `(π_b, π_w)` with
   `π_i = G(β(s*(π_i)))`. The belief about each group is a fixed point of the
   one-dimensional map `π ↦ G(β(s*(π)))`, so in equilibrium employers' beliefs
   are correct. A discriminatory equilibrium has `π_b < π_w`, possible only
   because (3) has several solutions, and the groups are identical ex ante.
8. **Existence of several solutions** (p. 1226, Proposition 1). Under
   continuity and strict monotonicity of `φ` and `G(0) = 0`, if
   `G(β(s)) > φ(s)/[r + φ(s)]` for some `s`, there are at least two nonzero
   solutions. The EE locus is `π = φ(s)/[r + φ(s)]`. The no-investment belief
   `π = 0` always solves (3) when `G(0) = 0`.
9. **Stability** (p. 1226). The adjustment process
   `π^{t+1} = G(β(s*(π^t)))` converges to a solution near it only if EE is
   steeper than WW there. Footnote 11 says the no-investment equilibrium is
   locally stable.
10. **Affirmative action** (pp. 1227–1228, eq. 4, Definition 2). An employer
    must assign the two groups to task one at equal rates, and chooses
    standards to maximise expected payoff subject to that constraint.
11. **Proposition 2** (p. 1229). If `ρ(s)`, the assignment rate a group
    facing `s` would have in a self-confirming equilibrium, is decreasing,
    every equilibrium under affirmative action has equal beliefs.
12. **The uniform example** (pp. 1230–1232). Costs uniform on `[0,1]`, the
    signal yields pass, fail or unclear. Without affirmative action an
    employer is liberal (`s = θ_q`) or conservative (`s = θ_u`), with
    self-confirming beliefs `π_ℓ = ω(1 − p_u)` and `π_c = ω(1 − p_q)`, and
    `(π_b, π_w) = (π_c, π_ℓ)` is a discriminatory equilibrium under (6). Under
    affirmative action an employer patronizes B's with probability
    `α(π_b) = (π_ℓ − π_b)/(1 − π_b)` (eq. 7), B's invest at
    `[1 − α(π_b)] π_ℓ` (eq. 8), and beliefs follow
    `π_b^{t+1} = [(1 − π_ℓ)/(1 − π_b^t)] π_ℓ` (eq. 9) from `π_b^0 = π_c`. The
    two equilibrium beliefs are `π_ℓ` and `1 − π_ℓ`. For `π_ℓ > 1/2` the
    orbit converges to `1 − π_ℓ`, the patronizing equilibrium
    (Proposition 3).
13. **Proposition 4** (p. 1234). With smooth distributions, if `ρ'(s) > 0`
    for some `s < ŝ`, there is an open set of parameters with a patronizing
    equilibrium under affirmative action.
14. **Subsidies** (pp. 1236–1238). A marginal subsidy to employers or to
    B workers raises `π_b` locally but cannot leave the basin of attraction of
    the original belief (footnote 25). A sufficiently large worker subsidy
    removes every locally stable nonzero self-confirming belief except one,
    and can then be phased out.

## Derivation chain of what Paper A uses

- Eq. (1) is Bayes' rule, and (2) follows from comparing the expected payoff
  `ξ x_q − (1 − ξ) x_u` with zero (`assign_iff`).
- The EE locus is (2) at equality, solved for `π` (`EE_locus`).
- `π = 0` solves (3) from `G(0) = 0` and `β(1) = 0`
  (`zero_self_confirming`).
- Eq. (9) is (8) read as an adjustment process. Its fixed points are `π_ℓ`
  and `1 − π_ℓ` (`g_fixed_iff`), and for `π_ℓ > 1/2` every start below
  `π_ℓ` converges to `1 − π_ℓ` (`converges_to_patronizing`).

## What could not be reconstructed

- Proposition 4's proof uses the implicit-function theorem and a figure
  argument. Paper A does not rely on it, and it is not formalised.
- Condition (5) on p. 1229 is partly illegible in the text layer and only
  partly legible in the image. Paper A does not rely on it.
- The welfare ranking of equilibria (p. 1226) is not formalised.
