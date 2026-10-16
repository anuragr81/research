# Kim and Loury (2012), reconstruction

Young Chul Kim and Glenn C. Loury, "Stereotypes and Inequality: A 'Signaling'
Theory of Identity Choice", *KDI Journal of Economic Policy* 34(2) (issue
2012 II, no. 115), 2012, pp. 1–15. Received 4 February 2012. JEL D60, J71,
J15.

## Source read

- The whole issue, a 209-page PDF, cached at `~/.cache/paper_a/novelty/kl_kdi.pdf`,
  sha256 `b97e4d37…97e1`, with its text layer at `kl_kdi.txt`, sha256
  `d4b3f712…a7b3`. The article is PDF pages 4 to 18, journal pp. 1 to 15
  (journal page = PDF page − 3). Nothing outside those pages is attributed.
  Pages in this file and in the Lean file are journal pages. Pages in
  `CLAIMS.md` are PDF pages of the issue, as the suite checks them.
- Read in full on 16 October 2026 from the text layer. The text layer loses
  most mathematical symbols, so page images of PDF pages 4 to 18 were rendered
  at 110 dpi with `pdftoppm` and read, which covers every equation, Lemma 1 and
  2, Propositions 1 and 2, and Figures 1 to 3. The images are reproducible
  from the pinned PDF and not pinned separately. Evidence tag [F].
- Not the published version in the *Journal of Public Economics* (2019), which
  is not read (KLI-D1). `kl_identity.txt` in the same cache is an earlier
  extract starting at the issue's contents page, and is not used (KLI-D8).

## The model, from its own primitives

1. **Affect** (p. 6). Each agent chooses an affect, a group identity,
   `i ∈ {A, B}`. Choosing A costs `k ∈ R`, with CDF `H`, symmetric,
   `H(k) = 1 − H(−k)`, and density single-peaked at 0. An agent chooses A if
   and only if the anticipated return exceeds `k`.
2. **Skill** (pp. 6–7). Each agent chooses `e ∈ {0, 1}` at cost `c ≥ 0`, CDF
   `G`, density single-peaked at `ĉ`. The costs `c` and `k` are independent
   (KLI-Q7). Ability means a low `c`. Skill is chosen, not given.
3. **Employers** (p. 7, eq. 1). Employers see the group and a signal `t`
   with density `f_e(t)` under MLRP. With `f(π, t) = π f₁(t) + (1 − π) f₀(t)`
   the posterior is `ρ(π, t) = π f₁(t)/f(π, t)`, and the wage is
   `W(π, t) = ω ρ(π, t)` (KLI-Q8, KLI-Q9). `π` is the employers' belief about
   the group's investment rate, its collective reputation.
4. **Payoffs** (pp. 7–8, eqs. 2 to 9). `V_e(π) = ∫₀¹ f_e(t) W(π, t) dt`, with
   `V_e′ > 0`. The return to skill is `R(π) = V₁(π) − V₀(π)`, concave by
   eq. (6), with `R(0) = R(1) = 0`, so it peaks at an interior `π̄`. A worker
   with cost `c` in a group believed to invest at `π` gets
   `U(π, c) = max {V₁(π) − c; V₀(π)}` (eq. 9) and invests if and only if
   `c ≤ R(π)`.
5. **PSE** (p. 8). With exogenous groups, a group's investment rate is
   `G(R(π))`, and a Phenotypic Stereotyping Equilibrium is `π̂ = G(R(π̂))`,
   the Coate–Loury fixed point. With `G` S-shaped there are one or three
   (Figure 1).
6. **ASE** (pp. 9–10, eqs. 10 to 14). With beliefs `(πA, πB)`, the gain from A
   is `ΔU(πA, πB; c) = U(πA, c) − U(πB, c)`. A type `(c, k)` chooses A if and
   only if `ΔU ≥ k`. The fraction choosing A is `Σ^A = ∫ H(ΔU) dG` (eq. 10),
   the fraction choosing A and investing is `σ^A = ∫₀^{R(πA)} H(ΔU) dG`
   (eq. 11), and the actual investment rate of group A is
   `φ(πA, πB) = σ^A/Σ^A` (eq. 14). An Affective Stereotyping Equilibrium is
   a pair with `π*_A = φ(π*_A, π*_B)` and `π*_B = φ(π*_B, π*_A)` (KLI-Q10).
   Each group's believed rate equals the actual rate of the members who chose
   it.

## Derivation chain of the main results

- **Eq. (15) and Lemma 1** (p. 10). `U(π, c) = max {R(π) − c; 0} + V₀(π)`
  (`eq_15`). Hence `ΔU` is `V₁(πA) − V₁(πB)` below both returns
  (`lemma1_low`), `V₀(πA) − V₀(πB)` above both (`lemma1_high`), and linear in
  `c` between them, eq. (16) (`lemma1_mid`). It is non-increasing in `c`
  when `R(πA) ≥ R(πB)` (`dU_antitone`). Derived from eq. (9) alone.
- **Proposition 1** (pp. 10–11). From `V_e′ > 0`, `πA > πB` gives
  `ΔU > 0` for every `c` (`prop1_pointwise`), and with `H(0) = 1/2` more than
  half choose A (`prop1`). Derived from the monotonicity of `V_e` and the
  symmetry of `H`, and conditioned on the beliefs `π`.
- **Lemma 2 and Proposition 2** (p. 11). When `R(πA) > R(πB)`, the weight
  `H(ΔU(c))` on a type is non-increasing in `c`, so the members of A are
  tilted toward low costs and `φ(πA, πB) > G(R(πA))`, while
  `φ(πB, πA) < G(R(πB))` (`selRate_ge`, `prop2`). Derived from Lemma 1, the
  monotonicity of `H`, and the independence of `c` and `k`. Conditioned on
  the returns `R`, not on the beliefs `π` (KLI-D2). The benchmark is the
  investment rate under random membership at the same beliefs, `G(R(π))`,
  not the group's rate before anyone moves.
- **Trivial ASE** (p. 10). With `πA = πB`, `ΔU = 0` and `φ(x, x) = G(R(x))`,
  so every PSE gives an ASE `(x, x)` (`trivial_ase`).
- **Non-trivial ASE and the inequality comparison** (p. 11). From
  Proposition 2 the authors argue that the pair `(π_h, π_l)` of two PSE
  cannot be stable, that two non-trivial ASE exist, and that inequality
  between endogenous groups "can be greater" (KLI-Q12, KLI-Q13). The
  argument speaks of entering cohorts and stability, but the article defines
  no law of motion and gives no existence proof (KLI-D5).
- **Ex-cons** (p. 12). A costly, certifiable programme that those who will
  go straight are more willing to join, because they gain more from a job
  (KLI-Q14). Stated in words, without a model.

## The dynamic structure

The article is static. The ASE is a fixed point of `(πA, πB) ↦
(φ(πA, πB), φ(πB, πA))`. The discussion on p. 11 reads the map as carrying a
group's current reputation to the skill rate of its entering cohort, and
argues from that reading that `(π_h, π_l)` is not stable. No law of motion is
written down, and neither stability nor convergence is proved.

## What could not be reconstructed

- The existence of a non-trivial ASE and the claim that endogenous-group
  inequality is "at least as great" (abstract, p. 2, and introduction, p. 6)
  are not proved in the
  article. The body says "can be greater" (KLI-Q13).
- Eqs. (2) to (8) are checked by hand against the image of p. 11 and
  `R(π) < ω` on p. 12 by hand. They are not formalised. `V₀` and `V₁` enter
  the Lean file as given functions, so the hump of `R` is not derived there.
- The continuous `G` is replaced by a finite distribution in Lean, and the
  strict inequalities of Proposition 2 are proved in weak form (KLI-D6).
- The signal's support is `R+` in the text and `[0, 1]` in the integrals
  (KLI-D4).
