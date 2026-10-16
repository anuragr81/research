# Austen-Smith and Fryer (2003), reconstruction

David Austen-Smith and Roland G. Fryer, Jr., "The Economics of 'Acting
White'", NBER Working Paper 9904, August 2003. Published, with a different
title, as "An Economic Analysis of 'Acting White'", *Quarterly Journal of
Economics* 120(2), 2005. The published version is not read (AS-D4).

## Source read

- The NBER working paper, 39 PDF pages. PDF page 1 is the NBER cover, PDF
  page 2 the abstract, and from PDF page 3 on the printed page is the PDF
  page minus 1. Cached at `~/.cache/paper_a/sources/austen_smith_fryer_2005.pdf`,
  707,216 bytes, sha256 `8705cf80…3ed9e`, PDF creation date 29 July 2003.
  Text layer at `~/.cache/paper_a/sources/austen_smith_fryer_2005.txt`,
  sha256 `625275d1…a023`.
- Read in full on 16 October 2026 from the text layer, every page, so the
  evidence tag is [F]. Page images at 110 dpi were read for PDF pages 11, 14,
  26, 28, 35 and 36.

## The model, from its own primitives

1. **Individuals** (PDF pp. 8–10). A type `θ` drawn from a smooth common
   prior `F` on `[0, θ̄)`, private to the individual and fixed. In the school
   years (`t = 0`) the individual chooses observable education `s ∈ [0, 1]`
   at cost `c(s, θ)` with the single-crossing conditions (1), and leisure
   `1 − s` worth `v(1 − s|a₀)`, more when accepted by the peer group (3).
2. **Contributions** (PDF p. 9). In each later period Nature sets a required
   contribution `κ_t ∈ {0, k}`, with `κ_t = 0` with probability `π`. Its
   effort cost to `θ` is `θk`, so higher types find contributing more onerous.
3. **Firms** (PDF p. 10). Bertrand bidding for output `y(s, θ)` gives the wage
   `w(s) = ∫ y(s, θ) dF(θ|s)`, eq. (8), the conditional mean of output given
   education. The same belief `F(θ|s)` is the group's (AS-Q5).
4. **Peer group** (PDF pp. 10–11). An anthropomorphised group gets `b > 0` in
   each period from an accepted member who contributes, `−Bκ_t` from one who
   does not, and 0 from a rejected individual. Assumption (7) is
   `π < min{Bk/(Bk + b), v(1|0)/v(1|1)}`.
5. **Equilibrium** (PDF pp. 12–13). Perfect Bayesian equilibrium, beliefs by
   Bayes' rule where possible.
6. **Post-school behaviour** (PDF pp. 14–15, Proposition 1). Under the peer
   pressure strategy [P1] (expel after one refusal) there is a unique critical
   type `θ̂(δ)`, rising in the individual's discount factor `δ`. Types at or
   below it always contribute, types above it refuse the high contribution.
   The group accepts in the school years if and only if
   `F(θ̂|σ(θ)) ≥ F₀`, eq. (11).
7. **School years** (PDF pp. 15–22). No fully separating equilibrium
   (Proposition 2). Accepted and rejected sets are intervals, and if the
   rejected types separate, some accepted types are above `θ̂` (Proposition 3).
   In D1 equilibria all accepted types pool on one education level and the
   rejected types separate (Proposition 4). D1 equilibria exist for impatient
   individuals and not for patient ones (Propositions 5 and 6). For patient
   individuals a refined set has several education levels among the accepted
   (Proposition 7). Some accepted types later renege and are expelled with
   the low wage (Corollary 1).
8. **Tolerant group** (PDF pp. 22–24). With cheap talk about intentions and a
   group that accepts everyone, a tolerant equilibrium exists if and only if
   `δ ≥ 1/(1 + π)` (Proposition 8), and the group prefers peer pressure if
   and only if a threshold on `F(θ̂|σ)` holds (Proposition 9, AS-D2).
9. **Conclusion** (PDF pp. 24–25). Race enters only here. Minority
   individuals face the peer audience and majority individuals do not, so one
   education level is reached by pooling in the minority and by separation in
   the majority (AS-Q13, AS-Q14).

## Derivation chain

- Lemma 1 (PDF pp. 26–27). Comparing (12) and (13) gives (14) (`eq14`). The left
  side of (15) is strictly decreasing in the type (`G_strictAnti`), positive at
  `θ = 0` by (3) and negative at `θ = 1/k` by (7), so it has a unique zero
  `θ̂(δ) ∈ (0, 1/k)` (`lemma1_exists_unique`), and [P2] is the best response
  (`lemma1_threshold`). The left side of (15) rises with `δ` at a fixed type,
  so `θ̂(δ)` rises with `δ` (`lemma1_mono`), and it tends to 0 as `δ` does
  (`lemma1_limit`). The paper uses implicit differentiation for the
  monotonicity, and the formalisation compares the two zeros directly.
- Lemma 2 (PDF pp. 27–28). The group's value of accepting is
  `z = F γb/(1−γ) + (1 − F) γ[πb − (1−π)Bk]/(1 − πγ)`, the second term from the
  lottery faced each period by a type above `θ̂`. `z ≥ 0` if and only if
  `F ≥ F₀` (`lemma2_accept_iff`), and (7) puts `F₀` in `(0, 1)` (`F0_mem`). The
  comparative statics of `F₀` stated on PDF p. 15 follow by cross-multiplying
  (`F0_anti_pi`, `F0_anti_gamma`, `F0_anti_b`, `F0_mono_Bk`).
- Proposition 1 is Lemmas 1 and 2 together (`prop1`).
- Lemma 3 (PDF pp. 28–29) adds the two incentive constraints and uses
  `c_sθ < 0` (`lemma3`). Lemma 4 (PDF pp. 29–30) compares the local incentive
  condition (18) with the first-order condition (19) (`lemma4_step`), and
  Remark 1 compares (19) across group decisions (`remark1`).
- Proposition 3(3) (PDF p. 31) rests on Lemma 2 at a revealed type below `θ̂`
  (`prop3_3_core`). A pool reaching above `θ̂` dilutes `F(θ̂|s)`, and under a
  uniform prior the pool `[0, θ*]` is accepted if and only if `θ* ≤ θ̂/F₀`
  (`pool_accept_iff`, `pool_admits_unreliable`). Corollary 1 follows in that
  case from Lemma 1 (`corollary1_uniform`).
- Proposition 8 (PDF pp. 36–37) compares honest and deceptive statements under
  [T1] (`prop8_honest_iff`) and checks the tolerant group's acceptance
  (`prop8_tolerant_accepts`). Proposition 9 (PDF p. 37) takes the difference
  of the two group payoffs (`prop9_derived`).

## What could not be reconstructed

- Proposition 2, which argues over separating strategies as functions with
  the concavity of `U` in `s`.
- Propositions 4 to 7, which rest on the D1 refinement (Cho and Sobel 1990)
  and, for Propositions 5 and 6, on the envelope argument for `Δ(θ̂(δ))`.
- Proposition 3, claims (1) and (2).
- Lemma 4's differential equation (17) and Remark 1's derivation of the order
  of the two first-order conditions from (3). The closing comparisons are
  formalised.
- Corollary 1 beyond a uniform prior, and Proposition 8's step to
  `θ^T ≤ θ̂(δ)` and its separating tolerant equilibrium.
- The printed threshold of Proposition 9 for `γ > 0` (AS-D2).
