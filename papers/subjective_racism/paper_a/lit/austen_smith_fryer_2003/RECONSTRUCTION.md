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

## Derivation chain of what Paper A1 uses

- The group's value of accepting is `z = F γb/(1−γ) + (1 − F) γ[πb − (1−π)Bk]/(1 − πγ)`
  (PDF p. 28), the second term from the lottery faced each period by a type
  above `θ̂`. `z ≥ 0` if and only if `F ≥ F₀` (`lemma2_accept_iff`), and (7)
  puts `F₀` in `(0, 1)` (`F0_mem`).
- A pool reaching above `θ̂` dilutes `F(θ̂|s)`. Under a uniform prior the pool
  `[0, θ*]` has `F = θ̂/θ*` and is accepted if and only if `θ* ≤ θ̂/F₀`
  (`pool_accept_iff`), so types up to `θ̂/F₀`, above `θ̂`, are accepted
  (`pool_admits_unreliable`). This is the mechanism of Proposition 3(3), shown
  here for a uniform prior only.
- Proposition 8's condition comes from comparing honest and deceptive
  statements under [T1] (`prop8_honest_iff`). Proposition 9's threshold comes
  from the difference of the two group payoffs (`prop9_derived`).

## What could not be reconstructed

- Propositions 2 and 4 to 7 rest on D1 arguments (Cho and Sobel 1990) and
  are not formalised.
- The existence of `θ̂(δ)` and its monotonicity in `δ` (Lemma 1) need the
  functional form of `v` and are taken as given.
- The printed threshold of Proposition 9 is not reproduced for `γ > 0`
  (AS-D2).
