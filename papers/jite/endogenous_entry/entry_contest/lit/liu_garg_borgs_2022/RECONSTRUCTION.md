# Reconstruction of Liu, Garg and Borgs, "Strategic Ranking"

**Source read.** arXiv:2109.08240v2 (21 February 2022), 30 pages with a text layer, the version
of the AISTATS 2022 paper (PMLR 151: 2489-2518), read in full [F, arXiv version]. The PDF is
cached at `~/.cache/entry_contest/liu_garg_borgs_2022.pdf`. Pages are PDF pages. The main text
is two-column, so the suite reads the text layer in layout and in reading order.

**Why this paper.** The search of 15 October 2026 placed it closest to the entrants' share:
its *access*, the admission probability of a disadvantaged group, is the nearest construct
found.

## Model (pp.3-4, 7)

- A unit mass of applicants (a continuum). Skill rank `θ` is uniform, skill is `f(θ)`.
- Each applicant chooses an effort `e ≥ 0` at cost `p(e)`, strictly convex, and scores
  `v = g(e) f(θ)`, with `g` concave and strictly increasing.
- The school admits post-effort rank `θ` with probability `λ(θ)`, a non-decreasing step
  function with `E[λ] = ρ`, the capacity. It commits to `λ` before effort.
- Section 4: the score is multiplied by an environment factor `ψ_A > ψ_B`, for two groups of
  equal size and equal skill law.

## Results

1. **Rank preservation (Proposition 2.1).** In every equilibrium the post-effort reward equals
   the pre-effort one: effort does not change who is admitted.
2. **Equilibrium efforts (Theorem 2.2).** Effort works like a second-price payment: each band
   exerts just enough to keep the band below from overtaking it.
3. **Welfare (Section 3).** Pure randomization maximises applicant welfare (Proposition 3.1),
   and the school's private utility rises with the cut-off (Proposition 3.2). A three-level
   policy can beat deterministic admission for the school (Proposition 3.3). Societal utility
   can peak at an interior cut-off (Proposition 3.4).
4. **Groups (Section 4).** Rank preservation holds on an environment-scaled rank
   (Proposition 4.1). The welfare gap is non-negative and rises with the cut-off
   (Propositions 4.2, 4.3). Access is highest under pure randomization and, if `f⁻¹` is convex,
   non-increasing in the cut-off (Proposition 4.4).
5. **Appendix B.** With several skills and a linear `g`, rank is preserved on the best weighted
   skill. With an unmeasurable skill, some randomization can be optimal.

## What is formalised

See `lean/mathlib/LiuGargBorgs.lean`:

- the core of rank preservation: Lemma C.3, Lemma C.4 and the closing convexity step;
- Proposition 3.2;
- Proposition 4.4: access below `ρ`, access `ρ` under pure randomization, and access
  non-increasing in the cut-off, with a control.
