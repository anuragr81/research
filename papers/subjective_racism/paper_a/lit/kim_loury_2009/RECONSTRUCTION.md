# Kim and Loury (2009), reconstruction

Young Chul Kim and Glenn C. Loury, "Group Reputation and the Dynamics of
Statistical Discrimination", MPRA Paper No. 18765, version dated November 12,
2009, 42 PDF pages.

## Source read

- The MPRA posting of the working paper. The MPRA cover (PDF p. 1) gives the
  date 1 May 2009 and "posted 20 Nov 2009", and the title page (PDF p. 2)
  dates the paper November 12, 2009 (KLA-D8). PDF page `n` is printed page
  `n − 1` from PDF p. 3 to PDF p. 32. PDF pp. 33 to 42 are figures.
- Cached at `~/.cache/paper_a/novelty/mpra18765.pdf`, sha256
  `b898397b…de27`, with its text layer extracted to
  `~/.cache/paper_a/novelty/mpra18765.txt`, sha256 `11a605f0…bd5d`. Pages
  of the text are split by form feeds, index 0 being PDF p. 1.
- Every page, PDF pp. 1 to 42, was read in full from the text on 16 October
  2026, so the evidence tag is [F]. PDF pp. 6, 7, 11, 12, 16, 17, 20 and 21,
  where the extraction garbles equations, were also read from 110 dpi page
  images rendered from the pinned PDF.
- This is an earlier version of the paper later posted as MPRA 54950,
  "Collective Reputation and the Dynamics of Statistical Discrimination"
  (2014), and published in the *International Economic Review* (2018). The
  later version has its own record. The published article is not read
  (KLA-D9).

## The model, from its own primitives

1. **Static stage** (pp. 6–7, section 2). This is the Coate and Loury (1993)
   job assignment. Identical employers assign workers to task one or task
   zero. A worker assigned to task one gets `w`. The employer gets `Xq` from a
   qualified worker in task one and loses `Xu` on an unqualified one, with
   `ρ = Xq/Xu`. The employer sees the worker's group `i` and a signal
   `θ ∈ [0, θ̄]` with densities `fq`, `fu` and a nonincreasing likelihood ratio
   `ψ = fu/fq`. Given the group's proportion qualified `Π^i`, the hiring
   standard is `s*(Π^i) = min{θ | ψ(θ) ≤ ρΠ^i/(1 − Π^i)}` (eq. 1). A worker
   with cost `c` invests iff `W[Fu(s) − Fq(s)] ≥ c`, so the group invests at
   rate `G(β̃(s))`. Proposition 1 gives at least three steady states under a
   crossing condition.
2. **Group reputation** (p. 8, eqs. 2–3). Employers do not hold a prior. They
   read the group's proportion qualified from the observed aggregate signal
   distribution, `Π^i = (Fu − F^i)/(Fu − Fq)`, and call it group reputation.
   Individual reputation is the posterior `R(i, θ) = Π^i fq/(Π^i fq + (1 − Π^i) fu)`,
   increasing in `Π^i` (p. 9).
3. **Dynamics** (pp. 9–10, eqs. 4–9, Theorem 1). Workers invest once at birth,
   die at Poisson rate `λ`, discount at `δ`. The normalised lifetime benefit
   `V_t = (δ + λ)∫ β_τ e^{−(δ+λ)(τ−t)} dτ` is a jump variable and
   `V̇ = (δ + λ)(V − β)`. Group reputation is a stock with `Π̇ = λ(φ − Π)` and
   `φ = G(V/(δ + λ))` (eq. 11).
4. **Simple model** (pp. 10–12, section 3.3). Uniform signals, `fu` on
   `[0, θu]` and `fq` on `[θq, θ̄]`, `θq < θu`. A signal in `(θq, θu)` is
   unclear. `Pq = (θ̄ − θu)/(θ̄ − θq)`, `Pu = θq/θu`, and Assumption 1 is
   `Pq < Pu`, equivalently `θq + θu > θ̄`. Costs take three values, so a
   fraction `Πl` always invests, `1 − Πh` never does, and the rest invest iff
   `V ≥ (δ + λ)cm`. Employers give the benefit of the doubt (BOD) to unclear
   signals iff `Π ≥ Π* = (1 − Pu)/(ρ(1 − Pq) + 1 − Pu)`, equal to
   `(θ̄ − θq)/(ρθu + θ̄ − θq)`. The current benefit of qualifying is `β = ωPu`
   with BOD and `ωPq` without (eq. 10). With `Π* ∈ (Πl, Πh)` and
   `(δ + λ)cm ∈ (ωPq, ωPu)` the steady states are `Qh(ωPu, Πh)`, `Ql(ωPq, Πl)`,
   and a conditional `Qm` that needs a mixed assignment, which the authors
   note is inconsistent with eq. (1) (footnote 1, KLA-Q8).
5. **Phase analysis** (pp. 12–16). In the normalised `(v, Π)` plane `Qm` is
   unstable and paths spiral out of it (Lemma 1), paths are concave or convex
   on either side of `v*` (Lemma 2), `Qh` and `Ql` are economically stable
   (Proposition 2), and `Qh` Pareto dominates `Ql` for every worker type and
   for employers (Proposition 3). The overlap `[π^o, π^p]` is the range of
   initial reputations from which either state is reachable by expectations.
   Below it lies the reputation trap (Definitions 2, 3). Lemma 3 gives a
   separation condition, eq. (12) the overlap size, and Proposition 4 its
   comparative statics in `δ`, `λ`, `cm`.
6. **General model** (pp. 16–17, section 3.4). The steady states of the
   dynamic system are those of the static model (footnote 2). `Qh`, `Ql` are
   saddles and `Qm` a source (Lemma 4). Paths spiral around `Qm` when `δ/λ`
   is small (Lemma 5). Theorem 2 gives two economically stable states with
   overlapping paths.
7. **Application** (pp. 17–18, section 4). A group starting very low after
   overt discrimination rises only to `Πl` when `Ql` is a trap, while a group
   starting in the overlap can take the optimistic path. The two groups have
   identical underlying characteristics (KLA-Q10).
8. **Monopolistic principals** (pp. 18–20, section 5, Proposition 5).
   Competitive employers cannot move a trapped group (KLA-Q11). Coordinated
   employers can lower the BOD threshold from `Π*` to `Π*′` or subsidise the
   training cost by `S′`, and do so iff the discounted profits (eqs. 13, 14)
   exceed `YQl = Pq Πl xq / r`.
9. **Further discussion** (pp. 20–21, section 6). Strong and weak optimism
   and pessimism along spiralling paths. Market learning (Appendix A).
   White flight and tipping points in housing as a proposed application,
   with no model (KLA-Q12).
10. **Conclusion** (pp. 21–22). Proposed extensions to crime, brands and
    college reputation, with no model (KLA-Q13).
11. **Appendix A** (pp. 23–24). A worker's type is revealed at Poisson rate
    `η`. The `V̇ = 0` locus becomes `((δ + λ)β + ηω)/(δ + λ + η)`, which shifts
    right with `η`, and `Ql` stops being a steady state when
    `η > η* = (δ + λ)((δ + λ)cm − wPq)/(w − (δ + λ)cm)` (KLA-Q14).
12. **Appendix B** (pp. 25–29). Proofs of Lemmas 1, 2, 4, 5, Proposition 3
    and Appendix A.

## Derivation chain of what Paper A could use or contrast with

- Assumption 1 in the uniform case is `θ̄ < θq + θu` (`assumption1_iff`).
- The employer's expected payoff on an unclear signal is nonnegative iff
  `Π ≥ Π*` (`bod_iff`), and `Π*` has the uniform closed form of p. 11
  (`piStar_uniform`).
- `β = ωPu` with BOD and `ωPq` without (`beta_values`), so with `Pq ≤ Pu` the
  benefit of investing is nondecreasing in reputation (`beta_mono`). This is
  the complementarity behind multiplicity. `R` is strictly increasing in `Π`
  (`R_strictMono`).
- Both `Qh` and `Ql` are fixed points of `Π = φ(β(Π))` under the two interval
  conditions (`two_steady_states`).
- Employers earn more per period at `Qh` than at `Ql` when `Πl < Π* < Πh`,
  which uses `bod_iff` at `Πh` and `Πh > Πl` (`employer_prefers_Qh`).
- The Jacobian of Lemma 4 has trace `δ` and determinant
  `λ(δ + λ)(β′φ′ − 1)` (`jacobian_trace_det`), and the spiral condition of
  Lemma 5 is the sign of the discriminant (`spiral_iff`).
- The market-learning threshold `η*` (`market_learning_threshold`), and the
  benefit `ω` independent of reputation when the signal reveals type
  (`revealed_type_no_group_effect`).

## Derivation chain of the dynamics

- Within a region of the `(v, Π)` plane both equations are linear, so a path
  is `v = ξ + (v₀ − ξ)e^{(δ+λ)t}`, `Π = X + (Π₀ − X)e^{−λt}` (`V_path`,
  `reputation_path`), and `|v − ξ|^{ρ′}(Π − X)` is constant along it
  (`region_invariant`). This is (15), (16), (18), (19).
- Eliminating the crossing points gives (17) and (20) (`eq17`, `eq20`).
  Tangent lines to the concave `x^{ρ′}` then show the return point lies
  farther out (`lemma1_spiral_out`). At `δ = 0`, `ρ′ = 1`, the orbit closes
  (`control_lemma1_vortex`).
- Solving the invariant for `Π` gives `Π = X + C|v − ξ|^{−ρ′}`, convex or
  concave by the sign of `C` (`lemma2_right`, `lemma2_left`).
- The vertical lines `v = 1` and `v = 0` carry paths into `Qh` and `Ql`
  (`prop2_Qh`, `prop2_Ql`).
- The invariant through `(1, Π*)` and `(0, Π*)` gives `π^o` and `π^p`
  (`piO_on_path`, `piP_on_path`), hence Lemma 3 by logarithms, eq. (12),
  and Proposition 4 by monotonicity of `x^{−ρ′}` in the base and in the
  exponent.
- Section 5 uses the same invariant to place the path through `(v*, Πl)`
  (`prop5_threshold_path`) or `(v*′, Πl)` (`prop5_subsidy_path`).
- Appendix A replaces `δ + λ` by `δ + λ + η` in the discount and adds the
  value of revelation (`appA_revelation_value`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` has a row for every numbered result. Of its
47 rows, 34 are formalised in the paper's generality, 7 in a special case and
6 not at all.

- **Not formalised.** Theorem 2 rests on the stable manifold theorem and on
  the phase diagram of Figure 5, and Mathlib has neither. Eqs. (13) and (14)
  are defined in the text and not evaluated there, so only their parts are
  checked (`YQl_eq`, `prop5_TC`). The rise of `Πh` and `Πl` with `η` in the
  general model is read from Appendix Figure 1. The p. 6 remark on more
  informative signals, footnote 4 and the verbal sections 4 and 6 make no
  precise claim.
- **Special cases.** `Fq ≤ Fu` is proved for the uniform signals of section
  3.3, not for a general monotone likelihood ratio. Eqs. (4), (5) and (7)
  are proved where `β` or `φ` is constant, which covers every path of the
  simple model piece by piece but not a general `β`. Lemma 4 is the linear
  algebra at a steady state, with the sign of `β′φ′ − 1` at each state taken
  as a hypothesis read from Figure 5, and without the linearisation theorem.
  Lemma 3 is proved in the form that follows from the printed `π^o` and
  `π^p`, which differs from the printed condition (KLA-D2). Appendix A's
  differential equation is checked through its locus only.
- **Hypotheses added.** Proposition 1 needs boundary signs at `0` and `θ̄`
  that the paper leaves unstated (KLA-D10). Lemma 1 takes the crossing
  points of Appendix Figure 2 as given and is proved by a tangent-line
  argument rather than by the paper's conditions 1 and 2.
