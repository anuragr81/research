# Notes — Costrell & Loury, pass 1

Run: `python3 verify_cl.py` — **10 checks, 0 failures** (as of the Lean pass,
2026-10-06; pass 1 had 6 checks). The Lean pass is the last section of this
file.

**Scope.** Our reading is arithmetically consistent **except for CL-I**, set
out below. Nothing here reproves any result of the paper.

---

## What the survey gets right, and it is most of it

§sec:assignment is careful and holds up against the source:

- **CL-A/CL-B confirmed almost verbatim.** The single-sufficient-statistic
  claim is the paper's own (p.9: *"the role of the ability distribution in the
  wage function is entirely captured by a single sufficient statistic,
  `μ̂ = F⁻¹(θ)`"*), and the pivot rule is **stated by them**, not inferred by
  us: *"the effect of a more unequal ability distribution depends on whether
  the worker on the margin between jobs, at quantile `θ`, lies in the upper or
  lower tail [...] `θ` is high [...] raises `μ̂` and narrows the wage
  distribution. Conversely, if `θ` is low [...] reduces `μ̂` and widens the
  wage distribution."* The survey's "single-pivot precedent" framing is
  well-founded.
- **CL-F confirmed exactly.** The Appendix proof does construct
  `Γ(ρ) = ∫_ρ^1[G⁻¹−F⁻¹]dx`, sign it at the crossings, extend it to all `ρ`
  via `Γ(0)=Γ(1)=0`, and integrate against `dβ`.
- **CL-D, CL-G, CL-H** confirmed against Prop 6, Theorem 1 and Prop 10.

---

## ERROR FOUND: CL-I misdescribes Proposition 5's hypotheses

The "Consequence for open item 2" paragraph says Suen's Prop 2 and
Costrell–Loury's Prop 5 sign an aggregate **"using the same technique:
quantile-difference decomposition at the crossing points, concavity of the
payoff map, and a hazard-rate or log-concavity condition on the own-side
distribution."**

For Costrell–Loury that is wrong on two of the three counts.

1. **The condition is monotonicity, not concavity.** The proof's final step is
   `Q(G) − Q(F) = ∫₀^1 Γ(p)dβ(p) ≥ 0`, and the paper's own words for why are
   *"recall that `β(·)` is **non-decreasing**"*. Concavity of `β` appears in
   **Proposition 6**, a different result about wage *inequality*. Proposition 5
   is an *output* result and needs only that `β` is non-decreasing.

2. **There is no hazard-rate or log-concavity condition at all.** Searching the
   full text of the draft for `log-concav` and `hazard` returns **zero
   occurrences**. That condition belongs to Suen — and the survey's own
   §sec:assignment paragraph correctly attributes it there (*"concavity of
   `θ, φ` and log-concavity of `1−F`"*). The error is confined to the
   synthesis paragraph, which merges the two papers' hypotheses and then
   attributes the union to both.

**Why this matters.** The synthesis is not decorative — it drives the proposed
route for open item 2: *"replace the pivot hypothesis with log-concavity of
`1 − Λ` and apply the Suen integration."* If Costrell–Loury reach a
general-spread aggregate result with **only monotonicity of the weight**, then
log-concavity may be a stronger hypothesis than this class of argument needs,
and the cheaper route should be tried first. The handover repeats the same
framing in its next-steps item 3, so the error is in two places.

**Suggested replacement** for the synthesis paragraph:

> Two independent published sources sign an aggregate under a general
> mean-preserving spread, by the same first move but under different
> hypotheses. Both decompose the quantile difference at the crossing points.
> Costrell–Loury's Proposition 5 then needs only that the weight `β` is
> **non-decreasing**; Suen's Proposition 2 needs concavity of `θ, φ` together
> with log-concavity of `1−F`. The weaker of the two hypotheses is
> Costrell–Loury's, and it is the one to try against P9-gen first.

**Note this is a lead, not a result.** Costrell–Loury sign a smooth aggregate
`Q`; `k*` is an integer count with a threshold, not an integral against a
monotone weight. Whether the argument transfers at all is exactly what has not
been tried. It should not be claimed until it has.

---

## Results

| ID | Result |
|---|---|
| CL-1 | CONSISTENT. `∂W/∂μ̂ = +(1−θ)(β₁−β₀) > 0` below the margin and `−θ(β₁−β₀) < 0` above. Opposite signs, as claimed. |
| CL-2 | CONSISTENT. The two branches agree exactly at `μ = μ̂`; a transcription check on (1)–(2). |
| CL-3 | CONSISTENT. Slopes are `β₀` and `β₁`, matching the no-arbitrage condition. |
| CL-4 | CONSISTENT. Non-negative integrand against a non-decreasing integrator is non-negative — the operative condition is monotonicity. |
| CL-5 | CONTROL passes. With `Γ = 2, dβ = −3` the contribution is `−6 < 0`, so CL-4 tests the hypothesis rather than restating an identity. |
| CL-6 | CONSISTENT. On an explicit mean-preserving pair (`F⁻¹ = p`, `G⁻¹ = 3p²−2p³`, mean preserved, single crossing at `p = 1/2`, range `[0,1]`), a convex `ψ` integrates higher (`+0.038`) and a concave `ψ` lower (`−0.027`), as Lemma 1 states. |

## Bug found and fixed during this pass

CL-6's first spread quantile left `[0,1]`, making `√x` complex and crashing the
comparison. Replaced with `G⁻¹ = 3p² − 2p³`, which pins both endpoints and
stays inside `[0,1]` — which is what CL's atomless-full-support hypothesis
actually requires. A second, softer bug followed: I asserted a strictly
positive slope, but this quantile has slope `6p(1−p)`, zero **at the two
endpoints**. Relaxed to non-negative on `[0,1]` with strict positivity checked
in the interior, matching "strictly increasing on `[0,1]`" as CL use it.

## The disanalogy that CL-C must carry

Recorded fully in `RECONSTRUCTION.md` §7; the short version:

In Costrell–Loury the marginal worker's quantile `θ` is **fixed by
technology** — jobs come in fixed proportions, so `θ` does not move when `F`
does; only `μ̂ = F⁻¹(θ)` moves. In P9-gen the marginal entrant's index `k*` is
**endogenous** and is what the comparative static is about. The analogy is in
the sign logic, not in the status of the margin. Their object is a continuum
wage schedule; ours is an integer count. If P9-gen is presented as "the
entry-count analogue", that sentence should say which part is analogous.

## A positioning gain

Proposition 10 (Cobb–Douglas, variable proportions) **reverses** Proposition 6:
under fixed proportions with concave `β` an MPS widens the span, but *"the
reoptimizing assignment reverses these results."* So the direction of the
inequality effect in this literature is not robust to whether the allocation
re-optimises.

Our analogue of "fixed assignment" is that `Δ` does not move under a wealth
spread — and for us that is not an assumption but a **proved** proposition
(anonymity, `PROOFS.tex` `prop:anon`). Worth stating in the positioning: the
literature's own experience is that this sign flips when the allocation
re-optimises, and P9 is safe because the absence of feedback was proved, not
assumed.

## Not attempted

- Prop 6's proof beyond Lemma 1; Lemma 1's own claim that `G MUT F ⟹ F⁻¹ MUT
  G⁻¹`; Theorem 1's Blackwell argument; Props 7, 8, 9, 11 and the CES results.
- Step 2 of the Prop 5 proof — the passage from "`Γ ≥ 0` at critical points"
  to "`Γ ≥ 0` everywhere". Real analysis; out of reach of both tools.
- Anything leaning on the above is **[A]-grade** despite the [F, draft] tag.

## Outstanding

The draft is not the published article. Proposition numbers **must** be
re-checked against *JPE* 112(6) before any of CL-A…CL-I is cited by number.
The survey already lists this; this pass does not discharge it.

---

## Lean pass, 2026-10-06

### Source

The Drive file `job_assignment.pdf` (id `1ZER3CMY4nv4fFSEc7ux_hEQk-1uumFlC`)
is the paper. Its title page reads "Distribution of Ability and Earnings in a
Hierarchical Job Assignment Model", by Robert M. Costrell (Chief Economist,
Commonwealth of Massachusetts, on leave from the University of Massachusetts
at Amherst) and Glenn C. Loury (Boston University), dated December 11, 2003,
37 pages. The file is the draft that pass 1 read and not the published *JPE*
112(6) article. A Drive search for "Costrell" and for the title phrase found
no other copy, so every number below is a draft page or a draft proposition
number, and the published-version check stays open. The whole text layer, 949
extracted lines, was read for this pass.

The PDF text layer garbles symbols. In the quotes below `µ̂` renders the
layer's `ˆµ`, `F⁻¹` renders `F L1`, `(·)` renders `(О)`, and `∫_a^b` renders
the layer's `Z ba`. The layer scrambles word order in the Appendix proof of
Proposition 5 (p.33) and in the proof of Proposition 10 (p.35), so from those
two passages only fragments that read in order are quoted.

### What the file formalises, and what enters as hypothesis

`CostrellLoury.lean` is core Lean 4 with no Mathlib, in
`namespace CostrellLoury`, and compiles with `lean CostrellLoury.lean`. The
paper's integrals become finite sums over ranks and the paper's reals become
`Int`. A rank is a natural number, an ability or a weight is an integer, and
the job share `θ` is carried as the integer pair `(th, n)` with `θ = th/n`, so
the two-job wages of eqs (1) and (2) appear multiplied by `n`. `sumTo f n` is
`Σ_{i<n} f i`. `Gam d n i` sums the quantile difference `d = G⁻¹ − F⁻¹` over
ranks `i` to `n−1`, the discrete form of the paper's
`Γ(ρ) = ∫_ρ^1 [G⁻¹ − F⁻¹]`.

Two analytic facts enter as named hypotheses rather than as proofs.

1. `RiskierQ F G n` says `Γ(0) = 0` and `Γ ≥ 0` at every rank. `RiskierQ` is
   the conclusion of steps 1 and 2 of the Appendix proof of Proposition 5, and
   it is the step "G MUT F implies F⁻¹ MUT G⁻¹" inside the proof of Lemma 1.
   The passage from the paper's CDF definition of "riskier" to `RiskierQ` is
   not formalised. For a single-crossing spread the file proves `RiskierQ`
   (`single_crossing_riskier`).
2. The Cobb–Douglas closed forms of the Appendix (pp.34–35) enter
   `prop10_output_decides` as monotonicity hypotheses. Output is `B·r(M)` with
   `r` strictly increasing, `w(1)` is `B·u(M)` with `u` decreasing, and a
   fixed-ability wage gap is `B·s(M)·(φ(µ) − φ(µ'))` with `s` decreasing and
   `φ` increasing. `M` stands for `∫µ^{1/η}` and `B > 0` for the constant
   through which `β` enters. The four monotonicities are facts about the real
   powers `M^η`, `M^{η−1}` and `µ^{1/η}`, and the file does not prove them.

Everything else is proved in the file, including the two-job wage algebra,
the summation by parts behind Proposition 5, and the double summation by parts
behind Lemma 1.

### Theorem table

| Theorem | Claim | Locator | Verbatim quote |
|---|---|---|---|
| `branches_agree_at_margin` | CL-A | §2, eqs (1)–(2), p.7 | "W(µ) = β0µ + (1 − θ)(β1 − β0)µ̂, µ ≤ µ̂ (1)" and "W(µ) = β1µ − θ(β1 − β0)µ̂, µ ≥ µ̂ (2)" |
| `wage_eq_lo`, `wage_eq_hi` | CL-A (the case split of the schedule) | eqs (1)–(2), p.7 | as in the row above |
| `wage_rises_below_margin` | CL-A | §2, p.9 | "any shift in the ability distribution on fixed support [0,1] which raises µ̂ to, say, µ̂', will raise the earnings of low-ability workers" |
| `wage_falls_above_margin` | CL-A | §2, p.9 | "and reduce the earnings of high-ability workers, and conversely for any shift which reduces µ̂" |
| `span_closed_form` | CL-A | eqs (1)–(2), p.7, read with p.9 | "the role of the ability distribution in the wage function is entirely captured by a single sufficient statistic, µ̂ = F⁻¹(θ)" |
| `span_falls_iff_margin_rises` | CL-A, CL-B | §2, p.9 | "raises µ̂ and narrows the wage distribution" |
| `tail_rule_high_theta` | CL-B, CL-J | §2, p.9 | "If proper job assignment is most critical in the top jobs (i.e. if most jobs are not skill-sensitive — θ is high), then the worker on the margin is in the right tail, so a more unequal ability distribution raises µ̂ and narrows the wage distribution." |
| `tail_rule_low_theta` | CL-B, CL-J | §2, p.9 | "Conversely, if assignment matters most toward to the bottom of the spectrum (θ is low) then a more unequal distribution reduces µ̂ and widens the wage distribution." |
| `control_tail_rule_silent_between` | CL-B (control, finding F2) | §2, p.9 | "This improves ability in the right tail and reduces ability in the left tail (with possible multiple crossings in between)." |
| `pivot_spread_at_fixed_rank` | CL-C, CL-J | §2, p.9 | "the effect of a more unequal ability distribution depends on whether the worker on the margin between jobs, at quantile θ, lies in the upper or lower tail of the ability distribution" |
| `control_margin_moves` | CL-J (control) | §2, p.6 | "The key feature of the model is that jobs must be filled in fixed proportions to one another, θ and (1 − θ)." and "Thus, θ,β0 and β1 completely characterize the technology." |
| `sumTo_zero`, `sumTo_succ`, `sumTo_congr`, `sumTo_sub`, `sum_nonneg`, `sum_nonpos`, `tail_sum_nonneg` | CL-F (finite sums standing in for the integrals) | eq (4), p.10 | "Total output in the hierarchy, Q, is: Q(F) = ∫_0^1 q(p)dp" |
| `output_diff` | CL-F | eq (4), p.10; Appendix, p.33 | "Q(G) − Q(F) = ∫_0^1 {∫_p^1 [G⁻¹(x) − F⁻¹(x)]dx}dβ(p)" |
| `abel` | CL-F | Appendix, proof of Prop 5, p.33 | "Integrate by parts, and recall that β(·) is non-decreasing" |
| `gam_top_zero` | CL-F | Appendix, p.33 | "Since Γ(0) = Γ(1) = 0, it must be that Γ(ρ) ≥ 0,∀ρ ∈ [0,1]." |
| `prop5_integral_form` | CL-F | Appendix, p.33 | "Q(G) − Q(F) = ∫_0^1 {∫_p^1 [G⁻¹(x) − F⁻¹(x)]dx}dβ(p) = ∫_0^1 Γ(p)dβ(p) ≥ 0." |
| `prop5_monotone_weight` | CL-F, CL-M | Prop 5, p.19; proof, p.33 | "Suppose the ability distribution G(·) is “riskier” than F(·), but the two distributions have the same mean. Then Q(G) ≥ Q(F)." |
| `single_crossing_riskier` | CL-F | §4.3, p.19 | "We can see this intuitively in the simple case of a single crossing of CDFs, where ability rises in the right tail of distribution and drops in the left tail." |
| `prop5_single_crossing` | CL-F, CL-M | §4.3, p.19 | "This result holds more generally, for a mean-preserving spread with possibly multiple crossings" |
| `control_prop5_decreasing_weight` | CL-M (control) | Appendix, p.33 | "recall that β(·) is non-decreasing" |
| `control_prop5_contraction` | CL-F (control) | Appendix, p.33 | "it must be that Γ(ρ) ≥ 0,∀ρ ∈ [0,1]" |
| `sumTo_telescope`, `sumTo_shift` | CL-E (the fixed endpoints in the double summation) | proof of Lemma 1, p.20 | "F⁻¹(0) = G⁻¹(0) = 0,F⁻¹(1) = G⁻¹(1) = 1" |
| `lemma1_identity` | CL-E | proof of Lemma 1, p.20 | "So the inequality of the lemma is the principle characterization result for a second-order stochastic dominance (the expectation of a convex function is no less under a riskier distribution)." |
| `lemma1_concave`, `lemma1_convex` | CL-E | Lemma 1, eq (17), p.20 | "G MUT F and ψ(·) convex [concave] imply ∫_0^1 ψ(y)dG⁻¹(y) ≤ [≥] ∫_0^1 ψ(y)dF⁻¹(y) (17)" |
| `prop6_span_widens_concave` | CL-D | Prop 6(i), p.20; eq (15), p.19 | "(i) if β is concave, then wG(0) ≤ wF(0) and ΔwG ≥ ΔwF" with "ΔwF(p) ≡ wF(1) − wF(0) = ∫_0^1 β(y)dF⁻¹(y) (15)" |
| `prop6_span_narrows_convex` | CL-D | Prop 6(ii), p.20 | "(ii) if β is convex, then wG(1) ≤ wF(1) and ΔwG ≤ ΔwF." |
| `control_curvature_decides_span` | CL-D, CL-M (control) | discussion of Prop 6, p.21 | "If output per unit of ability (β(·)) is convex (concave) function of a worker’s position in the hierarchy, then a greater dispersion of ability (on fixed support) lowers wages for the most (least) skilled workers, and compresses (widens) the wage range." |
| `prop10_output_decides` | CL-H | Prop 10, p.27; proof, p.35 | "any change in the distribution of (expected) ability on [0,1] that raises (reduces) output: (i) raises (reduces) w(0); (ii) reduces (raises) w(1); and (iii) narrows (widens) the wage span between any two workers of given abilities, W(µ) − W(µ')" and, p.35, "Proposition 10(i) follows immediately from (ii) above, since w(0) = (1−η)Q." |
| `reversal_concave` | CL-K | §5.4, p.28 | "Specifically, under concave b(·), with fixed assignment, a mean-preserving spread of abilities reduces w(0) and widens w(1) − w(0), so the reoptimizing assignment reverses these results." |
| `no_reversal_convex` | CL-K (finding F1) | §5.5, p.28 | "We have found opposite effects on the wage span from a mean-preserving spread in abilities under Cobb-Douglas and Leontief crowding technologies (h=ϕ^η and h → min[ϕ,1]) for concave β(·)." |

The 39 theorems are all accounted for above, seven helpers in one row and the
pairs in shared rows.

### How the statements sit against the source

- **Two-job schedule.** `wLo` and `wHi` are eqs (1) and (2) multiplied by
  `n`, and `wage` uses (1) at or below the margin and (2) above the margin.
  `span_closed_form` gives `W(top) − W(0) = n·(β1·top − (β1 − β0)·µ̂)`, which
  contains `µ̂` and no other feature of the distribution.
  `span_falls_iff_margin_rises` shows that this span falls exactly when `µ̂`
  rises, for `β1 > β0`. The file reads the source's "narrows the wage
  distribution" as this span together with the wage movements on each side
  of the margin.
- **Tail rule at a fixed margin.** `TailSpread F G L U n` says `G⁻¹ ≤ F⁻¹` at
  ranks up to `L` and `G⁻¹ ≥ F⁻¹` at ranks from `U` to `n−1`, with any sign
  pattern between `L` and `U`, which is the source's description of a
  mean-preserving spread on p.9. `tail_rule_high_theta` takes one rank `th`
  and uses that same `th` in `F⁻¹(th)`, in `G⁻¹(th)` and in the wage
  coefficient `th/n`. Holding one `th` across both distributions is how the
  file states CL-J. Mean preservation is not a hypothesis of the tail rule,
  because the source's argument on p.9 uses only the signs in the tails.
- **Bridge to P9-gen (CL-C).** `PivotSpread` is restated from
  `lean/EntryContest.lean` in the shape `lit/hopkins_kornienko/Dispersive.lean`
  uses, at `Int` with `≤`. `pivot_spread_at_fixed_rank` takes
  `G⁻¹ = T ∘ F⁻¹` with `T` a pivot-spread about `x0`, and shows that the
  two-job span narrows when `µ̂ = F⁻¹(θ) ≥ x0` and widens when `µ̂ ≤ x0`. The
  two-job rule is therefore the pivot-spread sign logic applied to the single
  agent at the fixed rank `θ`. The theorem supports the analogy in the sign
  logic and says nothing about an endogenous margin, which `PROOFS.tex`
  treats in `prop:endogenous` and `RECONSTRUCTION.md` §7 records as the
  disanalogy.
- **Proposition 5.** `output β µ n = Σ_{i<n} β_i µ_i` is eq (4). `abel` is
  summation by parts, and `prop5_integral_form` is the discrete
  `Q(G) − Q(F) = ∫Γ dβ`, namely `Σ_i (β_{i+1} − β_i)·Γ_{i+1}`.
  `prop5_monotone_weight` then needs `β_i ≤ β_{i+1}` and no condition on the
  curvature of `β`, which is CL-M. Monotonicity is required only between ranks
  inside the population (`i + 1 < n`), because the top-rank term carries
  `Γ(1) = 0` (`gam_top_zero`).
- **Lemma 1 and the span part of Proposition 6.**
  `expect ψ H n = Σ_{i<n} ψ_i (H_{i+1} − H_i)` is `∫ψ dH` with the quantile
  function `H` used as a CDF, as the source does on p.20. `lemma1_identity`
  applies summation by parts twice and gives
  `∫ψ dG⁻¹ − ∫ψ dF⁻¹ = Σ_i Δ²ψ_i · S_{i+2}`, where `Δ²ψ` is the second
  difference of `ψ` and `S` the lower partial sums of `G⁻¹ − F⁻¹`, which
  `RiskierQ` makes non-positive. The span change in Proposition 6 is signed by
  the second difference of `β`, whereas the output change in Proposition 5 is
  signed by the first difference of `β`, under the same `RiskierQ` hypothesis
  on the spread.
- **Proposition 10 against Proposition 6.** `prop10_output_decides` derives
  from "output rises" that `w(0)` rises, `w(1)` falls and every fixed-ability
  gap narrows, for every `B > 0`. In the source's closed form `B` is the only
  route by which `β` reaches these formulas, so the conclusion holds for every
  shape of `β`. `reversal_concave` and `no_reversal_convex` place the
  fixed-proportions span result beside the Proposition 10 result under one
  hypothesis set, and their content beyond their two parts is that
  juxtaposition alone. Under concave `β` the fixed-proportions span widens
  while the Cobb–Douglas gap narrows. Under convex `β` the fixed-proportions
  span and the Cobb–Douglas gap both narrow.

### Non-vacuity controls (README rule 3)

1. `control_tail_rule_silent_between`. Two seven-rank spreads `gUp` and
   `gDown` of `F⁻¹ = (0,10,20,30,40,50,60)` share the same tails (left tail
   through rank 1, right tail from rank 5), are non-decreasing, and satisfy
   `RiskierQ`, so both are discrete mean-preserving spreads, each with three
   crossings. At margin rank 3, between the tails, `µ̂` rises from 30 to 31
   under `gUp` and falls from 30 to 29 under `gDown`, and with `β0 = 1`,
   `β1 = 2` the span moves from 630 to 623 under `gUp` and to 637 under
   `gDown`. A margin between the tails gets no sign from the rule, so the
   hypothesis that the margin lies in a tail is load-bearing.
2. `control_margin_moves`. The rank-wise improvement `G⁻¹(p) = F⁻¹(p) + 1`
   raises `µ̂` and narrows the span (350 to 340) when the margin rank stays
   at 5. When the margin rank moves from 5 to 3 the same improvement lowers
   the marginal ability from 5 to 4 and widens the span (350 to 360). Fixing
   `θ` is load-bearing for the sufficient-statistic logic.
3. `control_prop5_decreasing_weight`. `F⁻¹ = (1,1)` and `G⁻¹ = (0,2)` satisfy
   `RiskierQ`, and with the decreasing weight `β = (1,0)` output falls from 1
   to 0. Dropping "β non-decreasing" breaks Proposition 5.
4. `control_prop5_contraction`. The reverse pair `F⁻¹ = (0,2)`,
   `G⁻¹ = (1,1)` preserves the mean and fails `RiskierQ`, and with the
   non-decreasing weight `β = (0,1)` output falls from 2 to 1. Mean
   preservation with a monotone weight does not sign output, and the
   `Γ ≥ 0` hypothesis does the work.
5. `control_curvature_decides_span`. One spread, `F⁻¹ = (0,1,2,3)` to
   `G⁻¹ = (0,0,3,3)` with fixed endpoints, and two non-decreasing weights. The
   concave weight `(0,2,3,3)` widens the span from 5 to 6 and the convex
   weight `(0,0,1,3)` narrows the span from 1 to 0, while output rises under
   both weights (8 to 9, and 2 to 3). A monotone `β` signs output and does
   not sign the span.
6. Check CL-L4 in `verify_cl.py` appends the negation of control 3 to the
   source and requires Lean to reject the result, so the compile check can
   fail. The audit parser of CL-L3 was exercised once by hand on a throwaway
   file whose theorem uses `Classical.em`, and the parser reported
   `Classical.choice` as disallowed, so CL-L3 can fail as well.

### Findings against our documents

- **F1 (CL-K), `PROOFS.tex` 1250–1252 and 1678–1680.** `PROOFS.tex` says
  that under Cobb–Douglas crowding "the direction of the inequality effect
  *reverses* relative to their fixed-proportions Proposition~6". The source
  states the reversal for concave `β` only, twice on p.28, in "under concave
  b(·), with fixed assignment, a mean-preserving spread of abilities reduces
  w(0) and widens w(1) − w(0), so the reoptimizing assignment reverses these
  results" and in "opposite effects on the wage span ... for concave β(·)".
  Under convex `β`, Proposition 6(ii) already has `w(1)` falling and the span
  narrowing, which are Proposition 10's directions, so for convex `β` nothing
  reverses (`no_reversal_convex`, and control 5 for a strict instance). A
  faithful wording is "reverses relative to their fixed-proportions
  Proposition 6 when β is concave". The conclusion `PROOFS.tex` draws, that
  the direction of the inequality effect is not robust to re-optimisation,
  survives, because one curvature class is enough to show non-robustness.
- **F2 (CL-B), `LITERATURE.tex` 742–744 and 1074–1075.** `LITERATURE.tex`
  calls the two-job rule "a single-pivot sign rule located at the marginal
  agent's quantile relative to the spread's crossing point" and says the sign
  "flips according to whether the marginal worker's quantile lies above or
  below the crossing". The source locates the rule in the tails and allows
  "possible multiple crossings in between" (p.9). With several crossings there
  is no single crossing for the margin to sit above or below, and a margin
  between the tails gets no sign (control 1). The crossing wording is right
  for a single-crossing spread and too strong for a multiple-crossing spread.
  `PROOFS.tex` 1239–1242 quotes the tail wording and is correct. The source's
  rule has the same silent middle region as the band of `PROOFS.tex`
  §prop:band; that parallel is recorded here and is not claimed in any
  manuscript text.
- **F3 (CL-E), `LITERATURE.tex` 754–756.** `LITERATURE.tex` says "Their
  Lemma~1 (in the proof of Proposition~5) shows that $G$ riskier than $F$ on
  fixed support implies the quantile functions reverse the order". In the
  source Lemma 1 sits in the proof of Proposition 6 ("Proof of Proposition 6:
  The results follow immediately from the following lemma.", p.20). The
  statement of Lemma 1 is the convex and concave integral inequality (17), and
  the order reversal is asserted inside the proof of Lemma 1 with the pointer
  "(see the proof of Proposition 5)". A faithful wording is "in the proof of
  their Lemma 1, which serves Proposition 6, they assert that ... and refer to
  the proof of Proposition 5".
- **F4 (CL-M), `PROOFS.tex` 1316–1318 and 1433–1439.** `PROOFS.tex` says
  Suen and Costrell–Loury "sign smooth aggregates and must therefore control
  the quantile difference at every rank, which is what forces integration by
  parts and a log-concavity or monotone-weight condition", and calls the
  Costrell–Loury object "a smooth aggregate --- a wage schedule". For
  Costrell–Loury output (Proposition 5) the monotone-weight description is
  exact. For the Costrell–Loury wage schedule (Proposition 6) the condition is
  the curvature of `β`, and a monotone weight alone does not sign the span
  (control 5). The two sentences are accurate when "monotone-weight" is read
  as describing output, and too strong when read as covering the wage
  schedule.
- **F5, internal to this directory.** CLAIMS.md gave p.9 for eqs (1)–(2),
  which are on p.7 (corrected in CLAIMS.md). `RECONSTRUCTION.md` §2 states
  `β₁ > β₀ > 0`, whereas the source admits `β₀ = 0` ("In the simplest case,
  output depends only on the ability of those in production jobs; support
  jobs generate no output", p.6, and "(β0 = 0)", p.8). `RECONSTRUCTION.md` §2
  quotes "most toward the bottom" where the source reads "most toward to the
  bottom" (p.9). None of the three touches a manuscript claim. The Lean file
  assumes only `β₀ < β₁`, or `β₀ ≤ β₁` in the wage-movement theorems.
- **F6 (CL-I).** The CL-I text quoted in CLAIMS.md is no longer in
  `LITERATURE.tex`, where lines 782–806 now carry the corrected comparison.
  The corrected text agrees with the source. The full text layer has zero
  occurrences of "hazard" and of "log-concav", re-checked on 2026-10-06
  including hyphenated and spaced variants.
- **Open, unchanged from pass 1.** The draft is the only copy on Drive, so
  the published proposition numbers could not be checked.

### Not formalised, and why

- The `w(0)` and `w(1)` parts of Proposition 6. The source obtains them from
  (16) through `ψ0` and `ψ1`, whose curvature follows from the curvature of
  `β` by differentiation (pp.20–21). Only the span part, where `ψ2 = β`, is
  formalised.
- The CDF-to-quantile step, meaning steps 1 and 2 of the Proposition 5 proof
  and "G MUT F implies F⁻¹ MUT G⁻¹" in Lemma 1. The step enters as
  `RiskierQ`. Step 2 is the extreme-value argument pass 1 already flagged.
- The analytic content of Proposition 10, namely the closed forms (i) and
  (ii) on pp.34–35 and the claim that a mean-preserving spread raises
  `∫µ^{1/η}` because `µ^{1/η}` is convex. Both enter as the hypotheses on
  `r`, `s`, `u` and `φ`.
- Theorem 1 (CL-G), a Blackwell garbling argument about probability
  measures, which lies outside the order-theoretic scope of the file.
- CL-C beyond the bridge theorem. "Entry-count analogue" is a positioning
  sentence. The file shows where the sign logic coincides and says nothing
  about an endogenous count.

### Run

`python3 lit/costrell_loury_2004/verify_cl.py`, run from the paper root,
reports `COSTRELL-LOURY SUMMARY: 10 checks, 0 failures`. The Lean audit line
reads `theorems: 39; audited: 39; axiom-free: 2; propext/Quot.sound only: 37;
other axioms: 0; sorry: 0`. `lean CostrellLoury.lean` alone exits 0 with no
output.

Our reading is arithmetically consistent with the source except where F1 to
F4 record a discrepancy. Nothing here reproves a result of the paper.
