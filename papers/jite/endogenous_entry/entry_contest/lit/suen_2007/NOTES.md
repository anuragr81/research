# Notes — Suen (2007), passes 1 and 2

Run: `python3 verify_suen.py` — **11 checks, 0 failures** (pass 2,
2026-10-06; pass 1 ran 6 checks). Pass 2 adds SU-7 and the Lean block SU-L.

**Scope.** Our reading is arithmetically consistent, and the survey's
description of Proposition 2 is **exact**. Two corrections follow, neither
about Prop 2 itself. Nothing here reproves any result of the paper. Pass 2
corrects one inference of pass 1 (the SU-6 section below) and records a sign
misprint in Suen's eq. (6); see "Lean formalisation" at the end.

---

## The survey's description of Proposition 2 is precise

Rare enough to say plainly. `LITERATURE.tex` §sec:assignment describes the
proof as handling *"a general Rothschild–Stiglitz spread with an arbitrary
finite number of crossings `k₀ < k₁ < ⋯ < k_{2n}` of the quantile functions:
the quantile difference alternates in sign, each partial integral is signed by
second-order dominance, concavity controls the recombination, and the result is
integrated by parts against `ρ = (1−F)/f`."*

Every clause checks out against pp.154–155, including the crossing notation,
the alternation, and `ρ`'s identity as the inverse hazard function. The
hypotheses (concavity of `θ, φ`; log-concavity of `1−F`) are quoted correctly.

Pass 2 adds two qualifications, neither of which changes the verdict. The
label "Rothschild–Stiglitz" is the survey's and does not occur in the paper,
which defines the spread at p.153 as "the distribution G₁ is said to be more
dispersed than G₀ if G₀ second-order stochastically dominates G₁ and the two
distributions have the same mean (see, for example, [8])", with [8] the
Mas-Colell, Whinston and Green textbook. The printed second line of eq. (6)
carries a sign misprint, recorded as finding P2-1 below.

**SU-3 sharpens one point in the survey's favour.** Log-concavity of `1−F` is
not merely *sufficient* for `ρ' ≤ 0` — it is exactly equivalent, since
`d²/dx² log(1−F) = ρ'/ρ²` identically. So the hypothesis is doing precise work
at the final step, and cannot be weakened there without changing the argument.

---

## ERROR: the two sources are not independent

**Status 2026-10-06.** `LITERATURE.tex` has been corrected. Its paragraph
"Consequence for open item 2" now reads "they are not independent" (claim
SU-G). The superseded wording survives only as the quotation in claim SU-F of
`CLAIMS.md` and in row 5 of `lit/TRACEABILITY.md`.

The synthesis paragraph calls them *"Two **independent** published sources."*
They are not. Suen's proof imports its key step from Costrell–Loury by name
(p.154):

> "if `G₀` second-order stochastically dominate [sic] `G₁` and the two
> distributions have the same mean, then `G₁⁻¹` second-order stochastically
> dominates `G₀⁻¹` (see, for example, **[2]**)"

where **[2] is Costrell and Loury**. The printed verb is "dominate"; pass 1
quoted it as "dominates". Suen's footnote 1 also engages directly
with Costrell–Loury's rank-rescaling device. So this is **one lineage, not two
independent confirmations** — Suen builds on Costrell–Loury and says so.

This matters because "two independent sources" is doing rhetorical work in the
survey: it reads as convergent validation from separate directions, which would
raise confidence that the technique is robust. It is not that. Combined with
the Costrell–Loury finding (their Prop 5 needs only monotonicity of `β`, and
contains no log-concavity or hazard-rate condition at all — see
`../costrell_loury_2004/NOTES.md`), the corrected picture is:

- **One shared first move**, the quantile-difference decomposition, which
  originates with Costrell–Loury and which Suen cites.
- **Two different second moves.** Costrell–Loury integrate against a
  non-decreasing weight `dβ` and need nothing more. Suen integrates against
  `ρ' ≤ 0` and needs concavity of `θ, φ` plus log-concavity of `1−F`.

The weaker set of hypotheses is Costrell–Loury's, and it is the one to try
first against P9-gen.

---

## The finding that most affects the proposed route: footnote 1

The survey's plan for open item 2 is to *"replace the pivot hypothesis with
log-concavity of `1 − Λ` and apply the Suen integration to `k*` written as a
functional of `κ ∘ Λ⁻¹`."*

Note the composition `κ ∘ Λ⁻¹` — that is precisely a **rank-space rescaling**,
and it is exactly what Suen's footnote 1 declines to do:

> "In Costrell and Loury [2], the characteristic `y` is redefined by letting
> `ŷ = G(y)`. [...] the production function would then have to be modified to
> `Q = θ(x)φ̂(ŷ)`, where `φ̂(·) = φ(G⁻¹(·))`. **Unless the density function `g`
> is increasing, concavity of `φ` does not imply concavity of `φ̂`.** Since the
> subsequent analysis relies on the curvature properties of the production
> function and it is difficult to interpret these curvature properties in terms
> of the re-scaled units, such re-scaling is not adopted in this paper."

**SU-5 makes the failure concrete.** Take `φ = √y`, concave everywhere
(`φ'' = −1/(4y^{3/2}) < 0`). Take `G(y) = 1−(1−y)²`, so `g = 2(1−y)` is
strictly decreasing. Then `φ̂ = φ∘G⁻¹` has

| `u` | `φ̂''(u)` |
|---|---|
| 1/5 | −1.7399 |
| 1/2 | −0.1353 |
| 4/5 | **+1.1193** |

`φ̂` is **convex** on the upper range. Concavity did not survive the change of
variable.

**Consequence.** If our analogue of `φ` is `κ`, then writing `k*` as a
functional of `κ ∘ Λ⁻¹` does not let us carry `κ`'s curvature across. Suen's
step (c) needs concavity *in the rescaled variable*, and that would have to be
established separately — or the wealth density `Λ'` would have to be
increasing, which is a substantive restriction on the wealth distribution and
one the model currently does not make. This is a live obstacle in the proposed
route that the survey does not record, raised by the source itself.

It also interacts with the Hopkins–Kornienko finding: the dispersive-order
machinery that motivates `T = Λ_β⁻¹∘Λ_α` **is** rank-space machinery, so this
trap sits on the main road, not a side path.

Pass 2 proves both directions of footnote 1 in discrete form (SU-L11). An
increasing density is exactly the condition under which concavity survives for
every concave non-decreasing `φ` (`fn1_iff`), and a decreasing density
destroys concavity of a strictly concave `φ` (`fn1_counterexample`).

---

## A second remark the survey omits: concavity is not necessary

p.155: *"the concavity assumption is sufficient but not necessary for the
conclusion of Proposition 2. If `θ` and `φ` are both linear, for example, the
proof of Proposition 2 indicates that `W₁` is still strictly lower than `W₀`."*

SU-6 confirms the mechanism: with `θ'' = 0`, `(5)` collapses to `H = θ'Ĥ`, and
`Ĥ < 0`, `θ' > 0`, `ρ' ≤ 0` still sign `(4)`.

**Correction, pass 2.** Pass 1 concluded here that concavity "is not
load-bearing for the direction of Prop 2". That inference is wrong. Linear
`θ` and `φ` are weakly concave, and p.151 states the assumption in exactly
that form ("This means that θ and φ are both weakly concave."), so Suen's
linear example stays inside the hypothesis rather than outside it. Concavity
of `φ` cannot be dropped. SU-7 takes `F` uniform, `θ(x) = x`, the SU-4 pair
`G₀⁻¹(t) = t`, `G₁⁻¹(t) = 3t² − 2t³`, and the convex increasing `φ = y²`.
Every hypothesis of Prop 2 other than concavity of `φ` holds, and
`W₁ − W₀ = 1/420 > 0`, whereas the concave `φ = 2y − y²` gives `−1/28` and
the linear `φ = y` gives `−1/60`. The Lean control `control_convex_phi` is
the discrete twin. Suen's sentence stays true as written, because "not
necessary" asserts only that some non-concave `φ` would also work, while the
text exhibits no such `φ`. The falsifier of the correction is a convex `φ`
with `W₁ ≤ W₀` on every same-mean spread, and SU-7 rules it out.

---

## Results

| ID | Result |
|---|---|
| SU-1 | CONSISTENT. `μ = G⁻¹∘F` is increasing with `μ(0)=0`, `μ(1)=1` on all four `F,G` pairs, so it qualifies as a CDF, as the paper notes. |
| SU-2 | CONSISTENT. FOC `(2)` and the SOC gap `θ'·(d/dx)φ(μ)` are as stated. |
| SU-3 | CONSISTENT, and stronger than stated: `d²/dx² log(1−F) − ρ'/ρ² = 0` identically, so log-concavity of `1−F` **is** `ρ' ≤ 0`. |
| SU-4 | CONSISTENT. On an explicit mean-preserving pair, `∫₀^k[G₁⁻¹−G₀⁻¹]dt = −k²(k−1)²/2 ≤ 0` with equality exactly at `k = 0, 1`. |
| SU-5 | CONFIRMS FOOTNOTE 1, and the result matters. See above. |
| SU-6 | CONSISTENT. The linear case still signs the conclusion; the linear case is weakly concave (pass 2). |
| SU-7 | CORRECTS PASS 1. A convex `φ` reverses Prop 2 with every other hypothesis holding, `W₁ − W₀ = 1/420`. |
| SU-L | Lean file compiles; 51 theorems, all audited, 6 axiom-free and 45 using only `propext` and `Quot.sound`; no `sorry`. |

## Not attempted

Pass 1 left inequality `(6)`, the derivation of `(4)`, and the step from the
local maxima to `Ĥ ≤ 0` unchecked. Pass 2 covers all three at the level of
the discrete skeleton (SU-L1 to SU-L8), with the weak inequality in place of
the strict one. What remains outside both passes:

- The strict inequalities `Ĥ(x) < 0` for `x ≠ kᵢ`, `H(x) < 0` for almost all
  `x`, and `W₁ < W₀`. The Lean proves `≤` throughout.
- The passage from the continuum to the grid. The tangent-line bound, the
  monotone slope, and the change of variable from `x` to rank in `Ĥ` enter
  the Lean as named hypotheses, not as theorems.
- Propositions 1 and 3, Corollaries 1–2.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## Net effect on open item 2

**Status 2026-10-06.** `LITERATURE.tex` now records the three obstacles below
(paragraph "Consequence for open item 2", the enumerated list), and
`PROOFS.tex` ("What genuinely remains", item 2) closes open item 2 through the
tail condition rather than through the Suen machinery. The text below is the
pass 1 record.

Three things now stand between the survey's plan and a result, none of them
recorded in `LITERATURE.tex`:

1. **The aggregate/count gap.** Both papers sign a smooth integral (`Q`, `W`).
   `k*` is an integer count defined by a threshold crossing. Neither source
   signs a count.
2. **The rescaling trap** (SU-5): curvature does not survive `κ ∘ Λ⁻¹` unless
   the wealth density is increasing.
3. **Cross-side versus same-side** (SU-E): Suen compares two populations across
   a market; we compare one population before and after. The map class is the
   same, the object compared is not.

The lead is still worth pursuing, and Costrell–Loury's weaker hypothesis is the
place to start. But `LITERATURE.tex`'s "concrete, checkable lead" should carry
these three qualifications, and its closing sentence — "It has not been tried
and should not be claimed until it has" — remains exactly right.

---

## Lean formalisation (pass 2, 2026-10-06)

**Source read.** Drive file `suen_contests_compstats.pdf`
(id `1ioT14oJOpjUt5qsTebIavaegCvZBcC0-`), 10 pages, read in full through the
Drive text layer. Page 151 (footnote 1) and page 154 (eqs. (4) to (6)) were
also rendered as images and read from the image. The title page reads "The
comparative statics of differential rents in two-sided matching markets",
Wing Suen, J Econ Inequal (2007) 5:149–158, DOI 10.1007/s10888-006-9034-8,
published version. The file name says "contests", but the paper is a
two-sided matching paper and contains no contest model.

**File.** `Suen2007.lean`, core Lean 4, no Mathlib, `namespace Suen2007`.
It compiles with `lean Suen2007.lean` under v4.33.1 and v4.34.1 (the elan
default `stable` resolved to v4.34.1 on 2026-10-06). The suite compiles it,
rejects `sorry`, `admit`, `native_decide` and user `axiom` declarations, checks
that the 25 claim-mapped theorems are present, and prints `#print axioms` for
every declared theorem. The audit allows no axioms or only `propext` and
`Quot.sound`.

**Index map.** The Lean works on a rank grid, so Suen's rank variable `t` and
the grid index coincide and the `x`-versus-rank distinction in `Ĥ` disappears.

| Suen | Lean |
|---|---|
| `k_{2j}`, `k_{2j+1}` | `e j`, `o j` |
| Suen's block `j` (1-based), `[k_{2j−2}, k_{2j}]` | Lean block `j−1`, `[e (j−1), e j)` |
| `G₁⁻¹(t) − G₀⁻¹(t)` | `D t`, or `x1 t − x0 t` in `prop2_from_sosd` |
| `φ(G₁⁻¹(t)) − φ(G₀⁻¹(t))` | `dphi t` |
| `φ'(G₀⁻¹(t))`; block weight `φ'(G₀⁻¹(k_{2j−1}))` | `s t`; `s (o (j−1))` |
| `∫₀^k (G₁⁻¹ − G₀⁻¹)`; `Ĥ` at rank `k` | `psum D k`; `psum dphi k` |
| `θ'(x)`; `H(x)` | `tp x`; `wsum tp dphi x` |
| `ρ(x)`, `ρ(1) = 0` | `ρ x`, `ρ (e n) = 0` |
| `W₁ − W₀` | `wsum ρ (fun t => tp t * dphi t) (e n)` |
| `N·∫_{−∞}^y G` for an `N`-atom distribution | `shortfall x N y = Σᵢ max(y − xᵢ, 0)` |
| `φ̂ = φ∘G⁻¹`; `g` increasing | `fun u => φ (q u)`; `ConcaveN q` (the increments of `q` are the reciprocal density) |

**Named hypotheses and their analytic source.** `htan` is the tangent-line
bound `φ(b) − φ(a) ≤ φ'(a)(b − a)`, which is concavity of `φ`. `hs` says the
slope `s` is non-increasing, which is concavity of `φ` along the increasing
`G₀⁻¹`. `hs0`, `hsgn_neg` and `hsgn_pos` come from `φ` increasing. `hneg` and
`hpos` are the alternation stated on p.154. `hCL` is the Costrell–Loury step.
`htp0` and `htp` are `θ` increasing and concave. `hρ` is `ρ` non-increasing,
which SU-3 shows is log-concavity of `1−F`, and `hρN` is `1 − F(1) = 0`.

### Faithfulness table

| Claim | Theorems | Locator | Verbatim quote from the PDF | Lean statement |
|---|---|---|---|---|
| SU-L1 | `abel_end`, `eq6_identity` | eq. (6), second line, p.154 | (6) second line, first term "φ′(G₀⁻¹(k₂ᵢ₋₁)) ∫₀^{k₂ᵢ} (G₀⁻¹(t) − G₁⁻¹(t)) dt" | The first line of (6) equals `a_i·∫₀^{k_{2i}}(G₁⁻¹ − G₀⁻¹) + Σ_{j<i}(a_j − a_{j+1})∫₀^{k_{2j}}(G₁⁻¹ − G₀⁻¹)`, with `G₁⁻¹ − G₀⁻¹` in the first term |
| SU-L2 | `eq6_printed_not_identity` | eq. (6), p.154 | as for SU-L1 | One block, weight 1, block integral −1. The left side is −1 and the printed right side is +1 |
| SU-L3 | `dsum_nonpos`, `eq6_sign` | p.154, after (6) | "Hence, each of the integrals in Equation (6) is negative, and therefore Ĥ(k₂ᵢ) ≤ 0." | Non-negative last weight, non-increasing weights and non-positive partial sums give a non-positive weighted sum |
| SU-L4 | `recombination`, `range_bound`, `eq6_inequality`, `telescope`, `e_mono`, `eq6_hhat_nonpos` | eq. (6), first line, p.154 | "where the inequality stems from the concavity of φ." and "By second-order stochastic dominance, G₁⁻¹(t) − G₀⁻¹(t) is negative for t ∈ [0,k₁], is positive for t ∈ [k₁,k₂], is negative for t ∈ [k₂,k₃], and so on." | `htan` plus alternation plus non-increasing `s` give `psum dphi (e i) ≤ Σ_{j<i} s(o j)·(psum D (e (j+1)) − psum D (e j))`, and with `hCL` at the even crossings `psum dphi (e i) ≤ 0` |
| SU-L5 | `psum_antitone_on`, `psum_monotone_on`, `block_cover`, `hhat_nonpos_between`, `cl_at_even_crossings` | p.154, last sentence | "Because Ĥ(x) reaches its local maxima at x = k₂ᵢ, this in turn implies that Ĥ(x) < 0 for all x ≠ kᵢ, i = 0,2,...,2n." | `Ĥ ≤ 0` at every `e j` gives `Ĥ ≤ 0` at every rank up to `e n` (weak form). Applied to `D` itself, partial sums at the even crossings bound the partial sums at every rank |
| SU-L6 | `eq5_identity`, `eq5_H_nonpos`, `psum_mul_eq_wsum` | eq. (5), p.154; p.155 | "Now, H(x) can be rewritten as" (5); "By Equation (5), the negativity of Ĥ and the concavity of θ implies that H(x) < 0 for almost all x." | `H = θ'·Ĥ + Σ(θ'_t − θ'_{t+1})·Ĥ_{t+1}` and `H ≤ 0` from `θ' ≥ 0` non-increasing and `Ĥ ≤ 0` |
| SU-L7 | `wsum_const_sub`, `product_rule`, `eq4_first_equality`, `eq4_identity`, `eq4_sign` | eq. (4), p.154; p.155 | "Upon integration by parts, the difference W₁ − W₀ can be expressed as" (4); "Finally, log-concavity of 1 − F implies that ρ′ ≤ 0. Equation (4) then shows that W₁ < W₀." | `Σ_x m_x·(w₁−w₀)(x) = Σ_t (w₁′−w₀′)_t·(mass above t)`; with `ρ (e n) = 0` the sum against `ρ` equals `Σ(ρ_t − ρ_{t+1})H_{t+1}`, which is `≤ 0` when `ρ` is non-increasing and `H ≤ 0` |
| SU-L8 | `prop2_skeleton`, `prop2_from_sosd`, `psum_sub` | Prop 2, p.154 | "Assume that θ and φ are both concave, and that 1 − F is log-concave. Then W₁ < W₀ if G₁ is more dispersed than G₀." | `W₁ − W₀ ≤ 0` from the named hypotheses; `prop2_from_sosd` replaces `hCL` by the shortfall ordering and derives `hCL` through SU-L9 |
| SU-L9 | `psum_le_psum`, `psum_le_of_nonneg`, `shortfall_ge`, `shortfall_below`, `shortfall_above`, `shortfall_at_quantile`, `psum_const_sub`, `cl_quantile_reversal` | p.154, after (6) | "Now, if G₀ second-order stochastically dominate G₁ and the two distributions have the same mean, then G₁⁻¹ second-order stochastically dominates G₀⁻¹ (see, for example, [2])." | `Σᵢ max(y − x0ᵢ, 0) ≤ Σᵢ max(y − x1ᵢ, 0)` for every `y`, with `x1` sorted, gives `psum x1 k ≤ psum x0 k` for every `k ≤ N` |
| SU-L10 | `dsum_nonpos_of_drops`, `wsum_nonpos_of_drops`, `psum_le_wsum`, `hhat_nonpos_direct` | none; our observation | none | The weighted sum is `≤ 0` when the partial sums are `≤ 0` where the weight drops and at the top; `Ĥ ≤ 0` at every rank from `htan` with pointwise slopes and `hCL` at every rank, with no crossing points |
| SU-L11 | `diff_anti`, `incr_shift`, `mono_shift`, `fn1_concave_comp`, `fn1_converse`, `fn1_iff`, `fn1_counterexample` | fn. 1, p.151 | "Unless the density function g is increasing, concavity of φ does not imply concavity of φ̂." | `φ` concave non-decreasing and `q` concave non-decreasing give `φ∘q` concave; for non-decreasing `q`, `φ∘q` concave for every such `φ` holds exactly when `q` is concave; `φ = min(4y, 3y+1, 2y+3)` and `q = (0, 1, 3, 6, ...)` give `φ∘q` with increments 4 then 5 |
| SU-LC1 | `control_weight_increasing` | eq. (6) | as SU-L3 | Weights (1, 2), quantile difference (−1, 1), partial sums (−1, 0), weighted sum +1 |
| SU-LC2 | `control_convex_phi` | Prop 2, p.154 | as SU-L8 | `G₀⁻¹ = (1, 1)`, `G₁⁻¹ = (0, 2)`, `φ = y²`, `θ′ = 1`, `ρ = (1, 1, 0)`. Partial sums (0, −1, 0), `htan` fails at the slope 2, `Ĥ` at the top is 2 and `W₁ − W₀ = 2` |
| SU-LC3 | `control_interior_rank` | eqs. (4), (6) | as SU-L3 | Weights (3, 2, 1, 0), quantile difference (0, 1, −1), partial sums (0, 0, 1, 0), weighted sum +1 |
| SU-LC4 | `control_rho_increasing` | eq. (4), p.155 | as SU-L7 | `ρ = (1, 2, 0)`, increments (−1, 1), `H = (0, −1, 0)`, sum +1 |
| SU-LC5 | `control_cl_unsorted` | p.154 | as SU-L9 | `x0 = (1, 1)`, unsorted `x1 = (2, 0)`. The shortfall ordering holds at every `y`, the totals agree, and the first partial sum is 2 against 1 |

### Non-vacuity (rule 3)

Each named hypothesis of the chain has a control in which that hypothesis
alone fails and the conclusion fails with it. Weight monotonicity, the
discrete form of concavity of `φ` in (6) and of `θ` in (5), has SU-LC1. The
tangent bound has SU-LC2. The Costrell–Loury partial sums have SU-LC3, at an
interior rank. Monotone `ρ`, the form log-concavity of `1−F` takes in (4), has
SU-LC4. The quantile reading in the Costrell–Loury step has SU-LC5. Footnote
1 has `fn1_counterexample`. Each control is proved by `decide` or `omega` on
concrete integers, so a wrong number fails the compile. The suite itself was
mutation-tested on 2026-10-06 with an injected `sorry`, an injected
`Classical.em`, a user `axiom`, a renamed control, a falsified control, and
`lean` removed from `PATH`; each mutation produced at least one `[FAIL]`.

### Findings, pass 2

**P2-1. Sign misprint in eq. (6), p.154.** The first term of the second line
is printed as `φ′(G₀⁻¹(k₂ᵢ₋₁)) ∫₀^{k₂ᵢ} (G₀⁻¹(t) − G₁⁻¹(t)) dt`. Summation by
parts of the first line gives `G₁⁻¹ − G₀⁻¹` in that term (`eq6_identity`), and
the printed form is not an identity (`eq6_printed_not_identity`). Read as
printed, the next sentence "each of the integrals in Equation (6) is
negative" fails for the first term, because the Costrell–Loury step makes
`∫₀^{k₂ᵢ}(G₀⁻¹ − G₁⁻¹)` non-negative. Read with the corrected sign, the
argument goes through (`eq6_sign`). The misprint was confirmed on the
rendered page image, not only on the text layer. Pass 1 stated that "every
clause checks out" while leaving (6) unchecked; `LITERATURE.tex` and
`PROOFS.tex` do not reproduce (6), so neither inherits the misprint.

**P2-2. Concavity of `φ` is load-bearing.** Recorded above in the SU-6
section. Consequence for `LITERATURE.tex` (claim SU-H). The wording "needs
concavity of $\theta,\phi$" contradicts Suen's own words at p.155 ("sufficient
but not necessary"), yet matches the mathematics in the sense that concavity
of `φ` cannot simply be removed (SU-7, SU-LC2). A wording that carries both
sides is "assumes concavity of `θ, φ`, which Suen calls sufficient but not
necessary; a convex `φ` reverses the conclusion".

**P2-3. `PROOFS.tex` names only log-concavity as Suen's condition (claim
SU-K).** The sentence "in Suen's case, log-concavity of $1-F$" omits the
concavity of `θ` and `φ` that Proposition 2 also assumes (p.154), and P2-2
shows the `φ` part is load-bearing. The contrast `PROOFS.tex` draws, an
aggregate that needs these conditions against a count that needs neither, is
unaffected; the sentence understates what the aggregate needs.

**P2-4. `CLAIMS.md` SU-F quotes superseded text.** Recorded in `CLAIMS.md`
and in the ERROR section above. `lit/TRACEABILITY.md` row 5 still lists "two
independent sources" as a live survey error; that file is outside this
directory and was not edited.

**P2-5. Quotation fidelity.** Pass 1 quoted p.154 with "dominates" where the
page prints "dominate". Corrected here with [sic], and in the SU-4 printout of
`verify_suen.py`. `RECONSTRUCTION.md` §4(c) carries the same slip and was left
as the pass 1 record.

**P2-6. `PROOFS.tex` claim SU-J is consistent with the source.** Suen's route
uses the sign of `G₁⁻¹ − G₀⁻¹` on every block of `[0,1]` (`hneg`, `hpos`) and
the partial integral at every even crossing up to `k_{2n} = 1` (`hCL`), then
integrates `H` against `ρ′` over all of `[0,1]`. SU-LC3 shows that when the
weight drops at every rank, as `ρ` does when `1−F` is strictly log-concave,
one positive interior partial sum flips the aggregate. SU-LC3 is the Suen side
of the contrast in `PROOFS.tex`, against a count that reads the quantile function
only down to the margin. The count side is not formalised here.

**P2-7. Observation, ours and not Suen's.** The finite crossing structure is
bookkeeping rather than a restriction the sign needs. `hhat_nonpos_direct`
derives `Ĥ ≤ 0` at every rank from the tangent bound with pointwise slopes
and the Costrell–Loury step at every rank, with no crossing points.
`cl_at_even_crossings` shows that under alternation the partial sums at the
even crossings already bound the partial sums at every rank, so Suen's
even-crossing use of the Costrell–Loury step and the every-rank use carry the
same information. The survey's phrase "arbitrary finite number of crossings"
describes Suen's proof correctly; the finiteness is not what makes the sign
come out.

**P2-8. Observation on the Costrell–Loury step as Suen states it.** Suen
states the step with the premise "the two distributions have the same mean".
`cl_quantile_reversal` proves the partial-sum ordering without that premise
and without sortedness of the dominating vector, from the shortfall ordering
and sortedness of the dominated vector alone. The same-mean premise matters
for the equality at the top rank, not for the inequality. P2-8 is a statement
about the discrete skeleton; `../costrell_loury_2004` owns the check of the
step against Costrell–Loury's text.

**P2-9. Notation in (6).** Suen defines `Ĥ` as a function of `x` and then
evaluates it at rank points ("Ĥ(k₂ᵢ)", "local maxima at x = k₂ᵢ"). The
evaluation is meaningful as `Ĥ` at the `x` with `F(x) = k₂ᵢ`. The Lean works
in rank space, where the two readings coincide.

### Attributed claims not formalised

- SU-A (the matching function as a distribution function) and the wage ODE
  (2). These are calculus facts, checked in SymPy as SU-1 and SU-2.
- SU-3 (log-concavity of `1−F` is `ρ′ ≤ 0`). A calculus identity, checked in
  SymPy; the Lean takes `ρ` non-increasing as the hypothesis.
- SU-D (Proposition 3 and Corollaries 1–2). Our documents attribute only
  their existence, which a reading confirms.
- SU-E (cross-side versus same-side). A statement about which populations are
  compared, with no arithmetic content.
- Strictness of every inequality, for the reason given under "Not attempted".
