# Notes — Suen (2007), pass 1

Run: `python3 verify_suen.py` — **6 checks, 0 failures**.

**Scope.** Our reading is arithmetically consistent, and the survey's
description of Proposition 2 is **exact**. Two corrections follow, neither
about Prop 2 itself. Nothing here reproves any result of the paper.

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

**SU-3 sharpens one point in the survey's favour.** Log-concavity of `1−F` is
not merely *sufficient* for `ρ' ≤ 0` — it is exactly equivalent, since
`d²/dx² log(1−F) = ρ'/ρ²` identically. So the hypothesis is doing precise work
at the final step, and cannot be weakened there without changing the argument.

---

## ERROR: the two sources are not independent

The synthesis paragraph calls them *"Two **independent** published sources."*
They are not. Suen's proof imports its key step from Costrell–Loury by name:

> "if `G₀` second-order stochastically dominates `G₁` and the two distributions
> have the same mean, then `G₁⁻¹` second-order stochastically dominates
> `G₀⁻¹` (see, for example, **[2]**)"

where **[2] is Costrell and Loury**. Suen's footnote 1 also engages directly
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

---

## A second remark the survey omits: concavity is not necessary

p.155: *"the concavity assumption is sufficient but not necessary for the
conclusion of Proposition 2. If `θ` and `φ` are both linear, for example, the
proof of Proposition 2 indicates that `W₁` is still strictly lower than `W₀`."*

SU-6 confirms the mechanism: with `θ'' = 0`, `(5)` collapses to `H = θ'Ĥ`, and
`Ĥ < 0`, `θ' > 0`, `ρ' ≤ 0` still sign `(4)`. So concavity is not load-bearing
for the *direction* of Prop 2 — which is mildly encouraging for a transplant,
since it is concavity that footnote 1 says will not survive rescaling.

---

## Results

| ID | Result |
|---|---|
| SU-1 | CONSISTENT. `μ = G⁻¹∘F` is increasing with `μ(0)=0`, `μ(1)=1` on all four `F,G` pairs, so it qualifies as a CDF, as the paper notes. |
| SU-2 | CONSISTENT. FOC `(2)` and the SOC gap `θ'·(d/dx)φ(μ)` are as stated. |
| SU-3 | CONSISTENT, and stronger than stated: `d²/dx² log(1−F) − ρ'/ρ² = 0` identically, so log-concavity of `1−F` **is** `ρ' ≤ 0`. |
| SU-4 | CONSISTENT. On an explicit mean-preserving pair, `∫₀^k[G₁⁻¹−G₀⁻¹]dt = −k²(k−1)²/2 ≤ 0` with equality exactly at `k = 0, 1`. |
| SU-5 | CONFIRMS FOOTNOTE 1, and the result matters. See above. |
| SU-6 | CONSISTENT. The linear case still signs the conclusion. |

## Not attempted

- Inequality `(6)` — the recombination of partial integrals under concavity of
  `φ`. This is the technical core of Prop 2 and was **not** checked.
- The derivation of `(4)`'s middle equality and the definition of `H`.
- The step from `Ĥ ≤ 0` at the local maxima to `Ĥ < 0` almost everywhere.
- Propositions 1 and 3, Corollaries 1–2.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## Net effect on open item 2

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
