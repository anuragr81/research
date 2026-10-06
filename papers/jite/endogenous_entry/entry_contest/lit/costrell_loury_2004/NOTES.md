# Notes — Costrell & Loury, pass 1

Run: `python3 verify_cl.py` — **6 checks, 0 failures**.

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
