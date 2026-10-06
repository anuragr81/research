# Notes — Ryvkin & Drugov (2020), pass 1

Run: `python3 verify_rd.py` — **7 checks, 0 failures**.

**Scope.** Our reading is arithmetically consistent. The handover's
"computation, not a read" is done, with a partial answer. One label in the
Verdict is wrong. Nothing here reproves any RD result, and **nothing here is a
new result about `Δ(0,Q)`**.

---

## The reading is solid, and independently verified

`b_k = E[f(X_{(k−1:k−1)})]` is confirmed (RD-1): `(3)` and `(9)` agree, and
`F^{k−1}` is the CDF of the maximum of `k−1` draws, so `b_k` is the expected
noise density at the best rival's shock. Their interpretation: a marginal
effort increase is pivotal exactly at a tie.

**RD-2 is the strongest check in this directory.** Their worked example — type
I generalized logistic, `F(x) = 1/(1+e^{−x})^a` — was re-derived from the
primitives by changing variable to `u = F`, giving `f = a·u(1−u^{1/a})` and

> `b_k = a(k−1)/[k(ak+1)]`

matching the paper exactly, with `b_{k+1} − b_k` proportional to
`1 + a − ak(k−1)` with **positive** constant of proportionality, also as
stated. Reproducing a non-trivial closed form from scratch is much better
evidence that `(3)` was read correctly than any restatement.

---

## The computation the handover asked for

**Result: the correspondence is exact, and one of the two hypotheses
transfers.**

Our P-MU weight is `W = G^{Q−1}(1−G)·F`. S11 established it is hump-shaped in
`G` with an interior maximum at `G = (Q−1)/Q`. RD's log-supermodular kernel is

> `−H_θ = F^{k−1} − F^k = F^{k−1}(1−F)`,  peaking at `F = (k−1)/k`

Under `G ↔ F`, `Q ↔ k` these are **the same function** (RD-4). Our extra factor
`F` is the incumbent's CDF and carries no `Q`.

So S11 — which we derived independently, to show FOSD alone cannot sign the
comparison — turns out to be Ryvkin and Drugov's kernel. That is worth saying
in `PROOFS.tex` on its own: the hump-shape is not an artefact of our
construction but the standard object in this literature.

**Hypothesis 1 transfers (RD-5).** Log supermodularity requires
`∂² log[z^{n−1}(1−z)]/∂z∂n ≥ 0`; the cross-partial is exactly `1/z > 0` on
`(0,1)`. The factor `F` does not involve `Q`, so it adds nothing to the
cross-partial and the property is inherited by `W`.

**Hypothesis 2 does not (RD-6).** Karlin's argument also needs the integrand in
the role of `u'` to be single crossing **`+−`**. Ours is
`F·(f−g) = −F·φ'`, where `φ = G−F ≥ 0` vanishes at both endpoints by P2 and
the regularity in §Primitives. `φ` is hump-shaped, so `φ'` crosses `+−` and
`−φ'` crosses **`−+`** — the reverse orientation. Verified concretely on
`F = x²`, `G = x`: `φ' = 1−2x` crosses `+−`, while `f−g = 2x−1` crosses `−+`.

**So the answer to "do the conditions coincide?" is: one does, exactly; the
other is reversed.** That is more informative than a yes or a no, and it
locates the gap precisely.

**What is NOT established.** `Δ(0,Q)` is **not** shown to be unimodal in `Q`.
Karlin's conclusion does not transfer as-is. Two routes remain, neither tried:

1. Work out what a `−+` crossing yields. The machinery is symmetric enough
   that it should give an *anti*-unimodal (single-troughed) conclusion — i.e.
   `Δ(0,Q)` with an interior *minimum* in `Q`. That would be a genuine result,
   and it is consistent with R2's refutation (the sign of the `Q` effect
   reverses at `μ*`), but **it has not been derived**.
2. Reformulate so the `+−` orientation is restored.

Per `lit/README.md` rule 1 and the handover's standing instruction: this is a
lead, not a result, and must not be claimed until derived.

---

## ERROR: the Verdict mislabels which RD result P-MU parallels

`LITERATURE.tex` §Verdict item 3 calls P-MU "the discrete analogue of the
\citet{RyvkinDrugov2020} **hazard-rate** result".

RD have two distinct results with two distinct order statistics:

| object | quantity | order statistic |
|---|---|---|
| **individual** effort | `b_k = E[f(X_{(k−1:k−1)})]` — the **density** | max of `k−1` draws |
| **aggregate** effort (quadratic cost) | `E(h(X_{(k−1:k)}))` — the **hazard rate** | second-highest of `k` draws |

RD-7 confirms these differ: the pdfs stand in ratio `k(1−F)`, not 1.

`Δ(0)` is an **individual** gain — the marginal challenger's incentive — so the
right comparator is `b_k`, the *density* result. The survey itself says this
correctly elsewhere ("their `b_k = E[f(X_(k−1:k−1))]` against our `Δ(0)`"), so
the Verdict's "hazard-rate result" is a slip in the compressed summary,
inconsistent with the survey's own §sec-level text.

**Fix:** in the Verdict, replace "the RyvkinDrugov2020 hazard-rate result" with
"the RyvkinDrugov2020 density result `b_k = E[f(X_{(k−1:k−1)})]`", and keep the
hazard rate for their aggregate-effort statement if it is mentioned at all.

---

## Results

| ID | Result |
|---|---|
| RD-1 | CONSISTENT. `(3)` and `(9)` agree identically; `b_k = E[f(X_{(k−1:k−1)})]`. |
| RD-2 | CONSISTENT, independently derived. `b_k = a(k−1)/[k(ak+1)]` and `b_{k+1}−b_k ∝ 1+a−ak(k−1)`, both matching the paper. |
| RD-3 | CONSISTENT. `−H_θ = z^{k−1}(1−z)`, interior peak at `(k−1)/k`. |
| RD-4 | CONSISTENT. P-MU's weight contains exactly this kernel; S11's peak `(Q−1)/Q` is RD's `(k−1)/k`. |
| RD-5 | CONSISTENT. Cross-partial of the log kernel is `1/z > 0`; log-supermodularity is inherited by `W`. |
| RD-6 | CONSISTENT, and it is the blocker. Our integrand crosses `−+` where RD need `+−`. |
| RD-7 | CONSISTENT. The two order statistics differ by a factor `k(1−F)`; the survey picks the right comparator in its body text. |

## Housekeeping

RD-7 was initially written with a hardcoded `True` — the same vacuity bug as
`verify_p89.py:210` and the HK-5 episode. Replaced with an assertion that the
two order-statistic densities differ by exactly `k(1−F)`. A sweep across all
literature suites and `checks/` now finds **no remaining hardcoded-`True`
checks**.

## Not attempted

- Proposition 1's proof and the Karlin apparatus; Lemmas 1–2; the PSD family.
- Propositions 2–6, Corollaries 3–6, Proposition 7 / Appendix A.1 existence.
- §2.4's multiplicative-shock reduction; Appendix A.2's `TP_r` extension to
  multimodal densities — **this last one may matter**, since it relaxes
  unimodality, and our `φ` is hump-shaped by construction.

Anything leaning on these is **[A]-grade** despite the [F] tag.
