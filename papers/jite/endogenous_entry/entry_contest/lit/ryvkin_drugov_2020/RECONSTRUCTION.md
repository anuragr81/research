# Reconstruction — Ryvkin & Drugov (2020)

**Source.** Dmitry Ryvkin and Mikhail Drugov, "The shape of luck and
competition in winner-take-all tournaments", *Theoretical Economics* 15 (2020),
pp. 1587–1626. Published version, text layer present, 40pp.

This is the paper behind the third surviving contribution (P-MU), and the
handover's next-step 4 calls the comparison *"a computation, not a read"*.
This reconstruction does that computation.

---

## 1. The question

In a winner-take-all tournament with `k` symmetric agents, performance is
effort plus noise. How do individual and aggregate equilibrium effort depend
on the **number of players**, and on the **shape** of the noise distribution?

## 2. Primitives

Agent `i` exerts effort `e_i` at cost `c(e_i)`; output is `e_i + x_i` with
`x_i` i.i.d. from `F` with density `f`. Winner takes the prize. Group size may
be stochastic, `K̃` with pmf `p`.

## 3. The central coefficient

Their `(3)`:

> `b_k = (k−1) ∫ F(x)^{k−2} f(x) dF(x)`

and equivalently `(9)`:

> `b_k = ∫ f(x) dF(x)^{k−1} = ∫ f(x) f_{(k−1:k−1)}(x) dx = E[f(X_{(k−1:k−1)})]`

Since `F^{k−1}` is the CDF of the maximum of `k−1` i.i.d. draws, `b_k` is the
**expected density of the noise, evaluated at the best rival's shock**. The
interpretation `(5)`: `c'(e*_k) = b_k`, so `b_k` is the *individual* marginal
benefit of effort. A marginal effort increase is pivotal exactly at a tie, so
the density at the tie point is what matters.

For stochastic group size, `(4)`: `c'(e*) = B_p = Σ_k p̃_k b_k`, and `(6)`
rewrites `B_p = ∫ f(x) dG̃(F(x))` with `G̃` the pgf of `K̃`.

## 4. The comparative-statics machinery

The paper's engine is Karlin (1968) variation-diminishing. For
`γ(θ) = ∫ u(z) dH(z|θ)` with `H` a CDF FOSD-increasing in `θ`, integration by
parts gives `γ_θ(θ) = −∫ u'(z) H_θ(z|θ) dz`. Then:

> if `u'(z)` is single crossing **`+−`** and `−H_θ(z|θ)` is **log
> supermodular**, then `γ_θ` is single crossing `+−`, hence `γ` is unimodal.

For deterministic size, `θ = k` and `H(z|θ) = F(x)^{k−1}`, so

> `−H_θ = F^{k−1} − F^k = F^{k−1}(1−F)`

which is log supermodular. With `u = f` unimodal, `u' = f'` is single crossing
`+−`, giving:

> **Corollary 1.** In tournaments with deterministic size `k`, if `f(x)` is
> unimodal, then `e*_k` is unimodal in `k`.

**Proposition 1** extends this to stochastic size under log-supermodularity of
`−G̃_θ`; **Corollary 2** applies it to the PSD family.

**Aggregate effort is a different object.** p.1593: with quadratic cost,
aggregate effort is `E(h(X_{(k−1:k)}))` where `h` is the **failure (hazard)
rate** and `X_{(k−1:k)}` is the **second-highest** of `k` draws.

## 5. Worked example, reproduced independently

Type I generalized logistic, `F(x) = 1/(1+e^{−x})^a`. Changing variable to
`u = F` gives `f = a·u·(1 − u^{1/a})`, and

> `b_k = a(k−1)/[k(ak+1)]`,  `b_{k+1} − b_k ∝ 1 + a − ak(k−1)`

Both were derived here from the primitives and match the paper exactly (RD-2).
This is the strongest available check that the reading of `(3)` is correct.

## 6. What could not be reconstructed

1. Proposition 1's proof (the Karlin apparatus itself), Lemmas 1–2, and the
   PSD-family results.
2. Propositions 2–6, Corollaries 3–6, and the existence conditions of
   Proposition 7 / Appendix A.1.
3. The multiplicative-shock reduction (§2.4) and Appendix A.2's `TP_r`
   generalisation to multimodal densities.

Anything leaning on these is **[A]-grade** despite the [F] tag.

## 7. The P-MU computation

Our P-MU (`PROOFS.tex` `prop:PMU`):

> `Δ(0,Q+1) − Δ(0,Q) = −∫ W (dF − dG)`,  `W = G^{Q−1}(1−G)F`

and S11 established `W` is hump-shaped in `G` with interior maximum at
`G = (Q−1)/Q`.

**The correspondence is exact, not analogical.** RD's log-supermodular kernel
is `F^{k−1}(1−F)`, peaking at `F = (k−1)/k`. Our kernel is `G^{Q−1}(1−G)`,
peaking at `G = (Q−1)/Q`. Under `G ↔ F`, `Q ↔ k` these are the *same
function*. Our additional factor `F` is the incumbent's CDF and carries no `Q`.

So S11 — which we derived independently to show FOSD cannot sign the
comparison — is RD's kernel. That is worth stating on its own.

**Hypothesis 1 transfers.** Log supermodularity needs
`∂² log[z^{n−1}(1−z)]/∂z∂n ≥ 0`; the cross-partial is `1/z > 0` on `(0,1)`
(RD-5). The extra factor `F` does not involve `Q`, so it contributes nothing to
the cross-partial and the property is inherited.

**Hypothesis 2 does not.** RD need the integrand playing the role of `u'` to be
single crossing **`+−`**. Ours is `F·(f−g) = −F·φ'` with `φ = G−F ≥ 0`
vanishing at both endpoints. Since `φ` is hump-shaped, `φ'` crosses `+−`, so
`−φ'` crosses **`−+`** — the opposite orientation (RD-6, verified on
`F = x², G = x`).

**Conclusion.** The conditions coincide *in one of two places*. The handover
asked whether they coincide, expecting a yes/no; the answer is that the
log-supermodularity condition is literally the same, while the single-crossing
orientation is reversed. So Karlin's argument does **not** transfer as-is, and
`Δ(0,Q)` is **not** thereby shown to be unimodal in `Q`.

What would close it: either an argument for the reversed orientation (Karlin's
machinery is symmetric enough that a `−+` crossing should give an
*anti*-unimodal — single-troughed — conclusion, which would be a statement
about `Δ(0,Q)` having an interior *minimum* in `Q`), or a reformulation that
restores the `+−` orientation. **Neither has been attempted, and nothing here
should be claimed as a result about `Δ(0,Q)`.**
