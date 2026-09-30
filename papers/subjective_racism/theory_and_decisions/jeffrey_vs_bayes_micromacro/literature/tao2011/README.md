# Tao, T. (2011), *An Introduction to Measure Theory*

Graduate Studies in Mathematics 126, American Mathematical Society.

**Source.** The copy used is the author's "preliminary version of the book …
made available with the permission of the AMS" (`tao2011_measure.pdf`,
265 pp.). It is not the Drive copy (Tao is not in Drive). Page numbers below
are those of the preliminary version, written `pre-p.N`. Theorem and exercise
numbers are assumed to match the published book, but this was not checked. The
statements used were read in the text extracted from the PDF on 2026-09-30:

* Exercise 1.4.23 (pre-p.93);
* Theorems 1.7.15 and 1.7.18 (pre-p.200-203);
* Corollary 1.7.23 (pre-p.206).

## Claims formalized

Paper B's proof of Theorem LOS (Appendix A.3) cites Tao at two steps:

* **Step 3 (MS:1136).** "Tonelli's theorem \citep[Corollary~1.7.23]{tao2011measure}
  gives `L(c) = ∬_flip |u| g ≤ cM ∬_{u∈B_c} g = cM μ(B_c)`". Corollary 1.7.23 is
  the **Fubini-Tonelli theorem**. It is stated for measurable `f : X × Y → ℂ`
  that is absolutely integrable in the iterated sense, and concludes that
  Fubini's theorem applies. For a nonnegative integrand the relevant statement
  is **Tonelli's theorem**:
  * Theorem 1.7.15 (incomplete version): σ-finite spaces and
    `f : X × Y → [0, +∞]` measurable for the product σ-algebra; (i) the partial
    integrals are measurable; (ii) the double integral equals both iterated
    integrals.
  * Theorem 1.7.18 (complete version): the same for `f` measurable for the
    completed product σ-algebra, with a.e.-defined sections, Eq. (1.37).
* **Step 4 (MS:1150).** "As `μ` is finite, continuity from above
  \citep[\S1.4]{tao2011measure} yields `μ(B_c) ↓ μ(∅) = 0`". This is
  **Exercise 1.4.23(iii)** (downward monotone convergence). If
  `E₁ ⊃ E₂ ⊃ …` are measurable and `μ(E_n) < ∞` for at least one `n`, then
  `μ(⋂ E_n) = lim μ(E_n) = inf μ(E_n)`. The exercise also asks for an example
  showing that the finiteness hypothesis cannot be dropped.

## Result

`lean/Tao.lean` is a symlink to `lean/Literature/Tao.lean`. It is a **bridge
file**: it states each result in Tao's form and proves it from the matching
Mathlib theorem. It is standalone on Mathlib, checks with `lake env lean`, and
contains no `sorry`. All 14 of its theorems use only the axioms `[propext,
Classical.choice, Quot.sound]` (checked in a scratch copy).

| Lean theorem | Tao | Mathlib theorem used |
|---|---|---|
| `ex_1_4_23_ii` | Ex. 1.4.23(ii), upward monotone convergence | `Monotone.measure_iUnion`, `tendsto_measure_iUnion_atTop` |
| `ex_1_4_23_iii` | **Ex. 1.4.23(iii), continuity from above** | `Antitone.measure_iInter`, `tendsto_measure_iInter_atTop` |
| `ex_1_4_23_iii_needs_finite` | the exercise's counterexample: `E_n = [n, ∞)` | `Real.volume_Ici` |
| `thm_1_7_15` | **Thm 1.7.15 (i) and (ii)** | `Measurable.lintegral_prod_right'`, `Measurable.lintegral_prod_left'`, `lintegral_prod`, `lintegral_prod_symm` |
| `thm_1_7_18` | **Thm 1.7.18 (i)-(iii)**, with "measurable for the completion" rendered as `AEMeasurable f (μX.prod μY)` | `Measure.ae_ae_of_ae_prod`, `AEMeasurable.lintegral_prod_right'/left'`, `lintegral_prod`, `lintegral_prod_symm` |
| `tonelli_swap` | swapping iterated integrals | `lintegral_lintegral_swap` |
| `measure_band_tendsto_zero` | **the lemma Step 4 uses**: nested measurable `B_c` (`c' ≤ c ⇒ B_{c'} ⊆ B_c`) with `⋂_{c>0} B_c = ∅` in a finite measure space give `μ(B_c) → 0` as `c ↓ 0` | `tendsto_measure_biInter_gt` |
| `band`, `measurableSet_band`, `band_mono`, `band_iInter_empty` | the manuscript's `B_c = {u : 0 < \|u\| ≤ cM}`. It is measurable, nests for `M ≥ 0`, and exhausts (`u = 0` lies in no band) | — |
| `los_step4` | Step 4 for any finite `μ` on `ℝ`, with atoms at `0` allowed | the above |
| `los_step4_littleO` | Step 4's conclusion: `0 ≤ L(c) ≤ cM μ(B_c)` gives `L(c)/c → 0` | `ENNReal.tendsto_toReal`, squeeze |
| `los_step3_marginal` | **Step 3's identity**: `∫_δ ∫_u 1_B(u) g(u,δ) = ∫_B (∫_δ g(u,δ))`, the `u`-marginal mass of `B` | `lintegral_lintegral_swap`, `lintegral_indicator` |
| `los_step3_bound` | **Step 3's bound**: if the flip set lies over `B` and its weight is `≤ K`, then `∬_flip w g ≤ K μ_u(B)` | `lintegral_mono`, `lintegral_const_mul` |

`sympy/check_los.py` (10/10 PASS) checks the following:

* `μ(B_c) → 0` for three laws of `u`: `N(0,1)`, an atom of mass `1/2` at `0`
  mixed with `N(0,1)`, and `U[-1,1]`. For the mixture, `μ{|u| ≤ c}` tends to
  `1/2`, not `0`, so excluding `u = 0` from the band matters.
* The `[n, ∞)` counterexample.
* Both iterated integrals of a correlated nonnegative joint density, and
  Step 3's marginal identity.
* For `u ~ N(0,1)` and `δ ~ U[-1,1]` independent (`M = 1`):
  * the exact loss `L(c) = 1/√(2π) − erf(c/√2)/(2c)`;
  * the bound `L(c) ≤ cMμ(B_c)` at five values of `c`;
  * `L(c)/c → 0`;
  * Step 5's constant: `L(c) = f(0)/2 · E[δ²] c² + O(c⁴)`, with coefficient
    `1/(6√(2π))`;
  * a first-order flip share, `P(flip) = f(0)E|δ| c + O(c³)`.

### What `lean/JeffreyOrder/Decision.lean` already uses

`Decision.lean` formalizes Steps 1-2 and the per-`δ` form of Step 3. It
imports only `MeasureTheory.Integral.Lebesgue.Basic`,
`MeasureTheory.Measure.Lebesgue.Basic` and `Tactic.Linarith`, and uses:

* `setLIntegral_const`, `lintegral_mono_ae`, `ae_restrict_mem`;
* `Real.volume_Ico`, `measure_empty`, `measurableSet_Ico`;
* `ENNReal.ofReal_le_ofReal`.

It proves the following for a fixed `δ`:

* `lintegral_stake_le`: `∫_{flip} |u| dμ ≤ |cδ| μ(flip)`;
* `volume_flipSet`: the flip set has Lebesgue measure `|cδ|`.

It uses **neither** result cited from Tao. No file in `JeffreyOrder/` or
`AssocLocality*` uses a product measure, Tonelli (`lintegral_prod`,
`lintegral_lintegral_swap`) or continuity from above (`tendsto_measure_*`,
`measure_iInter_*`). The integration over `δ` (Step 3) and the limit
`c ↓ 0` (Step 4) are formalized only here, in `Tao.lean`
(`los_step3_marginal`, `los_step3_bound`, `los_step4`, `los_step4_littleO`).

## What formalizing revealed

1. **Step 4 applies Tao to a continuum-indexed family.** Exercise 1.4.23(iii)
   is stated for sequences `E₁ ⊃ E₂ ⊃ …`, but the manuscript applies it to
   `(B_c)_{c>0}` with `c ↓ 0`. The passage is standard: `B_c` is monotone in
   `c`, so it suffices to take `c = 1/n`. It is not literally Tao's statement.
   Mathlib's `tendsto_measure_biInter_gt` does this passage directly.
2. **Both Tao hypotheses hold, and are used.** Measurability of `B_c` and
   finiteness of `μ` (the marginal law of `u` is a probability measure) are the
   hypotheses of Ex. 1.4.23(iii). The manuscript states finiteness ("As `μ` is
   finite"). Exhaustion needs `u = 0` to be excluded from every band. The
   manuscript's `B_c = {0 < |u| ≤ cM}` does this, and it is right to insist on
   it ("That the band excludes `u = 0` does matter in the later Step 4"):
   `{|u| ≤ cM}` would decrease to `{0}`, whose mass need not be `0`.
3. **Step 3 needs only Tonelli, not Fubini-Tonelli.** Every integrand in
   Step 3 (`|u| g 1_flip`, `g 1_{B_c}`) is nonnegative, so Theorem 1.7.15
   applies with no integrability hypothesis. Corollary 1.7.23 adds an
   absolute-integrability hypothesis that is not needed, for a conclusion
   about signed integrands that is not used.

## Bearing on Paper B

The mathematics of Steps 3-4 is correct as written. `los_step3_bound` and
`los_step4_littleO` prove the chain `L(c) ≤ cMμ(B_c)`, `μ(B_c) → 0`,
`L(c) = o(c)` in the manuscript's generality: any finite `u`-marginal, and any
joint density for the `(u, δ)` form. Only the pinpoint citations need fixing.

## Audit findings (2026-09-30)

This section responds to `notes/citation_audit.md` item M16.

* **M16 (VERIFIED-WITH-CAVEAT): "Tonelli's theorem [Corollary 1.7.23]" and
  "continuity from above [§1.4]".** Confirmed. Corollary 1.7.23 is the
  Fubini-Tonelli theorem. For the nonnegative integrand of Step 3, the exact
  reference is Tonelli, Theorem 1.7.15. If completeness of the product
  σ-algebra is wanted, cite Theorem 1.7.18. "Continuity from above" is
  Exercise 1.4.23(iii), and its finiteness hypothesis holds. Two additions:
  * Tao states the exercise for sequences. The manuscript's `c ↓ 0` is the
    standard continuum version (point 1 above).
  * The copy consulted is the author's preliminary version. Numbering and
    pagination of the printed book are unverified.
* **Bibliographic entry** `tao2011measure` (bibliography.bib:197). The
  preprint names the AMS as publisher. It does not print the series, the
  volume (126), the year or the ISBN, so those fields are unverified here. The
  entry's `note` points to the author's freely available version, presumably the
  version consulted here.

### Proposed corrected wording (text only, not applied)

* **MS:1135-1136**, replace "Tonelli's theorem
  \citep[Corollary~1.7.23]{tao2011measure} gives" with:
  > Tonelli's theorem \citep[Theorem~1.7.15]{tao2011measure}, applicable since
  > the integrand is nonnegative, gives
* **MS:1149-1150**, replace "As $\mu$ is finite, continuity from above
  \citep[\S1.4]{tao2011measure} yields" with:
  > As $\mu$ is finite and $B_c$ decreases as $c\downarrow0$, continuity from
  > above \citep[Exercise~1.4.23(iii)]{tao2011measure}, applied along any
  > sequence $c_n\downarrow0$, yields

## Not formalized

* Tao's own proofs (monotone class lemma, Prop. 1.7.11). Mathlib's theorems
  are used as black boxes.
* Corollary 1.7.23 itself (Fubini for signed or complex integrands). The
  manuscript does not need it.
* The identification of Tao's completed product σ-algebra with Mathlib's
  `AEMeasurable` hypothesis. It is argued in the Lean header, not proved as an
  equivalence of σ-algebras.
