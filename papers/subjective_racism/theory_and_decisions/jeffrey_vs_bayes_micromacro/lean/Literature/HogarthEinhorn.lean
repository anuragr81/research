/-
# Hogarth & Einhorn (1992), "Order Effects in Belief Updating: The Belief-Adjustment Model"

*Cognitive Psychology* 24, 1-55.

Formalization of the paper's own model and order-effect derivations, not of
Paper B's claims.  The source used is a compiled LaTeX **transcription** of the
article (42 pages), not the journal scan; page references `T-p.N` are to that
transcription, since the journal pagination is not available.  The equations in
the transcription were "reconstructed from the (OCR-mangled) source"; every
identity below was re-derived, so a transcription error in a displayed equation
would have shown up as a failed proof (none did).

## The model (T-p.6-12)

  * Eq. (1)  `S_k = S_{k-1} + w_k [s(x_k) - R]`, `0 ≤ w_k ≤ 1`             `adjust`
  * Eq. (2)  evaluation mode, `R = 0`: `S_k = S_{k-1} + w_k s(x_k)`          `evalStep`
  * Eq. (3)  estimation mode, `R = S_{k-1}`                                   `estStep`
  * Eq. (4)  the averaging form `S_k = (1 - w_k) S_{k-1} + w_k s(x_k)`       `estStep_averaging`
  * Eqs. (6a)/(6b) contrast weights `w_k = α S_{k-1}` if `s(x_k) ≤ R`,
    `w_k = β (1 - S_{k-1})` if `s(x_k) > R`                                   `contrastWeight`
  * Eqs. (7a)/(7b) the Step-by-Step (SbS) model with those weights            `estContrast`, `evalContrast`
  * Eq. (5)  End-of-Sequence (EoS): `S_k = S_0 + w_k [s(x_1,…,x_k) - R]`     `eos`
  * Eq. (8)  EoS with the first item as anchor:
             `S_k = s(x_1) + w_k [s(x_2,…,x_k) - R]`                          `eq8`

An order effect is `D = S_ab - S_ba` for two items with `s(x_a) < s(x_b)`
(Appendix B, T-p.34): `D > 0` is recency, `D < 0` is primacy.

## What is proved

**SbS, estimation mode (`R = S_{k-1}`), Appendix B.**
  * `appB_orderEffect` — Eq. (B.3), for arbitrary item- and position-specific
    weights.
  * `appB_recency` — Eqs. (B.4)/(B.5): under the Anderson-Hovland assumption
    `w_a = w_ba`, `w_b = w_ab` (each item carries its own weight wherever it
    stands), `D = w_a w_b (s_b - s_a) > 0`: recency.
  * `positional_orderEffect`, `positional_primacy_iff` — weights attached to
    positions instead (`w₁` first, `w₂` second): `D = (s_b - s_a)(w₂(1 + w₁) - w₁)`,
    so primacy iff `w₂ (1 + w₁) < w₁`.  This is the two-item content of HE's
    "decrements in α and β that will eventually induce primacy" (T-p.13).
  * `contrast_mixed_orderEffect`, `contrast_mixed_recency` — **with HE's own
    contrast weights (6a/6b)** and mixed evidence (`s_a < S_0 < s_b`), a closed
    form whose every term is nonnegative, so recency holds.  HE do not prove
    this; Appendix B only covers the Anderson-Hovland weights, and HE note that
    "in our model it is not necessarily the case that `w_a = w_ba` and that
    `w_b = w_ab`" (T-p.35).
  * `contrast_consistent_pos_nocross` — consistent evidence above the anchor,
    closed form `D = β²(1-S₀)²(s_b - s_a)(1 - β(s_a + s_b - 2S₀))`, which is
    negative (primacy) when `β(s_a + s_b - 2S₀) > 1`.
  * `contrast_consistent_primacy_asym`, `contrast_consistent_primacy_equal` —
    explicit parameter values (the second with `α = β`) for which HE's SbS
    model with `R = S_{k-1}` gives **primacy**.  So the text's claim "when
    `R = S_{k-1}`, the SbS process always predicts recency (for α, β ≠ 0)"
    (T-p.11; Table 2 "Recency" in the SbS `R = S_{k-1}` column, T-p.12) holds
    for the Anderson-Hovland weights and for mixed evidence, but not for
    consistent evidence under HE's own weights.

**SbS, evaluation mode (`R = 0`), Appendix C.**
  * `appC_consistent_neg`, `appC_consistent_pos` — Eqs. (C.1)-(C.4): no order
    effect for consistent evidence.
  * `appC_mixed_orderEffect`, `appC_mixed_recency` — Eq. (C.7):
    `D = -αβ s(x⁻) s(x⁺) > 0`, independent of `S_0`.
  * `variant_constant_noEffect`, `variant_reversed_mixed_primacy` — the variants
    of T-p.13-14: constant `w` gives no order effect; reversing the contrast
    assumption gives primacy for mixed evidence.

**EoS, Eq. (8).**
  * `eq8_estimation`, `eq8_evaluation` — the effective weights: with the
    aggregate a weighted average `∑ c_j s(x_j)`, the first item gets `1 - w`
    (estimation, `R = s(x_1)`) or `1` (evaluation, `R = 0`); item `j` gets `w c_j`.
  * `eq8_estimation_first_dominates` (`w < 1/2`), `eq8_evaluation_first_dominates`
    (`w < 1`) — the first item's effective weight exceeds every other's, which is
    HE's "force toward primacy" (T-p.12).
  * `eq8_estimation_uniform_iff` — with the plain mean over `n` later items the
    first item dominates iff `w < n/(n+1)`; `eq8_estimation_not_always` — so the
    statement "must be greater" is conditional in estimation mode.
  * `eos2_est_orderEffect`, `eos2_eval_orderEffect` — two items:
    `D = (2w - 1)(s_b - s_a)` (estimation) and `D = -(1 - w)(s_b - s_a)` (evaluation).
  * `eos_symmetric_noEffect` — Eq. (5) with an explicit `S_0` and a symmetric
    aggregate has no order effect: EoS primacy needs the first item to *be* the
    anchor.

**Comparison: the one-sided construction (not HE's).**  First cue adopted in
full, then the second cue damped by `w`, for a single scalar.
  * `oneSided_eq_eq8` — it is exactly HE's Eq. (8) (EoS, anchor = first item,
    estimation) with `k = 2`, and `oneSided_eq_positional` — it is the positional
    SbS model with `w₁ = 1`.
  * `oneSided_orderEffect`, `oneSided_primacy_iff` — `D = (2w - 1)(s_b - s_a)`:
    primacy iff `w < 1/2`, recency iff `w > 1/2`.
  * `twoSided_orderEffect`, `twoSided_recency` — HE's SbS (`R = S_{k-1}`) with
    the same `w` on both cues: `D = w²(s_b - s_a) > 0` for every `w ∈ (0,1]`.
  * `sided_gap`, `sided_disagree` — the difference is `(1-w)²(s_b - s_a)`; for
    `0 < w < 1/2` the two models predict opposite order effects.
  * `twoAttr_twoSided_noEffect`, `twoAttr_oneSided_orderEffect` — with two
    attributes each moved only by its own cue, HE's two-sided rule has no order
    effect at all, while the one-sided rule has `(1 - w)(s_A - S_A)` on the
    attribute read first.  The protection of the first-read attribute comes from
    its full adoption, not from partial adjustment.

## Not formalized

Long series with `α`, `β` declining over more than two items (only the two-item
positional form above); the task-characteristic mapping of Fig. 1 and Table 2
beyond the cells derived here; the Experiments 1-5 statistics (see
`literature/hogarth_einhorn1992/sympy/check_counts.py`); boundedness of
`S_k` in `[0,1]` beyond the weight bound `contrastWeight_mem_Icc`.
-/
import Mathlib

namespace Literature.HogarthEinhorn

open Finset

/-! ## The model -/

/-- **Eq. (1)** (T-p.6): `S_k = S_{k-1} + w_k [s(x_k) - R]`, with `S` the anchor,
`s` the evaluation of the new evidence and `R` the reference point. -/
def adjust (w R S s : ℝ) : ℝ := S + w * (s - R)

/-- **Eq. (2)** (T-p.7): evaluation mode, `R = 0`. -/
def evalStep (w S s : ℝ) : ℝ := adjust w 0 S s

/-- **Eq. (3)** (T-p.7): estimation mode, `R = S_{k-1}`. -/
def estStep (w S s : ℝ) : ℝ := adjust w S S s

theorem evalStep_eq (w S s : ℝ) : evalStep w S s = S + w * s := by
  unfold evalStep adjust; ring

/-- **Eq. (4)** (T-p.7): the estimation mode is the averaging form
`S_k = (1 - w_k) S_{k-1} + w_k s(x_k)`. -/
theorem estStep_averaging (w S s : ℝ) : estStep w S s = (1 - w) * S + w * s := by
  unfold estStep adjust; ring

/-- **Eqs. (6a)/(6b)** (T-p.10): the contrast weights, `α S_{k-1}` for evidence
at or below the reference point and `β (1 - S_{k-1})` above it. -/
noncomputable def contrastWeight (α β R S s : ℝ) : ℝ :=
  if s ≤ R then α * S else β * (1 - S)

/-- HE require `0 ≤ w_k ≤ 1` (T-p.6); the contrast weights satisfy it whenever
`0 ≤ α, β ≤ 1` (T-p.11) and `0 ≤ S ≤ 1`. -/
theorem contrastWeight_mem_Icc {α β R S s : ℝ} (hα0 : 0 ≤ α) (hα1 : α ≤ 1)
    (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1) (hS0 : 0 ≤ S) (hS1 : S ≤ 1) :
    0 ≤ contrastWeight α β R S s ∧ contrastWeight α β R S s ≤ 1 := by
  unfold contrastWeight
  split_ifs
  · exact ⟨mul_nonneg hα0 hS0, by nlinarith⟩
  · exact ⟨mul_nonneg hβ0 (by linarith), by nlinarith⟩

/-- **Eqs. (7a)/(7b)** in estimation mode: the SbS step with `R = S_{k-1}` and
the contrast weights. -/
noncomputable def estContrast (α β S s : ℝ) : ℝ :=
  adjust (contrastWeight α β S S s) S S s

/-- **Eqs. (7a)/(7b)** in evaluation mode: the SbS step with `R = 0`. -/
noncomputable def evalContrast (α β S s : ℝ) : ℝ :=
  adjust (contrastWeight α β 0 S s) 0 S s

theorem estContrast_le {α β S s : ℝ} (h : s ≤ S) :
    estContrast α β S s = S + α * S * (s - S) := by
  simp [estContrast, contrastWeight, adjust, h]

theorem estContrast_gt {α β S s : ℝ} (h : S < s) :
    estContrast α β S s = S + β * (1 - S) * (s - S) := by
  simp [estContrast, contrastWeight, adjust, not_le.mpr h]

theorem evalContrast_le {α β S s : ℝ} (h : s ≤ 0) :
    evalContrast α β S s = S * (1 + α * s) := by
  simp [evalContrast, contrastWeight, adjust, h]; ring

theorem evalContrast_gt {α β S s : ℝ} (h : 0 < s) :
    evalContrast α β S s = S * (1 - β * s) + β * s := by
  simp [evalContrast, contrastWeight, adjust, not_le.mpr h]; ring

/-! ## SbS with `R = S_{k-1}`: Appendix B -/

/-- **Eq. (B.3)** (T-p.35).  With `w_a`, `w_ba` the weights of item `a` when
processed first and second, and `w_b`, `w_ab` those of item `b`, the order effect
`D = S_ab - S_ba` of the estimation-mode SbS process is
`[s_a - S₀][w_a - w_a w_ab - w_ba] + [s_b - S₀][w_ab - w_b + w_b w_ba]`.
An identity in all weights: no restriction on them is used. -/
theorem appB_orderEffect (S₀ a b wa wab wb wba : ℝ) :
    estStep wab (estStep wa S₀ a) b - estStep wba (estStep wb S₀ b) a
      = (a - S₀) * (wa - wa * wab - wba) + (b - S₀) * (wab - wb + wb * wba) := by
  unfold estStep adjust; ring

/-- **Eqs. (B.4)/(B.5)** (T-p.35): under Anderson and Hovland's assumption
`w_a = w_ba`, `w_b = w_ab`, the order effect is `D = w_a w_b [s(x_b) - s(x_a)]`,
so "because `s(x_b) > s(x_a)`, `D > 0` and recency always obtains". -/
theorem appB_recency (S₀ a b wa wb : ℝ) :
    estStep wb (estStep wa S₀ a) b - estStep wa (estStep wb S₀ b) a
      = wa * wb * (b - a) := by
  rw [appB_orderEffect]; ring

theorem appB_recency_pos {S₀ a b wa wb : ℝ} (ha : 0 < wa) (hb : 0 < wb) (hab : a < b) :
    0 < estStep wb (estStep wa S₀ a) b - estStep wa (estStep wb S₀ b) a := by
  rw [appB_recency]; exact mul_pos (mul_pos ha hb) (by linarith)

/-- **Positional weights.**  If the weight depends on the position (`w₁` for the
first item, `w₂` for the second) rather than on the item, (B.3) gives
`D = (s_b - s_a)(w₂(1 + w₁) - w₁)`. -/
theorem positional_orderEffect (S₀ a b w₁ w₂ : ℝ) :
    estStep w₂ (estStep w₁ S₀ a) b - estStep w₂ (estStep w₁ S₀ b) a
      = (b - a) * (w₂ * (1 + w₁) - w₁) := by
  rw [appB_orderEffect]; ring

/-- Hence, with positional weights, primacy holds exactly when the second weight
has fallen below `w₁ / (1 + w₁)`: a decline of the weight is necessary but not
sufficient (the two-item content of "decrements in α and β that will eventually
induce primacy", T-p.13). -/
theorem positional_primacy_iff {S₀ a b w₁ w₂ : ℝ} (hab : a < b) :
    estStep w₂ (estStep w₁ S₀ a) b - estStep w₂ (estStep w₁ S₀ b) a < 0
      ↔ w₂ * (1 + w₁) < w₁ := by
  rw [positional_orderEffect]
  have h : 0 < b - a := by linarith
  constructor
  · intro hD
    by_contra hc
    have hc' := not_lt.mp hc
    nlinarith
  · intro hc
    nlinarith

/-- **HE's contrast weights, mixed evidence.**  With `lo ≤ S₀ < hi` (one item at
or below the anchor, one above), the estimation-mode SbS process with the
weights (6a)/(6b) has the order effect
`D = αβ[uv + S₀(1-S₀)(u+v) + α S₀² u² + β (1-S₀)² v²]`, `u = S₀ - lo`,
`v = hi - S₀`.  Not in the paper: Appendix B covers only the Anderson-Hovland
weights. -/
theorem contrast_mixed_orderEffect {α β S₀ lo hi : ℝ} (hα : 0 ≤ α) (hβ : 0 ≤ β)
    (hS0 : 0 ≤ S₀) (hS1 : S₀ ≤ 1) (hlo : lo ≤ S₀) (hhi : S₀ < hi) :
    estContrast α β (estContrast α β S₀ lo) hi - estContrast α β (estContrast α β S₀ hi) lo
      = α * β * ((S₀ - lo) * (hi - S₀) + S₀ * (1 - S₀) * ((S₀ - lo) + (hi - S₀))
          + α * S₀ ^ 2 * (S₀ - lo) ^ 2 + β * (1 - S₀) ^ 2 * (hi - S₀) ^ 2) := by
  have e1 : estContrast α β S₀ lo = S₀ + α * S₀ * (lo - S₀) := estContrast_le hlo
  have e2 : estContrast α β S₀ hi = S₀ + β * (1 - S₀) * (hi - S₀) := estContrast_gt hhi
  have h1 : S₀ + α * S₀ * (lo - S₀) < hi := by
    have : α * S₀ * (lo - S₀) ≤ 0 :=
      mul_nonpos_of_nonneg_of_nonpos (mul_nonneg hα hS0) (by linarith)
    linarith
  have h2 : lo ≤ S₀ + β * (1 - S₀) * (hi - S₀) := by
    have : 0 ≤ β * (1 - S₀) * (hi - S₀) :=
      mul_nonneg (mul_nonneg hβ (by linarith)) (by linarith)
    linarith
  rw [e1, e2, estContrast_gt h1, estContrast_le h2]
  ring

/-- **Recency for mixed evidence under HE's own weights.**  For `α, β > 0` and
`lo < S₀ < hi`, the SbS estimation-mode process ends higher when the high item
comes last. -/
theorem contrast_mixed_recency {α β S₀ lo hi : ℝ} (hα : 0 < α) (hβ : 0 < β)
    (hS0 : 0 ≤ S₀) (hS1 : S₀ ≤ 1) (hlo : lo < S₀) (hhi : S₀ < hi) :
    0 < estContrast α β (estContrast α β S₀ lo) hi
        - estContrast α β (estContrast α β S₀ hi) lo := by
  rw [contrast_mixed_orderEffect hα.le hβ.le hS0 hS1 hlo.le hhi]
  have hu : 0 < S₀ - lo := by linarith
  have hv : 0 < hi - S₀ := by linarith
  have h1 : 0 < (S₀ - lo) * (hi - S₀) := mul_pos hu hv
  have h2 : 0 ≤ S₀ * (1 - S₀) * ((S₀ - lo) + (hi - S₀)) :=
    mul_nonneg (mul_nonneg hS0 (by linarith)) (by linarith)
  have h3 : 0 ≤ α * S₀ ^ 2 * (S₀ - lo) ^ 2 := by positivity
  have h4 : 0 ≤ β * (1 - S₀) ^ 2 * (hi - S₀) ^ 2 := by positivity
  exact mul_pos (mul_pos hα hβ) (by linarith)

/-- **HE's contrast weights, consistent evidence above the anchor, no crossing.**
If `S₀ < lo < hi`, `0 ≤ β ≤ 1`, and after the high item the anchor is still below
the low item (so both steps use (6b) in both orders), then
`D = β²(1-S₀)²(hi - lo)(1 - β(lo + hi - 2S₀))`.  This is negative, i.e.
**primacy**, whenever `β(lo + hi - 2S₀) > 1`. -/
theorem contrast_consistent_pos_nocross {α β S₀ lo hi : ℝ} (hβ0 : 0 ≤ β) (hβ1 : β ≤ 1)
    (hS0 : 0 ≤ S₀) (hlo : S₀ < lo) (hlh : lo < hi)
    (hnc : S₀ + β * (1 - S₀) * (hi - S₀) < lo) :
    estContrast α β (estContrast α β S₀ lo) hi - estContrast α β (estContrast α β S₀ hi) lo
      = β ^ 2 * (1 - S₀) ^ 2 * (hi - lo) * (1 - β * (lo + hi - 2 * S₀)) := by
  have e1 : estContrast α β S₀ lo = S₀ + β * (1 - S₀) * (lo - S₀) := estContrast_gt hlo
  have e2 : estContrast α β S₀ hi = S₀ + β * (1 - S₀) * (hi - S₀) :=
    estContrast_gt (by linarith)
  have h1 : S₀ + β * (1 - S₀) * (lo - S₀) < hi := by
    have hb : β * (1 - S₀) ≤ 1 := by nlinarith
    have : β * (1 - S₀) * (lo - S₀) ≤ lo - S₀ := by nlinarith
    linarith
  rw [e1, e2, estContrast_gt h1, estContrast_gt hnc]
  ring

/-- **Primacy under HE's own model, asymmetric sensitivity.**  `α = 1/10`,
`β = 1`, `S₀ = 1/10`, consistent positive evidence `1/2` and `1`: the order
ending on the stronger item gives `7516/10000`, the reverse `87269/100000`, so
`D < 0`.  Against T-p.11 "when `R = S_{k-1}`, the SbS process always predicts
recency (for α, β ≠ 0)". -/
theorem contrast_consistent_primacy_asym :
    estContrast (1/10) 1 (estContrast (1/10) 1 (1/10) (1/2)) 1 = 7516 / 10000 ∧
    estContrast (1/10) 1 (estContrast (1/10) 1 (1/10) 1) (1/2) = 87269 / 100000 ∧
    estContrast (1/10) 1 (estContrast (1/10) 1 (1/10) (1/2)) 1
      - estContrast (1/10) 1 (estContrast (1/10) 1 (1/10) 1) (1/2) < 0 := by
  have a1 : estContrast (1/10) 1 (1/10) (1/2) = 46/100 := by
    rw [estContrast_gt (by norm_num)]; norm_num
  have a2 : estContrast (1/10) 1 (46/100) 1 = 7516/10000 := by
    rw [estContrast_gt (by norm_num)]; norm_num
  have b1 : estContrast (1/10) 1 (1/10) 1 = 91/100 := by
    rw [estContrast_gt (by norm_num)]; norm_num
  have b2 : estContrast (1/10) 1 (91/100) (1/2) = 87269/100000 := by
    rw [estContrast_le (by norm_num)]; norm_num
  rw [a1, a2, b1, b2]
  norm_num

/-- **Primacy under HE's own model, equal sensitivity `α = β = 1`.**  `S₀ = 9/10`,
consistent negative evidence `0` and `1/5`: ending on the higher item gives
`1901/10000`, the reverse `1971/10000`, so `D < 0`. -/
theorem contrast_consistent_primacy_equal :
    estContrast 1 1 (estContrast 1 1 (9/10) 0) (1/5) = 1901 / 10000 ∧
    estContrast 1 1 (estContrast 1 1 (9/10) (1/5)) 0 = 1971 / 10000 ∧
    estContrast 1 1 (estContrast 1 1 (9/10) 0) (1/5)
      - estContrast 1 1 (estContrast 1 1 (9/10) (1/5)) 0 < 0 := by
  have a1 : estContrast 1 1 (9/10) 0 = 9/100 := by
    rw [estContrast_le (by norm_num)]; norm_num
  have a2 : estContrast 1 1 (9/100) (1/5) = 1901/10000 := by
    rw [estContrast_gt (by norm_num)]; norm_num
  have b1 : estContrast 1 1 (9/10) (1/5) = 27/100 := by
    rw [estContrast_le (by norm_num)]; norm_num
  have b2 : estContrast 1 1 (27/100) 0 = 1971/10000 := by
    rw [estContrast_le (by norm_num)]; norm_num
  rw [a1, a2, b1, b2]
  norm_num

/-! ## SbS with `R = 0`: Appendix C -/

/-- **Eqs. (C.1)/(C.2)** (T-p.35): consistent negative evidence, no order effect. -/
theorem appC_consistent_neg {α β S₀ x y : ℝ} (hx : x ≤ 0) (hy : y ≤ 0) :
    evalContrast α β (evalContrast α β S₀ x) y = evalContrast α β (evalContrast α β S₀ y) x := by
  rw [evalContrast_le hx, evalContrast_le hy, evalContrast_le hy, evalContrast_le hx]; ring

/-- **Eqs. (C.3)/(C.4)** (T-p.35): consistent positive evidence, no order effect;
both orders give `S₀ + β(1 - S₀)[x + y - β x y]`. -/
theorem appC_consistent_pos {α β S₀ x y : ℝ} (hx : 0 < x) (hy : 0 < y) :
    evalContrast α β (evalContrast α β S₀ x) y
        = S₀ + β * (1 - S₀) * (x + y - β * x * y) ∧
    evalContrast α β (evalContrast α β S₀ y) x
        = S₀ + β * (1 - S₀) * (x + y - β * x * y) := by
  rw [evalContrast_gt hx, evalContrast_gt hy, evalContrast_gt hy, evalContrast_gt hx]
  constructor <;> ring

/-- **Eq. (C.7)** (T-p.36): mixed evidence, `D = S(-,+) - S(+,-) = -αβ s(x⁻) s(x⁺)`,
with no dependence on the initial anchor `S₀`. -/
theorem appC_mixed_orderEffect {α β S₀ n p : ℝ} (hn : n ≤ 0) (hp : 0 < p) :
    evalContrast α β (evalContrast α β S₀ n) p - evalContrast α β (evalContrast α β S₀ p) n
      = -(α * β * n * p) := by
  rw [evalContrast_le hn, evalContrast_gt hp, evalContrast_gt hp, evalContrast_le hn]; ring

/-- (C.7) continued: for `α, β > 0` and strictly mixed evidence, `D > 0`, recency. -/
theorem appC_mixed_recency {α β S₀ n p : ℝ} (hα : 0 < α) (hβ : 0 < β) (hn : n < 0)
    (hp : 0 < p) :
    0 < evalContrast α β (evalContrast α β S₀ n) p
        - evalContrast α β (evalContrast α β S₀ p) n := by
  rw [appC_mixed_orderEffect hn.le hp]
  have : 0 < α * β * (-n) * p := mul_pos (mul_pos (mul_pos hα hβ) (by linarith)) hp
  linarith

/-- **Variant (T-p.13-14): constant weight in evaluation mode**: no order effect. -/
theorem variant_constant_noEffect (w S₀ x y : ℝ) :
    evalStep w (evalStep w S₀ x) y = evalStep w (evalStep w S₀ y) x := by
  unfold evalStep adjust; ring

/-- The reversed contrast assumption of T-p.14: `w = α (1 - S)` for negative and
`w = β S` for positive evidence (evaluation mode). -/
def reversedStep (α β S s : ℝ) (neg : Prop) [Decidable neg] : ℝ :=
  if neg then S + α * (1 - S) * s else S + β * S * s

/-- **Variant (T-p.14): reversing the contrast assumption "would predict primacy
for mixed evidence"**: `D = αβ n p < 0`. -/
theorem variant_reversed_mixed_primacy {α β S₀ n p : ℝ} (hα : 0 < α) (hβ : 0 < β)
    (hn : n < 0) (hp : 0 < p) :
    reversedStep α β (reversedStep α β S₀ n True) p False
        - reversedStep α β (reversedStep α β S₀ p False) n True = α * β * n * p ∧
    reversedStep α β (reversedStep α β S₀ n True) p False
        - reversedStep α β (reversedStep α β S₀ p False) n True < 0 := by
  have e : reversedStep α β (reversedStep α β S₀ n True) p False
        - reversedStep α β (reversedStep α β S₀ p False) n True = α * β * n * p := by
    simp [reversedStep]; ring
  refine ⟨e, ?_⟩
  rw [e]
  have : 0 < α * β * (-n) * p := mul_pos (mul_pos (mul_pos hα hβ) (by linarith)) hp
  linarith

/-! ## End-of-Sequence: Eqs. (5) and (8) -/

/-- **Eq. (5)** (T-p.9): one adjustment of the initial anchor by the aggregate. -/
def eos (S₀ w R agg : ℝ) : ℝ := S₀ + w * (agg - R)

/-- With an explicit initial anchor and an aggregate symmetric in the items (the
mean of two), Eq. (5) has no order effect: the EoS force toward primacy needs
the first item itself to serve as the anchor (T-p.12). -/
theorem eos_symmetric_noEffect (S₀ w R a b : ℝ) :
    eos S₀ w R ((a + b) / 2) = eos S₀ w R ((b + a) / 2) := by
  unfold eos; ring

/-- **Eq. (8)** (T-p.12): `S_k = s(x₁) + w_k [s(x₂,…,x_k) - R]`, with the aggregate
of the `n = k - 1` later items taken as a weighted average `∑ c j * x j` (HE:
"some function, possibly weighted average", T-p.9). -/
def eq8 (w R s₁ : ℝ) {n : ℕ} (c x : Fin n → ℝ) : ℝ :=
  s₁ + w * ((∑ j, c j * x j) - R)

/-- Eq. (8), estimation mode (`R` = the anchor `s(x₁)`): the first item carries
effective weight `1 - w` and item `j` carries `w c_j`. -/
theorem eq8_estimation (w s₁ : ℝ) {n : ℕ} (c x : Fin n → ℝ) :
    eq8 w s₁ s₁ c x = (1 - w) * s₁ + ∑ j, (w * c j) * x j := by
  simp only [eq8, mul_assoc, ← Finset.mul_sum]; ring

/-- Eq. (8), evaluation mode (`R = 0`): the first item carries weight `1`. -/
theorem eq8_evaluation (w s₁ : ℝ) {n : ℕ} (c x : Fin n → ℝ) :
    eq8 w 0 s₁ c x = s₁ + ∑ j, (w * c j) * x j := by
  simp only [eq8, mul_assoc, ← Finset.mul_sum]; ring

theorem weight_le_one {n : ℕ} {c : Fin n → ℝ} (hc : ∀ j, 0 ≤ c j) (hsum : ∑ j, c j = 1)
    (j : Fin n) : c j ≤ 1 := by
  rw [← hsum]
  exact Finset.single_le_sum (fun i _ => hc i) (Finset.mem_univ j)

/-- **"The effective weight accorded to `s(x₁)` must be greater than the weight
attached to any of the other pieces of evidence"** (T-p.12), estimation mode:
true for every averaging aggregate when `w < 1/2`. -/
theorem eq8_estimation_first_dominates {w : ℝ} {n : ℕ} {c : Fin n → ℝ} (hw0 : 0 ≤ w)
    (hw : w < 1 / 2) (hc : ∀ j, 0 ≤ c j) (hsum : ∑ j, c j = 1) (j : Fin n) :
    w * c j < 1 - w := by
  have := weight_le_one hc hsum j
  nlinarith

/-- The same, evaluation mode: true for every averaging aggregate when `w < 1`. -/
theorem eq8_evaluation_first_dominates {w : ℝ} {n : ℕ} {c : Fin n → ℝ} (hw0 : 0 ≤ w)
    (hw : w < 1) (hc : ∀ j, 0 ≤ c j) (hsum : ∑ j, c j = 1) (j : Fin n) :
    w * c j < 1 := by
  have := weight_le_one hc hsum j
  nlinarith

/-- With the plain mean over `n ≥ 1` later items (`c_j = 1/n`), the first item
dominates in estimation mode exactly when `w (n + 1) < n`, i.e. `w < n/(n+1)`. -/
theorem eq8_estimation_uniform_iff {w n : ℝ} (hn : 0 < n) :
    w * (1 / n) < 1 - w ↔ w * (n + 1) < n := by
  have e : w * (1 / n) = w / n := by ring
  rw [e, div_lt_iff₀ hn]
  constructor <;> intro h <;> nlinarith

/-- So in estimation mode the dominance of the first item is conditional: with a
single later item and `w = 3/4`, Eq. (8) gives `S₂ = (1/4) s(x₁) + (3/4) s(x₂)`,
and the first item's effective weight is below the second's. -/
theorem eq8_estimation_not_always (a b : ℝ) :
    eq8 (3/4) a a (fun _ : Fin 1 => 1) (fun _ => b) = (1/4) * a + (3/4) * b ∧
    ¬ ((3:ℝ)/4 * 1 < 1 - 3/4) := by
  refine ⟨?_, by norm_num⟩
  simp [eq8]; ring

/-- Eq. (8) with two items, estimation mode: `S = s(x₁) + w[s(x₂) - s(x₁)]`. -/
def eos2Est (w a b : ℝ) : ℝ := eq8 w a a (fun _ : Fin 1 => 1) (fun _ => b)

/-- Eq. (8) with two items, evaluation mode: `S = s(x₁) + w s(x₂)`. -/
def eos2Eval (w a b : ℝ) : ℝ := eq8 w 0 a (fun _ : Fin 1 => 1) (fun _ => b)

/-- Two-item EoS order effect, estimation mode: `D = (2w - 1)(s_b - s_a)`;
primacy for `w < 1/2`. -/
theorem eos2_est_orderEffect (w a b : ℝ) :
    eos2Est w a b - eos2Est w b a = (2 * w - 1) * (b - a) := by
  simp [eos2Est, eq8]; ring

/-- Two-item EoS order effect, evaluation mode: `D = -(1 - w)(s_b - s_a)`;
primacy for every `w < 1`. -/
theorem eos2_eval_orderEffect (w a b : ℝ) :
    eos2Eval w a b - eos2Eval w b a = -((1 - w) * (b - a)) := by
  simp [eos2Eval, eq8]; ring

/-! ## Comparison: the one-sided construction (not Hogarth-Einhorn's)

First cue adopted in full, second cue damped by `w`, on a single scalar.  This is
the single-attribute shadow of the damped family in `JeffreyOrder/Anchoring.lean`
(which is not imported here). -/

/-- The one-sided rule: `S₁ = s_a` (full adoption, weight 1), then
`S₂ = (1 - w) S₁ + w s_b`. -/
def oneSided (w S₀ a b : ℝ) : ℝ := estStep w (estStep 1 S₀ a) b

/-- HE's two-sided SbS estimation-mode rule with the same weight on both cues. -/
def twoSided (w S₀ a b : ℝ) : ℝ := estStep w (estStep w S₀ a) b

/-- The one-sided rule is HE's **Eq. (8)** with `k = 2`: EoS, the first item as
anchor, estimation mode.  It is HE's EoS anchoring structure, not their SbS
partial adjustment. -/
theorem oneSided_eq_eq8 (w S₀ a b : ℝ) : oneSided w S₀ a b = eos2Est w a b := by
  simp [oneSided, eos2Est, eq8, estStep, adjust]

/-- It is also the positional SbS model with `w₁ = 1`. -/
theorem oneSided_eq_positional (w S₀ a b : ℝ) :
    oneSided w S₀ a b = estStep w (estStep 1 S₀ a) b := rfl

/-- One-sided order effect: `D = (2w - 1)(s_b - s_a)`. -/
theorem oneSided_orderEffect (w S₀ a b : ℝ) :
    oneSided w S₀ a b - oneSided w S₀ b a = (2 * w - 1) * (b - a) := by
  unfold oneSided; rw [positional_orderEffect]; ring

/-- One-sided: primacy iff `w < 1/2`. -/
theorem oneSided_primacy_iff {w S₀ a b : ℝ} (hab : a < b) :
    oneSided w S₀ a b - oneSided w S₀ b a < 0 ↔ w < 1 / 2 := by
  rw [oneSided_orderEffect]
  have h : 0 < b - a := by linarith
  constructor
  · intro hD; by_contra hc; have hc' := not_lt.mp hc; nlinarith
  · intro hw; nlinarith

/-- Two-sided (HE, SbS, `R = S_{k-1}`, constant weight): `D = w²(s_b - s_a)`. -/
theorem twoSided_orderEffect (w S₀ a b : ℝ) :
    twoSided w S₀ a b - twoSided w S₀ b a = w ^ 2 * (b - a) := by
  unfold twoSided; rw [appB_recency]; ring

/-- Two-sided: recency for every `w > 0`. -/
theorem twoSided_recency {w S₀ a b : ℝ} (hw : 0 < w) (hab : a < b) :
    0 < twoSided w S₀ a b - twoSided w S₀ b a := by
  rw [twoSided_orderEffect]; exact mul_pos (by positivity) (by linarith)

/-- The two predictions differ by `(1 - w)²(s_b - s_a)`. -/
theorem sided_gap (w S₀ a b : ℝ) :
    (twoSided w S₀ a b - twoSided w S₀ b a) - (oneSided w S₀ a b - oneSided w S₀ b a)
      = (1 - w) ^ 2 * (b - a) := by
  rw [twoSided_orderEffect, oneSided_orderEffect]; ring

/-- **Where they disagree.**  For `0 < w < 1/2`, HE's two-sided SbS rule predicts
recency and the one-sided rule predicts primacy. -/
theorem sided_disagree {w S₀ a b : ℝ} (hw0 : 0 < w) (hw : w < 1 / 2) (hab : a < b) :
    0 < twoSided w S₀ a b - twoSided w S₀ b a ∧
    oneSided w S₀ a b - oneSided w S₀ b a < 0 :=
  ⟨twoSided_recency hw0 hab, (oneSided_primacy_iff hab).mpr hw⟩

/-- **Two attributes, each moved only by its own cue.**  Under HE's two-sided rule
the attribute's final value is `estStep w S_A s_A` whichever position its cue
occupies: no order effect. -/
theorem twoAttr_twoSided_noEffect (w SA sA : ℝ) :
    estStep w SA sA - estStep w SA sA = 0 := sub_self _

/-- Under the one-sided rule the attribute read first is set to its cue, and the
one read second is damped: the order effect on attribute `A` is
`(1 - w)(s_A - S_A)`.  This is the single-scalar form of `(1-δ)(α - q₀)` in
`orderEffect_damped_at_indep`; it comes from the full adoption of the first
cue. -/
theorem twoAttr_oneSided_orderEffect (w SA sA : ℝ) :
    estStep 1 SA sA - estStep w SA sA = (1 - w) * (sA - SA) := by
  unfold estStep adjust; ring

end Literature.HogarthEinhorn
