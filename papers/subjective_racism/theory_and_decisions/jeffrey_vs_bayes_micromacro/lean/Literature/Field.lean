/-
# Field (1978), "A Note on Jeffrey Conditionalization"

*Philosophy of Science* 45(3), 361-367.

Field's point is that Jeffrey's parameter `q = P'(E)` cannot be an *input*
parameter, since it depends on the prior `p = P(E)` as well as on the
stimulation (p. 363). He reparametrizes Jeffrey's rule (3) by

  * eq. (4), p. 364: `α = (1/2) log ((q/p) / ((1-q)/(1-p)))`,
  * eq. (5), p. 364: `q = p e^α / (p e^α + (1-p) e^{-α})`,
  * eq. (6), p. 364: `P'(A) = (e^α P(A ∧ E) + e^{-α} P(A ∧ ¬E)) / (e^α P(E) + e^{-α} P(¬E))`,

and shows that two successive changes, on `E` with `α` and then on `E'`
with `α'`, give the "simple and symmetric" law (7), p. 366, with weights
`e^{±α±α'}` on the four cells `E ∧ E'`, `E ∧ ¬E'`, `¬E ∧ E'`, `¬E ∧ ¬E'`.
In the credence parameters `q, q'` the same composite is "a very complicated
law; moreover, it is an asymmetric law" (p. 365). The `k`-cell version is
(3')-(6'), pp. 366-367.

## What is proved

* `qOf_alphaOf`, `alphaOf_qOf` — (4) and (5) are mutually inverse, for
  `0 < p, q < 1` and every real `α`.
* `exp_two_alphaOf` — `e^{2α}` is the odds ratio `(q/p)/((1-q)/(1-p))`.
  `exp_two_alphaOf_bayes` — if `q` is the posterior of `p` under a Bayes
  update with likelihoods `ℓ₁` on `E` and `ℓ₀` on `¬E`, then `e^{2α} = ℓ₁/ℓ₀`.
  So **`e^{2α}` is the likelihood ratio (Bayes factor) itself**, and `e^{α}` is
  its square root (`exp_alphaOf_sq`); `α` is *half* the log-odds shift. (This
  corrects the earlier README, which called `e^{2α}` a squared likelihood
  ratio.)
* `tilt_eq_jeffrey` — (6) is Jeffrey's rule (3) with `q` given by (5).
  `tilt_zero` — `α = 0` leaves `P` unchanged (p. 365).
* `reweight_reweight`, and hence `field_eq7`, `field_eq7_comm` — **eq. (7)**:
  the two tilts, in either order, equal the closed form with weights
  `e^{±α±α'}`, on any finite space and any two binary partitions.
* `reweight_cells_eq_jeffrey` — (5')/(6'): tilting a `k`-cell partition by
  `e^{αᵢ}` is Jeffrey's rule (3') with `qᵢ = pᵢ e^{αᵢ} / Σⱼ pⱼ e^{αⱼ}`.
* `jeffrey_not_comm` — the credence-input updates do **not** commute: an
  explicit rational `2 × 2` prior and targets `q = 7/10` on `E`, `q' = 3/10`
  on `E'` give `147/760` and `63/370` in cell `E ∧ E'`, a gap of `651/28120`.

## What is not formalized

Field's claim that `α` is an input parameter ("some evidence", p. 365) is
philosophical. The limits `α → ±∞` (strict conditioning as a limiting case,
p. 365) and the `(4')` normalization `Σ αᵢ = 0` are not formalized; the
latter is a choice of scale (`reweight` is unchanged if every `αᵢ` is shifted
by the same constant).
-/
import Mathlib

namespace Literature.Field

open Real Finset

/-! ## Eqs. (4) and (5): the reparametrization -/

/-- Field's eq. (4): `α = (1/2) log ((q/p)/((1-q)/(1-p)))`, `p = P(E)`, `q = P'(E)`. -/
noncomputable def alphaOf (p q : ℝ) : ℝ := (1 / 2) * Real.log ((q / p) / ((1 - q) / (1 - p)))

/-- Field's eq. (5) (Garber's eq. (3)): `q = p e^α / (p e^α + (1-p) e^{-α})`. -/
noncomputable def qOf (α p : ℝ) : ℝ := p * exp α / (p * exp α + (1 - p) * exp (-α))

/-- The odds ratio `(q/p)/((1-q)/(1-p))` of the new to the old probability of `E`. -/
noncomputable def oddsRatio (p q : ℝ) : ℝ := (q / p) / ((1 - q) / (1 - p))

theorem oddsRatio_pos {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    0 < oddsRatio p q := by
  unfold oddsRatio
  have : 0 < 1 - p := by linarith
  have : 0 < 1 - q := by linarith
  positivity

/-- **`e^{2α}` is the odds ratio**, by eq. (4). -/
theorem exp_two_alphaOf {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    exp (2 * alphaOf p q) = oddsRatio p q := by
  have h := oddsRatio_pos hp0 hp1 hq0 hq1
  unfold alphaOf
  rw [show 2 * (1 / 2 * Real.log ((q / p) / ((1 - q) / (1 - p)))) =
      Real.log ((q / p) / ((1 - q) / (1 - p))) by ring]
  exact exp_log h

/-- `e^{α}` is the square root of the odds ratio: `(e^α)² = (q/p)/((1-q)/(1-p))`. -/
theorem exp_alphaOf_sq {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    exp (alphaOf p q) ^ 2 = oddsRatio p q := by
  rw [← exp_two_alphaOf hp0 hp1 hq0 hq1, sq, ← exp_add]; ring_nf

/-- The posterior probability of `E` after a Bayes update of the prior `p` with
likelihood `ℓ₁` of the evidence under `E` and `ℓ₀` under `¬E`. -/
noncomputable def bayesPost (p ℓ₁ ℓ₀ : ℝ) : ℝ := p * ℓ₁ / (p * ℓ₁ + (1 - p) * ℓ₀)

/-- **`e^{2α}` is the likelihood ratio.** If `q` is what a Bayes update with
likelihoods `ℓ₁, ℓ₀` makes of the prior `p`, Field's `α` of the pair `(p, q)`
satisfies `e^{2α} = ℓ₁/ℓ₀`, whatever `p` is. (Audit item L2.) -/
theorem exp_two_alphaOf_bayes {p ℓ₁ ℓ₀ : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (h1 : 0 < ℓ₁)
    (h0 : 0 < ℓ₀) : exp (2 * alphaOf p (bayesPost p ℓ₁ ℓ₀)) = ℓ₁ / ℓ₀ := by
  have hq : 0 < 1 - p := by linarith
  have hD : 0 < p * ℓ₁ + (1 - p) * ℓ₀ := by positivity
  have hq0 : 0 < bayesPost p ℓ₁ ℓ₀ := by unfold bayesPost; positivity
  have hq1 : bayesPost p ℓ₁ ℓ₀ < 1 := by
    unfold bayesPost; rw [div_lt_one hD]; nlinarith
  rw [exp_two_alphaOf hp0 hp1 hq0 hq1]
  unfold oddsRatio bayesPost
  have h1q : 1 - p * ℓ₁ / (p * ℓ₁ + (1 - p) * ℓ₀) = (1 - p) * ℓ₀ / (p * ℓ₁ + (1 - p) * ℓ₀) := by
    field_simp; ring
  rw [h1q]
  field_simp

/-- Field's (5) is the Bayes update with likelihoods `e^{α}` and `e^{-α}`,
whose ratio is `e^{2α}`. -/
theorem qOf_eq_bayesPost (α p : ℝ) : qOf α p = bayesPost p (exp α) (exp (-α)) := rfl

theorem qOf_pos {α p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) : 0 < qOf α p := by
  have : 0 < 1 - p := by linarith
  unfold qOf; positivity

theorem qOf_lt_one {α p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) : qOf α p < 1 := by
  have : 0 < 1 - p := by linarith
  unfold qOf
  rw [div_lt_one (by positivity)]
  have : 0 < (1 - p) * exp (-α) := by positivity
  linarith

/-- (5) after (4) is the identity: `q(α(p,q), p) = q`. -/
theorem qOf_alphaOf {p q : ℝ} (hp0 : 0 < p) (hp1 : p < 1) (hq0 : 0 < q) (hq1 : q < 1) :
    qOf (alphaOf p q) p = q := by
  have hsq := exp_alphaOf_sq hp0 hp1 hq0 hq1
  set E := exp (alphaOf p q) with hE
  have hEpos : 0 < E := exp_pos _
  have hneg : exp (-alphaOf p q) = E⁻¹ := by rw [exp_neg]
  have h1p : 0 < 1 - p := by linarith
  have h1q : 0 < 1 - q := by linarith
  unfold qOf
  rw [hneg, ← hE]
  unfold oddsRatio at hsq
  field_simp at hsq ⊢
  linear_combination hsq

/-- (4) after (5) is the identity: `α(p, q(α,p)) = α`. -/
theorem alphaOf_qOf (α : ℝ) {p : ℝ} (hp0 : 0 < p) (hp1 : p < 1) :
    alphaOf p (qOf α p) = α := by
  have h1p : 0 < 1 - p := by linarith
  have hr : oddsRatio p (qOf α p) = exp (2 * α) := by
    have := exp_two_alphaOf_bayes (ℓ₁ := exp α) (ℓ₀ := exp (-α)) hp0 hp1 (exp_pos _) (exp_pos _)
    rw [← qOf_eq_bayesPost] at this
    rw [← exp_two_alphaOf hp0 hp1 (qOf_pos hp0 hp1) (qOf_lt_one hp0 hp1), this, ← exp_sub]
    ring_nf
  unfold alphaOf
  rw [show (qOf α p / p) / ((1 - qOf α p) / (1 - p)) = oddsRatio p (qOf α p) from rfl, hr, log_exp]
  ring

/-! ## Eqs. (3) and (6) on a finite space -/

variable {Ω : Type*} [Fintype Ω]

/-- Reweight `P` by `w` and renormalize. -/
noncomputable def reweight (w P : Ω → ℝ) : Ω → ℝ := fun ω => w ω * P ω / ∑ ω', w ω' * P ω'

/-- `P(E)` for a binary partition given by `E : Ω → Bool`. -/
noncomputable def probE (E : Ω → Bool) (P : Ω → ℝ) : ℝ := ∑ ω, if E ω then P ω else 0

/-- The weights `e^{α}` on `E` and `e^{-α}` on `¬E`. -/
noncomputable def tiltW (E : Ω → Bool) (α : ℝ) : Ω → ℝ := fun ω => if E ω then exp α else exp (-α)

/-- Field's eq. (6): the update with input parameter `α` on `E`. -/
noncomputable def tilt (E : Ω → Bool) (α : ℝ) (P : Ω → ℝ) : Ω → ℝ := reweight (tiltW E α) P

/-- Jeffrey's eq. (3), pointwise: `P'(ω) = q P(ω)/P(E)` on `E`, `(1-q) P(ω)/P(¬E)` off it. -/
noncomputable def jeffrey (E : Ω → Bool) (q : ℝ) (P : Ω → ℝ) : Ω → ℝ := fun ω =>
  if E ω then q * P ω / probE E P else (1 - q) * P ω / probE (fun ω => !E ω) P

/-- Reweighting twice is reweighting once by the product of the weights. -/
theorem reweight_reweight (w₁ w₂ P : Ω → ℝ) (hZ : ∑ ω, w₁ ω * P ω ≠ 0) :
    reweight w₂ (reweight w₁ P) = reweight (fun ω => w₁ ω * w₂ ω) P := by
  funext ω
  simp only [reweight]
  rw [show (∑ ω', w₂ ω' * (w₁ ω' * P ω' / ∑ ω, w₁ ω * P ω)) =
      (∑ ω', w₁ ω' * w₂ ω' * P ω') / ∑ ω, w₁ ω * P ω by
    rw [Finset.sum_div]; exact Finset.sum_congr rfl fun _ _ => by ring]
  by_cases h2 : ∑ ω', w₁ ω' * w₂ ω' * P ω' = 0
  · simp [h2]
  · field_simp

theorem sum_tiltW_pos (E : Ω → Bool) (α : ℝ) {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω)
    (hP1 : ∑ ω, P ω = 1) : 0 < ∑ ω, tiltW E α ω * P ω := by
  have hmin : ∀ ω, min (exp α) (exp (-α)) * P ω ≤ tiltW E α ω * P ω := fun ω => by
    apply mul_le_mul_of_nonneg_right _ (hP ω)
    unfold tiltW; split_ifs <;> simp
  calc (0 : ℝ) < min (exp α) (exp (-α)) * ∑ ω, P ω := by
        rw [hP1, mul_one]; exact lt_min (exp_pos _) (exp_pos _)
    _ = ∑ ω, min (exp α) (exp (-α)) * P ω := by rw [Finset.mul_sum]
    _ ≤ _ := Finset.sum_le_sum fun ω _ => hmin ω

/-- `α = 0` is the uninformative stimulation: `P' = P` (p. 365). -/
theorem tilt_zero (E : Ω → Bool) {P : Ω → ℝ} (hP1 : ∑ ω, P ω = 1) : tilt E 0 P = P := by
  funext ω
  simp [tilt, reweight, tiltW, hP1]

/-- **Eq. (6) is Jeffrey's rule (3) with `q` from (5).** For a prior with
`0 < P(E) < 1`, the `α`-update equals Jeffrey's update to `q = q(α, P(E))`. -/
theorem tilt_eq_jeffrey (E : Ω → Bool) (α : ℝ) {P : Ω → ℝ} (hP1 : ∑ ω, P ω = 1)
    (hE0 : 0 < probE E P) (hE1 : probE E P < 1) :
    tilt E α P = jeffrey E (qOf α (probE E P)) P := by
  have hnE : probE (fun ω => !E ω) P = 1 - probE E P := by
    rw [← hP1]; unfold probE
    rw [eq_sub_iff_add_eq, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl fun ω _ => by cases h : E ω <;> simp [h]
  have hZ : ∑ ω, tiltW E α ω * P ω = exp α * probE E P + exp (-α) * (1 - probE E P) := by
    rw [← hnE]; unfold probE tiltW
    rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl fun ω _ => by cases h : E ω <;> simp [h]
  funext ω
  simp only [tilt, reweight, jeffrey, hZ, qOf, hnE]
  unfold tiltW
  generalize probE E P = p at *
  have h1p : 0 < 1 - p := by linarith
  have hD : 0 < exp α * p + exp (-α) * (1 - p) := by positivity
  cases E ω
  · simp only [Bool.false_eq_true, ↓reduceIte]
    have h1q : 1 - p * exp α / (p * exp α + (1 - p) * exp (-α)) =
        (1 - p) * exp (-α) / (p * exp α + (1 - p) * exp (-α)) := by
      rw [eq_div_iff (by nlinarith [exp_pos α, exp_pos (-α)])]; field_simp; ring
    rw [h1q]; field_simp; try ring
  · simp only [↓reduceIte]
    field_simp; try ring

/-! ## Eq. (7): successive `α`-updates commute and have a closed form -/

/-- The closed form of eq. (7): weights `e^{±α ± α'}`, the signs set by whether the
cell lies in `E` and in `E'`. -/
noncomputable def eq7 (E E' : Ω → Bool) (α α' : ℝ) (P : Ω → ℝ) : Ω → ℝ :=
  reweight (fun ω => exp ((if E ω then α else -α) + (if E' ω then α' else -α'))) P

/-- **Eq. (7).** The update on `E` with `α` followed by the update on `E'` with `α'`
is the closed form (7). -/
theorem field_eq7 (E E' : Ω → Bool) (α α' : ℝ) {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω)
    (hP1 : ∑ ω, P ω = 1) : tilt E' α' (tilt E α P) = eq7 E E' α α' P := by
  unfold tilt eq7
  rw [reweight_reweight _ _ _ (sum_tiltW_pos E α hP hP1).ne']
  congr 1
  funext ω
  unfold tiltW
  split_ifs <;> rw [exp_add]

/-- **Eq. (7) is symmetric:** the two `α`-updates commute. -/
theorem field_eq7_comm (E E' : Ω → Bool) (α α' : ℝ) {P : Ω → ℝ} (hP : ∀ ω, 0 ≤ P ω)
    (hP1 : ∑ ω, P ω = 1) : tilt E' α' (tilt E α P) = tilt E α (tilt E' α' P) := by
  rw [field_eq7 E E' α α' hP hP1, field_eq7 E' E α' α hP hP1]
  unfold eq7
  congr 1
  funext ω
  rw [add_comm]

/-! ## Eqs. (3')-(6'): a `k`-cell partition -/

/-- `P(Fᵢ)` for a partition given by `cell : Ω → ι`. -/
noncomputable def probCell {ι : Type*} [DecidableEq ι] (cell : Ω → ι) (P : Ω → ℝ) (i : ι) : ℝ :=
  ∑ ω ∈ univ.filter (fun ω => cell ω = i), P ω

/-- **(5')/(6').** Reweighting the cells of a finite partition by `e^{αᵢ}` is Jeffrey's
rule (3') with `qᵢ = pᵢ e^{αᵢ} / Σⱼ pⱼ e^{αⱼ}` (here `pᵢ = P(Fᵢ)`). -/
theorem reweight_cells_eq_jeffrey {ι : Type*} [Fintype ι] [DecidableEq ι] (cell : Ω → ι)
    (a : ι → ℝ) {P : Ω → ℝ} (hP : ∀ ω, 0 < P ω) (ω : Ω) :
    reweight (fun ω => exp (a (cell ω))) P ω =
      (probCell cell P (cell ω) * exp (a (cell ω)) /
        ∑ j, probCell cell P j * exp (a j)) * P ω / probCell cell P (cell ω) := by
  have hZ : ∑ ω', exp (a (cell ω')) * P ω' = ∑ j, probCell cell P j * exp (a j) := by
    rw [← Finset.sum_fiberwise univ cell (fun ω' => exp (a (cell ω')) * P ω')]
    refine Finset.sum_congr rfl fun j _ => ?_
    unfold probCell
    rw [Finset.sum_mul]
    refine Finset.sum_congr rfl fun x hx => ?_
    rw [(Finset.mem_filter.1 hx).2, mul_comm]
  have hpos : 0 < probCell cell P (cell ω) :=
    Finset.sum_pos (fun x _ => hP x) ⟨ω, by simp⟩
  simp only [reweight, hZ]
  field_simp

/-! ## The credence-input (Jeffrey) updates do not commute (p. 365) -/

/-- A `2 × 2` prior on `Bool × Bool` (first coordinate `E`, second `E'`):
`P(E∧E') = 1/10`, `P(E∧¬E') = 2/10`, `P(¬E∧E') = 3/10`, `P(¬E∧¬E') = 4/10`. -/
noncomputable def P₀ : Bool × Bool → ℝ
  | (true, true) => 1 / 10
  | (true, false) => 2 / 10
  | (false, true) => 3 / 10
  | (false, false) => 4 / 10

/-- Jeffrey to `q = 7/10` on `E`, then to `q' = 3/10` on `E'`: cell `E ∧ E'`. -/
theorem jeffrey_EE' :
    jeffrey Prod.snd (3 / 10) (jeffrey Prod.fst (7 / 10) P₀) (true, true) = 147 / 760 := by
  simp [jeffrey, probE, P₀, Fintype.sum_prod_type]
  norm_num

/-- The other order: `q' = 3/10` on `E'` first, then `q = 7/10` on `E`. -/
theorem jeffrey_E'E :
    jeffrey Prod.fst (7 / 10) (jeffrey Prod.snd (3 / 10) P₀) (true, true) = 63 / 370 := by
  simp [jeffrey, probE, P₀, Fintype.sum_prod_type]
  norm_num

/-- **Field, p. 365: in the credence parameters the composite is asymmetric.** The
two orders of the Jeffrey updates differ, by `651/28120` in cell `E ∧ E'`. -/
theorem jeffrey_not_comm :
    jeffrey Prod.snd (3 / 10) (jeffrey Prod.fst (7 / 10) P₀) (true, true) -
      jeffrey Prod.fst (7 / 10) (jeffrey Prod.snd (3 / 10) P₀) (true, true) = 651 / 28120 ∧
    jeffrey Prod.snd (3 / 10) (jeffrey Prod.fst (7 / 10) P₀) ≠
      jeffrey Prod.fst (7 / 10) (jeffrey Prod.snd (3 / 10) P₀) := by
  refine ⟨by rw [jeffrey_EE', jeffrey_E'E]; norm_num, fun h => ?_⟩
  have := congrFun h (true, true)
  rw [jeffrey_EE', jeffrey_E'E] at this
  norm_num at this

/-- The same two stimulations, taken as `α`-inputs computed against the prior,
commute (instance of `field_eq7_comm` on the same prior). -/
theorem tilt_comm_P₀ (α α' : ℝ) :
    tilt Prod.snd α' (tilt Prod.fst α P₀) = tilt Prod.fst α (tilt Prod.snd α' P₀) := by
  refine field_eq7_comm _ _ _ _ (fun ω => ?_) ?_
  · rcases ω with ⟨_ | _, _ | _⟩ <;> simp [P₀] <;> norm_num
  · simp [P₀, Fintype.sum_prod_type]; norm_num

end Literature.Field
