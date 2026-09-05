/-
# Lemma SCR and Proposition DRF, Sections 5.1.2 and 5.2.2

Everything here is a *linear* read-out of the gap, so it follows from the
entrywise derivatives of Proposition DIV by linearity:

  `lemmaSCR_gap_AB`, `lemmaSCR_gap_BA`
      for any weight `v`, `d/dc ⟨v, Pᴶ_σ - Pᴮ⟩|₀ = κ_σ ⟨v, R_σ⟩`.
      With `v` the desirability vector this is **Lemma SCR**, the first-order
      score gap, with `δ_σ = ⟨v, κ_σ R_σ⟩`; with `v` constant it vanishes, and
      with `v ∈ span{J, ∇assoc}` it vanishes too -- the protected-weight row of
      Table 1.

  `jeffreyA_pins_mA1`, `PJba_mA1`, `PJab_mB1`
      a Jeffrey step pins its own marginal *exactly in `c`*: the attribute read
      LAST carries the delivered credence identically.  This is the asymmetry
      Proposition DRF turns on.

  `propDRF_route_AB`, `propDRF_route_BA`, `propDRF`
      the `A`-marginal of the route that reads `A` FIRST is protected
      (`O(c²)`), the route that reads it LAST drifts at `Θ(c)` with coefficient
      `K = q₀(1-q₀)(r₀-β)/Z`, and the aggregate drift is `(1-λ)K`.
-/
import JeffreyOrder.PropDIV

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ lam : ℝ}

/-! ### Linear read-outs of the gap -/

private theorem inner_sub_eq (v : Mat) (X Y : Mat) :
    Mat.inner v (X.sub Y) =
      v.a00 * (X.a00 - Y.a00) + v.a01 * (X.a01 - Y.a01)
        + v.a10 * (X.a10 - Y.a10) + v.a11 * (X.a11 - Y.a11) := by
  simp only [Mat.inner, Mat.sub]

/-- **Lemma SCR** (general form).  For any weight `v`, the first-order departure
of `⟨v, ·⟩` along the `A`-first route is `κ ⟨v, R₁⟩`.  Taking `v` to be the
desirability vector gives the score gap `δ_AB` of Section 4.1. -/
theorem lemmaSCR_gap_AB (v : Mat) (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => Mat.inner v ((PJab α β c q₀ r₀).sub (PB α β c q₀ r₀)))
      (kappa α β q₀ r₀ * Mat.inner v (R1 q₀)) 0 := by
  have h00 := HasDerivAt.const_mul v.a00 (propDIV_gap_AB_a00 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h01 := HasDerivAt.const_mul v.a01 (propDIV_gap_AB_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h10 := HasDerivAt.const_mul v.a10 (propDIV_gap_AB_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h11 := HasDerivAt.const_mul v.a11 (propDIV_gap_AB_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h : HasDerivAt (fun c : ℝ =>
      v.a00 * ((PJab α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
        + v.a01 * ((PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
        + v.a10 * ((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
        + v.a11 * ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (v.a00 * (kappa α β q₀ r₀ * (R1 q₀).a00) + v.a01 * (kappa α β q₀ r₀ * (R1 q₀).a01)
        + v.a10 * (kappa α β q₀ r₀ * (R1 q₀).a10) + v.a11 * (kappa α β q₀ r₀ * (R1 q₀).a11)) 0 :=
    HasDerivAt.add (HasDerivAt.add (HasDerivAt.add h00 h01) h10) h11
  have heq : (fun c : ℝ => Mat.inner v ((PJab α β c q₀ r₀).sub (PB α β c q₀ r₀)))
      = fun c : ℝ =>
        v.a00 * ((PJab α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
          + v.a01 * ((PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
          + v.a10 * ((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
          + v.a11 * ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11) := by
    funext c; rw [inner_sub_eq]
  rw [heq]
  convert h using 1
  simp only [Mat.inner, R1]
  ring

/-- **Lemma SCR**, mirror: along the `B`-first route the coefficient is
`κ' ⟨v, R₂⟩`. -/
theorem lemmaSCR_gap_BA (v : Mat) (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => Mat.inner v ((PJba α β c q₀ r₀).sub (PB α β c q₀ r₀)))
      (kappa' α β q₀ r₀ * Mat.inner v (R2 r₀)) 0 := by
  have h00 := HasDerivAt.const_mul v.a00 (propDIV_gap_BA_a00 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h01 := HasDerivAt.const_mul v.a01 (propDIV_gap_BA_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h10 := HasDerivAt.const_mul v.a10 (propDIV_gap_BA_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h11 := HasDerivAt.const_mul v.a11 (propDIV_gap_BA_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h : HasDerivAt (fun c : ℝ =>
      v.a00 * ((PJba α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
        + v.a01 * ((PJba α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
        + v.a10 * ((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
        + v.a11 * ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (v.a00 * (kappa' α β q₀ r₀ * (R2 r₀).a00) + v.a01 * (kappa' α β q₀ r₀ * (R2 r₀).a01)
        + v.a10 * (kappa' α β q₀ r₀ * (R2 r₀).a10) + v.a11 * (kappa' α β q₀ r₀ * (R2 r₀).a11)) 0 :=
    HasDerivAt.add (HasDerivAt.add (HasDerivAt.add h00 h01) h10) h11
  have heq : (fun c : ℝ => Mat.inner v ((PJba α β c q₀ r₀).sub (PB α β c q₀ r₀)))
      = fun c : ℝ =>
        v.a00 * ((PJba α β c q₀ r₀).a00 - (PB α β c q₀ r₀).a00)
          + v.a01 * ((PJba α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
          + v.a10 * ((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
          + v.a11 * ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11) := by
    funext c; rw [inner_sub_eq]
  rw [heq]
  convert h using 1
  simp only [Mat.inner, R2]
  ring

/-- **Lemma SCR, second half**: a constant desirability vector gives no score
gap -- both posteriors are normalised, so the score telescopes. -/
theorem lemmaSCR_const_weight (k : ℝ) :
    kappa α β q₀ r₀ * Mat.inner (Mat.smul k Jmat) (R1 q₀) = 0 := by
  simp only [Mat.inner, Mat.smul, Jmat, R1]; ring

/-- **Table 1, protected-weight row**: a weight in `span{J, ∇assoc}` gives no
first-order score gap on either route. -/
theorem lemmaSCR_protected_weight (x y : ℝ) :
    kappa α β q₀ r₀ * Mat.inner ((Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀))) (R1 q₀) = 0
      ∧ kappa' α β q₀ r₀
          * Mat.inner ((Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀))) (R2 r₀) = 0 := by
  constructor <;>
    · simp only [Mat.inner, Mat.add, Mat.smul, Jmat, gradAssoc, R1, R2]; ring

/-! ### A Jeffrey step pins its own marginal, exactly in `c` -/

/-- The attribute read LAST carries the delivered credence identically -- this
is exact, not first order. -/
theorem jeffreyA_pins_mA1 (Q : Mat) (h : Q.mA1 ≠ 0) (q₀ : ℝ) :
    (jeffreyA Q q₀).mA1 = 1 - q₀ := by
  simp only [jeffreyA, Mat.mA1] at *
  field_simp
  try ring

theorem jeffreyA_pins_mA0 (Q : Mat) (h : Q.mA0 ≠ 0) (q₀ : ℝ) :
    (jeffreyA Q q₀).mA0 = q₀ := by
  simp only [jeffreyA, Mat.mA0] at *
  field_simp
  try ring

theorem jeffreyB_pins_mB1 (Q : Mat) (h : Q.mB1 ≠ 0) (r₀ : ℝ) :
    (jeffreyB Q r₀).mB1 = 1 - r₀ := by
  simp only [jeffreyB, Mat.mB1] at *
  field_simp
  try ring

/-- On the `B`-first route the `A`-cue is processed last, so `Pᴶ_BA(A=1) = q₁`
identically in `c` (Proposition DRF, first display of the proof). -/
theorem PJba_mA1 (h : (jeffreyB (prior α β c) r₀).mA1 ≠ 0) :
    (PJba α β c q₀ r₀).mA1 = 1 - q₀ :=
  jeffreyA_pins_mA1 _ h q₀

/-- Symmetrically, on the `A`-first route `Pᴶ_AB(B=1) = r₁` identically. -/
theorem PJab_mB1 (h : (jeffreyA (prior α β c) q₀).mB1 ≠ 0) :
    (PJab α β c q₀ r₀).mB1 = 1 - r₀ :=
  jeffreyB_pins_mB1 _ h r₀

/-! ### Proposition DRF: marginal-probability drift -/

/-- `K = q₀(1-q₀)(r₀-β)/Z`, the coefficient of the aggregate `A`-marginal
drift.  It is exactly `-κ'`. -/
noncomputable def Kdrift (α β q₀ r₀ : ℝ) : ℝ := q₀ * (1 - q₀) * (r₀ - β) / Zpar α β

theorem Kdrift_eq_neg_kappa' (hZ : Zpar α β ≠ 0) :
    Kdrift α β q₀ r₀ = -kappa' α β q₀ r₀ := by
  unfold Kdrift kappa'
  field_simp
  ring

/-- **Proposition DRF, route `AB`**: the marginal of the attribute read FIRST is
protected -- its first-order coefficient is exactly zero. -/
theorem propDRF_route_AB (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        ((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
          + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)) 0 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      ((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
        + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (kappa α β q₀ r₀ * (R1 q₀).a10 + kappa α β q₀ r₀ * (R1 q₀).a11) 0 :=
    HasDerivAt.add (propDIV_gap_AB_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
      (propDIV_gap_AB_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  convert h using 1
  simp only [R1]
  ring

/-- **Proposition DRF, route `BA`**: the marginal of the attribute read LAST
drifts at first order, with coefficient `K = -κ'`. -/
theorem propDRF_route_BA (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hZ : Zpar α β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        ((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
          + ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (Kdrift α β q₀ r₀) 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      ((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
        + ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (kappa' α β q₀ r₀ * (R2 r₀).a10 + kappa' α β q₀ r₀ * (R2 r₀).a11) 0 :=
    HasDerivAt.add (propDIV_gap_BA_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
      (propDIV_gap_BA_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  convert h using 1
  rw [Kdrift_eq_neg_kappa' hZ]
  simp only [R2]
  ring

/-- **Proposition DRF**: the aggregate `A`-marginal drift is
`c(1-λ) q₀(1-q₀)(r₀-β)/Z + O(c²)`.  For `λ < 1` its sign is `sgn(r₀ - β)`; at
`λ = 1` it vanishes at first order, and then the `B`-marginal drifts instead
(`propDRF_B_at_one`). -/
theorem propDRF (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) (hZ : Zpar α β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        lam * (((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
                + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
        + (1 - lam) * (((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
                + ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)))
      ((1 - lam) * Kdrift α β q₀ r₀) 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      lam * (((PJab α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
              + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      + (1 - lam) * (((PJba α β c q₀ r₀).a10 - (PB α β c q₀ r₀).a10)
              + ((PJba α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11)))
      (lam * 0 + (1 - lam) * Kdrift α β q₀ r₀) 0 :=
    HasDerivAt.add
      (HasDerivAt.const_mul lam (propDRF_route_AB (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ'))
      (HasDerivAt.const_mul (1 - lam)
        (propDRF_route_BA (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ' hZ))
  convert h using 1
  ring

/-- At `λ = 1` the `A`-marginal drift vanishes at first order. -/
theorem propDRF_at_one (hZ : Zpar α β ≠ 0) :
    (1 - (1:ℝ)) * Kdrift α β q₀ r₀ = 0 := by ring

/-- ... and the `B`-marginal then drifts with coefficient `-κ`, of sign
`sgn(q₀ - α)`. -/
theorem propDRF_B_at_one (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        ((PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
          + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (-kappa α β q₀ r₀) 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      ((PJab α β c q₀ r₀).a01 - (PB α β c q₀ r₀).a01)
        + ((PJab α β c q₀ r₀).a11 - (PB α β c q₀ r₀).a11))
      (kappa α β q₀ r₀ * (R1 q₀).a01 + kappa α β q₀ r₀ * (R1 q₀).a11) 0 :=
    HasDerivAt.add (propDIV_gap_AB_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
      (propDIV_gap_AB_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  convert h using 1
  simp only [R1]
  ring

/-- The sign of the aggregate `A`-marginal drift is that of `r₀ - β`: the
impression on `B` must genuinely surprise, and the direction of the drift
follows the direction of that surprise. -/
theorem Kdrift_pos (hq : 0 < q₀) (hq' : q₀ < 1) (hZ : 0 < Zpar α β) (h : β < r₀) :
    0 < Kdrift α β q₀ r₀ := by
  have h1 : (0:ℝ) < 1 - q₀ := by linarith
  have h2 : (0:ℝ) < r₀ - β := by linarith
  simp only [Kdrift]
  positivity

theorem Kdrift_neg (hq : 0 < q₀) (hq' : q₀ < 1) (hZ : 0 < Zpar α β) (h : r₀ < β) :
    Kdrift α β q₀ r₀ < 0 := by
  have h1 : (0:ℝ) < 1 - q₀ := by linarith
  have h2 : (0:ℝ) < β - r₀ := by linarith
  have : 0 < q₀ * (1 - q₀) * (β - r₀) / Zpar α β := by positivity
  simp only [Kdrift]
  have heq : q₀ * (1 - q₀) * (r₀ - β) / Zpar α β
      = -(q₀ * (1 - q₀) * (β - r₀) / Zpar α β) := by
    field_simp
    ring
  rw [heq]
  linarith

theorem Kdrift_eq_zero (hβr : r₀ = β) : Kdrift α β q₀ r₀ = 0 := by
  simp only [Kdrift, hβr, sub_self, mul_zero, zero_div]

end JeffreyOrder
