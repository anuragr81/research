/-
# Lemma ASC and Proposition PRO, Sections 4.2 and 5.4

Proposition PRO is the paper's central characterisation:

  (i)  *Protection.*  If `dF` at `q ⊗ r`, restricted to the simplex tangent
       space, is a scalar multiple of `d assoc`, then
       `F(P̄_λ) - F(Pᴮ) = O(c²)` for every prior and every `λ`.
  (ii) *Uniqueness.*  Fix `λ ∈ (0,1)`.  If `F(P̄_λ) - F(Pᴮ) = o(c)` for all
       `(α, β)` in some open set, then `dF` at `q ⊗ r`, restricted to the
       simplex, is a multiple of `d assoc`.

Both halves reduce to finite-dimensional algebra once the chain rule has been
applied, and that algebra is what is formalised here:

  * the two route directions `R₁ = q ⊗ (1,-1)` and `R₂ = (1,-1) ⊗ r` are
    linearly independent (Appendix A.4, Step 2);
  * their annihilator is exactly two-dimensional and is spanned by the all-ones
    matrix `J` and `∇assoc` -- `inner_gradAssoc_R₁ = 0` is Lemma ASC;
  * hence part (i): any `∇F ∈ span{J, ∇assoc}` annihilates the whole route
    plane `M_λ`, identically in `(α, β, λ)`;
  * and part (ii): the leading coefficient, cleared of the positive factor `Z`,
    is affine in `α` with a nonzero slope, so vanishing at two distinct priors
    forces `⟨∇F, R₁⟩ = ⟨∇F, R₂⟩ = 0`, i.e. `∇F ∈ span{J, ∇assoc}`.

The interior-`λ` hypothesis is essential, and the file records why: the
annihilator of a *single* route direction is three-dimensional.
-/
import JeffreyOrder.PropDEC

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ lam : ℝ}

/-- The route direction of the `A`-first sequence, `R₁ = q ⊗ (1,-1)`.  It holds
the `A`-marginal fixed and moves only the `B`-conditional. -/
def R1 (q₀ : ℝ) : Mat := ⟨q₀, -q₀, 1 - q₀, -(1 - q₀)⟩

/-- The route direction of the `B`-first sequence, `R₂ = (1,-1) ⊗ r`. -/
def R2 (r₀ : ℝ) : Mat := ⟨r₀, 1 - r₀, -r₀, -(1 - r₀)⟩

/-- The all-ones matrix.  On the tangent space of the simplex -- whose vectors
have coordinates summing to zero -- it acts as zero. -/
def Jmat : Mat := ⟨1, 1, 1, 1⟩

/-- `∇assoc` evaluated at the independent posterior `q ⊗ r`. -/
def gradAssoc (q₀ r₀ : ℝ) : Mat :=
  ⟨(1 - q₀) * (1 - r₀), -((1 - q₀) * r₀), -(q₀ * (1 - r₀)), q₀ * r₀⟩

/-- `Z := αβ(1-α)(1-β)`. -/
def Zpar (α β : ℝ) : ℝ := α * β * (1 - α) * (1 - β)

/-- The leading coefficient of the `A`-first gap (Proposition DIV). -/
noncomputable def kappa (α β q₀ r₀ : ℝ) : ℝ := (α - q₀) * r₀ * (1 - r₀) / Zpar α β

/-- The leading coefficient of the `B`-first gap. -/
noncomputable def kappa' (α β q₀ r₀ : ℝ) : ℝ := (β - r₀) * q₀ * (1 - q₀) / Zpar α β

/-- The route plane `M_λ = λ κ R₁ + (1-λ) κ' R₂`: the direction in which the
aggregate departs from the benchmark at first order (Appendix A.4, Step 1). -/
noncomputable def Mlam (lam α β q₀ r₀ : ℝ) : Mat :=
  (Mat.smul (lam * kappa α β q₀ r₀) (R1 q₀)).add
    (Mat.smul ((1 - lam) * kappa' α β q₀ r₀) (R2 r₀))

/-! ### Step 2: the annihilator of the route plane -/

/-- `∇assoc` is indeed the gradient of `assoc` at `q ⊗ r`: it reproduces the
first-order change of `assoc` along any direction. -/
theorem gradAssoc_is_gradient (q₀ r₀ t : ℝ) (D : Mat) :
    ((indep q₀ r₀).add (Mat.smul t D)).assoc
      = (indep q₀ r₀).assoc + t * Mat.inner (gradAssoc q₀ r₀) D + t ^ 2 * D.assoc := by
  simp only [indep, gradAssoc, Mat.add, Mat.smul, Mat.assoc, Mat.inner]
  ring

@[simp] theorem inner_Jmat_R1 (q₀ : ℝ) : Mat.inner Jmat (R1 q₀) = 0 := by
  simp only [Jmat, R1, Mat.inner]; ring

@[simp] theorem inner_Jmat_R2 (r₀ : ℝ) : Mat.inner Jmat (R2 r₀) = 0 := by
  simp only [Jmat, R2, Mat.inner]; ring

/-- **Lemma ASC** (the computational heart).  `⟨∇assoc(q⊗r), R₁⟩ = 0`: the
`A`-first route direction moves the belief without moving the association at
first order. -/
@[simp] theorem inner_gradAssoc_R1 (q₀ r₀ : ℝ) :
    Mat.inner (gradAssoc q₀ r₀) (R1 q₀) = 0 := by
  simp only [gradAssoc, R1, Mat.inner]; ring

/-- **Lemma ASC**, mirror half: `⟨∇assoc(q⊗r), R₂⟩ = 0`. -/
@[simp] theorem inner_gradAssoc_R2 (q₀ r₀ : ℝ) :
    Mat.inner (gradAssoc q₀ r₀) (R2 r₀) = 0 := by
  simp only [gradAssoc, R2, Mat.inner]; ring

/-- **Lemma ASC in the form the manuscript states it**: along either route the
believed association departs from the benchmark only at second order, because
the first-order term is an inner product that vanishes. -/
theorem lemmaASC_first_order_vanishes (q₀ r₀ t κ : ℝ) :
    ((indep q₀ r₀).add (Mat.smul t (Mat.smul κ (R1 q₀)))).assoc
      = (indep q₀ r₀).assoc + t ^ 2 * (Mat.smul κ (R1 q₀)).assoc := by
  rw [gradAssoc_is_gradient]
  have : Mat.inner (gradAssoc q₀ r₀) (Mat.smul κ (R1 q₀)) = 0 := by
    simp only [gradAssoc, R1, Mat.smul, Mat.inner]; ring
  rw [this]; ring

theorem lemmaASC_first_order_vanishes_BA (q₀ r₀ t κ : ℝ) :
    ((indep q₀ r₀).add (Mat.smul t (Mat.smul κ (R2 r₀)))).assoc
      = (indep q₀ r₀).assoc + t ^ 2 * (Mat.smul κ (R2 r₀)).assoc := by
  rw [gradAssoc_is_gradient]
  have : Mat.inner (gradAssoc q₀ r₀) (Mat.smul κ (R2 r₀)) = 0 := by
    simp only [gradAssoc, R2, Mat.smul, Mat.inner]; ring
  rw [this]; ring

/-- **Step 2: `R₁` and `R₂` are linearly independent.**  The appendix's own
argument: adding the top-row entries of `aR₁ + bR₂` gives `b(r₀ + r₁) = b`. -/
theorem R1_R2_indep (hq : q₀ ≠ 0) {a b : ℝ}
    (h : (Mat.smul a (R1 q₀)).add (Mat.smul b (R2 r₀)) = ⟨0, 0, 0, 0⟩) :
    a = 0 ∧ b = 0 := by
  have h00 : a * q₀ + b * r₀ = 0 := congrArg Mat.a00 h
  have h01 : a * -q₀ + b * (1 - r₀) = 0 := congrArg Mat.a01 h
  have hb : b = 0 := by linarith
  refine ⟨?_, hb⟩
  have : a * q₀ = 0 := by rw [hb] at h00; linarith
  rcases mul_eq_zero.mp this with h' | h'
  · exact h'
  · exact absurd h' hq

/-- **Step 2: `J` and `∇assoc` are linearly independent**, so together they
span the (two-dimensional) annihilator of the route plane. -/
theorem Jmat_gradAssoc_indep (hq : 0 < q₀) (hq' : q₀ < 1) (hr : 0 < r₀) (hr' : r₀ < 1)
    {x y : ℝ} (h : (Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀)) = ⟨0, 0, 0, 0⟩) :
    x = 0 ∧ y = 0 := by
  have h00 : x * 1 + y * ((1 - q₀) * (1 - r₀)) = 0 := congrArg Mat.a00 h
  have h01 : x * 1 + y * (-((1 - q₀) * r₀)) = 0 := congrArg Mat.a01 h
  have h10 : x * 1 + y * (-(q₀ * (1 - r₀))) = 0 := congrArg Mat.a10 h
  have hy : y * (1 - q₀) = 0 := by nlinarith [h00, h01]
  have hy' : y = 0 := by
    rcases mul_eq_zero.mp hy with h' | h'
    · exact h'
    · exfalso; linarith
  refine ⟨?_, hy'⟩
  rw [hy'] at h00; linarith

/-! ### Part (i): protection -/

/-- **Proposition PRO (i).**  If `∇F = xJ + y∇assoc` then its inner product with
the whole route plane vanishes -- identically in `(α, β, λ)`.  The aggregate
first-order coefficient is therefore zero and `F` is protected. -/
theorem propPRO_protection (x y : ℝ) :
    Mat.inner ((Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀))) (Mlam lam α β q₀ r₀) = 0 := by
  simp only [Mlam, Mat.inner, Mat.add, Mat.smul, Jmat, gradAssoc, R1, R2]
  ring

/-- The same, stated through the two route directions separately: protection is
*annihilation* of each route direction, not cancellation between them. -/
theorem propPRO_protection_each (x y : ℝ) :
    Mat.inner ((Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀))) (R1 q₀) = 0
      ∧ Mat.inner ((Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀))) (R2 r₀) = 0 := by
  constructor <;> · simp only [Mat.inner, Mat.add, Mat.smul, Jmat, gradAssoc, R1, R2]; ring

/-! ### Step 2 converse: the annihilator IS `span{J, ∇assoc}` -/

/-- **The annihilator of the route plane is exactly `span{J, ∇assoc}`.**  Given
`⟨V, R₁⟩ = ⟨V, R₂⟩ = 0`, the witnesses are
`y = (V₀₀ - V₀₁)/(1 - q₀)` and `x = V₀₁ + r₀(V₀₀ - V₀₁)`. -/
theorem annihilator_eq_span (V : Mat) (hq' : (1:ℝ) - q₀ ≠ 0)
    (h1 : Mat.inner V (R1 q₀) = 0) (h2 : Mat.inner V (R2 r₀) = 0) :
    V = (Mat.smul (V.a01 + r₀ * (V.a00 - V.a01)) Jmat).add
        (Mat.smul ((V.a00 - V.a01) / (1 - q₀)) (gradAssoc q₀ r₀)) := by
  simp only [Mat.inner, R1, R2] at h1 h2
  ext
  · simp only [Mat.add, Mat.smul, Jmat, gradAssoc]; field_simp; ring
  · simp only [Mat.add, Mat.smul, Jmat, gradAssoc]; field_simp; ring
  · simp only [Mat.add, Mat.smul, Jmat, gradAssoc]
    field_simp
    linear_combination (1 - r₀) * h1 + (q₀ - 1) * h2
  · simp only [Mat.add, Mat.smul, Jmat, gradAssoc]
    field_simp
    linear_combination (-r₀) * h1 + (q₀ - 1) * h2

/-! ### Part (ii): uniqueness -/

/-- Clearing the positive factor `Z` from the aggregate first-order coefficient
`λκx + (1-λ)κ'y` leaves an expression affine in `α`.  This is the identity the
uniqueness argument runs on. -/
theorem coeff_times_Z (x y : ℝ) (hZ : Zpar α β ≠ 0) :
    (lam * kappa α β q₀ r₀ * x + (1 - lam) * kappa' α β q₀ r₀ * y) * Zpar α β
      = lam * (α - q₀) * r₀ * (1 - r₀) * x + (1 - lam) * (β - r₀) * q₀ * (1 - q₀) * y := by
  simp only [kappa, kappa']
  field_simp

/-- **Proposition PRO (ii).**  Fix an interior `λ`.  If the aggregate
first-order coefficient vanishes at two distinct priors `α₁ ≠ α₂` (the "open
set of priors" of the statement), then both route inner products vanish, i.e.
`∇F ∈ span{J, ∇assoc}` by `annihilator_eq_span`.

`x` and `y` here are `⟨∇F, R₁⟩` and `⟨∇F, R₂⟩`; the hypotheses are the
manuscript's: `λ ∈ (0,1)`, `q₀, r₀ ∈ (0,1)`, and `β ≠ r₀` (off the locus where
`κ'` itself vanishes). -/
theorem propPRO_uniqueness {α₁ α₂ x y : ℝ}
    (hlam : 0 < lam) (hlam' : lam < 1)
    (hq : 0 < q₀) (hq' : q₀ < 1) (hr : 0 < r₀) (hr' : r₀ < 1)
    (hβr : β ≠ r₀) (hne : α₁ ≠ α₂)
    (e1 : lam * (α₁ - q₀) * r₀ * (1 - r₀) * x + (1 - lam) * (β - r₀) * q₀ * (1 - q₀) * y = 0)
    (e2 : lam * (α₂ - q₀) * r₀ * (1 - r₀) * x + (1 - lam) * (β - r₀) * q₀ * (1 - q₀) * y = 0) :
    x = 0 ∧ y = 0 := by
  have hsub : lam * (α₁ - α₂) * r₀ * (1 - r₀) * x = 0 := by linarith
  have hx : x = 0 := by
    have hα : α₁ - α₂ ≠ 0 := sub_ne_zero_of_ne hne
    have h1 : (0:ℝ) < 1 - r₀ := by linarith
    have hprod : lam * (α₁ - α₂) * r₀ * (1 - r₀) ≠ 0 := by
      apply mul_ne_zero
      apply mul_ne_zero
      apply mul_ne_zero (ne_of_gt hlam) hα
      · exact ne_of_gt hr
      · exact ne_of_gt h1
    exact (mul_eq_zero.mp hsub).resolve_left hprod
  refine ⟨hx, ?_⟩
  rw [hx, mul_zero] at e1
  have h1 : (1:ℝ) - lam ≠ 0 := by linarith
  have h2 : β - r₀ ≠ 0 := sub_ne_zero_of_ne hβr
  have h3 : (1:ℝ) - q₀ ≠ 0 := by linarith
  have hprod : (1 - lam) * (β - r₀) * q₀ * (1 - q₀) ≠ 0 := by
    apply mul_ne_zero
    apply mul_ne_zero
    apply mul_ne_zero h1 h2
    · exact ne_of_gt hq
    · exact h3
  have : (1 - lam) * (β - r₀) * q₀ * (1 - q₀) * y = 0 := by linarith
  exact (mul_eq_zero.mp this).resolve_left hprod

/-- **Why the interior-`λ` hypothesis is essential.**  At `λ = 1` only one route
direction enters the first-order coefficient, and the annihilator of a single
direction is three-dimensional rather than two: here is a matrix annihilating
`R₁` that is *not* in `span{J, ∇assoc}` -- the `A`-marginal read-out, which
Proposition DRF shows is indeed protected at `λ = 1` while being unprotected at
every interior `λ`. -/
def margA : Mat := ⟨0, 0, 1, 1⟩

theorem margA_annihilates_R1 (q₀ : ℝ) : Mat.inner margA (R1 q₀) = 0 := by
  simp only [margA, R1, Mat.inner]; ring

theorem margA_not_annihilates_R2 (hr : 0 < r₀) (hr' : r₀ < 1) :
    Mat.inner margA (R2 r₀) ≠ 0 := by
  simp only [margA, R2, Mat.inner]
  intro h
  linarith

/-- The `A`-marginal read-out is therefore outside `span{J, ∇assoc}`: it
annihilates one route direction but not the other, whereas everything in the
span annihilates both. -/
theorem margA_not_in_span (hr : 0 < r₀) (hr' : r₀ < 1) :
    ¬ ∃ x y : ℝ, margA = (Mat.smul x Jmat).add (Mat.smul y (gradAssoc q₀ r₀)) := by
  rintro ⟨x, y, h⟩
  have := (propPRO_protection_each (q₀ := q₀) (r₀ := r₀) x y).2
  rw [← h] at this
  exact margA_not_annihilates_R2 hr hr' this

end JeffreyOrder
