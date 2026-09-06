/-
# Proposition ORD -- the between-order contrast, Section 5 (companion to DRF)

Propositions DRF and SCR compare a *route* (or the mixed aggregate `P̄_λ`) to
the sequence-free benchmark `Pᴮ`.  This module compares the two *routes to each
other*.  The entrywise first-order between-order gap is already machine-checked
in `PropDIV` (`propDIV_seq_a00`..`propDIV_seq_a11`): for each cell,

  `d/dc ((Pᴶ_AB)ᵢⱼ - (Pᴶ_BA)ᵢⱼ)|₀ = κ (R₁)ᵢⱼ - κ' (R₂)ᵢⱼ`.

Here that is assembled into the read-out an auditor actually computes:

  `lemmaORD_gap`
      for ANY weight `v`, `d/dc ⟨v, Pᴶ_AB - Pᴶ_BA⟩|₀ = κ⟨v,R₁⟩ - κ'⟨v,R₂⟩`.

  `propORD_Amarg`, `propORD_Bmarg`
      the `A`-marginal between-order gap has coefficient `κ' = -K` (DRF's `K`
      with a sign flip); the `B`-marginal has coefficient `-κ`.  Both are
      `Θ(c)` generically.

  `propORD_const`
      the constant weight gives `0`: a between-order contrast on a statistic
      that ignores the cells (total mass) sees nothing, as it must.

The point of ORD, versus DRF, is that the right-hand side mentions **neither
`Pᴮ` nor the mixing weight `λ`**: `Pᴶ_AB - Pᴶ_BA` never refers to either.  DRF's
aggregate drift is `(1-λ)K` and is measured against the benchmark; the
between-order gap is `-K` regardless of `λ` and needs no benchmark.  It is the
statistic available to an auditor who records which cue each evaluator read
first -- and unavailable to one holding only a pooled aggregate.  (The
association's between-order gap is `Θ(c²)`, already in
`AssocLocality.assoc_order_gap_structural`; so of the natural read-outs, the
marginals and decision score separate the orders at first order and the
association does not.)
-/
import JeffreyOrder.Aggregate

namespace JeffreyOrder
open Mat

variable {α β c q₀ r₀ : ℝ}

/-- **Lemma ORD** (general form).  For any weight `v`, the first-order
between-order gap of `⟨v, ·⟩` is `κ⟨v,R₁⟩ - κ'⟨v,R₂⟩`.  Assembled cell by cell
from `PropDIV`'s route-difference derivatives. -/
theorem lemmaORD_gap (v : Mat) (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ => Mat.inner v ((PJab α β c q₀ r₀).sub (PJba α β c q₀ r₀)))
      (kappa α β q₀ r₀ * Mat.inner v (R1 q₀) - kappa' α β q₀ r₀ * Mat.inner v (R2 r₀)) 0 := by
  have h00 := HasDerivAt.const_mul v.a00 (propDIV_seq_a00 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h01 := HasDerivAt.const_mul v.a01 (propDIV_seq_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h10 := HasDerivAt.const_mul v.a10 (propDIV_seq_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h11 := HasDerivAt.const_mul v.a11 (propDIV_seq_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  have h : HasDerivAt (fun c : ℝ =>
      v.a00 * ((PJab α β c q₀ r₀).a00 - (PJba α β c q₀ r₀).a00)
        + v.a01 * ((PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
        + v.a10 * ((PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
        + v.a11 * ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11))
      (v.a00 * (kappa α β q₀ r₀ * (R1 q₀).a00 - kappa' α β q₀ r₀ * (R2 r₀).a00)
        + v.a01 * (kappa α β q₀ r₀ * (R1 q₀).a01 - kappa' α β q₀ r₀ * (R2 r₀).a01)
        + v.a10 * (kappa α β q₀ r₀ * (R1 q₀).a10 - kappa' α β q₀ r₀ * (R2 r₀).a10)
        + v.a11 * (kappa α β q₀ r₀ * (R1 q₀).a11 - kappa' α β q₀ r₀ * (R2 r₀).a11)) 0 :=
    HasDerivAt.add (HasDerivAt.add (HasDerivAt.add h00 h01) h10) h11
  have heq : (fun c : ℝ => Mat.inner v ((PJab α β c q₀ r₀).sub (PJba α β c q₀ r₀)))
      = fun c : ℝ =>
        v.a00 * ((PJab α β c q₀ r₀).a00 - (PJba α β c q₀ r₀).a00)
        + v.a01 * ((PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
        + v.a10 * ((PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
        + v.a11 * ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11) := by
    funext c; simp only [Mat.inner, Mat.sub]
  rw [heq]
  have hval : kappa α β q₀ r₀ * Mat.inner v (R1 q₀) - kappa' α β q₀ r₀ * Mat.inner v (R2 r₀)
      = v.a00 * (kappa α β q₀ r₀ * (R1 q₀).a00 - kappa' α β q₀ r₀ * (R2 r₀).a00)
        + v.a01 * (kappa α β q₀ r₀ * (R1 q₀).a01 - kappa' α β q₀ r₀ * (R2 r₀).a01)
        + v.a10 * (kappa α β q₀ r₀ * (R1 q₀).a10 - kappa' α β q₀ r₀ * (R2 r₀).a10)
        + v.a11 * (kappa α β q₀ r₀ * (R1 q₀).a11 - kappa' α β q₀ r₀ * (R2 r₀).a11) := by
    simp only [Mat.inner, R1, R2]; ring
  rw [hval]; exact h

/-- **Proposition ORD, `A`-marginal**: the between-order gap of `Q(A=1)` has
first-order coefficient `κ' = (β-r₀)q₀(1-q₀)/Z`, i.e. `-K` for DRF's `K`. -/
theorem propORD_Amarg (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        ((PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
          + ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11))
      (kappa' α β q₀ r₀) 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      ((PJab α β c q₀ r₀).a10 - (PJba α β c q₀ r₀).a10)
        + ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11))
      ((kappa α β q₀ r₀ * (R1 q₀).a10 - kappa' α β q₀ r₀ * (R2 r₀).a10)
        + (kappa α β q₀ r₀ * (R1 q₀).a11 - kappa' α β q₀ r₀ * (R2 r₀).a11)) 0 :=
    HasDerivAt.add (propDIV_seq_a10 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
      (propDIV_seq_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  convert h using 1
  simp only [R1, R2]
  ring

/-- The `A`-marginal ORD coefficient is exactly `-K`, DRF's drift coefficient
(`Kdrift = q₀(1-q₀)(r₀-β)/Z`), with the sign flip that comes from `Pᴶ_BA`
pinning `A` while `Pᴶ_AB` drifts. -/
theorem propORD_Amarg_eq_negKdrift (hZ : Zpar α β ≠ 0) :
    kappa' α β q₀ r₀ = -Kdrift α β q₀ r₀ := by
  rw [Kdrift_eq_neg_kappa' hZ]; ring

/-- **Proposition ORD, `B`-marginal**: the between-order gap of `Q(B=1)` has
first-order coefficient `-κ = (q₀-α)r₀(1-r₀)/Z`. -/
theorem propORD_Bmarg (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        ((PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
          + ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11))
      (-kappa α β q₀ r₀) 0 := by
  have h : HasDerivAt (fun c : ℝ =>
      ((PJab α β c q₀ r₀).a01 - (PJba α β c q₀ r₀).a01)
        + ((PJab α β c q₀ r₀).a11 - (PJba α β c q₀ r₀).a11))
      ((kappa α β q₀ r₀ * (R1 q₀).a01 - kappa' α β q₀ r₀ * (R2 r₀).a01)
        + (kappa α β q₀ r₀ * (R1 q₀).a11 - kappa' α β q₀ r₀ * (R2 r₀).a11)) 0 :=
    HasDerivAt.add (propDIV_seq_a01 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
      (propDIV_seq_a11 (q₀ := q₀) (r₀ := r₀) hα hα' hβ hβ')
  convert h using 1
  simp only [R1, R2]
  ring

/-- **Proposition ORD, constant weight**: a between-order contrast on total
mass sees nothing -- its first-order coefficient is `0`.  (Both routes are
normalised, so the constant read-out cannot separate them.) -/
theorem propORD_const (hα : α ≠ 0) (hα' : (1:ℝ) - α ≠ 0)
    (hβ : β ≠ 0) (hβ' : (1:ℝ) - β ≠ 0) :
    HasDerivAt (fun c : ℝ =>
        Mat.inner ⟨1, 1, 1, 1⟩ ((PJab α β c q₀ r₀).sub (PJba α β c q₀ r₀))) 0 0 := by
  have h := lemmaORD_gap (α := α) (β := β) (q₀ := q₀) (r₀ := r₀)
    ⟨1, 1, 1, 1⟩ hα hα' hβ hβ'
  convert h using 1
  simp only [Mat.inner, R1, R2]
  ring

end JeffreyOrder
