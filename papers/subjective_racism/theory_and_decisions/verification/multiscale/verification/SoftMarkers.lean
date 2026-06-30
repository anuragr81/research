/-
  Multiple soft markers: Bayes commutes, Jeffrey does not.  Lean 4 + Mathlib.

  Two facts behind the decision-level result:
    * BAYES order-independence (general): combining two cues as likelihoods gives the
      same posterior in either order, because likelihood multiplication commutes and
      the normaliser is symmetric.  Proved in general below.
    * JEFFREY non-commutativity (witness): registering each cue as the impression it
      produces (a target marginal, conditionals held rigid) is order-dependent.
      Shown by a concrete 2x2 rational example.

  Caveat: written for Lean 4 / recent Mathlib, NOT compiled in the environment that
  produced it. `bayes_order_indep` should be robust. For the Jeffrey witness the
  arithmetic is exact and verified by hand and in SymPy (P12 0 0 = 18/41,
  P21 0 0 = 54/85, 18/41 - 54/85 = -684/3485 ≠ 0); if `decide`/`norm_num` needs a
  nudge on ℚ division in your toolchain, `native_decide` will close it, and the
  target values above tell you what it must compute.
-/
import Mathlib

open Finset

namespace SoftMarkers

/-! ## Bayes: order-independent (general finite state space) -/

variable {C : Type*} [Fintype C]

/-- Posterior after combining two cues given as likelihoods `l₁, l₂` on a prior `p`. -/
noncomputable def bpost (p l₁ l₂ : C → ℝ) : C → ℝ :=
  fun c => p c * l₁ c * l₂ c / (∑ c', p c' * l₁ c' * l₂ c')

/-- **Bayesian updating is order-independent.** Swapping the two cues leaves the
posterior unchanged: likelihoods multiply, multiplication commutes, and the
normaliser is symmetric. (The sequential two-step update reduces to `bpost`: the
first-step normaliser cancels, `(p·l₁/Z₁)·l₂ / Σ(p·l₁/Z₁)·l₂ = p·l₁·l₂ / Σ p·l₁·l₂`.) -/
theorem bayes_order_indep (p l₁ l₂ : C → ℝ) :
    bpost p l₁ l₂ = bpost p l₂ l₁ := by
  funext c
  unfold bpost
  congr 1
  · ring
  · apply Finset.sum_congr rfl
    intro c' _
    ring

/-! ## Jeffrey: order-dependent (concrete 2×2 witness)

State = two binary attributes; a "table" is `Fin 2 → Fin 2 → ℚ`. Cue 1 sets the
A-marginal to `q` (conditionals on A held rigid); cue 2 sets the B-marginal to `r`. -/

abbrev Tbl := Fin 2 → Fin 2 → ℚ

def Amarg (D : Tbl) (i : Fin 2) : ℚ := D i 0 + D i 1
def Bmarg (D : Tbl) (j : Fin 2) : ℚ := D 0 j + D 1 j

/-- Jeffrey update setting the A-marginal to `q`, holding `B | A` rigid. -/
def jA (D : Tbl) (q : Fin 2 → ℚ) : Tbl := fun i j => q i * D i j / Amarg D i
/-- Jeffrey update setting the B-marginal to `r`, holding `A | B` rigid. -/
def jB (D : Tbl) (r : Fin 2 → ℚ) : Tbl := fun i j => r j * D i j / Bmarg D j

/-- A correlated prior, and two cue-impressions. -/
def prior : Tbl := ![![9/20, 1/20], ![1/4, 1/4]]
def qv : Fin 2 → ℚ := ![4/5, 1/5]
def rv : Fin 2 → ℚ := ![1/2, 1/2]

def P12 : Tbl := jB (jA prior qv) rv   -- cue order A then B
def P21 : Tbl := jA (jB prior rv) qv   -- cue order B then A

/-- **Jeffrey updating is order-dependent.** Same two cues, different order, different
posterior: here `P12 0 0 = 18/41` while `P21 0 0 = 54/85`. -/
theorem jeffrey_not_comm : P12 0 0 ≠ P21 0 0 := by
  unfold P12 P21 jA jB Amarg Bmarg prior qv rv
  norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.head_cons]

/-- The concrete cell values, for reference / an alternative check. -/
example : P12 0 0 = 18/41 ∧ P21 0 0 = 54/85 := by
  unfold P12 P21 jA jB Amarg Bmarg prior qv rv
  constructor <;>
    norm_num [Matrix.cons_val_zero, Matrix.cons_val_one, Matrix.head_cons]

end SoftMarkers
