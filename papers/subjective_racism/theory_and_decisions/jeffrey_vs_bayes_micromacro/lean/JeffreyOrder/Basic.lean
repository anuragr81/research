/-
# The model of PAPER_B_MANUSCRIPT.tex, Section 2

"Order-Sensitivity of Belief and Decision Statistics under Jeffrey Conditioning".

A belief over two binary attributes `(A, B) ∈ {0,1}²` is a 2×2 joint law: the
row index is the value of `A`, the column index the value of `B`.  The prior is
parametrised by its marginals `P(A=0) = α`, `P(B=0) = β` and the prior
covariance `c`, and the two cues deliver credences `q = (q₀, 1-q₀)` on `A`'s
partition and `r = (r₀, 1-r₀)` on `B`'s partition.
-/
import Mathlib.Data.Real.Basic
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.Ring
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.LinearCombination

namespace JeffreyOrder

/-- A 2×2 array of reals: a joint law, a gradient, or a route direction. -/
structure Mat where
  a00 : ℝ
  a01 : ℝ
  a10 : ℝ
  a11 : ℝ

namespace Mat

@[ext] theorem ext {X Y : Mat} (h00 : X.a00 = Y.a00) (h01 : X.a01 = Y.a01)
    (h10 : X.a10 = Y.a10) (h11 : X.a11 = Y.a11) : X = Y := by
  cases X; cases Y; simp_all

/-- Total mass. -/
def total (Q : Mat) : ℝ := Q.a00 + Q.a01 + Q.a10 + Q.a11

/-- `Q(A = 0)`: the first row sum. -/
def mA0 (Q : Mat) : ℝ := Q.a00 + Q.a01
/-- `Q(A = 1)`: the second row sum. -/
def mA1 (Q : Mat) : ℝ := Q.a10 + Q.a11
/-- `Q(B = 0)`: the first column sum. -/
def mB0 (Q : Mat) : ℝ := Q.a00 + Q.a10
/-- `Q(B = 1)`: the second column sum. -/
def mB1 (Q : Mat) : ℝ := Q.a01 + Q.a11

/-- The cross-attribute association of Definition 1:
`assoc Q = Q₀₀Q₁₁ - Q₀₁Q₁₀`.  It exists only on the joint law, never on the
two marginals separately. -/
def assoc (Q : Mat) : ℝ := Q.a00 * Q.a11 - Q.a01 * Q.a10

/-- The odds ratio `Q₀₀Q₁₁ / (Q₀₁Q₁₀)`. -/
noncomputable def oddsRatio (Q : Mat) : ℝ := (Q.a00 * Q.a11) / (Q.a01 * Q.a10)

/-- Entrywise (Frobenius) inner product `⟨X, Y⟩`. -/
def inner (X Y : Mat) : ℝ :=
  X.a00 * Y.a00 + X.a01 * Y.a01 + X.a10 * Y.a10 + X.a11 * Y.a11

/-- Scalar multiple. -/
def smul (k : ℝ) (X : Mat) : Mat := ⟨k * X.a00, k * X.a01, k * X.a10, k * X.a11⟩

/-- Entrywise sum. -/
def add (X Y : Mat) : Mat :=
  ⟨X.a00 + Y.a00, X.a01 + Y.a01, X.a10 + Y.a10, X.a11 + Y.a11⟩

/-- Entrywise difference. -/
def sub (X Y : Mat) : Mat :=
  ⟨X.a00 - Y.a00, X.a01 - Y.a01, X.a10 - Y.a10, X.a11 - Y.a11⟩

/-- Normalise to total mass one. -/
noncomputable def normalize (Q : Mat) : Mat :=
  ⟨Q.a00 / Q.total, Q.a01 / Q.total, Q.a10 / Q.total, Q.a11 / Q.total⟩

end Mat

open Mat

/-- The prior `P` of Section 2.1, with marginals `α`, `β` and covariance `c`. -/
def prior (α β c : ℝ) : Mat :=
  ⟨α * β + c, α * (1 - β) - c, (1 - α) * β - c, (1 - α) * (1 - β) + c⟩

/-- The independent posterior `q ⊗ r`, which is where both routes and the
benchmark sit at `c = 0` (Proposition IMM). -/
def indep (q₀ r₀ : ℝ) : Mat :=
  ⟨q₀ * r₀, q₀ * (1 - r₀), (1 - q₀) * r₀, (1 - q₀) * (1 - r₀)⟩

/-- A Jeffrey step on `A`'s partition: reset the `A`-marginal to `q`, holding
the conditional distribution of `B` given `A` fixed. -/
noncomputable def jeffreyA (Q : Mat) (q₀ : ℝ) : Mat :=
  ⟨q₀ * Q.a00 / Q.mA0, q₀ * Q.a01 / Q.mA0,
   (1 - q₀) * Q.a10 / Q.mA1, (1 - q₀) * Q.a11 / Q.mA1⟩

/-- A Jeffrey step on `B`'s partition. -/
noncomputable def jeffreyB (Q : Mat) (r₀ : ℝ) : Mat :=
  ⟨r₀ * Q.a00 / Q.mB0, (1 - r₀) * Q.a01 / Q.mB1,
   r₀ * Q.a10 / Q.mB0, (1 - r₀) * Q.a11 / Q.mB1⟩

/-- The unnormalised Bayes-factor reweighting on the `A`-cue alone:
each cell is multiplied by the matched likelihood `ℓᴬᵢ ∝ qᵢ / P(A=i)`. -/
noncomputable def bayesAW (Q : Mat) (α q₀ : ℝ) : Mat :=
  ⟨Q.a00 * (q₀ / α), Q.a01 * (q₀ / α),
   Q.a10 * ((1 - q₀) / (1 - α)), Q.a11 * ((1 - q₀) / (1 - α))⟩

/-- The benchmark update on the `A`-cue alone. -/
noncomputable def bayesA (Q : Mat) (α q₀ : ℝ) : Mat := (bayesAW Q α q₀).normalize

/-- The unnormalised Bayes-factor reweighting on both cues: `P(i,j) ℓᴬᵢ ℓᴮⱼ`. -/
noncomputable def bayesW (Q : Mat) (α β q₀ r₀ : ℝ) : Mat :=
  ⟨Q.a00 * (q₀ / α) * (r₀ / β),
   Q.a01 * (q₀ / α) * ((1 - r₀) / (1 - β)),
   Q.a10 * ((1 - q₀) / (1 - α)) * (r₀ / β),
   Q.a11 * ((1 - q₀) / (1 - α)) * ((1 - r₀) / (1 - β))⟩

/-- The Bayes-factor benchmark `Pᴮ`: one update on the combined Bayes-factor
content of both cues, sequence-invariant by construction. -/
noncomputable def bayes (Q : Mat) (α β q₀ r₀ : ℝ) : Mat := (bayesW Q α β q₀ r₀).normalize

/-- `Pᴶ_AB`: the `A`-cue read first, then the `B`-cue. -/
noncomputable def PJab (α β c q₀ r₀ : ℝ) : Mat := jeffreyB (jeffreyA (prior α β c) q₀) r₀

/-- `Pᴶ_BA`: the `B`-cue read first, then the `A`-cue. -/
noncomputable def PJba (α β c q₀ r₀ : ℝ) : Mat := jeffreyA (jeffreyB (prior α β c) r₀) q₀

/-- `Pᴮ`: the sequence-free benchmark at the prior `prior α β c`. -/
noncomputable def PB (α β c q₀ r₀ : ℝ) : Mat := bayes (prior α β c) α β q₀ r₀

/-- The mean belief `P̄_λ = λ Pᴶ_AB + (1-λ) Pᴶ_BA` of Section 5. -/
def meanBelief (lam : ℝ) (X Y : Mat) : Mat := (Mat.smul lam X).add (Mat.smul (1 - lam) Y)

/-- A standing hypothesis package: the prior marginals and both delivered
credences lie strictly inside `(0,1)`.  This is the open cube on which all the
paper's genericity statements are made. -/
structure Interior (α β q₀ r₀ : ℝ) : Prop where
  hα0 : 0 < α
  hα1 : α < 1
  hβ0 : 0 < β
  hβ1 : β < 1
  hq0 : 0 < q₀
  hq1 : q₀ < 1
  hr0 : 0 < r₀
  hr1 : r₀ < 1

namespace Interior
variable {α β q₀ r₀ : ℝ}

theorem α_ne (h : Interior α β q₀ r₀) : α ≠ 0 := ne_of_gt h.hα0
theorem one_sub_α_ne (h : Interior α β q₀ r₀) : (1 : ℝ) - α ≠ 0 := by
  have := h.hα1; linarith
theorem β_ne (h : Interior α β q₀ r₀) : β ≠ 0 := ne_of_gt h.hβ0
theorem one_sub_β_ne (h : Interior α β q₀ r₀) : (1 : ℝ) - β ≠ 0 := by
  have := h.hβ1; linarith
theorem q_ne (h : Interior α β q₀ r₀) : q₀ ≠ 0 := ne_of_gt h.hq0
theorem one_sub_q_ne (h : Interior α β q₀ r₀) : (1 : ℝ) - q₀ ≠ 0 := by
  have := h.hq1; linarith
theorem r_ne (h : Interior α β q₀ r₀) : r₀ ≠ 0 := ne_of_gt h.hr0
theorem one_sub_r_ne (h : Interior α β q₀ r₀) : (1 : ℝ) - r₀ ≠ 0 := by
  have := h.hr1; linarith

/-- `Z := αβ(1-α)(1-β) > 0`, the denominator appearing throughout Section 4. -/
theorem Z_pos (h : Interior α β q₀ r₀) : 0 < α * β * (1 - α) * (1 - β) := by
  have ha : (0:ℝ) < α := h.hα0
  have hb : (0:ℝ) < β := h.hβ0
  have h1 : (0:ℝ) < 1 - α := by have := h.hα1; linarith
  have h2 : (0:ℝ) < 1 - β := by have := h.hβ1; linarith
  positivity

end Interior

/-- Normalising a law that already has total mass one changes nothing. -/
theorem Mat.normalize_of_total_one {Q : Mat} (h : Q.total = 1) : Q.normalize = Q := by
  ext <;> simp [Mat.normalize, h]

/-- The prior's `A`-marginal is `α`, exactly in `c` (the parametrisation of
Section 2.1 fixes the marginals and moves only the covariance). -/
@[simp] theorem prior_mA0 (α β c : ℝ) : (prior α β c).mA0 = α := by
  simp [prior, Mat.mA0]; ring

@[simp] theorem prior_mA1 (α β c : ℝ) : (prior α β c).mA1 = 1 - α := by
  simp [prior, Mat.mA1]; ring

@[simp] theorem prior_mB0 (α β c : ℝ) : (prior α β c).mB0 = β := by
  simp [prior, Mat.mB0]; ring

@[simp] theorem prior_mB1 (α β c : ℝ) : (prior α β c).mB1 = 1 - β := by
  simp [prior, Mat.mB1]; ring

/-- The prior's association is exactly the covariance parameter: `assoc P = c`. -/
@[simp] theorem prior_assoc (α β c : ℝ) : (prior α β c).assoc = c := by
  simp [prior, Mat.assoc]; ring

/-- The prior is a probability distribution. -/
@[simp] theorem prior_total (α β c : ℝ) : (prior α β c).total = 1 := by
  simp [prior, Mat.total]; ring

end JeffreyOrder
