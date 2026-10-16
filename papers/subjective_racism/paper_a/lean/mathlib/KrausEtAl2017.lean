import Mathlib

/-! Kraus, Park and Tan (2017), "Signs of Social Class: The Experience of
Economic Inequality in Everyday Life", Perspectives on Psychological Science 12,
accepted manuscript, in the manuscript's terms.

Two structures are formalised. The first is the chance benchmark of the
quartile and half analysis (pp. 8 to 12). Observer estimates and the speaker
composite are each split into quartiles or halves of the sample, and an
estimate is correct when the two categories match, against chance of 25% and
50%. The second is our reading of the verbal argument on pp. 14 and 15, that
"similarity enhances liking", so that accurate class signals create "barriers
for relationship formation across class boundaries", together with the cited
finding of Côté et al. that upper-class participants affiliate more with upper
class signals and lower-class participants with lower class signals. The
manuscript states these in words. The formal version is ours. -/

namespace KrausEtAl2017

open Finset

/-! ### The chance benchmark -/

/-- The probability that a stimulus-blind guess is correct. The guess picks
category `k` with probability `q k`, independently of the target, whose
category is `k` with probability `s k`. -/
def blindHit {n : ℕ} (q s : Fin n → ℝ) : ℝ := ∑ k, q k * s k

/-- With categories defined as sample quantiles, each holds the share `1/n`, and
every stimulus-blind guess is correct with probability exactly `1/n`, whatever
its own distribution. -/
theorem blindHit_uniform {n : ℕ} (q : Fin n → ℝ) (hq : ∑ k, q k = 1) :
    blindHit q (fun _ => 1 / (n : ℝ)) = 1 / n := by
  unfold blindHit
  rw [← Finset.sum_mul, hq, one_mul]

/-- Quartiles, the 25% benchmark of Figure 2. -/
theorem chance_quartile (q : Fin 4 → ℝ) (hq : ∑ k, q k = 1) :
    blindHit q (fun _ => 1 / 4) = 1 / 4 := by
  have := blindHit_uniform q hq
  norm_num at this ⊢
  exact this

/-- Halves, the 50% benchmark of Figure 2. -/
theorem chance_half (q : Fin 2 → ℝ) (hq : ∑ k, q k = 1) :
    blindHit q (fun _ => 1 / 2) = 1 / 2 := by
  have := blindHit_uniform q hq
  norm_num at this ⊢
  exact this

/-- Control. Drop the sample-quantile construction and 25% is no longer chance.
If the four categories were the population's lower, working, middle and upper
classes, with most people in the middle two (Hout, as cited on p. 8), say
shares `.05, .45, .45, .05`, then always guessing the second category is
correct 45% of the time. -/
theorem control_nonuniform_categories :
    1 / 4 < blindHit ![0, 1, 0, 0] ![5 / 100, 45 / 100, 45 / 100, 5 / 100] := by
  simp [blindHit, Fin.sum_univ_four]
  norm_num

/-! ### Our reading of the sorting argument

The observer of class `cᵢ` sees a signal `ĉ` of the partner's class and forms a
relationship when the squared distance between them is at most `Δ`. -/

/-- The affiliation rule, our reading of "similarity enhances liking". -/
def affiliates (Δ cᵢ ĉ : ℝ) : Prop := (ĉ - cᵢ) ^ 2 ≤ Δ

/-- An accurate signal, `ĉ = cⱼ`, blocks every relationship across a class
distance larger than the threshold. -/
theorem accurate_signal_blocks {Δ cᵢ cⱼ : ℝ} (h : Δ < (cⱼ - cᵢ) ^ 2) :
    ¬ affiliates Δ cᵢ cⱼ := by
  unfold affiliates; linarith

/-- A signal that shows nothing, every partner read as the same `m` close to the
observer, admits a partner whose class is far from the observer's, whom an
accurate signal would block. This is our reading of the invisible-wealth
condition of Nishi et al. as cited on p. 21. -/
theorem invisible_signal_admits {Δ cᵢ m cⱼ : ℝ} (hm : (m - cᵢ) ^ 2 ≤ Δ)
    (_hfar : Δ < (cⱼ - cᵢ) ^ 2) :
    affiliates Δ cᵢ ((fun _ : ℝ => m) cⱼ) ∧ ¬ affiliates Δ cᵢ cⱼ :=
  ⟨hm, accurate_signal_blocks _hfar⟩

/-- Utility from a partner read as `ĉ`, for an observer of class `cᵢ`. -/
def U (cᵢ ĉ : ℝ) : ℝ := -(ĉ - cᵢ) ^ 2

/-- Côté et al. as cited on p. 15. With `L < H`, the upper-class observer
prefers the upper-class signal and the lower-class observer the lower-class
signal. The ranking of partners depends on the observer's own class. -/
theorem own_class_ranking {L H : ℝ} (h : L < H) :
    U H L < U H H ∧ U L H < U L L := by
  unfold U
  have : 0 < (H - L) ^ 2 := by nlinarith
  constructor <;> nlinarith

/-- Control. Drop the dependence on own class and the crossing fails. With a
vertical utility `ĉ`, observers of both classes prefer the upper-class signal. -/
theorem control_vertical_common_ranking {L H : ℝ} (h : L < H) :
    ¬ ((fun (_ ĉ : ℝ) => ĉ) H L < (fun (_ ĉ : ℝ) => ĉ) H H ∧
        (fun (_ ĉ : ℝ) => ĉ) L H < (fun (_ ĉ : ℝ) => ĉ) L L) := by
  simp only
  intro ⟨_, h2⟩
  linarith

end KrausEtAl2017
