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

/-- With an accurate signal the sorting runs both ways. The observer of class
`cᵢ` avoids `cⱼ` exactly when `cⱼ` avoids `cᵢ`. -/
theorem sorting_symmetric (Δ cᵢ cⱼ : ℝ) : affiliates Δ cᵢ cⱼ ↔ affiliates Δ cⱼ cᵢ := by
  unfold affiliates
  rw [show (cⱼ - cᵢ) ^ 2 = (cᵢ - cⱼ) ^ 2 by ring]

/-! ### Accuracy and the spread of class (p. 13)

Our reading of KPT-Q12. A signal is class plus independent noise,
`ĉ = c + ε`, with `var c = vc` and `var ε = ve`. The squared correlation of
signal with class, the share of the signal's variance that is class, is
`vc/(vc + ve)`. -/

/-- The squared correlation of a signal with class. -/
noncomputable def reliability (vc ve : ℝ) : ℝ := vc / (vc + ve)

/-- With noise of fixed size, more spread in class, more inequality, makes the
signal a better guide to class. Lower inequality makes class "harder to
discern". -/
theorem accuracy_rises_with_spread {v₁ v₂ ve : ℝ} (he : 0 < ve) (h1 : 0 ≤ v₁) (h12 : v₁ < v₂) :
    reliability v₁ ve < reliability v₂ ve := by
  unfold reliability
  rw [div_lt_div_iff₀ (by linarith) (by linarith)]
  nlinarith

/-- Control. Drop the noise and the dependence on inequality goes. A noiseless
signal is perfectly accurate at every positive spread. -/
theorem control_noiseless_flat {vc : ℝ} (h : 0 < vc) : reliability vc 0 = 1 := by
  unfold reliability; rw [add_zero, div_self h.ne']

/-! ### Stereotyping (pp. 16, 19)

"the stereotype content association of high competence to high social class",
and the Pygmalion question of p. 19, whether shifting one's signals upward
"will elicit others' judgments of heightened competence". Competence is judged
as a function `f` of perceived class. -/

/-- If competence is an increasing function of perceived class, an upward shift
of the class signal raises judged competence. -/
theorem signal_upgrade_raises_competence {f : ℝ → ℝ} (hf : StrictMono f) {ĉ δ : ℝ} (hδ : 0 < δ) :
    f ĉ < f (ĉ + δ) := hf (by linarith)

/-- Control. Drop the link from class to competence and the upgrade gains
nothing. -/
theorem control_flat_stereotype (k ĉ δ : ℝ) : (fun _ : ℝ => k) ĉ = (fun _ : ℝ => k) (ĉ + δ) := rfl

/-! ### Strategic sharing (p. 21)

"sharing resources with a person with low economic standing may be perceived as
risky behavior because such a sharing relationship has few direct economic
benefits for the sharer". The sharer values a partner by the partner's
perceived resources. -/

/-- With visible resources, `ĉ = c`, the sharer prefers the richer partner. -/
theorem visible_share_rich {cⱼ cₖ : ℝ} (h : cₖ < cⱼ) : (fun c : ℝ => c) cₖ < (fun c : ℝ => c) cⱼ := h

/-- With invisible resources every partner is read alike, so the sharer's
ranking of partners does not depend on their resources. -/
theorem invisible_share_indifferent (m cⱼ cₖ : ℝ) :
    (fun _ : ℝ => m) cⱼ = (fun _ : ℝ => m) cₖ := rfl

/-! ### Class conflict (pp. 16–18)

Signals "make salient that resources are unequally shared". The perceived gap
between two people is the distance between their signals. -/

/-- The perceived gap. -/
def perceivedGap (ĉᵢ ĉⱼ : ℝ) : ℝ := |ĉᵢ - ĉⱼ|

/-- An accurate signal makes the actual gap visible, and a signal that reads
everyone alike hides it. -/
theorem visible_gap (cᵢ cⱼ m : ℝ) :
    perceivedGap cᵢ cⱼ = |cᵢ - cⱼ| ∧ perceivedGap m m = 0 := by
  unfold perceivedGap; simp

end KrausEtAl2017
