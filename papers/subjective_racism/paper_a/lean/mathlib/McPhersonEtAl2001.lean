import Mathlib

/-! McPherson, Smith-Lovin and Cook (2001), "Birds of a Feather: Homophily in
Social Networks", *Annual Review of Sociology* 27, pp. 415–444.

The review states no model. This file formalises our reading of three
structures the review describes in words, in its own vocabulary, and nothing
here is a result the review proves.

* Baseline homophily (p. 419): the in-group share of a member's ties that random
  assortment from the opportunity pool would give, which is the group's share
  `p` of the pool. Inbreeding homophily is the observed in-group share `H` over
  and above it.
* Consolidation (pp. 419 n. 5, 420–421): homophily on one dimension induced by
  another dimension correlated with it. Here ties form only within class, at
  random, and class is correlated with race. The in-group share on race then
  exceeds the baseline although nobody chooses on race.
* Social distance versus same-different (pp. 422 n. 7, 427): for education the
  review reports that ties grow less likely as the difference grows, and for
  race that the distinction is same-different. -/

namespace McPhersonEtAl2001

/-- Inbreeding homophily, the observed in-group share `H` over the baseline `p`. -/
def inbreeding (H p : ℝ) : ℝ := H - p

/-- The baseline: a member of a category with share `p` of the pool, choosing at
random, has an in-group share `p`. A smaller category has a smaller baseline
in-group share than the larger one, so its members' networks are dominated by
the other category (p. 420). -/
theorem baseline_minority {p : ℝ} (h : p < 1 / 2) : p < 1 - p := by linarith

/-! ### Consolidation

Two classes, `H` (high) and `L` (low), of sizes `nH, nL > 0`. A fraction `aH` of
the high class and `aL` of the low class belong to race `A`. Ties form only
within class, with a partner drawn at random from one's own class. -/

/-- Race `A`'s share of the whole pool, its baseline. -/
noncomputable def popShare (nH nL aH aL : ℝ) : ℝ := (nH * aH + nL * aL) / (nH + nL)

/-- The expected share of same-race partners among race `A`'s ties when ties form
only within class: a member in class `k` meets race `A` with probability `a_k`,
weighted by how many race-`A` members are in each class. -/
noncomputable def sameRaceShare (nH nL aH aL : ℝ) : ℝ :=
  (nH * aH * aH + nL * aL * aL) / (nH * aH + nL * aL)

/-- Consolidation induces inbreeding homophily on race. With ties formed on class
alone, race `A`'s in-group share exceeds its baseline by
`nH nL (aH − aL)² / ((nH + nL)(nH aH + nL aL))`. -/
theorem consolidation_inbreeding {nH nL aH aL : ℝ} (hH : 0 < nH) (hL : 0 < nL)
    (hm : 0 < nH * aH + nL * aL) :
    inbreeding (sameRaceShare nH nL aH aL) (popShare nH nL aH aL)
      = nH * nL * (aH - aL) ^ 2 / ((nH + nL) * (nH * aH + nL * aL)) := by
  unfold inbreeding sameRaceShare popShare
  have h1 : nH + nL ≠ 0 := by positivity
  have h2 : nH * aH + nL * aL ≠ 0 := ne_of_gt hm
  field_simp
  ring

/-- The inbreeding consolidation induces is strictly positive when race and class
are correlated, `aH ≠ aL`. -/
theorem consolidation_pos {nH nL aH aL : ℝ} (hH : 0 < nH) (hL : 0 < nL)
    (hm : 0 < nH * aH + nL * aL) (hne : aH ≠ aL) :
    0 < inbreeding (sameRaceShare nH nL aH aL) (popShare nH nL aH aL) := by
  rw [consolidation_inbreeding hH hL hm]
  have : 0 < (aH - aL) ^ 2 := by
    have : aH - aL ≠ 0 := sub_ne_zero.mpr hne
    positivity
  have hs : 0 < nH + nL := by linarith
  positivity

/-- Control: without consolidation there is no inbreeding. When both classes have
the same racial mix, ties formed on class alone give race `A` exactly its
baseline in-group share. -/
theorem control_no_consolidation {nH nL a : ℝ} (hH : 0 < nH) (hL : 0 < nL)
    (hm : 0 < nH * a + nL * a) :
    inbreeding (sameRaceShare nH nL a a) (popShare nH nL a a) = 0 := by
  rw [consolidation_inbreeding hH hL hm]
  simp

/-! ### Social distance versus same-different -/

/-- A tie propensity with a social distance effect in a difference `d`, of the
quadratic form `v − κ d²`. -/
def distanceProp (v κ d : ℝ) : ℝ := v - κ * d ^ 2

/-- A tie propensity that depends only on same versus different. -/
noncomputable def sameDiffProp (h l d : ℝ) : ℝ := if d = 0 then h else l

/-- Social distance (p. 427): with `κ > 0`, the further apart two people are,
the less likely a tie. -/
theorem distance_effect {v κ d d' : ℝ} (hκ : 0 < κ) (h0 : 0 ≤ d) (hdd : d < d') :
    distanceProp v κ d' < distanceProp v κ d := by
  unfold distanceProp
  have : d ^ 2 < d' ^ 2 := by nlinarith
  nlinarith

/-- Control: a same-different rule has no distance effect. Any two nonzero
differences give the same propensity (p. 422 n. 7, for race). -/
theorem control_same_different_flat (h l : ℝ) {d d' : ℝ} (hd : d ≠ 0) (hd' : d' ≠ 0) :
    sameDiffProp h l d = sameDiffProp h l d' := by
  unfold sameDiffProp
  simp [hd, hd']

/-- Control: the distance effect needs `κ > 0`. With `κ = 0` the propensity is
the same at every difference. -/
theorem control_distance_needs_kappa (v d d' : ℝ) :
    distanceProp v 0 d = distanceProp v 0 d' := by
  unfold distanceProp; ring

end McPhersonEtAl2001
