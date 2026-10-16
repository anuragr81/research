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

/-! ### Homophily, the definition (p. 416) -/

/-- Homophily for a tie rate that depends on whether the pair is similar: contact
between similar people occurs at a higher rate than between dissimilar people. -/
def Homophilous (rate : Bool → ℝ) : Prop := rate false < rate true

/-- The ordering of dimensions in the abstract (p. 415): race and ethnicity, then
age, religion, education, occupation and gender, "in roughly that order". -/
inductive Dimension
  | race | age | religion | education | occupation | gender
  deriving DecidableEq

/-- The rank of a dimension in that ordering, 0 the strongest divide. -/
def Dimension.rank : Dimension → ℕ
  | .race => 0 | .age => 1 | .religion => 2 | .education => 3 | .occupation => 4 | .gender => 5

/-- Race comes strictly first in the ordering, and the ranks are distinct. -/
theorem race_strongest (d : Dimension) (h : d ≠ .race) : Dimension.rank .race < d.rank := by
  cases d <;> simp_all [Dimension.rank]

/-! ### Inbreeding counteracts the baseline for small categories (pp. 421, 425, 429)

Our reading of Laumann's negative rank correlation between a group's size and its
in-group choice, as reported on p. 422: at a given in-group share `H`, the
inbreeding `H − p` is larger the smaller the category. -/

theorem inbreeding_antitone_size {H p p' : ℝ} (h : p < p') :
    inbreeding H p' < inbreeding H p := by unfold inbreeding; linarith

/-! ### Consolidation and foci, in general

`k` foci (or classes) with sizes `n i ≥ 0` and race-`A` fractions `a i`. Ties form only
within a focus, at random. This covers consolidation (p. 419 n. 5), foci that produce
baseline homophily (p. 433), and value homophily derived from status (p. 419). -/

/-- Race `A`'s share of the pool. -/
noncomputable def poolShare {ι : Type*} (s : Finset ι) (n a : ι → ℝ) : ℝ :=
  (∑ i ∈ s, n i * a i) / ∑ i ∈ s, n i

/-- Race `A`'s expected same-race share when ties form only within foci. -/
noncomputable def fociSameShare {ι : Type*} (s : Finset ι) (n a : ι → ℝ) : ℝ :=
  (∑ i ∈ s, n i * a i * a i) / ∑ i ∈ s, n i * a i

/-- Ties formed only within foci never give race less than its baseline in-group
share, whatever the number of foci (Cauchy–Schwarz). -/
theorem foci_inbreeding_nonneg {ι : Type*} (s : Finset ι) (n a : ι → ℝ)
    (hn : ∀ i ∈ s, 0 ≤ n i) (hN : 0 < ∑ i ∈ s, n i) (hA : 0 < ∑ i ∈ s, n i * a i) :
    0 ≤ inbreeding (fociSameShare s n a) (poolShare s n a) := by
  unfold inbreeding fociSameShare poolShare
  have cs := Finset.sum_mul_sq_le_sq_mul_sq s (fun i => Real.sqrt (n i))
    (fun i => Real.sqrt (n i) * a i)
  have e1 : ∀ i ∈ s, Real.sqrt (n i) * (Real.sqrt (n i) * a i) = n i * a i := by
    intro i hi; rw [← mul_assoc, Real.mul_self_sqrt (hn i hi)]
  have e2 : ∀ i ∈ s, Real.sqrt (n i) ^ 2 = n i := fun i hi => Real.sq_sqrt (hn i hi)
  have e3 : ∀ i ∈ s, (Real.sqrt (n i) * a i) ^ 2 = n i * a i * a i := by
    intro i hi; rw [mul_pow, Real.sq_sqrt (hn i hi)]; ring
  rw [Finset.sum_congr rfl e1, Finset.sum_congr rfl e2, Finset.sum_congr rfl e3] at cs
  rw [sub_nonneg, div_le_div_iff₀ hN hA]
  nlinarith [cs]

/-- Control: foci with the same racial mix induce no inbreeding. -/
theorem control_foci_same_mix {ι : Type*} (s : Finset ι) (n : ι → ℝ) (c : ℝ)
    (hN : 0 < ∑ i ∈ s, n i) :
    inbreeding (fociSameShare s n (fun _ => c)) (poolShare s n (fun _ => c)) = 0 := by
  unfold inbreeding fociSameShare poolShare
  have hN' : (∑ i ∈ s, n i) ≠ 0 := ne_of_gt hN
  simp only [← Finset.sum_mul]
  field_simp
  ring

/-! ### Multiplex ties (p. 418)

Homophily on each type of relation cumulates. Our reading: each relation type `r`
multiplies the odds of a tie between similar people by `θ r > 1`, and a multiplex
tie carries the product. -/

theorem multiplex_cumulates {ι : Type*} [DecidableEq ι] (s : Finset ι) (θ : ι → ℝ)
    (h1 : ∀ r ∈ s, 1 ≤ θ r) {r r' : ι} (hr : r ∈ s) (hr' : r' ∈ s) (hne : r ≠ r')
    (hgt : 1 < θ r') : θ r < ∏ q ∈ s, θ q := by
  have hpos : ∀ q ∈ s, 0 ≤ θ q := fun q hq => le_trans zero_le_one (h1 q hq)
  rw [← Finset.mul_prod_erase s θ hr]
  have hr'e : r' ∈ s.erase r := Finset.mem_erase.mpr ⟨hne.symm, hr'⟩
  rw [← Finset.mul_prod_erase (s.erase r) θ hr'e]
  have hrest : 1 ≤ ∏ q ∈ (s.erase r).erase r', θ q := by
    apply Finset.prod_induction _ (fun x => 1 ≤ x)
    · intro x y hx hy; nlinarith
    · norm_num
    · intro q hq; exact h1 q (Finset.mem_of_mem_erase (Finset.mem_of_mem_erase hq))
  have hθr : 1 ≤ θ r := h1 r hr
  have : θ r * 1 < θ r * (θ r' * ∏ q ∈ (s.erase r).erase r', θ q) := by
    apply mul_lt_mul_of_pos_left _ (by linarith)
    nlinarith
  linarith

/-- Control: with every relation neutral (`θ = 1`) a multiplex tie is no more
homophilous than a simplex one. -/
theorem control_multiplex_neutral {ι : Type*} (s : Finset ι) :
    ∏ _q ∈ s, (1 : ℝ) = 1 := by simp

/-! ### Selective tie dissolution (pp. 435–436) -/

/-- The share of similar pairs among ties that survive, from an initial share `s`
with survival rates `rs` (similar) and `rd` (dissimilar). -/
noncomputable def surviveShare (s rs rd : ℝ) : ℝ := s * rs / (s * rs + (1 - s) * rd)

/-- When dissimilar ties dissolve faster, the surviving ties are more homophilous. -/
theorem dissolution_raises_homophily {s rs rd : ℝ} (hs0 : 0 < s) (hs1 : s < 1)
    (hrd : 0 < rd) (hr : rd < rs) : s < surviveShare s rs rd := by
  unfold surviveShare
  have hden : 0 < s * rs + (1 - s) * rd := by nlinarith
  rw [lt_div_iff₀ hden]
  nlinarith [mul_pos hs0 (sub_pos.mpr hs1)]

/-- Control: with equal dissolution rates the share is unchanged. -/
theorem control_equal_dissolution {s r : ℝ} (hr : 0 < r) : surviveShare s r r = s := by
  unfold surviveShare
  have : s * r + (1 - s) * r = r := by ring
  rw [this]; field_simp

/-! ### The edge of a niche (p. 436)

Our reading of "unlike the other members": the mean squared distance from `c` to the
members `x k` of a group. It decomposes into the squared gap to the group mean plus
the within-group variance, the same decomposition as Paper A1's
`(c̄_g − c_i)² + σ_g²`. -/

noncomputable def meanOf {n : ℕ} (x : Fin n → ℝ) : ℝ := (∑ k, x k) / n
noncomputable def varOf {n : ℕ} (x : Fin n → ℝ) : ℝ := (∑ k, (x k - meanOf x) ^ 2) / n
noncomputable def meanSqDist {n : ℕ} (c : ℝ) (x : Fin n → ℝ) : ℝ := (∑ k, (c - x k) ^ 2) / n

theorem niche_decomposition {n : ℕ} (hn : 0 < n) (c : ℝ) (x : Fin n → ℝ) :
    meanSqDist c x = (c - meanOf x) ^ 2 + varOf x := by
  unfold meanSqDist varOf meanOf
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hsum : ∑ k, (x k - (∑ j, x j) / n) = 0 := by
    rw [Finset.sum_sub_distrib]; simp; field_simp; ring
  have key : ∑ k, (c - x k) ^ 2
      = ∑ k, ((c - (∑ j, x j) / n) ^ 2 - 2 * (c - (∑ j, x j) / n) * (x k - (∑ j, x j) / n)
          + (x k - (∑ j, x j) / n) ^ 2) := by
    apply Finset.sum_congr rfl; intro k _; ring
  rw [key, Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.mul_sum, hsum]
  simp
  field_simp

/-- Control: the distance to the group mean alone misses the spread. Two groups with
the same mean `0`, members `{−1, 1}` and `{−2, 2}`, put a member at `0` at the same
distance from the mean but at different mean squared distances from the members. -/
theorem control_mean_only_misses_spread :
    meanOf ![(-1 : ℝ), 1] = meanOf ![(-2 : ℝ), 2] ∧
      meanSqDist 0 ![(-1 : ℝ), 1] ≠ meanSqDist 0 ![(-2 : ℝ), 2] := by
  unfold meanOf meanSqDist
  constructor <;> norm_num [Fin.sum_univ_two]

/-! ### Co-membership ties (p. 432) -/

/-- A group of `n` members creates `n (n − 1)/2` co-membership ties. -/
theorem comembership_ties (n : ℕ) : n.choose 2 = n * (n - 1) / 2 := Nat.choose_two_right n

/-- Larger groups create proportionally more co-membership ties: ties per member,
`(n − 1)/2`, rise with `n`. -/
theorem comembership_per_member {n : ℕ} (hn : 0 < n) :
    ((n.choose 2 : ℕ) : ℚ) / n < (((n + 1).choose 2 : ℕ) : ℚ) / (n + 1) := by
  rw [Nat.cast_choose_two, Nat.cast_choose_two]
  have hn' : (0 : ℚ) < n := by exact_mod_cast hn
  push_cast
  rw [div_lt_div_iff₀ hn' (by linarith)]
  nlinarith

/-- P. 420: only 8 percent of adults name a confidant of another race, "less than one
seventh" of the heterogeneity under random choice, so random choice would give more
than 56 percent. -/
theorem marsden_random_benchmark {r : ℝ} (h : 0.08 < r / 7) : 0.56 < r := by linarith

end McPhersonEtAl2001
