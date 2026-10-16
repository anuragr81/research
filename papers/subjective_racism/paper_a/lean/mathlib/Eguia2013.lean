import Mathlib

/-! Eguia, "Discrimination and Assimilation", NYU working paper of 10 September
2013, in the paper's notation as far as Lean allows. Half the agents have the
advantaged background `𝒜`, with endowment `eA`, and half the disadvantaged
background `𝒟`, with endowment 0. Ability is uniform on `[0, 1]` in each. A
fraction `l` of `𝒟` (the paper's `λ`, a reserved word in Lean) forms `𝒟_Y`,
whose members may assimilate into social group `A`. In Sections 1 and 2 skill
equals ability, and members of `𝒟_Y` assimilate if and only if their skill is
above a cutoff `s`. Eq. (3) gives the averages of the two social groups as
functions of `s`. -/

namespace Eguia2013

/-- Eq. (3). The average endowment of social group `A` when the members of
`𝒟_Y` above the cutoff `s` assimilate. -/
noncomputable def eAbar (eA l s : ℝ) : ℝ := eA / (1 + l * (1 - s))

/-- Eq. (3). The average skill of social group `A`, in closed form. -/
noncomputable def sA (l s : ℝ) : ℝ := (1 + l - l * s ^ 2) / (2 + 2 * l * (1 - s))

/-- Eq. (3). The average skill of social group `D`, in closed form. -/
noncomputable def sD (l s : ℝ) : ℝ := (1 - l + l * s ^ 2) / (2 - 2 * l * (1 - s))

/-- Eq. (3), first form for `s_A`. Social group `A` holds all of `𝒜` (mean
skill 1/2) and the members of `𝒟_Y` above `s` (relative mass `λ(1 − s)`, mean
skill `(1 + s)/2`). The mean of these members is the paper's closed form. -/
theorem eq3_sA {l s : ℝ} (hl : 0 ≤ l) (hs : s ≤ 1) :
    (1 / 2 + l * (1 - s) * ((1 + s) / 2)) * (1 / (1 + l * (1 - s))) = sA l s := by
  unfold sA
  have h1 : 0 < 1 + l * (1 - s) := by nlinarith
  have h2 : (2 + 2 * l * (1 - s)) ≠ 0 := by nlinarith
  field_simp
  ring

/-- Eq. (3), first form for `s_D`. Social group `D` holds the agents of `𝒟`
below `s` (relative mass `s`, mean `s/2`) and the fraction `1 − λ` of those
above `s` (mean `(1 + s)/2`). The mean of these members is the closed form. -/
theorem eq3_sD {l s : ℝ} (hpos : 0 < 1 - l * (1 - s)) :
    (s * (s / 2) + (1 - l) * (1 - s) * ((1 + s) / 2)) * (1 / (s + (1 - l) * (1 - s)))
      = sD l s := by
  unfold sD
  have h1 : s + (1 - l) * (1 - s) ≠ 0 := by nlinarith
  have h2 : (2 - 2 * l * (1 - s)) ≠ 0 := by nlinarith
  field_simp
  ring

/-- Composition. The masses of the two social groups, `(1 ± λ(1 − s))/2`,
times their average skills sum to the population's total skill 1/2.
Assimilation moves agents between the groups and leaves the population mean
unchanged. -/
theorem total_mean {l s : ℝ} (hl : 0 ≤ l) (hs : s ≤ 1) (hpos : 0 < 1 - l * (1 - s)) :
    (1 + l * (1 - s)) / 2 * sA l s + (1 - l * (1 - s)) / 2 * sD l s = 1 / 2 := by
  unfold sA sD
  have h1 : (2 + 2 * l * (1 - s)) ≠ 0 := by nlinarith
  have h2 : (2 - 2 * l * (1 - s)) ≠ 0 := by nlinarith
  have h3 : 1 + l * (1 - s) ≠ 0 := by nlinarith
  have h4 : 1 - l * (1 - s) ≠ 0 := by nlinarith
  have e1 : (1 + l * (1 - s)) / 2 * ((1 + l - l * s ^ 2) / (2 + 2 * l * (1 - s)))
      = (1 + l - l * s ^ 2) / 4 := by
    rw [show 2 + 2 * l * (1 - s) = 2 * (1 + l * (1 - s)) by ring]
    field_simp
    ring
  have e2 : (1 - l * (1 - s)) / 2 * ((1 - l + l * s ^ 2) / (2 - 2 * l * (1 - s)))
      = (1 - l + l * s ^ 2) / 4 := by
    rw [show 2 - 2 * l * (1 - s) = 2 * (1 - l * (1 - s)) by ring]
    field_simp
    ring
  rw [e1, e2]
  ring

/-- Assimilation from above raises the average skill of social group `A`
above that of `𝒜`, 1/2, whenever some agents assimilate and the cutoff is
interior. -/
theorem sA_gt_half {l s : ℝ} (hl : 0 < l) (hs0 : 0 < s) (hs1 : s < 1) : 1 / 2 < sA l s := by
  unfold sA
  have hd : 0 < 2 + 2 * l * (1 - s) := by nlinarith
  rw [lt_div_iff₀ hd]
  nlinarith [mul_pos hl (mul_pos hs0 (sub_pos.2 hs1))]

/-- The same assimilation lowers the average skill of social group `D` below
1/2. The most able leave, and the group they leave is left with a lower mean. -/
theorem sD_lt_half {l s : ℝ} (hl : 0 < l) (hs0 : 0 < s) (hs1 : s < 1)
    (hpos : 0 < 1 - l * (1 - s)) : sD l s < 1 / 2 := by
  unfold sD
  have hd : 0 < 2 - 2 * l * (1 - s) := by nlinarith
  rw [div_lt_iff₀ hd]
  nlinarith [mul_pos hl (mul_pos hs0 (sub_pos.2 hs1))]

/-- Assimilation dilutes the average endowment of social group `A`. -/
theorem eAbar_lt {eA l s : ℝ} (he : 0 < eA) (hl : 0 < l) (hs1 : s < 1) :
    eAbar eA l s < eA := by
  unfold eAbar
  have hd : 1 < 1 + l * (1 - s) := by nlinarith [mul_pos hl (sub_pos.2 hs1)]
  rw [div_lt_iff₀ (by linarith)]
  nlinarith

/-- The derivative of `s_A` printed on p. 32, from eq. (3). -/
theorem sA_hasDerivAt {l s : ℝ} (h : 1 + l * (1 - s) ≠ 0) :
    HasDerivAt (sA l)
      ((-2 * l * s * (1 + l * (1 - s)) + l * (1 + l - l * s ^ 2)) / (2 * (1 + l * (1 - s)) ^ 2))
      s := by
  have hn : HasDerivAt (fun s : ℝ => 1 + l - l * s ^ 2) (-(l * (2 * s))) s := by
    have := ((hasDerivAt_pow 2 s).const_mul l).const_sub (1 + l)
    simpa using this
  have hd : HasDerivAt (fun s : ℝ => 2 + 2 * l * (1 - s)) (-(2 * l)) s := by
    have := (((hasDerivAt_id s).const_sub 1).const_mul (2 * l)).const_add 2
    simpa using this
  have hd0 : (2 + 2 * l * (1 - s)) ≠ 0 := by
    intro h'; apply h; linarith
  have hq : HasDerivAt (sA l)
      ((-(l * (2 * s)) * (2 + 2 * l * (1 - s)) - (1 + l - l * s ^ 2) * -(2 * l))
        / (2 + 2 * l * (1 - s)) ^ 2) s := hn.div hd hd0
  convert hq using 1
  field_simp
  ring

/-- Reading EG-D1. At `s = 1` the derivative of `s_A` is `−λ/2`, the value
the paper's own eq. (6) gives there, and not the printed `(−λ − λ²)/2`. -/
theorem sA_deriv_one (l : ℝ) : HasDerivAt (sA l) (-l / 2) 1 := by
  have := sA_hasDerivAt (l := l) (s := 1) (by norm_num)
  convert this using 1
  ring

/-- Reading EG-D1, continued. The coefficient on `∂v/∂s_A` in eq. (6) at
`s = 1` is `−λ/2`, which differs from the printed `(−λ − λ²)/2` whenever
`λ > 0`. -/
theorem eq6_at_one {l : ℝ} (hl : 0 < l) :
    -l * (((1 + l) * (2 * 1 - 1) - l * 1 ^ 2) / 2) = -l / 2 ∧
      -l / 2 ≠ (-l - l ^ 2) / 2 := by
  constructor
  · ring
  · intro h; nlinarith [sq_pos_of_pos hl]

/-- Screening (p. 27). With a cost `c` strictly decreasing in skill, some
difficulty `d ≥ 0` admits a more skilled agent and turns away a less skilled
one, for any positive gain `G` from joining. -/
theorem screen_exists {c : ℝ → ℝ} (hc : StrictAnti c) (hpos : ∀ x, 0 < c x) {G si sj : ℝ}
    (hG : 0 < G) (hji : sj < si) :
    ∃ d, 0 ≤ d ∧ 0 ≤ G - d * c si ∧ G - d * c sj < 0 := by
  have hci := hpos si
  have hcj := hpos sj
  have hlt : c si < c sj := hc hji
  refine ⟨G / c si, (div_pos hG hci).le, ?_, ?_⟩
  · rw [div_mul_cancel₀ _ hci.ne']; simp
  · have : G / c si * c sj > G := by
      rw [gt_iff_lt, div_mul_eq_mul_div, lt_div_iff₀ hci]
      nlinarith
    linarith

/-- Control (p. 27). A lump-sum barrier on an immutable trait, a cost `k` that
does not depend on skill, cannot screen. No difficulty admits one agent and
turns away another. -/
theorem control_lump_sum_no_screen {G k : ℝ} :
    ¬ ∃ d : ℝ, 0 ≤ G - d * k ∧ G - d * k < 0 := by
  rintro ⟨d, h1, h2⟩
  linarith

/-- Control. Without mobility (`λ = 0`) nothing moves, and both social groups
keep the mean skill 1/2. `sD_lt_half` needs `λ > 0`. -/
theorem control_no_mobility (s : ℝ) : sA 0 s = 1 / 2 ∧ sD 0 s = 1 / 2 := by
  unfold sA sD
  constructor <;> norm_num

/-- Control. With the cutoff at 0, all of `𝒟_Y` assimilates, and the mean
skill of social group `A` stays 1/2. `sA_gt_half` needs `s > 0`. -/
theorem control_cutoff_zero {l : ℝ} (hl : 0 ≤ l) : sA l 0 = 1 / 2 := by
  unfold sA
  have : (2 + 2 * l * (1 - 0)) ≠ 0 := by nlinarith
  field_simp
  ring

end Eguia2013
