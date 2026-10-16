import Mathlib

/-! Chetty et al. (2022), "Social capital II: determinants of economic
connectedness", *Nature* 608, 122–134, Methods, in the paper's notation.

`wH` is the population share of high-SES people (0.5), `wHg` the share of
high-SES members of group `g`, `fHig` the share of high-SES friends that
individual `i` makes in group `g`, and `φ g` the share of `i`'s friends made in
group `g`. `IEC` is eq. (1), `Exposure` eq. (3), `FriendingBias` eq. (4), and
`eq2` is the decomposition (2). `xg` is the ratio of the number of friends of
high-SES to low-SES people in a group, as in eq. (5).

The no-homophily benchmark (5) is printed without a model of friending. We read
"the same probability of befriending a given high-SES person" with high-SES
people having `xg` times as many friends as giving a low-SES person the high-SES
friend share `wHg xg / (wHg xg + (1 − wHg))`, and `eq5` shows that this share
gives exactly the printed formula. That reading is ours.

Every result is a definition or an identity the paper states, or an arithmetic
check of a number it prints. Nothing here models why friending bias arises,
because the paper does not. -/

namespace ChettyEtAl2022b

open Finset

/-- Eq. (1). Individual economic connectedness, `IEC_{H,i} = f_{H,i} / w_H`. -/
noncomputable def IEC (fHi wH : ℝ) : ℝ := fHi / wH

/-- Eq. (3). Exposure of group `g`, `w_{H,g} / w_H`. -/
noncomputable def Exposure (wHg wH : ℝ) : ℝ := wHg / wH

/-- Eq. (4). Friending bias of `i` in group `g`, `1 − f_{H,i,g} / w_{H,g}`. -/
noncomputable def FriendingBias (fHig wHg : ℝ) : ℝ := 1 - fHig / wHg

/-- Eq. (2). When `i`'s share of high-SES friends is the friend-share-weighted
sum of the group shares, `f_{H,i} = Σ_g φ_{i,g} f_{H,i,g}`, individual EC is the
sum over groups of friend share times exposure times one minus friending bias.
Groups with `w_{H,g} = 0` are excluded, as the paper does (`hw`). -/
theorem eq2 {G : Type*} (s : Finset G) (φ fHig wHg : G → ℝ) {wH : ℝ}
    (hwH : wH ≠ 0) (hw : ∀ g ∈ s, wHg g ≠ 0) :
    IEC (∑ g ∈ s, φ g * fHig g) wH
      = ∑ g ∈ s, φ g * Exposure (wHg g) wH * (1 - FriendingBias (fHig g) (wHg g)) := by
  unfold IEC Exposure FriendingBias
  rw [Finset.sum_div]
  refine Finset.sum_congr rfl (fun g hg => ?_)
  have h := hw g hg
  field_simp
  ring

/-- Eq. (6) and (7). Recomposed EC is exposure times one minus bias, and its log
is the sum of the two logs. -/
theorem eq7 {E B : ℝ} (hE : 0 < E) (hB : B < 1) :
    Real.log (E * (1 - B)) = Real.log E + Real.log (1 - B) :=
  Real.log_mul (ne_of_gt hE) (by linarith)

/-- Random friending with equal numbers of friends gives zero friending bias:
`f_{H,i,g} = w_{H,g}` (Methods, after eq. (4)). -/
theorem bias_zero_of_random {wHg : ℝ} (h : wHg ≠ 0) : FriendingBias wHg wHg = 0 := by
  unfold FriendingBias; rw [div_self h]; ring

/-- The low-SES person's high-SES friend share under our reading of "no
homophily" with high-SES people having `xg` times as many friends. -/
noncomputable def shareNoHomophily (wHg xg : ℝ) : ℝ := wHg * xg / (wHg * xg + (1 - wHg))

/-- Eq. (5). Under that share, friending bias equals the printed
`(1 − xg) / (1 + (wHg/(1 − wHg)) xg)`. -/
theorem eq5 {wHg xg : ℝ} (h0 : 0 < wHg) (h1 : wHg < 1) (hx : 0 < xg) :
    FriendingBias (shareNoHomophily wHg xg) wHg
      = (1 - xg) / (1 + wHg / (1 - wHg) * xg) := by
  unfold FriendingBias shareNoHomophily
  have ha : 0 < 1 - wHg := by linarith
  have hb : 0 < wHg * xg + (1 - wHg) := by positivity
  have hc : 0 < 1 + wHg / (1 - wHg) * xg := by positivity
  field_simp
  ring

/-- Eq. (5). With `xg > 1` the benchmark friending bias is negative. -/
theorem eq5_neg {wHg xg : ℝ} (h0 : 0 < wHg) (h1 : wHg < 1) (hx : 1 < xg) :
    (1 - xg) / (1 + wHg / (1 - wHg) * xg) < 0 := by
  have ha : 0 < 1 - wHg := by linarith
  have hc : 0 < 1 + wHg / (1 - wHg) * xg := by
    have : 0 < wHg / (1 - wHg) * xg := by positivity
    linarith
  exact div_neg_of_neg_of_pos (by linarith) hc

/-- The printed benchmark. With `xg = 1.254` and `wHg = 0.5` friending bias is
`−0.254/2.254`, which lies in `(−0.12, −0.11)`, "−0.11" in the text. With the
high-school ratio `xg = 1.22` it is `−0.22/2.22`, in `(−0.1, −0.09)`, "−0.10". -/
theorem benchmark_values :
    (1 - (1.254 : ℝ)) / (1 + 0.5 / (1 - 0.5) * 1.254) = -0.254 / 2.254 ∧
      -0.12 < (-0.254 : ℝ) / 2.254 ∧ (-0.254 : ℝ) / 2.254 < -0.11 ∧
      (1 - (1.22 : ℝ)) / (1 + 0.5 / (1 - 0.5) * 1.22) = -0.22 / 2.22 ∧
      -0.1 < (-0.22 : ℝ) / 2.22 ∧ (-0.22 : ℝ) / 2.22 < -0.09 := by
  refine ⟨by norm_num, by norm_num, by norm_num, by norm_num, by norm_num, by norm_num⟩

/-- Control for `bias_zero_of_random` and `eq5`. Without equal numbers of
friends, random friending does not give zero bias. Under the no-homophily share,
friending bias is zero exactly when `xg = 1`. -/
theorem control_bias_zero_iff_equal_friends {wHg xg : ℝ} (h0 : 0 < wHg) (h1 : wHg < 1)
    (hx : 0 < xg) : FriendingBias (shareNoHomophily wHg xg) wHg = 0 ↔ xg = 1 := by
  rw [eq5 h0 h1 hx]
  have ha : 0 < 1 - wHg := by linarith
  have hc : 0 < 1 + wHg / (1 - wHg) * xg := by positivity
  rw [div_eq_zero_iff]
  constructor
  · rintro (h | h)
    · linarith
    · linarith
  · intro h; left; linarith

/-! ### The decomposition of Fig. 3, in the representative-agent form (14)

With two settings collapsed to one, recomposed EC is `φ · E · (1 − B)`. The share
of a gap attributed to a factor is the change in EC when that factor is equated,
divided by the whole gap. -/

/-- Control for the decomposition's shares. The step from equating exposure
depends on the level of friending bias at which it is taken. Unless one of the
two factors does not differ, the two orders of Fig. 3a attribute different
amounts to exposure, which is why the paper reports both. -/
theorem control_order_matters {φ E0 E1 B0 B1 : ℝ} (hφ : φ ≠ 0) (hE : E0 ≠ E1)
    (hB : B0 ≠ B1) :
    φ * E1 * (1 - B0) - φ * E0 * (1 - B0) ≠ φ * E1 * (1 - B1) - φ * E0 * (1 - B1) := by
  intro h
  have : φ * (E1 - E0) * (B1 - B0) = 0 := by linarith
  rcases mul_eq_zero.1 this with h1 | h1
  · rcases mul_eq_zero.1 h1 with h2 | h2
    · exact hφ h2
    · exact hE (by linarith)
  · exact hB (by linarith)

/-- Fig. 3a, the bars `0.826, 0.910, 1.209, 1.150, 1.534`. Equating friending
rates closes 12% of the gap. Exposure taken after friending rates brings the
cumulative share to 54%, and bias then closes the remaining 46% (main text,
exposure first). Taking bias first, bias explains 34% and exposure 54%
(Methods). Exposure alone in the first order explains 42%. -/
theorem fig3a_shares :
    let gap : ℝ := 1.534 - 0.826
    (0.11 < (0.910 - 0.826) / gap ∧ (0.910 - 0.826) / gap < 0.12) ∧
    (0.54 < (1.209 - 0.826) / gap ∧ (1.209 - 0.826) / gap < 0.545) ∧
    (0.455 < (1.534 - 1.209) / gap ∧ (1.534 - 1.209) / gap < 0.46) ∧
    (0.335 < (1.150 - 0.910) / gap ∧ (1.150 - 0.910) / gap < 0.345) ∧
    (0.54 < (1.534 - 1.150) / gap ∧ (1.534 - 1.150) / gap < 0.545) ∧
    (0.42 < (1.209 - 0.910) / gap ∧ (1.209 - 0.910) / gap < 0.425) := by
  norm_num

/-- Fig. 3b, the bars `0.520, 0.597, 1.080, 0.680, 1.222`. The printed 73% for
exposure and 16% for friending bias are the means of the shares in the two
orders. Friending rates explain 11%. -/
theorem fig3b_average_of_orders :
    let gap : ℝ := 1.222 - 0.520
    (0.725 < ((1.080 - 0.597) + (1.222 - 0.680)) / 2 / gap ∧
      ((1.080 - 0.597) + (1.222 - 0.680)) / 2 / gap < 0.735) ∧
    (0.155 < ((1.222 - 1.080) + (0.680 - 0.597)) / 2 / gap ∧
      ((1.222 - 1.080) + (0.680 - 0.597)) / 2 / gap < 0.165) ∧
    (0.105 < (0.597 - 0.520) / gap ∧ (0.597 - 0.520) / gap < 0.115) := by
  norm_num

/-- Control for `fig3b_average_of_orders`. Neither order alone gives 73% for
exposure. Exposure first gives under 69%, bias first over 77%. -/
theorem control_fig3b_single_order :
    let gap : ℝ := 1.222 - 0.520
    (1.080 - 0.597) / gap < 0.69 ∧ 0.77 < (1.222 - 0.680) / gap := by
  norm_num

/-- Total contribution to connectedness, read as the share of low-SES students
times their EC, `(1 − w) · (w / w_H) · (1 − B)` with `w_H = 1/2`. Holding bias
fixed, a high-SES share of one half maximises it, as the text says. -/
theorem tcec_max_at_half {w B : ℝ} (hB : B < 1) :
    (1 - w) * (w / (1 / 2)) * (1 - B) ≤ (1 - 1 / 2) * ((1 / 2) / (1 / 2)) * (1 - B) := by
  have h1 : 0 < 1 - B := by linarith
  have key : (1 - w) * (w / (1 / 2)) ≤ (1 - 1 / 2) * ((1 / 2) / (1 / 2)) := by
    nlinarith [sq_nonneg (w - 1 / 2)]
  exact mul_le_mul_of_nonneg_right key h1.le

/-- Control for `tcec_max_at_half`. Taken literally with the normalised exposure
`E = 2w` in both factors, the printed `(1 − E) · E · (1 − B)` is higher at a
high-SES share of one quarter than at one half, and negative at three quarters.
So the claim that a 50% share maximises TCEC needs the first factor to be the
low-SES share. -/
theorem control_tcec_printed {B : ℝ} (hB : B < 1) :
    (1 - 2 * (1 / 2 : ℝ)) * (2 * (1 / 2)) * (1 - B)
        < (1 - 2 * (1 / 4 : ℝ)) * (2 * (1 / 4)) * (1 - B) ∧
      (1 - 2 * (3 / 4 : ℝ)) * (2 * (3 / 4)) * (1 - B) < 0 := by
  have h1 : 0 < 1 - B := by linarith
  constructor <;> nlinarith

/-- Fig. 6. A slope of 0.89 is a marginal friending bias of 0.11, and the
attenuation-corrected slope `0.61/0.67` lies in `(0.91, 0.911)`. -/
theorem fig6_numbers :
    1 - (0.89 : ℝ) = 0.11 ∧ 0.91 < (0.61 : ℝ) / 0.67 ∧ (0.61 : ℝ) / 0.67 < 0.911 := by
  norm_num

/-! ### Further displayed claims and arithmetic of printed numbers -/

/-- PDF p. 2. EC of 0.776 is an under-representation of 22.4%. Fig. 3a. The gap
between the representative low- and high-SES persons is 0.708, "0.7". -/
theorem gap_numbers : 1 - (0.776 : ℝ) = 0.224 ∧ (1.534 : ℝ) - 0.826 = 0.708 := by
  norm_num

/-- PDF p. 3 with Extended Data Table 4. "About 20% more likely to befriend a
given high-SES person in their religious groups than in their neighbourhoods"
matches the difference in friending bias, `0.166 − (−0.032) = 0.198`. The ratio
of befriending rates at fixed exposure, `(1 − B_rel)/(1 − B_nbhd)`, is 1.24. -/
theorem religious_vs_neighbourhood :
    (0.166 : ℝ) - (-0.032) = 0.198 ∧
      1.23 < (1 - (-0.032 : ℝ)) / (1 - 0.166) ∧ (1 - (-0.032 : ℝ)) / (1 - 0.166) < 1.24 := by
  norm_num

/-- Eq. (6). Lowering friending bias by `d` at exposure `E` raises recomposed EC
by `E · d`, so a cut in bias is worth more where exposure is higher. -/
theorem bias_cut_raises_ec (E B d : ℝ) : E * (1 - (B - d)) - E * (1 - B) = E * d := by ring

/-- PDF p. 16. "Lower friending bias and greater exposure are complements, so a
factor has the largest effect if it is equated last." Raising exposure from
`E0` to `E1` adds more at the lower bias `B1` than at the higher bias `B0`. -/
theorem complements_last_largest {φ E0 E1 B0 B1 : ℝ} (hφ : 0 ≤ φ) (hE : E0 ≤ E1)
    (hB : B1 ≤ B0) :
    φ * E1 * (1 - B0) - φ * E0 * (1 - B0) ≤ φ * E1 * (1 - B1) - φ * E0 * (1 - B1) := by
  have : 0 ≤ φ * (E1 - E0) * (B0 - B1) := by
    apply mul_nonneg (mul_nonneg hφ (by linarith)) (by linarith)
  nlinarith

/-- Extended Data Table 1. Exposure varies 3.3 times more across counties than
across settings (0.30/0.09), and friending bias 3.3 times more across settings
than across counties (0.13/0.04 = 3.25). -/
theorem variance_ratios :
    3.3 < (0.30 : ℝ) / 0.09 ∧ (0.30 : ℝ) / 0.09 < 3.34 ∧ (0.13 : ℝ) / 0.04 = 3.25 := by
  norm_num

/-- Table 1 and PDF p. 7. The specification of column 1 regresses log upward
mobility on log EC, column 2 on log exposure and log(1 − bias). -/
noncomputable def spec1 (a b E B : ℝ) : ℝ := a + b * Real.log (E * (1 - B))

noncomputable def spec2 (a bE bB E B : ℝ) : ℝ := a + bE * Real.log E + bB * Real.log (1 - B)

/-- By eq. (7), column 1 is column 2 with equal coefficients on the two logs. -/
theorem spec1_is_restricted_spec2 {a b E B : ℝ} (hE : 0 < E) (hB : B < 1) :
    spec1 a b E B = spec2 a b b E B := by
  unfold spec1 spec2; rw [eq7 hE hB]; ring

/-- Control for `spec1_is_restricted_spec2`. With unequal coefficients, as
printed in column 2 (0.248 and 0.185), no single elasticity reproduces column 2
at both of two points that differ only in exposure and only in bias. -/
theorem control_spec_unequal {a bE bB : ℝ} (h : bE ≠ bB) :
    ¬ ∃ b, spec1 a b (Real.exp 1) 0 = spec2 a bE bB (Real.exp 1) 0 ∧
      spec1 a b 1 (1 - Real.exp 1) = spec2 a bE bB 1 (1 - Real.exp 1) := by
  rintro ⟨b, h1, h2⟩
  unfold spec1 spec2 at h1 h2
  simp [Real.log_exp] at h1 h2
  exact h (by linarith)

/-- PDF p. 7. An elasticity of 0.236 maps a 10% rise in EC to about 2.4%. -/
theorem elasticity_p7 : (0.236 : ℝ) * 10 = 2.36 := by norm_num

/-- PDF p. 8. A reliability of 0.58 turns a 10% higher estimate into 5.8%. -/
theorem reliability_p8 : (0.58 : ℝ) * 10 = 5.8 := by norm_num

/-- PDF p. 9. Average parental EC of 0.92 is 8% under-representation, and the
0.15 that bias costs at ETHS is "almost twice" 0.08. -/
theorem eths_p9 : 1 - (0.92 : ℝ) = 0.08 ∧ 1.8 < (0.15 : ℝ) / 0.08 ∧ (0.15 : ℝ) / 0.08 < 2 := by
  norm_num

/-! ### The cross-cohort design (Methods, Fig. 6) -/

/-- The cohort deviation, the value in cohort `c` minus the weighted mean over
the other cohorts `O` with weights `p` (Methods, "Cross-cohort estimates"). -/
def dev {ι : Type*} (x : ι → ℝ) (O : Finset ι) (p : ι → ℝ) (c : ι) : ℝ :=
  x c - ∑ j ∈ O, p j * x j

/-- If within a school EC in every cohort is exposure times one minus a bias `b`
common to the cohorts, the deviation in EC is `(1 − b)` times the deviation in
exposure. The slope of Fig. 6a is then one minus friending bias, the reading
"slope of 0.89 implies marginal friending bias of 0.11". -/
theorem cohort_slope {ι : Type*} (EC E : ι → ℝ) (O : Finset ι) (p : ι → ℝ) (c : ι) (b : ℝ)
    (h : ∀ j, EC j = E j * (1 - b)) : dev EC O p c = (1 - b) * dev E O p c := by
  unfold dev
  have hs : ∑ j ∈ O, p j * EC j = (1 - b) * ∑ j ∈ O, p j * E j := by
    rw [Finset.mul_sum]
    exact Finset.sum_congr rfl (fun j _ => by rw [h j]; ring)
  rw [hs, h c]; ring

/-- Control for `cohort_slope`. If bias differs across cohorts, the ratio of the
deviations need not be one minus any cohort's bias. Two cohorts, exposure 1 and
2, bias 0.1 in the first and 0.3 in the second, give a ratio of 0.5. -/
theorem control_cohort_varying_bias :
    let E : Fin 2 → ℝ := ![1, 2]
    let b : Fin 2 → ℝ := ![0.1, 0.3]
    let EC : Fin 2 → ℝ := fun j => E j * (1 - b j)
    dev EC {0} (fun _ => 1) 1 / dev E {0} (fun _ => 1) 1 = 0.5 := by
  simp [dev]
  norm_num

/-- Fig. 6b. If the impact of exposure falls one for one with true cohort bias,
`impact = κ − b`, and true bias is `λ b̂ + μ` in the leave-out estimate `b̂`,
the slope on `b̂` is `−λ`. With a slope of −0.61 and `λ = 0.67`, the implied
slope on true bias is `−0.61/0.67`. -/
theorem attenuation {κ lam μ β bhat : ℝ} :
    κ - β * (lam * bhat + μ) = (κ - β * μ) + (-(β * lam)) * bhat := by ring

/-! ### The regression discontinuity design (Methods, Extended Data Fig. 4) -/

/-- The side of the entry cut-off with greater high-SES exposure. -/
noncomputable def T (d : ℝ) : ℝ := if 0 < d then 1 else 0

/-- The specification, `EC_d = β0 + β1 T_d + β2 d + β3 T_d d`. -/
noncomputable def rdSpec (β0 β1 β2 β3 d : ℝ) : ℝ := β0 + β1 * T d + β2 * d + β3 * T d * d

/-- The jump in the specification at the cut-off is `β1`. -/
theorem rd_jump (β0 β1 β2 β3 : ℝ) :
    Filter.Tendsto (rdSpec β0 β1 β2 β3) (nhdsWithin 0 (Set.Ioi 0)) (nhds (β0 + β1)) ∧
      rdSpec β0 β1 β2 β3 0 = β0 := by
  constructor
  · have hc : Continuous (fun d : ℝ => β0 + β1 + (β2 + β3) * d) := by continuity
    have h0 : Filter.Tendsto (fun d : ℝ => β0 + β1 + (β2 + β3) * d) (nhds 0)
        (nhds (β0 + β1)) := by
      have := hc.tendsto 0; simpa using this
    refine (h0.mono_left nhdsWithin_le_nhds).congr' ?_
    filter_upwards [self_mem_nhdsWithin] with d hd
    have : (0 : ℝ) < d := hd
    simp [rdSpec, T, this]; ring
  · simp [rdSpec, T]

/-- Identification. If EC is a function `g` continuous at the cut-off plus the
effect `τ` of the exposure jump plus a jump `γ` in any other determinant, the
jump at the cut-off is `τ + γ`. With `γ = 0`, the identifying assumption, the
design recovers `τ`. -/
theorem rd_identifies (g : ℝ → ℝ) (hg : ContinuousAt g 0) (τ γ : ℝ) :
    Filter.Tendsto (fun d => g d + (τ + γ) * T d) (nhdsWithin 0 (Set.Ioi 0))
      (nhds (g 0 + (τ + γ))) ∧ g 0 + (τ + γ) * T 0 = g 0 := by
  constructor
  · have h1 : Filter.Tendsto g (nhdsWithin 0 (Set.Ioi 0)) (nhds (g 0)) :=
      hg.tendsto.mono_left nhdsWithin_le_nhds
    have h2 : Filter.Tendsto (fun d => (τ + γ) * T d) (nhdsWithin 0 (Set.Ioi 0))
        (nhds (τ + γ)) := by
      refine tendsto_const_nhds.congr' ?_
      filter_upwards [self_mem_nhdsWithin] with d hd
      have : (0 : ℝ) < d := hd
      simp [T, this]
    exact h1.add h2
  · simp [T]

/-- Control for `rd_identifies`. If another determinant jumps at the cut-off,
`γ ≠ 0`, the jump is not the effect of exposure. -/
theorem control_rd_other_jump {τ γ : ℝ} (hγ : γ ≠ 0) : τ + γ ≠ τ := by
  intro h; exact hγ (by linarith)

/-- Extended Data Fig. 4a. The jumps of 0.39 and 0.33 differ by 0.06. -/
theorem rd_numbers : (0.39 : ℝ) - 0.33 = 0.06 := by norm_num

/-! ### The puzzle in the source's measure

The no-homophily benchmark of eq. (5) depends on the group's high-SES share.
Taking `w = Exposure/2` from Extended Data Table 4 and the national ratio of
friend counts `x = 1.254` (the setting ratios are not printed), every setting's
mean friending bias for low-SES people lies above its benchmark, and for
high-SES people below it except in recreational groups, where it is at the
benchmark. These are our computations from printed means, and a mean of
individual ratios is not the ratio at the mean. -/

/-- The benchmark of eq. (5) as a function of the group share `w` and ratio `x`. -/
noncomputable def bench (w x : ℝ) : ℝ := (1 - x) / (1 + w / (1 - w) * x)

/-- Low-SES people. Extended Data Table 4, columns 3 and 4, against `bench`.
The excess over the benchmark is about 0.16 in high schools, 0.13 in colleges,
0.17 at employers, 0.15 in recreational groups, 0.10 in religious groups and
0.30 in neighbourhoods. -/
theorem excess_low :
    (0.16 < 0.043 - bench (0.935 / 2) 1.254 ∧ 0.043 - bench (0.935 / 2) 1.254 < 0.17) ∧
    (0.13 < 0.047 - bench (1.223 / 2) 1.254 ∧ 0.047 - bench (1.223 / 2) 1.254 < 0.14) ∧
    (0.17 < 0.054 - bench (0.961 / 2) 1.254 ∧ 0.054 - bench (0.961 / 2) 1.254 < 0.18) ∧
    (0.14 < 0.025 - bench (0.924 / 2) 1.254 ∧ 0.025 - bench (0.924 / 2) 1.254 < 0.15) ∧
    (0.09 < -0.032 - bench (0.864 / 2) 1.254 ∧ -0.032 - bench (0.864 / 2) 1.254 < 0.10) ∧
    (0.30 < 0.166 - bench (0.820 / 2) 1.254 ∧ 0.166 - bench (0.820 / 2) 1.254 < 0.31) := by
  unfold bench; norm_num

/-- High-SES people. Extended Data Table 4, columns 7 and 8. Friending bias
toward high-SES friends is below the benchmark in five settings. -/
theorem high_below_bench :
    -0.200 < bench (1.246 / 2) 1.254 ∧ -0.102 < bench (1.515 / 2) 1.254 ∧
      -0.133 < bench (1.334 / 2) 1.254 ∧ -0.147 < bench (1.300 / 2) 1.254 ∧
      -0.282 < bench (1.174 / 2) 1.254 := by
  unfold bench; norm_num

/-- Control for `high_below_bench`. In recreational groups high-SES bias, −0.066,
is within 0.001 of its benchmark, so the pattern does not hold in every
setting. -/
theorem control_rec_high_at_bench :
    |(-0.066 : ℝ) - bench (1.395 / 2) 1.254| < 0.001 := by
  unfold bench; rw [abs_lt]; constructor <;> norm_num

/-! ### The racial diversity index and the authors' suggested mechanism -/

/-- Extended Data Fig. 1e caption. The index `1 − Σ s_i²` over five shares. -/
def diversity (s : Fin 5 → ℝ) : ℝ := 1 - ∑ i, s i ^ 2

/-- With five shares summing to one the index is at most 0.8, reached at equal
shares, which is why the axes of Extended Data Figs. 1e and 3 end at 0.8. -/
theorem diversity_le (s : Fin 5 → ℝ) (h : ∑ i, s i = 1) :
    diversity s ≤ 4 / 5 ∧ diversity (fun _ => 1 / 5) = 4 / 5 := by
  constructor
  · unfold diversity
    simp only [Fin.sum_univ_five] at h ⊢
    nlinarith [sq_nonneg (s 0 - 1/5), sq_nonneg (s 1 - 1/5), sq_nonneg (s 2 - 1/5),
      sq_nonneg (s 3 - 1/5), sq_nonneg (s 4 - 1/5)]
  · unfold diversity; simp; norm_num

/-- Our reading of CB-Q7 as a model. A group has two races with shares `πA` and
`1 − πA` and high-SES shares `hA`, `hB` within them. Friendship forms only
within race, at random, with equal friend counts. A low-SES member of race A then
has high-SES friend share `hA`, and mean friending bias over the group's low-SES
members is one minus their mean high-SES friend share over `w`. -/
noncomputable def raceOnlyBias (πA hA hB : ℝ) : ℝ :=
  let w := πA * hA + (1 - πA) * hB
  1 - (πA * (1 - hA) * hA + (1 - πA) * (1 - hB) * hB) / ((1 - w) * w)

/-- Under that model mean friending bias is `πA πB (hA − hB)² / (w (1 − w))`.
It is positive whenever the races differ in SES, and it is proportional to the
two-race diversity index `1 − πA² − πB² = 2 πA πB` at a given SES gap and `w`. -/
theorem raceOnlyBias_formula {πA hA hB : ℝ}
    (hw0 : 0 < πA * hA + (1 - πA) * hB) (hw1 : πA * hA + (1 - πA) * hB < 1) :
    raceOnlyBias πA hA hB
      = (1 - πA ^ 2 - (1 - πA) ^ 2) / 2 * (hA - hB) ^ 2
          / ((πA * hA + (1 - πA) * hB) * (1 - (πA * hA + (1 - πA) * hB))) := by
  unfold raceOnlyBias
  have h1 : 0 < 1 - (πA * hA + (1 - πA) * hB) := by linarith
  field_simp
  ring

/-- Control for `raceOnlyBias_formula`. If the races have the same high-SES share,
race-only friending produces no SES friending bias, however diverse the group. -/
theorem control_raceOnly_no_gap {πA h : ℝ} (h0 : 0 < h) (h1 : h < 1) :
    raceOnlyBias πA h h = 0 := by
  unfold raceOnlyBias
  have e : πA * h + (1 - πA) * h = h := by ring
  simp only [e]
  have : (1 - h) * h ≠ 0 := mul_ne_zero (by linarith) (by linarith)
  rw [show πA * (1 - h) * h + (1 - πA) * (1 - h) * h = (1 - h) * h by ring, div_self this]
  ring

/-- Control for `diversity_le`. Without shares summing to one the bound fails.
Five zero shares give an index of 1. -/
theorem control_diversity_needs_shares : 4 / 5 < diversity (fun _ => 0) := by
  unfold diversity; simp; norm_num

end ChettyEtAl2022b
