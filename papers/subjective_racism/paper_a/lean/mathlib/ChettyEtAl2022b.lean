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

end ChettyEtAl2022b
