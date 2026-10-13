import Mathlib

/-!
# Fryer and Loury, "Valuing Identity" (NBER Working Paper 16568, 2010)

Published as "Valuing Diversity", Journal of Political Economy 121(4), 747-774 (2013);
the working paper is what was read.  Page numbers are the printed pages.

Setup (pp.5-7): a continuum of agents in groups `a`, `b` with population shares `λ_a`,
`λ_b`; an agent of group `i` draws an effort cost from `G_i` and chooses `e ∈ {0,1}`;
productivity has law `F_1` after effort and `F_0` without; slots are assigned top-down
through a market.  With a fraction `π` exerting effort the productivity law is
`F(π, µ) = π F_1(µ) + (1 - π) F_0(µ)`.

## What is proved

1. The mixture algebra of p.7 and the group gap of p.11:
   `s_b(z) - s_a(z) = (π_a - π_b) ΔF > 0` (`group_gap`, `group_gap_pos`), from a cost
   order when both groups face the same return (`group_gap_of_costs`).
2. Lemma 1 (p.8), the exchange step: a surplus-maximising assignment is monotone
   (`assignment_monotone`).
3. Corollary 1 (p.9): `dW*/dµ = β(Z(µ))` (`cor1`).
4. The step in Theorem 2's proof (p.11): the private return to effort is
   `∫ W* (f_1 - f_0) = ∫ (dW*/dµ) ΔF` when `ΔF` vanishes at both ends of the support
   (`gain_by_parts`).
5. The two-job version (pp.20-22): the rationing probability (eq. (14)), the
   representation constraint (eq. (17)), the derivative and sign in footnote 12
   (`R_hasDerivAt`, `R_deriv_pos`).
6. Not in the paper, for comparison with M31: in the two-job version the investors' share
   of slots is `π` when effort does not change productivity at the margin, and exceeds
   `π` when it raises it (`investor_share_eq`, `investor_share_gt`).

## Not formalised

That the likelihood-ratio orders of Assumptions 1 and 2 imply the distribution orders
(p.6-7; used here as hypotheses), Theorem 1's payment schedule, the change of variables
completing Theorem 2, Lemma 2, Theorems 3 and 4, and Proposition 1.
-/

open MeasureTheory intervalIntegral

namespace FryerLoury2010

/-! ## 1. Mixtures and the group gap (pp.7 and 11) -/

/-- `F(π, µ) = π F_1(µ) + (1 - π) F_0(µ)` at one productivity level. -/
def mix (π F1 F0 : ℝ) : ℝ := π * F1 + (1 - π) * F0

/-- p.7: the population law is the mixture at the population effort rate. -/
theorem pop_mix (la lb πa πb F1 F0 : ℝ) (hl : la + lb = 1) :
    la * mix πa F1 F0 + lb * mix πb F1 F0 = mix (la * πa + lb * πb) F1 F0 := by
  unfold mix
  have : lb = 1 - la := by linarith
  subst this
  ring

/-- p.11: the gap between the groups' cumulative shares at any slot. -/
theorem group_gap (πa πb F1 F0 : ℝ) :
    mix πb F1 F0 - mix πa F1 F0 = (πa - πb) * (F0 - F1) := by
  unfold mix; ring

theorem group_gap_pos (πa πb F1 F0 : ℝ) (hπ : πb < πa) (hF : F1 < F0) :
    0 < mix πb F1 F0 - mix πa F1 F0 := by
  rw [group_gap]
  exact mul_pos (by linarith) (by linarith)

/-- Eqs. (16) and p.11: with a common return `R` group `i` invests at rate `G_i(R)`, so a
cost order `G_b(R) < G_a(R)` gives the group gap. -/
theorem group_gap_of_costs (Ga Gb : ℝ → ℝ) (R F1 F0 : ℝ) (hG : Gb R < Ga R)
    (hF : F1 < F0) : 0 < mix (Gb R) F1 F0 - mix (Ga R) F1 F0 :=
  group_gap_pos _ _ _ _ hG hF

/-! ## 2. Lemma 1: the exchange step (p.8) -/

/-- If swapping the slots of two agents cannot raise surplus, the more productive one holds
the higher slot. -/
theorem assignment_monotone (β : ℝ → ℝ) (hβ : StrictMono β) {x x' z z' : ℝ} (hx : x' < x)
    (hswap : 0 ≤ (x - x') * (β z - β z')) : z' ≤ z := by
  by_contra h
  push_neg at h
  have : β z < β z' := hβ h
  have : (x - x') * (β z - β z') < 0 := mul_neg_of_pos_of_neg (by linarith) (by linarith)
  linarith

/-! ## 3. Corollary 1 (p.9) -/

/-- With `W(µ) = µ β(Z(µ)) - P(µ)` and the payment rule `P' = µ (β ∘ Z)'` of eq. (5),
`W' = β(Z(µ))`. -/
theorem cor1 (βZ P : ℝ → ℝ) (m b' : ℝ) (hβZ : HasDerivAt βZ b' m)
    (hP : HasDerivAt P (m * b') m) :
    HasDerivAt (fun x => x * βZ x - P x) (βZ m) m := by
  have h := ((hasDerivAt_id m).mul hβZ).sub hP
  refine h.congr_deriv ?_
  simp only [id]
  ring

/-! ## 4. The private return to effort (proof of Theorem 2, p.11) -/

/-- Integration by parts: the return to effort `∫ W (f_1 - f_0)` equals `∫ W' ΔF` with
`ΔF = F_0 - F_1`, when `ΔF` vanishes at both ends of the support `[0, M]`. -/
theorem gain_by_parts (W W' dF ddF : ℝ → ℝ) (M : ℝ)
    (hW : ∀ x ∈ Set.uIcc 0 M, HasDerivAt W (W' x) x)
    (hdF : ∀ x ∈ Set.uIcc 0 M, HasDerivAt dF (ddF x) x)
    (hW' : IntervalIntegrable W' volume 0 M) (hddF : IntervalIntegrable ddF volume 0 M)
    (h0 : dF 0 = 0) (hM : dF M = 0) :
    ∫ x in (0 : ℝ)..M, W x * (-ddF x) = ∫ x in (0 : ℝ)..M, W' x * dF x := by
  have h := integral_mul_deriv_eq_deriv_mul hW hdF hW' hddF
  rw [h0, hM, mul_zero, mul_zero, sub_zero, zero_sub] at h
  have hneg : ∫ x in (0 : ℝ)..M, W x * (-ddF x) = -∫ x in (0 : ℝ)..M, W x * ddF x := by
    rw [← intervalIntegral.integral_neg]
    congr 1
    funext x
    ring
  rw [hneg, h, neg_neg]

/-! ## 5. The two-job version (pp.20-22) -/

/-- Eq. (14): the probability that a willing buyer gets a slot under a price ceiling. -/
noncomputable def alphaC (θ F : ℝ) : ℝ := θ / (1 - F)

theorem alphaC_lt_one (θ F : ℝ) (hθ : 0 < θ) (hbind : θ < 1 - F) : alphaC θ F < 1 := by
  unfold alphaC
  rw [div_lt_one (by linarith)]
  exact hbind

/-- Eq. (17): the blind regulator's representation constraint. -/
def Rcon (ρb θ F Fb : ℝ) : ℝ := ρb * (1 - F) - θ * (1 - Fb)

/-- Eq. (17) says the target rate equals the rationing probability times group `b`'s
willing-buyer rate. -/
theorem Rcon_zero_iff (ρb θ F Fb : ℝ) (h : 1 - F ≠ 0) :
    Rcon ρb θ F Fb = 0 ↔ ρb = alphaC θ F * (1 - Fb) := by
  unfold Rcon alphaC
  constructor
  · intro h0; field_simp; linarith
  · intro h0; rw [h0]; field_simp; ring

/-- Footnote 12: the derivative of the constraint in the price ceiling. -/
theorem R_hasDerivAt (ρb θ : ℝ) (Fp Fbp : ℝ → ℝ) (f fb p : ℝ) (hF : HasDerivAt Fp f p)
    (hFb : HasDerivAt Fbp fb p) :
    HasDerivAt (fun q => Rcon ρb θ (Fp q) (Fbp q)) (θ * fb - ρb * f) p := by
  unfold Rcon
  have h := ((hF.const_sub 1).const_mul ρb).sub ((hFb.const_sub 1).const_mul θ)
  refine h.congr_deriv ?_
  ring

/-- Footnote 12: on the constraint, the derivative is positive when group `b`'s hazard at
the ceiling is above the population's. -/
theorem R_deriv_pos (ρb θ F Fb f fb : ℝ) (hθ : 0 < θ) (hF : F < 1)
    (hR : Rcon ρb θ F Fb = 0) (hhaz : f * (1 - Fb) < fb * (1 - F)) :
    0 < θ * fb - ρb * f := by
  have h1F : 0 < 1 - F := by linarith
  have hρ : ρb = θ * (1 - Fb) / (1 - F) := by
    unfold Rcon at hR
    field_simp
    linarith
  rw [hρ]
  have : θ * (1 - Fb) / (1 - F) * f = θ * (f * (1 - Fb)) / (1 - F) := by ring
  rw [this, sub_pos, div_lt_iff₀ h1F]
  nlinarith

/-! ## 6. The investors' share of slots (not in the paper; compare M31) -/

/-- With a measure `θ = 1 - F(π, p)` of slots going to everyone above `p`, investors hold
the fraction `π (1 - F_1(p)) / θ` of them. -/
noncomputable def investorShare (π F1 F0 : ℝ) : ℝ :=
  π * (1 - F1) / (1 - mix π F1 F0)

/-- When effort leaves the law above the threshold unchanged, investors hold slots in
proportion to their number, the continuum analogue of `ξ(k) = (k+1)/(Q+1)` at `F = G`. -/
theorem investor_share_eq (π F : ℝ) (hF : F < 1) : investorShare π F F = π := by
  unfold investorShare mix
  have : 1 - (π * F + (1 - π) * F) = 1 - F := by ring
  rw [this]
  have h1 : 1 - F ≠ 0 := by linarith
  field_simp

/-- When effort raises productivity at the threshold, investors hold more than their
number. -/
theorem investor_share_gt (π F1 F0 : ℝ) (hπ0 : 0 < π) (hπ1 : π < 1) (hF : F1 < F0)
    (hF0 : F0 < 1) : π < investorShare π F1 F0 := by
  unfold investorShare mix
  have hden : 0 < 1 - (π * F1 + (1 - π) * F0) := by nlinarith
  rw [lt_div_iff₀ hden]
  have key : π * (1 - F1) - π * (1 - (π * F1 + (1 - π) * F0)) = π * ((1 - π) * (F0 - F1)) := by
    ring
  have : 0 < π * ((1 - π) * (F0 - F1)) := by
    apply mul_pos hπ0; apply mul_pos <;> linarith
  linarith

end FryerLoury2010
