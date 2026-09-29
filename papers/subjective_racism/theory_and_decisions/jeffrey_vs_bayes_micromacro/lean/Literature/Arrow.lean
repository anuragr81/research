/-
# Arrow (1973), "The Theory of Discrimination"

In O. Ashenfelter and A. Rees (eds.), *Discrimination in Labor Markets*,
Princeton University Press, 1973, pp. 3-33.

**Pagination.** The Drive copy is *not* the published chapter but Princeton
Industrial Relations Section Working Paper No. 30A ("Presented at Conference on
'Discrimination in Labor Markets' October 7-8, 1971"), with its own pagination:
text pp. 1-31, references, and a two-page Appendix.  Every page reference below
is to that **working-paper pagination** (WP p. n is PDF page n + 1), not to the
book's pp. 3-33.

Formalization of the paper's own equations, not of Paper B's claims.

## The paper's models

* **§1, employer taste discrimination** (WP pp. 4-9).  The employer maximizes
  `U(π, B, W)` with profit `π = f(W + B) - w_W W - w_B B` (eq. 1).  Becker's
  discrimination coefficient `d_B` is "the negative of the marginal rate of
  substitution of profits for B labor"; `MP_B = w_B + d_B` (2), `MP_W = w_W + d_W`
  (3), hence `w_W - w_B = d_B - d_W > 0` (4); `π - π₀ = d_W W + d_B B` (5-6);
  if satisfaction depends only on the ratio `B/W`, `d_W W + d_B B = 0` (7) and the
  whole effect is a transfer from B to W workers; with heterogeneous firms,
  `W/L = d_B/(w_W - w_B)`, `B/L = -d_W/(w_W - w_B)` (p. 8).
* **§2, nonconvexities** (WP pp. 13-20).  Co-worker discrimination by perfect
  substitutes, `C(W, B) = w_W(W/L) W + w_B B` (12) with `w_W` decreasing: every
  mixed labor force costs more than one of the two segregated ones (p. 17).  The
  Appendix: a utility depending only on `B/W` and increasing in `π` cannot have
  convex indifference surfaces.
* **§3, costs of adjustment** (WP pp. 21-24): verbal; results quoted from
  Arrow (1971), Technical Note E.
* **§4, imperfect information** (WP pp. 25-31).  Two groups W, B; binary
  qualification for skilled jobs; the employer "does believe that the probability
  that a random W worker is qualified is `p_W` and that a random B worker is
  qualified is `p_B`" (p. 26).  Zero expected return on the personnel investment
  `r`: `r = (MP_S - w_W) p_W = (MP_S - w_B) p_B` (13), hence
  `w_W = q w_B + (1 - q) MP_S`, `q = p_B / p_W` (14), and `p_B < p_W` implies
  `w_W > w_B` (p. 27).  Endogenous qualification (pp. 28-29):
  `p_i = S(v_i)`, `v_i = w_i - w_U` (15), `MP_U = w_U` (16); "it is clear that
  there is a symmetric equilibrium" but "an open question whether this is the only
  equilibrium" (p. 29); Marshallian dynamics `dp_W/dt = k[S(v_W) - p_W]` and the
  stability condition `E (MP_S - w_S)/(w_S - w_U) < 1`, `E` the elasticity of `S`
  (pp. 30-31, proof deferred to Arrow 1971, Technical Note F).  Instability
  "strongly suggests, though it does not prove, that there are equilibria other
  than the symmetric, non-discriminatory, one" (p. 30).

## What is formalized

§1: `wage_gap` (4), `profit_change` (5-6), `ratio_utility_euler` and
`eq7_of_ratio_utility` (7, via Euler's relation for a ratio-dependent utility),
`segregation_shares` (p. 8).
§2: `mixed_costlier` (p. 17), `appendix_nonconvex` (the Appendix, with its final
step corrected: the paper prints `U(π', B₀, W₀) = U(π₀, B₀, W₀)` where the
argument gives `≥`).
§4: `eq14`, `wage_differential` (p. 27), `exists_symmetric` (the IVT hypothesis
under which "it is clear" there is a symmetric equilibrium), `desired_hasDerivAt`
and `stability_iff` (the stability condition as the sign of the
antisymmetric-mode eigenvalue), and `example_two_fixed_points` /
`example_discriminatory` / `example_stability`: an explicit parametric instance
of §4, with `MP_S` and `w_U` held fixed, in which a discriminatory equilibrium
`(p_W, p_B) = (1/2, 1/3)` coexists with two symmetric ones.
-/
import Mathlib

namespace Literature.Arrow

open Filter Topology Set

/-! ## §1. Employer taste discrimination (WP pp. 4-9) -/

/-- **Eq. (4)**, p. 6.  From `MP_B = w_B + d_B` (2), `MP_W = w_W + d_W` (3) and
perfect substitutability `MP_W = MP_B = MP_L`: `w_W - w_B = d_B - d_W`, and it is
positive when `d_B > 0 ≥ d_W`. -/
theorem wage_gap {MP wW wB dW dB : ℝ} (h2 : MP = wB + dB) (h3 : MP = wW + dW) :
    wW - wB = dB - dW ∧ (0 < dB → dW ≤ 0 → wB < wW) := by
  refine ⟨by linarith, fun hB hW => by linarith⟩

/-- Profits, eq. (1), p. 5: `π = f(W + B) - w_W W - w_B B`. -/
def profit (f : ℝ → ℝ) (wW wB W B : ℝ) : ℝ := f (W + B) - wW * W - wB * B

/-- **Eqs. (5)-(6)**, p. 7.  With `w_i = MP_L - d_i`, profits exceed the
non-discriminating level `π₀ = f(L) - MP_L · L` by `d_W W + d_B B`. -/
theorem profit_change (f : ℝ → ℝ) {MP wW wB dW dB : ℝ} (W B : ℝ)
    (hW : wW = MP - dW) (hB : wB = MP - dB) :
    profit f wW wB W B - (f (W + B) - MP * (W + B)) = dW * W + dB * B := by
  unfold profit; subst hW hB; ring

/-- **Euler's relation for a ratio-dependent utility** (the step behind eq. 7).
If utility depends on `(B, W)` only through `B/W`, say `h(B/W)`, its two partial
derivatives are `h'(B/W)/W` and `-h'(B/W) B/W²`, and `B U_B + W U_W = 0`. -/
theorem ratio_utility_euler (h : ℝ → ℝ) {h' B W : ℝ} (hW : W ≠ 0)
    (hd : HasDerivAt h h' (B / W)) :
    HasDerivAt (fun b => h (b / W)) (h' / W) B ∧
      HasDerivAt (fun w => h (B / w)) (-(h' * B / W ^ 2)) W ∧
      B * (h' / W) + W * (-(h' * B / W ^ 2)) = 0 := by
  refine ⟨?_, ?_, ?_⟩
  · have h1 : HasDerivAt (fun b : ℝ => b / W) (1 / W) B := by
      simpa using (hasDerivAt_id B).div_const W
    rw [show h' / W = h' * (1 / W) by ring]
    exact hd.comp B h1
  · have h1 : HasDerivAt (fun w : ℝ => B / w) (-(B / W ^ 2)) W := by
      have := (hasDerivAt_inv hW).const_mul B
      have e : (fun w : ℝ => B / w) = fun w => B * w⁻¹ := by
        funext w; rw [div_eq_mul_inv]
      rw [e]
      exact this.congr_deriv (by ring)
    rw [show -(h' * B / W ^ 2) = h' * -(B / W ^ 2) by ring]
    exact hd.comp W h1
  · field_simp
    ring

/-- **Eq. (7)**, p. 7.  Discrimination coefficients are the marginal disutilities
of `B` and `W` in profit units, `d_B = -U_B / U_π`, `d_W = -U_W / U_π`.  When
satisfaction depends only on the ratio (so `B U_B + W U_W = 0`),
`d_W W + d_B B = 0`: "employers neither gain nor lose by their discriminatory
behavior". -/
theorem eq7_of_ratio_utility {Uπ UB UW B W : ℝ} (heuler : B * UB + W * UW = 0) :
    (-UW / Uπ) * W + (-UB / Uπ) * B = 0 := by
  rcases eq_or_ne Uπ 0 with h | h
  · simp [h]
  · field_simp
    linarith

/-- **Labor-force shares**, p. 8.  Solving (4) and (7) with `L = W + B`:
`W/L = d_B/(w_W - w_B)` and `B/L = -d_W/(w_W - w_B)`. -/
theorem segregation_shares {wW wB dW dB W B : ℝ} (h4 : wW - wB = dB - dW)
    (h7 : dW * W + dB * B = 0) (hg : wW - wB ≠ 0) (hL : W + B ≠ 0) :
    W / (W + B) = dB / (wW - wB) ∧ B / (W + B) = -dW / (wW - wB) := by
  constructor
  · rw [div_eq_div_iff hL hg, h4]; linarith
  · rw [div_eq_div_iff hL hg, h4]; linarith

/-! ## §2. Nonconvexities (WP pp. 13-20 and Appendix) -/

/-- Cost of a labor force, eq. (12), p. 17: `C(W, B) = w_W(W/L) W + w_B B`. -/
noncomputable def cost (wWf : ℝ → ℝ) (wB W B : ℝ) : ℝ := wWf (W / (W + B)) * W + wB * B

/-- **Rushing to extremes**, p. 17.  If the W wage demanded at a mixed ratio
exceeds the all-W wage `w_W(1)` (as it does when `w_W` is decreasing), every
genuinely mixed labor force costs strictly more than the cheaper of the two
segregated ones, `w_B L` and `w_W(1) L`. -/
theorem mixed_costlier (wWf : ℝ → ℝ) {wB W B : ℝ} (hW : 0 < W) (hB : 0 < B)
    (hdec : wWf 1 < wWf (W / (W + B))) :
    min (wB * (W + B)) (wWf 1 * (W + B)) < cost wWf wB W B := by
  unfold cost
  have h1 : wWf 1 * W < wWf (W / (W + B)) * W := mul_lt_mul_of_pos_right hdec hW
  rcases le_total wB (wWf 1) with h | h
  · calc min (wB * (W + B)) (wWf 1 * (W + B)) ≤ wB * (W + B) := min_le_left _ _
      _ = wB * W + wB * B := by ring
      _ ≤ wWf 1 * W + wB * B := by nlinarith
      _ < _ := by linarith
  · calc min (wB * (W + B)) (wWf 1 * (W + B)) ≤ wWf 1 * (W + B) := min_le_right _ _
      _ = wWf 1 * W + wWf 1 * B := by ring
      _ ≤ wWf 1 * W + wB * B := by nlinarith
      _ < _ := by linarith

/-- **Appendix: nonconvexity of indifference maps depending on ratios.**
Let `U(π, B, W)` be unchanged when `B` and `W` are scaled by the same positive
constant, strictly increasing in `π`, and continuous in `(B, W)` at `(B₀, W₀)`.
Suppose some bundle with lower profit `π₁ < π₀` is indifferent to `(π₀, B₀, W₀)`
(Appendix eq. 1).  Then the indifference map is *not* convex: the upper contour
set of `(π₀, B₀, W₀)` is not closed under midpoints.

The paper's last display reads `U(π', B₀, W₀) = U(π₀, B₀, W₀)`; the limit
argument delivers `≥`, which already contradicts monotonicity since `π' < π₀`. -/
theorem appendix_nonconvex (U : ℝ → ℝ → ℝ → ℝ) {B₀ W₀ : ℝ}
    (hhom : ∀ π B W t, 0 < t → U π (t * B) (t * W) = U π B W)
    (hmono : StrictMono fun π => U π B₀ W₀)
    (hcont : ∀ π, ContinuousAt (fun p : ℝ × ℝ => U π p.1 p.2) (B₀, W₀))
    {π₀ π₁ B₁ W₁ : ℝ} (hπ : π₁ < π₀) (hind : U π₁ B₁ W₁ = U π₀ B₀ W₀) :
    ¬ ∀ π B W π' B' W', U π B W ≥ U π₀ B₀ W₀ → U π' B' W' ≥ U π₀ B₀ W₀ →
        U ((π + π') / 2) ((B + B') / 2) ((W + W') / 2) ≥ U π₀ B₀ W₀ := by
  intro hconv
  set πm := (π₁ + π₀) / 2
  -- for every `t > 0`, `U(πm, t B₁ + B₀, t W₁ + W₀) ≥ U₀`
  have hge : ∀ t : ℝ, 0 < t → U πm (t * B₁ + B₀) (t * W₁ + W₀) ≥ U π₀ B₀ W₀ := by
    intro t ht
    have h1 : U π₁ (t * B₁) (t * W₁) ≥ U π₀ B₀ W₀ := by rw [hhom _ _ _ _ ht, hind]
    have hmid := hconv _ _ _ _ _ _ h1 (le_refl _)
    have e := hhom πm ((t * B₁ + B₀) / 2) ((t * W₁ + W₀) / 2) 2 (by norm_num)
    have e1 : (2 : ℝ) * ((t * B₁ + B₀) / 2) = t * B₁ + B₀ := by ring
    have e2 : (2 : ℝ) * ((t * W₁ + W₀) / 2) = t * W₁ + W₀ := by ring
    rw [e1, e2] at e
    rw [e]
    exact hmid
  -- let `t → 0⁺`
  have hpath : Tendsto (fun t : ℝ => (t * B₁ + B₀, t * W₁ + W₀)) (𝓝[>] 0) (𝓝 (B₀, W₀)) := by
    have : Continuous fun t : ℝ => (t * B₁ + B₀, t * W₁ + W₀) := by fun_prop
    have h := this.tendsto 0
    simp only [zero_mul, zero_add] at h
    exact h.mono_left nhdsWithin_le_nhds
  have hlim := (hcont πm).tendsto.comp hpath
  have hle : U π₀ B₀ W₀ ≤ U πm B₀ W₀ :=
    ge_of_tendsto hlim (eventually_nhdsWithin_of_forall fun t ht => hge t ht)
  have hlt : U πm B₀ W₀ < U π₀ B₀ W₀ := hmono (by simp only [πm]; linarith)
  linarith

/-! ## §4. Imperfect information (WP pp. 25-31) -/

/-- **Eq. (14)**, p. 27.  From the zero-return conditions (13) for both groups,
`(MP_S - w_W) p_W = (MP_S - w_B) p_B`, the W wage is the weighted average
`w_W = q w_B + (1 - q) MP_S` with `q = p_B / p_W`. -/
theorem eq14 {MPS wW wB pW pB : ℝ} (hpW : pW ≠ 0)
    (h13 : (MPS - wW) * pW = (MPS - wB) * pB) :
    wW = pB / pW * wB + (1 - pB / pW) * MPS := by
  field_simp
  linarith

/-- **The wage differential**, p. 27.  "If, for any reason, `p_B < p_W`" and
`w_B < MP_S` (so the employer recoups his personnel investment), then `w_W > w_B`:
"the effect of the differential judgment as to the probability of being qualified
is reflected in a wage differential". -/
theorem wage_differential {MPS wW wB pW pB r : ℝ} (hpB : 0 < pB) (hlt : pB < pW)
    (hW : r = (MPS - wW) * pW) (hB : r = (MPS - wB) * pB) (hwB : wB < MPS) : wB < wW := by
  have hpW : 0 < pW := by linarith
  have h14 := eq14 hpW.ne' (by linarith : (MPS - wW) * pW = (MPS - wB) * pB)
  have hq : pB / pW < 1 := (div_lt_one hpW).mpr hlt
  have : wW - wB = (1 - pB / pW) * (MPS - wB) := by rw [h14]; ring
  have : 0 < (1 - pB / pW) * (MPS - wB) := mul_pos (by linarith) (by linarith)
  linarith

/-- **A symmetric equilibrium** (p. 29: "From the symmetric formulation of the
system, it is clear that there is a symmetric equilibrium").  Write `Φ(p)` for the
desired qualified share `S(v)` when both groups share the qualified share `p`
(and wages are set by (13), (16)).  A symmetric equilibrium is a fixed point of
`Φ`; one exists if `Φ` is continuous on `[a, 1]` with `Φ a ≥ a` and `Φ 1 ≤ 1`.
Symmetry alone does not give it; an intermediate-value hypothesis of this kind is
what "clear" is standing in for. -/
theorem exists_symmetric (Φ : ℝ → ℝ) {a : ℝ} (ha : a ≤ 1) (hc : ContinuousOn Φ (Icc a 1))
    (h0 : a ≤ Φ a) (h1 : Φ 1 ≤ 1) : ∃ p ∈ Icc a 1, Φ p = p := by
  have hcont : ContinuousOn (fun p => p - Φ p) (Icc a 1) :=
    (continuousOn_id.sub hc)
  have hmem : (0 : ℝ) ∈ Icc ((fun p => p - Φ p) a) ((fun p => p - Φ p) 1) := by
    simp only [mem_Icc]; constructor <;> linarith
  obtain ⟨p, hp, hp0⟩ := intermediate_value_Icc ha hcont hmem
  exact ⟨p, hp, by simp only at hp0; linarith⟩

/-- The desired qualified share of one group whose actual share is `p`, holding
`MP_S = M` and `w_U = u` fixed: by (13) its skilled wage is `M - r/p`, so the gain
from qualifying is `v = M - r/p - u` (15) and the desired level is `S(v)`. -/
noncomputable def desired (S : ℝ → ℝ) (M u r p : ℝ) : ℝ := S (M - r / p - u)

/-- The Marshallian right-hand side `S(v_W) - p_W` (p. 30, up to the speed `k`)
has derivative `S'(v) · r/p² - 1` in the group's own share. -/
theorem desired_hasDerivAt (S : ℝ → ℝ) {M u r p s' : ℝ} (hp : p ≠ 0)
    (hS : HasDerivAt S s' (M - r / p - u)) :
    HasDerivAt (fun x => desired S M u r x - x) (s' * (r / p ^ 2) - 1) p := by
  have hv : HasDerivAt (fun x : ℝ => M - r / x - u) (r / p ^ 2) p := by
    have hinv := (hasDerivAt_inv hp).const_mul r
    have e : (fun x : ℝ => M - r / x - u) = fun x => M - r * x⁻¹ - u := by
      funext x; rw [div_eq_mul_inv]
    rw [e]
    exact (((hasDerivAt_const p M).sub hinv).sub_const u).congr_deriv (by ring)
  exact (hS.comp p hv).sub (hasDerivAt_id p)

/-- **Arrow's stability condition**, p. 31.  At a symmetric equilibrium
(`S(v) = p`, `v = w_S - w_U ≠ 0`, `w_S = M - r/p`), with `E = S'(v) v / S(v)` the
elasticity of supply, the own-share derivative of the Marshallian right-hand side
is negative iff `E (MP_S - w_S)/(w_S - w_U) < 1`.

Along the antisymmetric direction (`δp_W = -δp_B`, weighted by group sizes) total
skilled and unskilled supplies, hence `MP_S` and `w_U`, are unchanged to first
order, so this derivative is exactly the antisymmetric eigenvalue of the two-group
system; `sympy/check_arrow.py` verifies that decomposition, and that the other
(symmetric) eigenvalue is automatically negative under diminishing returns.  The
paper's condition is thus the condition that the discriminatory perturbation
dies out. -/
theorem stability_iff {S : ℝ → ℝ} {M u r p s' : ℝ} (hp : 0 < p)
    (hfix : S (M - r / p - u) = p) (hv : M - r / p - u ≠ 0) :
    s' * (r / p ^ 2) - 1 < 0 ↔
      (s' * (M - r / p - u) / S (M - r / p - u)) * (M - (M - r / p)) /
        ((M - r / p) - u) < 1 := by
  rw [hfix]
  have e : (s' * (M - r / p - u) / p) * (M - (M - r / p)) / ((M - r / p) - u)
      = s' * (r / p ^ 2) := by
    rw [show M - (M - r / p) = r / p by ring, show (M - r / p) - u = M - r / p - u by ring]
    set v := M - r / p - u
    field_simp
  rw [e, sub_neg]

/-! ### An explicit instance of §4 with two symmetric and one discriminatory
equilibrium

Hold `MP_S` and `w_U` fixed with `MP_S - w_U = 4`, take `r = 1` and the increasing
supply schedule `S(v) = v / (v + 2)` on `v > 0`.  A group's share `p` is
self-confirming iff `p = S(4 - 1/p)`, i.e. `6p² - 5p + 1 = 0`, with roots `1/2`
and `1/3`. -/

/-- The example's supply schedule. -/
noncomputable def exS (v : ℝ) : ℝ := v / (v + 2)

/-- Both `1/2` and `1/3` are self-confirming shares. -/
theorem example_two_fixed_points :
    exS (4 - 1 / (1 / 2 : ℝ)) = 1 / 2 ∧ exS (4 - 1 / (1 / 3 : ℝ)) = 1 / 3 := by
  constructor <;> norm_num [exS]

/-- ... and they are the only positive self-confirming shares with `v > 0`. -/
theorem example_only_fixed_points {p : ℝ} (hp : 0 < p) (hv : 0 < 4 - 1 / p)
    (h : exS (4 - 1 / p) = p) : p = 1 / 2 ∨ p = 1 / 3 := by
  unfold exS at h
  have hd : (4 - 1 / p) + 2 ≠ 0 := by linarith
  rw [div_eq_iff hd] at h
  field_simp at h
  have : (p - 1 / 2) * (p - 1 / 3) = 0 := by nlinarith
  rcases mul_eq_zero.mp this with h1 | h1
  · left; linarith
  · right; linarith

/-- **A discriminatory equilibrium.**  `(p_W, p_B) = (1/2, 1/3)` satisfies (13) and
(15) for both groups, with skilled wages `w_W = MP_S - 2 > w_B = MP_S - 3`
(eq. 13: `w_i = MP_S - r/p_i`), even though the two groups have the same supply
schedule -- "`p_W` and `p_B` differ in reality, even though the intrinsic
abilities of W and B workers are identical" (p. 28). -/
theorem example_discriminatory (MPS : ℝ) :
    let pW : ℝ := 1 / 2
    let pB : ℝ := 1 / 3
    let wW := MPS - 1 / pW
    let wB := MPS - 1 / pB
    exS (4 - 1 / pW) = pW ∧ exS (4 - 1 / pB) = pB ∧
      (MPS - wW) * pW = 1 ∧ (MPS - wB) * pB = 1 ∧ wB < wW := by
  intro pW pB wW wB
  refine ⟨example_two_fixed_points.1, example_two_fixed_points.2, ?_, ?_, ?_⟩ <;>
    simp only [pW, pB, wW, wB] <;> norm_num

/-- **Stability in the example**, by Arrow's criterion: with `S'(v) = 2/(v+2)²`,
the quantity `E (MP_S - w_S)/(w_S - w_U)` is `1/2 < 1` at `p = 1/2` (stable) and
`2 > 1` at `p = 1/3` (unstable).  So in this instance the discriminatory pair has
its B coordinate on the unstable root, as Arrow's instability-based heuristic
would lead one to look for. -/
theorem example_stability :
    let crit := fun p : ℝ =>
      let v := 4 - 1 / p
      let s' := 2 / (v + 2) ^ 2
      (s' * v / exS v) * (1 / p) / v
    crit (1 / 2) = 1 / 2 ∧ crit (1 / 3) = 2 := by
  constructor <;> norm_num [exS]

end Literature.Arrow
