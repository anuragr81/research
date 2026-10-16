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

/-! ## The model of Sections 1 and 2 -/

/-- The primitives. `l` is the paper's `λ`. `c` is the cost of assimilation per
unit of difficulty, `ψ` the direct utility and `v` the externality
`v(s_i, e_J, s_J)` of eq. (1). -/
structure Primitives where
  eA : ℝ
  l : ℝ
  c : ℝ → ℝ
  ψ : ℝ → ℝ → ℝ
  v : ℝ → ℝ → ℝ → ℝ

/-- Eq. (1). The utility of an agent with endowment `e` and skill `s` in a
social group with average endowment `eJ` and average skill `sJ`, assimilation
decision `a ∈ {0, 1}` and difficulty `d`. -/
def U (P : Primitives) (e s eJ sJ d a : ℝ) : ℝ := P.ψ e s + P.v s eJ sJ - a * d * P.c s

/-- Eq. (2). The utility of a young agent under the peer-pressure threshold
`sP` with punishment `K`. -/
noncomputable def Upeer (P : Primitives) (e s eJ sJ d a sP K : ℝ) : ℝ :=
  U P e s eJ sJ d a - (if sP < s then K else 0)

/-- Eq. (1) for an agent who does not assimilate. -/
theorem eq1_nonassimilator (P : Primitives) (e s eJ sJ d : ℝ) :
    U P e s eJ sJ d 0 = P.ψ e s + P.v s eJ sJ := by
  unfold U; ring

/-- Eq. (2). Skill above the threshold costs `K`. -/
theorem eq2_pressure_cost (P : Primitives) {e s eJ sJ d a sP K : ℝ} (h : sP < s) :
    Upeer P e s eJ sJ d a sP K = U P e s eJ sJ d a - K := by
  unfold Upeer; rw [if_pos h]

/-- First step of the proof of Proposition 1 (p. 29). With `ψ` increasing in
skill, `v` weakly increasing in own skill and a cost of assimilation that falls
with skill, more skill never lowers utility, so `s_i = θ_i`. -/
theorem step1_skill_monotone (P : Primitives) {e eJ sJ d a s s' : ℝ}
    (hψ : Monotone (P.ψ e)) (hv : Monotone (fun x => P.v x eJ sJ)) (hc : Antitone P.c)
    (had : 0 ≤ a * d) (h : s ≤ s') : U P e s eJ sJ d a ≤ U P e s' eJ sJ d a := by
  unfold U
  have h1 := hψ h
  have h2 : P.v s eJ sJ ≤ P.v s' eJ sJ := hv h
  have h3 := hc h
  nlinarith [mul_le_mul_of_nonneg_left h3 had]

/-- The gain from assimilating for an agent of skill `si` when the cutoff is
`s`, the externality in `A` less that in `D` (eq. 1). -/
noncomputable def gain (P : Primitives) (si s : ℝ) : ℝ :=
  P.v si (eAbar P.eA P.l s) (sA P.l s) - P.v si 0 (sD P.l s)

/-- `d(s)` of p. 30, the difficulty at which the agent with skill `s` is
indifferent when the groups have the composition the cutoff `s` produces. -/
noncomputable def dcut (P : Primitives) (s : ℝ) : ℝ := gain P s s / P.c s

/-- p. 30. At `d(s)` the agent at the cutoff is indifferent. -/
theorem dcut_indifferent (P : Primitives) {s : ℝ} (hc : P.c s ≠ 0) :
    gain P s s - dcut P s * P.c s = 0 := by
  unfold dcut; field_simp; ring

/-- Second step of the proof of Proposition 1 (pp. 29–30). If an agent of
skill `sj` gains from assimilating, so does every more skilled agent, provided
the gain `G` does not fall with own skill. The paper takes this proviso for
granted (EG-D7). -/
theorem cutoff_rule {G c : ℝ → ℝ} (hG : Monotone G) (hc : Antitone c) {d si sj : ℝ}
    (hd : 0 ≤ d) (hj : 0 ≤ G sj - d * c sj) (hji : sj ≤ si) : 0 ≤ G si - d * c si := by
  have h1 := hG hji
  have h2 := hc hji
  nlinarith [mul_le_mul_of_nonneg_left h2 hd]

/-- Control. Without a gain that is monotone in own skill, the cutoff rule can
fail even with a cost strictly decreasing in skill. -/
theorem control_cutoff_needs_monotone_gain :
    let G : ℝ → ℝ := fun s => 2 - 2 * s
    let c : ℝ → ℝ := fun s => 1 - s / 2
    StrictAnti c ∧ 0 ≤ G 0 - 1 * c 0 ∧ G 1 - 1 * c 1 < 0 := by
  refine ⟨?_, by norm_num, by norm_num⟩
  intro a b h
  show 1 - b / 2 < 1 - a / 2
  linarith

/-- Third step of the proof of Proposition 1 (pp. 30–31), at `λ = 0`. Nothing
moves, so the groups keep mean skill 1/2 and `A` keeps the endowment `e_𝒜`. -/
theorem composition_lambda_zero (eA s : ℝ) :
    eAbar eA 0 s = eA ∧ sA 0 s = 1 / 2 ∧ sD 0 s = 1 / 2 := by
  unfold eAbar sA sD; norm_num

/-- Third step at `λ = 0`. With `v_{s_i e_J} = 0` the gain is a constant `K`,
positive when `v` rises in `e_J`, and `d(s) = K/c(s)` is strictly increasing
because `c` is positive and strictly decreasing. -/
theorem dcut_strictMono_lambda_zero {c : ℝ → ℝ} {K : ℝ} (hK : 0 < K) (hc : StrictAnti c)
    (hpos : ∀ x, 0 < c x) : StrictMono (fun s => K / c s) := by
  intro a b h
  show K / c a < K / c b
  exact div_lt_div_of_pos_left hK (hpos b) (hc h)

/-- Control. Without an endowment gap (`K = 0`), `d(s)` is flat and does not
pin down a cutoff. -/
theorem control_dcut_no_gap {c : ℝ → ℝ} : ¬ StrictMono (fun s => (0 : ℝ) / c s) := by
  intro h
  have := h (show (0 : ℝ) < 1 by norm_num)
  simp at this

/-- Proposition 1 (i), uniqueness of the cutoff. When `d(s)` is strictly
increasing, a difficulty is the indifference level of at most one cutoff. -/
theorem cutoff_unique {f : ℝ → ℝ} (hf : StrictMono f) {s₁ s₂ d₀ : ℝ} (h₁ : f s₁ = d₀)
    (h₂ : f s₂ = d₀) : s₁ = s₂ :=
  hf.injective (h₁.trans h₂.symm)

/-- Eq. (3) and p. 32. The derivative of `e_A(s)`. -/
theorem eAbar_hasDerivAt {eA l s : ℝ} (h : 1 + l * (1 - s) ≠ 0) :
    HasDerivAt (eAbar eA l) (l * eA / (1 + l * (1 - s)) ^ 2) s := by
  have hd : HasDerivAt (fun s : ℝ => 1 + l * (1 - s)) (-l) s := by
    have := (((hasDerivAt_id s).const_sub 1).const_mul l).const_add 1
    simpa using this
  have hq : HasDerivAt (eAbar eA l) ((0 * (1 + l * (1 - s)) - eA * -l) / (1 + l * (1 - s)) ^ 2) s :=
    (hasDerivAt_const s eA).div hd h
  convert hq using 1
  ring

/-- The left side of eq. (6), with `vE = ∂v/∂e_A` and `vS = ∂v/∂s_A`. -/
noncomputable def foc6 (eA l s vE vS : ℝ) : ℝ :=
  l * eA * vE - l * (((1 + l) * (2 * s - 1) - l * s ^ 2) / 2) * vS

/-- Eqs. (5) and (6). The first-order condition (5), with the derivatives of
p. 32, holds exactly when eq. (6) holds. -/
theorem eq5_iff_eq6 {eA l s vE vS : ℝ} (h : 1 + l * (1 - s) ≠ 0) :
    l * eA / (1 + l * (1 - s)) ^ 2 * vE
        + (-2 * l * s * (1 + l * (1 - s)) + l * (1 + l - l * s ^ 2)) / (2 * (1 + l * (1 - s)) ^ 2) * vS
        = 0 ↔ foc6 eA l s vE vS = 0 := by
  have e : l * eA / (1 + l * (1 - s)) ^ 2 * vE
        + (-2 * l * s * (1 + l * (1 - s)) + l * (1 + l - l * s ^ 2)) / (2 * (1 + l * (1 - s)) ^ 2) * vS
        = foc6 eA l s vE vS / (1 + l * (1 - s)) ^ 2 := by
    unfold foc6; field_simp; ring
  rw [e, div_eq_zero_iff]
  have : (1 + l * (1 - s)) ^ 2 ≠ 0 := pow_ne_zero 2 h
  tauto

/-- p. 33. `dv/ds > 0` for `s ≤ 1/2`. -/
theorem foc_pos_le_half {eA l s vE vS : ℝ} (hl : 0 < l) (he : 0 < eA) (hE : 0 < vE)
    (hS : 0 ≤ vS) (hs : s ≤ 1 / 2) : 0 < foc6 eA l s vE vS := by
  unfold foc6
  have h1 : (1 + l) * (2 * s - 1) - l * s ^ 2 ≤ 0 := by nlinarith
  have h2 : 0 < l * eA * vE := by positivity
  have h3 : 0 ≤ -(l * (((1 + l) * (2 * s - 1) - l * s ^ 2) / 2) * vS) := by
    have : l * (((1 + l) * (2 * s - 1) - l * s ^ 2) / 2) ≤ 0 := by nlinarith
    nlinarith
  linarith

/-- p. 33. Every root of eq. (6) lies above 1/2, so
`s*(s_i) ∈ (1/2, 1)` and `θ* > 1/2`. -/
theorem foc_root_gt_half {eA l s vE vS : ℝ} (hl : 0 < l) (he : 0 < eA) (hE : 0 < vE)
    (hS : 0 ≤ vS) (h : foc6 eA l s vE vS = 0) : 1 / 2 < s := by
  by_contra hc
  have := foc_pos_le_half hl he hE hS (not_lt.1 hc)
  linarith

/-- Control. Without mobility (`λ = 0`) eq. (6) holds at every cutoff, so it
locates nothing. -/
theorem control_foc_no_mobility (eA s vE vS : ℝ) : foc6 eA 0 s vE vS = 0 := by
  unfold foc6; ring

/-- p. 32, read as EG-D1. At `s = 1`, `dv/ds < 0` when `e_𝒜 < vS/(2 vE)`, so
`s = 1` is not optimal for a low enough endowment. -/
theorem foc_neg_at_one {eA l vE vS : ℝ} (hl : 0 < l) (h : 2 * vE * eA < vS) :
    foc6 eA l 1 vE vS < 0 := by
  unfold foc6
  nlinarith [mul_lt_mul_of_pos_left h hl]

/-- Control. With `e_𝒜 > vS/(2 vE)` the sign at `s = 1` reverses. -/
theorem control_foc_large_endowment {eA l vE vS : ℝ} (hl : 0 < l)
    (h : vS < 2 * vE * eA) : 0 < foc6 eA l 1 vE vS := by
  unfold foc6
  nlinarith [mul_lt_mul_of_pos_left h hl]

/-- The objective `v(s_i, e_A(s), s_A(s))` of eq. (4) when `v` is linear in
the group averages, `v = vE e_J + vS s_J` plus a term in `s_i` alone. -/
noncomputable def Flin (eA l vE vS s : ℝ) : ℝ := vE * eAbar eA l s + vS * sA l s

/-- The gain in the linear case from moving the cutoff below 1. -/
theorem Flin_sub_one {eA l vE vS s : ℝ} (hl : 0 ≤ l) (hs : s ≤ 1) :
    Flin eA l vE vS s - Flin eA l vE vS 1
      = l * (1 - s) / (1 + l * (1 - s)) * (vS * s / 2 - vE * eA) := by
  unfold Flin eAbar sA
  have h1 : 0 < 1 + l * (1 - s) := by nlinarith
  have h2 : (2 + 2 * l * (1 - s)) ≠ 0 := by nlinarith
  field_simp
  ring

/-- Proposition 1 (ii), linear case. With a small endowment,
`2 vE e_𝒜 < vS`, some interior cutoff beats `s = 1`, so `θ* < 1` and some
agents assimilate. -/
theorem prop1ii_linear {eA l vE vS : ℝ} (hl : 0 < l) (hE : 0 < vE) (hS : 0 < vS)
    (he : 0 ≤ eA) (hsmall : 2 * vE * eA < vS) :
    ∃ s, 0 < s ∧ s < 1 ∧ Flin eA l vE vS 1 < Flin eA l vE vS s := by
  set s := (1 + 2 * vE * eA / vS) / 2 with hsdef
  have hq : 2 * vE * eA / vS < 1 := by rw [div_lt_one hS]; exact hsmall
  have hq0 : 0 ≤ 2 * vE * eA / vS := by positivity
  have hs0 : 0 < s := by rw [hsdef]; linarith
  have hs1 : s < 1 := by rw [hsdef]; linarith
  refine ⟨s, hs0, hs1, ?_⟩
  have key := Flin_sub_one (eA := eA) (vE := vE) (vS := vS) hl.le hs1.le
  have hpos : 0 < vS * s / 2 - vE * eA := by
    have : vS * s = vS / 2 + vE * eA := by
      rw [hsdef]; field_simp
    linarith
  have hfrac : 0 < l * (1 - s) / (1 + l * (1 - s)) := by
    have : 0 < 1 - s := by linarith
    positivity
  nlinarith [mul_pos hfrac hpos]

/-- Control for Proposition 1 (ii). With `2 vE e_𝒜 ≥ vS` no cutoff at or below 1
beats `s = 1`, so nobody is admitted. -/
theorem control_prop1ii_large_endowment {eA l vE vS : ℝ} (hl : 0 ≤ l) (hS : 0 ≤ vS)
    (hbig : vS ≤ 2 * vE * eA) {s : ℝ} (hs1 : s ≤ 1) :
    Flin eA l vE vS s ≤ Flin eA l vE vS 1 := by
  have key := Flin_sub_one (eA := eA) (vE := vE) (vS := vS) hl hs1
  have hneg : vS * s / 2 - vE * eA ≤ 0 := by nlinarith
  have hfrac : 0 ≤ l * (1 - s) / (1 + l * (1 - s)) := by
    have : 0 ≤ 1 - s := by linarith
    have : 0 < 1 + l * (1 - s) := by nlinarith
    positivity
  nlinarith [mul_nonpos_of_nonneg_of_nonpos hfrac hneg]

/-- Proposition 1 (iii), the monotone comparative statics step. If the
objective `F s θ` has strictly decreasing differences in the cutoff and the
incumbent's skill, a more skilled incumbent never prefers a higher cutoff.
The paper obtains decreasing differences from `v_{s_i s_J} ≥ 0` and
`s_A′ < 0` at the optimum; here they are assumed. -/
theorem maximiser_antitone {F : ℝ → ℝ → ℝ}
    (hdd : ∀ m₁ m₂ θ₁ θ₂, m₁ < m₂ → θ₁ < θ₂ → F m₂ θ₂ - F m₁ θ₂ < F m₂ θ₁ - F m₁ θ₁)
    {θ₁ θ₂ m₁ m₂ : ℝ} (hθ : θ₁ < θ₂) (h₁ : ∀ m, F m θ₁ ≤ F m₁ θ₁) (h₂ : ∀ m, F m θ₂ ≤ F m₂ θ₂) :
    m₂ ≤ m₁ := by
  by_contra hc
  have hlt : m₁ < m₂ := not_le.1 hc
  have a := h₁ m₂
  have b := h₂ m₁
  have := hdd m₁ m₂ θ₁ θ₂ hlt hθ
  linarith

/-- Proposition 1 (iii). With `d` increasing in the cutoff and the preferred
cutoff falling in the incumbent's skill, the preferred difficulty
`d*_i = d(s*(s_i))` falls in skill, `d*_1 ≥ … ≥ d*_N`. -/
theorem prop1iii {d sstar : ℝ → ℝ} (hd : Monotone d) (hs : Antitone sstar) :
    Antitone (fun θ => d (sstar θ)) :=
  fun _ _ h => hd (hs h)

/-- Control for Proposition 1 (iii). With increasing differences instead, the
order reverses: an example in which the more skilled incumbent prefers the
higher cutoff. -/
theorem control_prop1iii_increasing_differences :
    let F : ℝ → ℝ → ℝ := fun m θ => -(m - θ) ^ 2
    (∀ m, F m 0 ≤ F 0 0) ∧ (∀ m, F m 1 ≤ F 1 1) ∧ ¬ ((1 : ℝ) ≤ 0) := by
  refine ⟨fun m => ?_, fun m => ?_, by norm_num⟩
  · show -(m - 0) ^ 2 ≤ -(0 - 0) ^ 2; nlinarith [sq_nonneg m]
  · show -(m - 1) ^ 2 ≤ -(1 - 1) ^ 2; nlinarith [sq_nonneg (m - 1)]

/-- An order statistic of the proposals, seen from one proposer, is the
proposer's own report clamped between two bounds set by the others. -/
def clampI (x L H : ℝ) : ℝ := max L (min x H)

/-- Fifth step of the proof of Proposition 1 (p. 33), with the rule in clamp
form. Under single-peaked preferences with peak `p`, reporting `p` is a best
response to any reports of the others (Moulin 1980). -/
theorem clamp_truthful {u : ℝ → ℝ} {p L H : ℝ} (hLH : L ≤ H)
    (hmono : MonotoneOn u (Set.Iic p)) (hanti : AntitoneOn u (Set.Ici p)) (x : ℝ) :
    u (clampI x L H) ≤ u (clampI p L H) := by
  unfold clampI
  have hy1 : L ≤ max L (min x H) := le_max_left _ _
  have hy2 : max L (min x H) ≤ H := max_le hLH (min_le_right _ _)
  rcases lt_or_ge p L with hp | hp
  · have e : max L (min p H) = L := by
      rw [max_eq_left]; exact le_trans (min_le_left _ _) hp.le
    rw [e]
    exact hanti (Set.mem_Ici.2 hp.le) (Set.mem_Ici.2 (le_trans hp.le hy1)) hy1
  · rcases le_or_gt p H with hq | hq
    · have e : max L (min p H) = p := by rw [min_eq_left hq, max_eq_right hp]
      rw [e]
      rcases le_total (max L (min x H)) p with h | h
      · exact hmono (Set.mem_Iic.2 h) (Set.mem_Iic.2 le_rfl) h
      · exact hanti (Set.mem_Ici.2 le_rfl) (Set.mem_Ici.2 h) h
    · have e : max L (min p H) = H := by
        rw [min_eq_right hq.le, max_eq_right hLH]
      rw [e]
      exact hmono (Set.mem_Iic.2 (le_trans hy2 hq.le)) (Set.mem_Iic.2 hq.le) hy2

/-- Control for the fifth step. Without single-peaked preferences a
proposer can gain by misreporting: with `u y = y²` and peak 0, reporting 1
beats reporting 0. -/
theorem control_clamp_not_single_peaked :
    (fun y : ℝ => y ^ 2) (clampI 0 0 1) < (fun y : ℝ => y ^ 2) (clampI 1 0 1) := by
  norm_num [clampI]

/-! ## Proposition 2 -/

/-- Eq. (7). The average endowment of `A` when `e_𝒟 ≥ 0`. -/
noncomputable def eAbar2 (eA eD l s : ℝ) : ℝ := (eA + l * (1 - s) * eD) / (1 + l * (1 - s))

/-- Eq. (8). The derivative of eq. (7). -/
theorem eq8 {eA eD l s : ℝ} (h : 1 + l * (1 - s) ≠ 0) :
    HasDerivAt (eAbar2 eA eD l) (l * (eA - eD) / (1 + l * (1 - s)) ^ 2) s := by
  have hn : HasDerivAt (fun s : ℝ => eA + l * (1 - s) * eD) (-(l * eD)) s := by
    have := ((((hasDerivAt_id s).const_sub 1).const_mul l).mul_const eD).const_add eA
    simpa using this
  have hd : HasDerivAt (fun s : ℝ => 1 + l * (1 - s)) (-l) s := by
    have := (((hasDerivAt_id s).const_sub 1).const_mul l).const_add 1
    simpa using this
  have hq : HasDerivAt (eAbar2 eA eD l)
      ((-(l * eD) * (1 + l * (1 - s)) - (eA + l * (1 - s) * eD) * -l) / (1 + l * (1 - s)) ^ 2) s :=
    hn.div hd h
  convert hq using 1
  ring

/-- Eq. (9), the left side, with `vE`, `vS` the partial derivatives. -/
noncomputable def foc9 (eA eD l s vE vS : ℝ) : ℝ :=
  l * (eA - eD) * vE - l * (((1 + l) * (2 * s - 1) - l * s ^ 2) / 2) * vS

/-- Eq. (9) is eq. (6) with `e_𝒜 − e_𝒟` for `e_𝒜`. -/
theorem eq9_eq_eq6 (eA eD l s vE vS : ℝ) : foc9 eA eD l s vE vS = foc6 (eA - eD) l s vE vS := by
  unfold foc9 foc6; ring

/-- Proposition 2 (i), linear case (`vE`, `vS` constant). A richer
disadvantaged group lowers the preferred cutoff. -/
theorem prop2_linear {eA eD₁ eD₂ l vE vS s₁ s₂ : ℝ} (hl : 0 < l) (hE : 0 < vE) (hS : 0 < vS)
    (hD : eD₁ < eD₂) (h₁1 : s₁ ≤ 1) (h₂1 : s₂ ≤ 1)
    (r₁ : foc9 eA eD₁ l s₁ vE vS = 0) (r₂ : foc9 eA eD₂ l s₂ vE vS = 0) : s₂ < s₁ := by
  unfold foc9 at r₁ r₂
  have e₁ : (eA - eD₁) * vE * 2 = ((1 + l) * (2 * s₁ - 1) - l * s₁ ^ 2) * vS := by
    have := mul_left_cancel₀ hl.ne' (show l * ((eA - eD₁) * vE * 2) =
      l * (((1 + l) * (2 * s₁ - 1) - l * s₁ ^ 2) * vS) by linarith)
    exact this
  have e₂ : (eA - eD₂) * vE * 2 = ((1 + l) * (2 * s₂ - 1) - l * s₂ ^ 2) * vS := by
    have := mul_left_cancel₀ hl.ne' (show l * ((eA - eD₂) * vE * 2) =
      l * (((1 + l) * (2 * s₂ - 1) - l * s₂ ^ 2) * vS) by linarith)
    exact this
  by_contra hc
  have hle : s₁ ≤ s₂ := not_lt.1 hc
  have hR : ((1 + l) * (2 * s₁ - 1) - l * s₁ ^ 2) ≤ ((1 + l) * (2 * s₂ - 1) - l * s₂ ^ 2) := by
    nlinarith [mul_nonneg (sub_nonneg.2 hle) (show (0 : ℝ) ≤ 2 + 2 * l - l * (s₁ + s₂) by nlinarith)]
  have : (eA - eD₂) * vE * 2 < (eA - eD₁) * vE * 2 := by nlinarith
  nlinarith [mul_le_mul_of_nonneg_right hR hS.le]

/-- Proposition 2 (ii), linear case. Raising both endowments by the same
amount leaves eq. (9), hence the cutoff, unchanged. A strict decrease needs
`v_{e_J e_J} < 0`, which the linear case does not have. -/
theorem prop2ii_linear (eA eD l s vE vS x : ℝ) :
    foc9 (eA + x) (eD + x) l s vE vS = foc9 eA eD l s vE vS := by
  unfold foc9; ring

/-- Control for Proposition 2. With a gap above `vS/(2 vE)`, eq. (9) has no
root at or below 1, and nobody is admitted. -/
theorem control_prop2_large_gap {eA eD l vE vS s : ℝ} (hl : 0 < l) (hS : 0 ≤ vS)
    (hbig : vS < 2 * (eA - eD) * vE) (hs1 : s ≤ 1) :
    0 < foc9 eA eD l s vE vS := by
  unfold foc9
  have hR : (1 + l) * (2 * s - 1) - l * s ^ 2 ≤ 1 := by
    nlinarith [mul_nonneg (sub_nonneg.2 hs1) (show 0 ≤ 2 + l - l * s by nlinarith)]
  have : ((1 + l) * (2 * s - 1) - l * s ^ 2) * vS ≤ vS := by nlinarith
  nlinarith [mul_lt_mul_of_pos_left hbig hl]

/-! ## Proposition 3 -/

/-- A young agent's skill under the peer-pressure threshold `sP`, when the
direct utility is linear in skill with slope `b` and the punishment is `K`.
Assimilation is set aside, as it is for members of `𝒜_Y`. -/
noncomputable def chosen (b K sP θ : ℝ) : ℝ :=
  if θ ≤ sP then θ else if K ≤ b * (θ - sP) then θ else sP

theorem chosen_le (b K sP θ : ℝ) : chosen b K sP θ ≤ θ := by
  unfold chosen; split_ifs with h1 h2 <;> linarith

/-- Proposition 3, the `𝒜` side (pp. 18, 36). The threshold `s^P_𝒜 = 1`
leaves every member's skill at its ability, so it gives every member at least
as much skill as any other threshold, and `l` chooses it. -/
theorem prop3_sPA_one (b K sP θ : ℝ) (hθ : θ ≤ 1) : chosen b K sP θ ≤ chosen b K 1 θ := by
  have : chosen b K 1 θ = θ := by unfold chosen; rw [if_pos hθ]
  rw [this]; exact chosen_le b K sP θ

/-- Control for Proposition 3. A threshold below ability with a punishment
that bites lowers the member's skill. -/
theorem control_prop3_pressure_bites {b K sP θ : ℝ} (h1 : sP < θ)
    (h2 : b * (θ - sP) < K) : chosen b K sP θ < θ := by
  unfold chosen
  rw [if_neg (not_le.2 h1), if_neg (not_le.2 h2)]
  exact h1

/-! ## Example 4 -/

/-- The column (3)−(2) of the table on p. 20 follows from columns (2) and (3). -/
theorem example4_differences :
    (1.314 : ℝ) - 1.341 = -0.027 ∧ (0.676 : ℝ) - 0.610 = 0.066 ∧ (0.488 : ℝ) - 0.487 = 0.001 ∧
      (14.074 : ℝ) - 14.077 = -0.003 ∧ (9.800 : ℝ) - 9.896 = -0.096 ∧
      (7.384 : ℝ) - 7.376 = 0.008 := by
  norm_num

end Eguia2013
