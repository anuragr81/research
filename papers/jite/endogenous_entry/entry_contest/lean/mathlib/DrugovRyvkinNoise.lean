import Mathlib

/-!
# Drugov and Ryvkin (2020), "How noise affects effort in tournaments"

Mikhail Drugov and Dmitry Ryvkin, NES Working Paper 256 (February 2020), published in the
*Journal of Economic Theory* 188 (2020). Locators are pages of the working paper.

`n` identical players choose effort `e` at cost `c(e)`, strictly increasing and strictly convex,
and produce `e + X_i` with i.i.d. noise. Prizes `V_1 ≥ ⋯ ≥ V_n` sum to one. The symmetric
equilibrium effort solves (4), `c'(e*) = Σ_{r=1}^{n−1} B_{r,n} D_r` with `D_r = V_r − V_{r+1} ≥ 0`,
where by (6) `B_{r,n} = K_{r,n} ∫_0^1 z^{n−r−1} (1 − z)^{r−1} m(z) dz`, `m(z) = f(F⁻¹(z))` and
`K_{r,n} = (n − 1)!/((n − r − 1)! (r − 1)!)`. `X` is more dispersed than `Y` exactly when
`m_X ≤ m_Y` on `[0, 1]` (p.10).

* Proposition 1 (p.10), sufficiency: more dispersed noise gives weakly lower effort (`B_le`,
  `effort_le`, `prop1_sufficiency`, and `prop1_eq6` in the form (4) with (6)).
* Scaling, p.11: `X = σ Y` gives `m_X = m_Y/σ`, so effort falls in `σ ≥ 1` (`quantile_scale`,
  `density_scale`, `m_scale`, `scale_lowers_m`, `scale_lowers_effort`).
* Section 5.3, pp.20–22, endogenous entry. The entrant's payoff is `π_n = 1/n − c(e*_n)` and the
  number of entrants satisfies (17) against the outside option `ω` (`payoff`, `IsEntryCount`).
  Claim 1 (`entry_count_le`, `entry_count_le_model`), Claim 2 (`claim2_cost`, `claim2_effort`),
  Claim 3 (`claim3_total_cost`), controls showing that the monotonicity of `π_Y` and the payoff
  dominance are each load-bearing in Claim 1 and `ω ≥ 0` in Claim 3, and the sign in the
  dispersion reading (`less_noise_fewer_entrants`).

The marginal cost `c'` is not tied to `c` below: the proofs use only that `c` is monotone and
`c'` strictly monotone, which strict monotonicity and strict convexity of `c` supply.
-/

open Set MeasureTheory

namespace DrugovRyvkin2020

/-! ## Proposition 1, sufficiency -/

/-- **Proposition 1, the integral step.** A pointwise smaller inverse quantile density gives a
weakly smaller `∫_0^1 w m` for any weight `w ≥ 0` on `[0, 1]`. -/
theorem B_le (w mX mY : ℝ → ℝ) (hw : ∀ z ∈ Icc (0 : ℝ) 1, 0 ≤ w z)
    (hm : ∀ z ∈ Icc (0 : ℝ) 1, mX z ≤ mY z)
    (hiX : IntegrableOn (fun z => w z * mX z) (Icc 0 1))
    (hiY : IntegrableOn (fun z => w z * mY z) (Icc 0 1)) :
    ∫ z in Icc (0 : ℝ) 1, w z * mX z ≤ ∫ z in Icc (0 : ℝ) 1, w z * mY z :=
  setIntegral_mono_on hiX hiY measurableSet_Icc fun z hz =>
    mul_le_mul_of_nonneg_left (hm z hz) (hw z hz)

/-- **Proposition 1, the first-order condition step.** With `c'` strictly increasing, a weakly
lower right side of (4) gives weakly lower effort. -/
theorem effort_le (c' : ℝ → ℝ) (hc : StrictMono c') (eX eY BX BY : ℝ) (hX : c' eX = BX)
    (hY : c' eY = BY) (hB : BX ≤ BY) : eX ≤ eY :=
  hc.le_iff_le.mp (by rw [hX, hY]; exact hB)

/-- **The strict version.** A strictly lower right side of (4) gives strictly lower effort. -/
theorem effort_lt (c' : ℝ → ℝ) (hc : StrictMono c') (eX eY BX BY : ℝ) (hX : c' eX = BX)
    (hY : c' eY = BY) (hB : BX < BY) : eX < eY :=
  hc.lt_iff_lt.mp (by rw [hX, hY]; exact hB)

/-- **Proposition 1 (sufficiency), over any finite set of ranks.** Write (4) with (6) as
`c'(e) = Σ_{r ∈ s} K_r D_r ∫_0^1 w_r m` with `K_r, D_r ≥ 0` and `w_r ≥ 0` on `[0, 1]`. If `X` is
more dispersed than `Y`, `m_X ≤ m_Y` on `[0, 1]`, then `e*_X ≤ e*_Y`. -/
theorem prop1_sufficiency {ι : Type*} (s : Finset ι) (K D : ι → ℝ) (w : ι → ℝ → ℝ)
    (mX mY c' : ℝ → ℝ) (hc : StrictMono c') (eX eY : ℝ)
    (hK : ∀ r ∈ s, 0 ≤ K r) (hD : ∀ r ∈ s, 0 ≤ D r)
    (hw : ∀ r ∈ s, ∀ z ∈ Icc (0 : ℝ) 1, 0 ≤ w r z)
    (hm : ∀ z ∈ Icc (0 : ℝ) 1, mX z ≤ mY z)
    (hiX : ∀ r ∈ s, IntegrableOn (fun z => w r z * mX z) (Icc 0 1))
    (hiY : ∀ r ∈ s, IntegrableOn (fun z => w r z * mY z) (Icc 0 1))
    (hX : c' eX = ∑ r ∈ s, K r * D r * ∫ z in Icc (0 : ℝ) 1, w r z * mX z)
    (hY : c' eY = ∑ r ∈ s, K r * D r * ∫ z in Icc (0 : ℝ) 1, w r z * mY z) :
    eX ≤ eY :=
  effort_le c' hc eX eY _ _ hX hY (Finset.sum_le_sum fun r hr =>
    mul_le_mul_of_nonneg_left (B_le (w r) mX mY (hw r hr) hm (hiX r hr) (hiY r hr))
      (mul_nonneg (hK r hr) (hD r hr)))

/-- The weight in (6), `z^{n−r−1} (1 − z)^{r−1}`. -/
noncomputable def weight (n r : ℕ) (z : ℝ) : ℝ := z ^ (n - r - 1) * (1 - z) ^ (r - 1)

/-- The weight in (6) is nonnegative on `[0, 1]`. -/
theorem weight_nonneg (n r : ℕ) {z : ℝ} (hz : z ∈ Icc (0 : ℝ) 1) : 0 ≤ weight n r z :=
  mul_nonneg (pow_nonneg hz.1 _) (pow_nonneg (sub_nonneg.2 hz.2) _)

/-- The weight in (6) is continuous. -/
theorem continuous_weight (n r : ℕ) : Continuous (weight n r) := by
  unfold weight
  fun_prop

/-- An integrable `m` stays integrable on `[0, 1]` after multiplying by the weight in (6). -/
theorem integrableOn_weight_mul (n r : ℕ) {m : ℝ → ℝ} (hm : IntegrableOn m (Icc 0 1)) :
    IntegrableOn (fun z => weight n r z * m z) (Icc 0 1) :=
  hm.continuousOn_mul (continuous_weight n r).continuousOn isCompact_Icc

/-- The constant in (6), `K_{r,n} = (n − 1)!/((n − r − 1)! (r − 1)!)`. -/
noncomputable def coeffK (n r : ℕ) : ℝ :=
  ((n - 1).factorial : ℝ) / (((n - r - 1).factorial : ℝ) * ((r - 1).factorial : ℝ))

/-- The constant in (6) is positive. -/
theorem coeffK_pos (n r : ℕ) : 0 < coeffK n r := by
  unfold coeffK
  positivity

/-- The right side of (4) with (6),
`Σ_{r=1}^{n−1} K_{r,n} (V_r − V_{r+1}) ∫_0^1 z^{n−r−1} (1 − z)^{r−1} m(z) dz`, with `K_{r,n}`
left general (the paper's value is `coeffK n r`). -/
noncomputable def focRHS (n : ℕ) (K V : ℕ → ℝ) (m : ℝ → ℝ) : ℝ :=
  ∑ r ∈ Finset.Icc 1 (n - 1), K r * (V r - V (r + 1)) * ∫ z in Icc (0 : ℝ) 1, weight n r z * m z

/-- **Proposition 1 (sufficiency), in the form (4) with (6).** Prizes weakly fall with rank,
`V_{r+1} ≤ V_r`, and `K_{r,n} ≥ 0` for `r = 1, …, n − 1`, and `m_X, m_Y` are integrable on
`[0, 1]`. If `X` is more dispersed than `Y`, `m_X ≤ m_Y` on `[0, 1]`, then `e*_X ≤ e*_Y`. -/
theorem prop1_eq6 (n : ℕ) (K V : ℕ → ℝ) (mX mY c' : ℝ → ℝ) (hc : StrictMono c') (eX eY : ℝ)
    (hK : ∀ r ∈ Finset.Icc 1 (n - 1), 0 ≤ K r)
    (hV : ∀ r ∈ Finset.Icc 1 (n - 1), V (r + 1) ≤ V r)
    (hmX : IntegrableOn mX (Icc 0 1)) (hmY : IntegrableOn mY (Icc 0 1))
    (hm : ∀ z ∈ Icc (0 : ℝ) 1, mX z ≤ mY z)
    (hX : c' eX = focRHS n K V mX) (hY : c' eY = focRHS n K V mY) : eX ≤ eY :=
  prop1_sufficiency (Finset.Icc 1 (n - 1)) K (fun r => V r - V (r + 1)) (weight n) mX mY c' hc
    eX eY hK (fun r hr => sub_nonneg.2 (hV r hr)) (fun r _ _ hz => weight_nonneg n r hz) hm
    (fun r _ => integrableOn_weight_mul n r hmX) (fun r _ => integrableOn_weight_mul n r hmY)
    hX hY

/-! ## Scaling, p.11 -/

/-- **Scaling, quantiles.** If `F_X(x) = F_Y(x/σ)` with `σ > 0` and `q_Y(z)` is a `z`-quantile
of `F_Y`, then `σ q_Y(z)` is a `z`-quantile of `F_X`. -/
theorem quantile_scale (FX FY qY : ℝ → ℝ) (σ : ℝ) (hσ : 0 < σ) (hF : ∀ x, FX x = FY (x / σ))
    (z : ℝ) (hq : FY (qY z) = z) : FX (σ * qY z) = z := by
  rw [hF, mul_div_cancel_left₀ _ hσ.ne', hq]

/-- **Scaling, densities.** If `F_X(x) = F_Y(x/σ)` and `F_Y` has density `f_Y(x/σ)` at `x/σ`, then
`F_X` has density `f_Y(x/σ)/σ` at `x`. -/
theorem density_scale (FX FY fY : ℝ → ℝ) (σ : ℝ) (hF : ∀ x, FX x = FY (x / σ)) (x : ℝ)
    (hd : HasDerivAt FY (fY (x / σ)) (x / σ)) : HasDerivAt FX (fY (x / σ) / σ) x := by
  have e : FX = fun y => FY (y / σ) := funext hF
  rw [e]
  have h := hd.comp x ((hasDerivAt_id x).div_const σ)
  convert h using 1
  simp [div_eq_mul_inv]

/-- **Scaling, p.11.** If `X = σ Y` with `σ > 0`, so that `f_X(x) = f_Y(x/σ)/σ` and
`q_X = σ q_Y`, then `m_X(z) = f_X(q_X(z)) = f_Y(q_Y(z))/σ = m_Y(z)/σ`. -/
theorem m_scale (fX fY qX qY : ℝ → ℝ) (σ : ℝ) (hσ : 0 < σ) (hf : ∀ x, fX x = fY (x / σ) / σ)
    (hq : ∀ z, qX z = σ * qY z) (z : ℝ) : fX (qX z) = fY (qY z) / σ := by
  rw [hf, hq, mul_div_cancel_left₀ _ hσ.ne']

/-- **Scaling up the noise lowers `m`**: for `σ ≥ 1` and `f_Y ≥ 0`, `m_Y(z)/σ ≤ m_Y(z)`. -/
theorem scale_lowers_m (fY qY : ℝ → ℝ) (σ : ℝ) (hσ : 1 ≤ σ) (z : ℝ) (hf : 0 ≤ fY (qY z)) :
    fY (qY z) / σ ≤ fY (qY z) :=
  div_le_self hf hσ

/-- **Effort falls in the scale of the noise** (p.11). For `X = σ Y` with `σ ≥ 1` and density
`f_Y ≥ 0`, the effort under `X` is weakly below the effort under `Y`, in the form (4) with (6). -/
theorem scale_lowers_effort (n : ℕ) (K V : ℕ → ℝ) (fX fY qX qY c' : ℝ → ℝ)
    (hc : StrictMono c') (σ : ℝ) (hσ : 1 ≤ σ) (hf : ∀ x, fX x = fY (x / σ) / σ)
    (hq : ∀ z, qX z = σ * qY z) (hfY : ∀ x, 0 ≤ fY x)
    (hK : ∀ r ∈ Finset.Icc 1 (n - 1), 0 ≤ K r)
    (hV : ∀ r ∈ Finset.Icc 1 (n - 1), V (r + 1) ≤ V r)
    (hmY : IntegrableOn (fun z => fY (qY z)) (Icc 0 1)) (eX eY : ℝ)
    (hX : c' eX = focRHS n K V (fun z => fX (qX z)))
    (hY : c' eY = focRHS n K V (fun z => fY (qY z))) : eX ≤ eY := by
  have hσ0 : 0 < σ := lt_of_lt_of_le one_pos hσ
  have hmeq : (fun z => fX (qX z)) = fun z => fY (qY z) / σ :=
    funext (m_scale fX fY qX qY σ hσ0 hf hq)
  have hmX : IntegrableOn (fun z => fX (qX z)) (Icc 0 1) := by
    rw [hmeq]
    exact hmY.div_const σ
  refine prop1_eq6 n K V _ _ c' hc eX eY hK hV hmX hmY (fun z _ => ?_) hX hY
  rw [m_scale fX fY qX qY σ hσ0 hf hq]
  exact scale_lowers_m fY qY σ hσ z (hfY _)

/-! ## Section 5.3, endogenous number of players -/

/-- The entrant's payoff with `n` entrants, `π_n = 1/n − k_n`, where `k_n = c(e*_n)` is the cost
of equilibrium effort; the prizes sum to one. -/
noncomputable def payoff (k : ℕ → ℝ) (n : ℕ) : ℝ := 1 / (n : ℝ) - k n

/-- Condition (17) on the number of entrants `n` out of `N` potential players: at least one
enters, entrants earn at least the outside option `ω`, and one more entrant would earn less than
`ω` unless all `N` are in. -/
def IsEntryCount (pay : ℕ → ℝ) (ω : ℝ) (N n : ℕ) : Prop :=
  1 ≤ n ∧ n ≤ N ∧ ω ≤ pay n ∧ (n < N → pay (n + 1) < ω)

/-- Total equilibrium cost with `n` entrants, `C_n = n k_n`. -/
noncomputable def totalCost (k : ℕ → ℝ) (n : ℕ) : ℝ := (n : ℝ) * k n

/-- **Section 5.3, Claim 1 (p.21).** If entrants under noise `X` earn no more than under noise
`Y` at every `n ≤ N`, and `π_Y` weakly falls in `n`, then `n_X ≤ n_Y`. Only the parts of (17)
the proof uses are assumed: `n_X` is admissible for `X`, and `n_Y + 1` entrants would earn less
than `ω` under `Y`. -/
theorem entry_count_le (πX πY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ)
    (hdom : ∀ n, 1 ≤ n → n ≤ N → πX n ≤ πY n)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N → πY b ≤ πY a)
    (hXin : 1 ≤ nX ∧ nX ≤ N ∧ ω ≤ πX nX) (hYout : nY < N → πY (nY + 1) < ω) : nX ≤ nY := by
  obtain ⟨h1, hN, hω⟩ := hXin
  by_contra h
  push_neg at h
  have hout := hYout (lt_of_lt_of_le h hN)
  have hd := hdec (nY + 1) nX (Nat.le_add_left 1 nY) (Nat.succ_le_of_lt h) hN
  linarith [hdom nX h1 hN]

/-- **Claim 1 with `π_X` decreasing instead of `π_Y`.** The proof runs through `π_X(n_Y + 1)`
instead of `π_Y(n_X)`. So a counterexample to Claim 1 without `hdec` needs both payoffs
non-monotone, as in `control_needs_decreasing`. -/
theorem entry_count_le_of_decX (πX πY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ)
    (hdom : ∀ n, 1 ≤ n → n ≤ N → πX n ≤ πY n)
    (hdecX : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N → πX b ≤ πX a)
    (hXin : 1 ≤ nX ∧ nX ≤ N ∧ ω ≤ πX nX) (hYout : nY < N → πY (nY + 1) < ω) : nX ≤ nY := by
  obtain ⟨h1, hN, hω⟩ := hXin
  by_contra h
  push_neg at h
  have hlt : nY + 1 ≤ nX := Nat.succ_le_of_lt h
  have hout := hYout (lt_of_lt_of_le h hN)
  have hd := hdecX (nY + 1) nX (Nat.le_add_left 1 nY) hlt hN
  linarith [hdom (nY + 1) (Nat.le_add_left 1 nY) (le_trans hlt hN)]

/-- **Higher effort, lower payoff.** With `c` monotone, `e*_{Y,n} ≤ e*_{X,n}` gives
`π_{X,n} ≤ π_{Y,n}`. -/
theorem profit_le (c : ℝ → ℝ) (hc : Monotone c) (eX eY : ℕ → ℝ) (n : ℕ) (he : eY n ≤ eX n) :
    payoff (fun m => c (eX m)) n ≤ payoff (fun m => c (eY m)) n := by
  unfold payoff
  linarith [hc he]

/-- **Claim 1 in the model.** With `π_{·,n} = 1/n − c(e*_{·,n})`, `c` monotone,
`e*_{X,n} ≥ e*_{Y,n}` for `1 ≤ n ≤ N` and `π_Y` weakly falling in `n`, `n_X ≤ n_Y`. -/
theorem entry_count_le_model (c : ℝ → ℝ) (hc : Monotone c) (eX eY : ℕ → ℝ) (ω : ℝ)
    (N nX nY : ℕ) (he : ∀ n, 1 ≤ n → n ≤ N → eY n ≤ eX n)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N →
      payoff (fun m => c (eY m)) b ≤ payoff (fun m => c (eY m)) a)
    (hXin : 1 ≤ nX ∧ nX ≤ N ∧ ω ≤ payoff (fun m => c (eX m)) nX)
    (hYout : nY < N → payoff (fun m => c (eY m)) (nY + 1) < ω) : nX ≤ nY :=
  entry_count_le _ _ ω N nX nY (fun n h1 hN => profit_le c hc eX eY n (he n h1 hN)) hdec hXin
    hYout

/-- From `n_X < n_Y` and (17) for both noises, the cost of effort with `n_X + 1` entrants under
`X` exceeds `1/(n_X + 1) − ω`, which is at least `1/n_Y − ω`, which bounds the cost under `Y`
with `n_Y` entrants. -/
theorem cost_gap (kX kY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ) (hlt : nX < nY)
    (hX : IsEntryCount (payoff kX) ω N nX) (hY : IsEntryCount (payoff kY) ω N nY) :
    1 / ((nX + 1 : ℕ) : ℝ) - ω < kX (nX + 1) ∧ 1 / (nY : ℝ) - ω ≤ 1 / ((nX + 1 : ℕ) : ℝ) - ω ∧
      kY nY ≤ 1 / (nY : ℝ) - ω := by
  have hXout := hX.2.2.2 (lt_of_lt_of_le hlt hY.2.1)
  have hYin := hY.2.2.1
  unfold payoff at hXout hYin
  have hle : ((nX + 1 : ℕ) : ℝ) ≤ (nY : ℝ) := by exact_mod_cast hlt
  have hinv : 1 / (nY : ℝ) ≤ 1 / ((nX + 1 : ℕ) : ℝ) :=
    one_div_le_one_div_of_le (by positivity) hle
  refine ⟨by linarith, by linarith, by linarith⟩

/-- **Section 5.3, Claim 2 (p.21).** Write `k_{·,n} = c(e*_{·,n})`. If `k_{Y,n} ≤ k_{X,n}` for
`1 ≤ n ≤ N`, `π_Y` weakly falls in `n`, and `n_X`, `n_Y` satisfy (17), then
`c(e*_{X,n_X}) ≥ c(e*_{Y,n_Y})` or `c(e*_{X,n_X+1}) > c(e*_{Y,n_Y})`. -/
theorem claim2_cost (kX kY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ)
    (hdom : ∀ n, 1 ≤ n → n ≤ N → kY n ≤ kX n)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N → payoff kY b ≤ payoff kY a)
    (hX : IsEntryCount (payoff kX) ω N nX) (hY : IsEntryCount (payoff kY) ω N nY) :
    kY nY ≤ kX nX ∨ kY nY < kX (nX + 1) := by
  have hle : nX ≤ nY := entry_count_le (payoff kX) (payoff kY) ω N nX nY
    (fun n h1 hN => by unfold payoff; linarith [hdom n h1 hN]) hdec
    ⟨hX.1, hX.2.1, hX.2.2.1⟩ hY.2.2.2
  rcases hle.lt_or_eq with hlt | heq
  · right
    obtain ⟨g1, g2, g3⟩ := cost_gap kX kY ω N nX nY hlt hX hY
    linarith
  · left
    subst heq
    exact hdom nX hX.1 hX.2.1

/-- **Section 5.3, Claim 2 in effort, as the paper states it (p.21).** With `c` strictly
increasing and `e*_{X,n} ≥ e*_{Y,n}` for `1 ≤ n ≤ N`, the cost comparison of `claim2_cost`
reads `e*_{X,n_X} ≥ e*_{Y,n_Y}` or `e*_{X,n_X+1} > e*_{Y,n_Y}`. -/
theorem claim2_effort (c : ℝ → ℝ) (hc : StrictMono c) (eX eY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ)
    (he : ∀ n, 1 ≤ n → n ≤ N → eY n ≤ eX n)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N →
      payoff (fun m => c (eY m)) b ≤ payoff (fun m => c (eY m)) a)
    (hX : IsEntryCount (payoff fun m => c (eX m)) ω N nX)
    (hY : IsEntryCount (payoff fun m => c (eY m)) ω N nY) :
    eY nY ≤ eX nX ∨ eY nY < eX (nX + 1) :=
  (claim2_cost _ _ ω N nX nY (fun n h1 hN => hc.monotone (he n h1 hN)) hdec hX hY).imp
    hc.le_iff_le.mp hc.lt_iff_lt.mp

/-- **Section 5.3, Claim 3 (pp.21–22).** Under the hypotheses of Claim 2 and `ω ≥ 0` (the paper
has `ω > 0`; `control_claim3_needs_nonneg_outside` shows the sign is needed), total cost
`C_{·,n} = n c(e*_{·,n})` satisfies `C_{X,n_X} ≥ C_{Y,n_Y}` or `C_{X,n_X+1} > C_{Y,n_Y}`. -/
theorem claim3_total_cost (kX kY : ℕ → ℝ) (ω : ℝ) (hω : 0 ≤ ω) (N nX nY : ℕ)
    (hdom : ∀ n, 1 ≤ n → n ≤ N → kY n ≤ kX n)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N → payoff kY b ≤ payoff kY a)
    (hX : IsEntryCount (payoff kX) ω N nX) (hY : IsEntryCount (payoff kY) ω N nY) :
    totalCost kY nY ≤ totalCost kX nX ∨ totalCost kY nY < totalCost kX (nX + 1) := by
  have hle : nX ≤ nY := entry_count_le (payoff kX) (payoff kY) ω N nX nY
    (fun n h1 hN => by unfold payoff; linarith [hdom n h1 hN]) hdec
    ⟨hX.1, hX.2.1, hX.2.2.1⟩ hY.2.2.2
  unfold totalCost
  rcases hle.lt_or_eq with hlt | heq
  · right
    obtain ⟨g1, g2, g3⟩ := cost_gap kX kY ω N nX nY hlt hX hY
    set a : ℝ := ((nX + 1 : ℕ) : ℝ) with ha_def
    have ha : 0 < a := by positivity
    have hab : a ≤ (nY : ℝ) := by rw [ha_def]; exact_mod_cast hlt
    have hb : 0 < (nY : ℝ) := lt_of_lt_of_le ha hab
    -- `a k_{X,n_X+1} > 1 − a ω ≥ 1 − n_Y ω ≥ n_Y k_{Y,n_Y}`
    have hA : 1 - a * ω < a * kX (nX + 1) := by
      have := mul_lt_mul_of_pos_left g1 ha
      rwa [mul_sub, mul_one_div_cancel ha.ne'] at this
    have hB : (nY : ℝ) * kY nY ≤ 1 - (nY : ℝ) * ω := by
      have := mul_le_mul_of_nonneg_left g3 hb.le
      rwa [mul_sub, mul_one_div_cancel hb.ne'] at this
    have hC : a * ω ≤ (nY : ℝ) * ω := mul_le_mul_of_nonneg_right hab hω
    linarith
  · left
    subst heq
    exact mul_le_mul_of_nonneg_left (hdom nX hX.1 hX.2.1) (Nat.cast_nonneg nX)

/-! ## Controls -/

/-- Control payoffs under `X` with a dip at two entrants: `(1, −1, 1)` on `{1, 2, 3}`. -/
noncomputable def dipX (n : ℕ) : ℝ := if n = 2 then -1 else 1

/-- Control payoffs under `Y` with a dip at two entrants: `(1, 0, 1)` on `{1, 2, 3}`. -/
noncomputable def dipY (n : ℕ) : ℝ := if n = 2 then 0 else 1

/-- **Control: Claim 1 needs `π_Y` to fall in `n`.** With `N = 3` and `ω = 1`, `dipX ≤ dipY` on
`{1, 2, 3}` (`hdom`), `n_X = 3` and `n_Y = 1` satisfy (17) for `dipX` and `dipY` (which contains
`hXin` and `hYout`), `dipY` is not decreasing (`hdec` fails), and `n_X ≤ n_Y` fails. Without
monotonicity (17) has two solutions for each payoff, `n = 1` and `n = 3`, and the choice
`n_X = 3`, `n_Y = 1` breaks the order. Neither payoff is monotone, as `entry_count_le_of_decX`
requires. -/
theorem control_needs_decreasing :
    (∀ n, 1 ≤ n → n ≤ 3 → dipX n ≤ dipY n) ∧ IsEntryCount dipX 1 3 3 ∧
      IsEntryCount dipY 1 3 1 ∧ ¬ (∀ a b, 1 ≤ a → a ≤ b → b ≤ 3 → dipY b ≤ dipY a) ∧
      ¬ ((3 : ℕ) ≤ 1) := by
  refine ⟨fun n _ _ => ?_, ?_, ?_, fun h => ?_, by norm_num⟩
  · unfold dipX dipY
    split_ifs <;> norm_num
  · refine ⟨by norm_num, le_rfl, by norm_num [dipX], fun h => absurd h (lt_irrefl 3)⟩
  · exact ⟨le_rfl, by norm_num, by norm_num [dipY], fun _ => by norm_num [dipY]⟩
  · have := h 2 3 (by norm_num) (by norm_num) le_rfl
    norm_num [dipY] at this

/-- Control payoffs under `X`, flat at one: `(1, 1, 1)` on `{1, 2, 3}`. -/
noncomputable def flatX (_n : ℕ) : ℝ := 1

/-- Control payoffs under `Y`, a step down after one entrant: `(1, 0, 0)` on `{1, 2, 3}`. -/
noncomputable def stepY (n : ℕ) : ℝ := if n = 1 then 1 else 0

/-- **Control: Claim 1 needs the payoff dominance `π_X ≤ π_Y`.** With `N = 3` and `ω = 1`,
`stepY` weakly falls on `{1, 2, 3}` (`hdec`; `flatX` does too), `n_X = 3` and `n_Y = 1` satisfy
(17) for `flatX` and `stepY` (which contains `hXin` and `hYout`) and are its only solutions,
dominance fails at `n = 2`, and `n_X ≤ n_Y` fails. -/
theorem control_needs_dominance :
    (∀ a b, 1 ≤ a → a ≤ b → b ≤ 3 → stepY b ≤ stepY a) ∧
      (∀ a b, 1 ≤ a → a ≤ b → b ≤ 3 → flatX b ≤ flatX a) ∧
      IsEntryCount flatX 1 3 3 ∧ IsEntryCount stepY 1 3 1 ∧
      ¬ (∀ n, 1 ≤ n → n ≤ 3 → flatX n ≤ stepY n) ∧ ¬ ((3 : ℕ) ≤ 1) := by
  refine ⟨fun a b ha hab _ => ?_, fun _ _ _ _ _ => le_rfl, ?_, ?_, fun h => ?_, by norm_num⟩
  · unfold stepY
    split_ifs with h1 h2 h2
    · exact le_rfl
    · omega
    · norm_num
    · exact le_rfl
  · exact ⟨by norm_num, le_rfl, by norm_num [flatX], fun h => absurd h (lt_irrefl 3)⟩
  · exact ⟨le_rfl, by norm_num, by norm_num [stepY], fun _ => by norm_num [stepY]⟩
  · have := h 2 (by norm_num) (by norm_num)
    norm_num [flatX, stepY] at this

/-- **Claim 1 is false without `hdec`.** -/
theorem entry_count_le_needs_decreasing :
    ¬ ∀ (πX πY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ), (∀ n, 1 ≤ n → n ≤ N → πX n ≤ πY n) →
      (1 ≤ nX ∧ nX ≤ N ∧ ω ≤ πX nX) → (nY < N → πY (nY + 1) < ω) → nX ≤ nY := by
  intro h
  obtain ⟨hdom, hX, hY, -, hfail⟩ := control_needs_decreasing
  exact hfail (h dipX dipY 1 3 3 1 hdom ⟨hX.1, hX.2.1, hX.2.2.1⟩ hY.2.2.2)

/-- **Claim 1 is false without `hdom`.** -/
theorem entry_count_le_needs_dominance :
    ¬ ∀ (πX πY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ),
      (∀ a b, 1 ≤ a → a ≤ b → b ≤ N → πY b ≤ πY a) →
      (1 ≤ nX ∧ nX ≤ N ∧ ω ≤ πX nX) → (nY < N → πY (nY + 1) < ω) → nX ≤ nY := by
  intro h
  obtain ⟨hdec, -, hX, hY, -, hfail⟩ := control_needs_dominance
  exact hfail (h flatX stepY 1 3 3 1 hdec ⟨hX.1, hX.2.1, hX.2.2.1⟩ hY.2.2.2)

/-- Control costs under `X`: `k_X = (0, 2, 4/3)` on `{1, 2, 3}`. -/
noncomputable def costX3 : ℕ → ℝ
  | 2 => 2
  | 3 => 4 / 3
  | _ => 0

/-- Control costs under `Y`: `k_Y = (0, 0, 4/3)` on `{1, 2, 3}`. -/
noncomputable def costY3 : ℕ → ℝ
  | 3 => 4 / 3
  | _ => 0

/-- **Control: Claim 3 needs `ω ≥ 0`.** With `N = 3` and `ω = −1`, `k_Y ≤ k_X` on `{1, 2, 3}`,
`π_Y = (1, 1/2, −1)` falls, and `n_X = 1`, `n_Y = 3` satisfy (17), so every hypothesis of
`claim3_total_cost` holds except `0 ≤ ω`. Yet `C_{Y,3} = 4` exceeds `C_{X,1} = 0` and equals
`C_{X,2} = 4`. -/
theorem control_claim3_needs_nonneg_outside :
    (∀ n, 1 ≤ n → n ≤ 3 → costY3 n ≤ costX3 n) ∧
      (∀ a b, 1 ≤ a → a ≤ b → b ≤ 3 → payoff costY3 b ≤ payoff costY3 a) ∧
      IsEntryCount (payoff costX3) (-1) 3 1 ∧ IsEntryCount (payoff costY3) (-1) 3 3 ∧
      ¬ (totalCost costY3 3 ≤ totalCost costX3 1 ∨
        totalCost costY3 3 < totalCost costX3 (1 + 1)) := by
  refine ⟨fun n h1 h3 => ?_, fun a b ha hab hb => ?_, ?_, ?_, ?_⟩
  · interval_cases n <;> norm_num [costX3, costY3]
  · interval_cases b <;> interval_cases a <;> norm_num [payoff, costY3]
  · exact ⟨le_rfl, by norm_num, by norm_num [payoff, costX3],
      fun _ => by norm_num [payoff, costX3]⟩
  · exact ⟨by norm_num, le_rfl, by norm_num [payoff, costY3], fun h => absurd h (lt_irrefl 3)⟩
  · norm_num [totalCost, costX3, costY3]

/-! ## The sign -/

/-- **Less noise, fewer entrants** (Section 5.3 in the dispersion reading). Suppose `Y` is more
dispersed than `X`, `m_Y ≤ m_X` on `[0, 1]`, and for each `1 ≤ n ≤ N` the efforts `e*_{X,n}` and
`e*_{Y,n}` solve (4) with (6) for the same prizes and constants. Proposition 1 then gives
`e*_{X,n} ≥ e*_{Y,n}`, and if `π_Y` weakly falls in `n`, Claim 1 gives `n_X ≤ n_Y`: the less
dispersed noise `X` draws weakly fewer entrants. -/
theorem less_noise_fewer_entrants (c c' : ℝ → ℝ) (hc : Monotone c) (hc' : StrictMono c')
    (K V : ℕ → ℕ → ℝ) (hK : ∀ n, ∀ r ∈ Finset.Icc 1 (n - 1), 0 ≤ K n r)
    (hV : ∀ n, ∀ r ∈ Finset.Icc 1 (n - 1), V n (r + 1) ≤ V n r)
    (mX mY : ℝ → ℝ) (hmX : IntegrableOn mX (Icc 0 1)) (hmY : IntegrableOn mY (Icc 0 1))
    (hdisp : ∀ z ∈ Icc (0 : ℝ) 1, mY z ≤ mX z) (eX eY : ℕ → ℝ) (ω : ℝ) (N nX nY : ℕ)
    (hfocX : ∀ n, 1 ≤ n → n ≤ N → c' (eX n) = focRHS n (K n) (V n) mX)
    (hfocY : ∀ n, 1 ≤ n → n ≤ N → c' (eY n) = focRHS n (K n) (V n) mY)
    (hdec : ∀ a b, 1 ≤ a → a ≤ b → b ≤ N →
      payoff (fun m => c (eY m)) b ≤ payoff (fun m => c (eY m)) a)
    (hXin : 1 ≤ nX ∧ nX ≤ N ∧ ω ≤ payoff (fun m => c (eX m)) nX)
    (hYout : nY < N → payoff (fun m => c (eY m)) (nY + 1) < ω) : nX ≤ nY :=
  entry_count_le_model c hc eX eY ω N nX nY
    (fun n h1 hN => prop1_eq6 n (K n) (V n) mY mX c' hc' (eY n) (eX n) (hK n) (hV n) hmY hmX
      hdisp (hfocY n h1 hN) (hfocX n h1 hN))
    hdec hXin hYout

end DrugovRyvkin2020
