/-
# Bohren, Imas & Rosenberg (2019), "The Dynamics of Discrimination: Theory and Evidence"

*American Economic Review* 109(10), 3395-3436.  The local copy is the January 2019
working paper; every page cited below is that paper's **printed** page (printed
page = PDF page - 1), not the AER pagination.

Formalization of the paper's own normal-normal model (their Section 2 and
Appendix A.1), not of Paper B's claims.

## The paper's setup (pp.9-11, 15)

Ability `a ~ N(μ_g, 1/τa)`, quality `q_t = a + ε_t` with `ε_t ~ N(0, 1/τε)`, signal
`s_t = q_t + η_t` with `η_t ~ N(0, 1/τη)`.  An evaluator of type `i` has subjective
prior mean `μ̂_g` and taste `c_g` (`c_M = 0`), and reports

  `v_i(h,s,g) = Ê_i[q | h,s,g] - c^i_g`                         (eq. 2, p.11)

Discrimination is `D_i(h,s) = v_i(h,s,M) - v_i(h,s,F)` (eq. 3, p.12).  The prior on
first-period quality has precision `τq = τa τε/(τa+τε)` (p.15).

## What is formalized

* `complete_square`, `posterior_kernel` -- the conjugacy step behind "`q₁|s₁` is
  normal with mean `(τq μ̂ + τη s₁)/(τq+τη)`" (pp.15-16), as the pointwise
  completing-the-square identity of the Gaussian kernels.  The posterior mean
  `postMean` is then used as a definition.
* `disc_initial` -- eq. (5), p.16; `disc_aggregate`, `disc_eq7` -- the aggregate
  formula and eq. (7) (pp.17, 20).
* `prop1_decreasing`, `prop1_constant`, `prop1_limit` -- Proposition 1 (p.16).
* `fn9_equivalent` -- footnote 9 (p.16): observational equivalence of belief- and
  preference-based partiality at one set of parameters.
* `exo_gap` -- footnote 10 (p.19): with *exogenous* informational content, the
  posterior mean is increasing in the prior mean (coefficient `τa/(τa+τεη)`).
* `endo_gap_eq12`, `endo_gap` -- the *endogenous* case that Proposition 2 is about:
  eq. (12) (p.50), and its simplification: the belief gap contracts by exactly
  `τa/(τa+τε)`, whatever the evaluation `v` and the signal precision `τη`.
  `channels` records the two opposing channels of p.19; `endo_faster` shows the
  endogenous contraction is strictly faster than the exogenous one (the "speeds
  up the mitigation" claim of p.2).
* `prop2_no_reversal`, `prop2_decreasing` -- Proposition 2 (p.18) along an
  arbitrary evaluation history, including the step "it remains to show that
  discrimination decreases", which the paper's proof (p.51) states but does not
  complete.
* `impartial_favors_female`, `prop3_sign`, `prop3_cutoff`, `prop3_condition_iff` --
  the structure of Proposition 3 (p.21) and its proof (pp.51-54).
-/
import Mathlib

namespace Literature.BohrenImasRosenberg

open Filter Topology Finset

/-! ## First period: conjugacy, eq. (4), eq. (5), Proposition 1 -/

/-- Posterior mean of quality after one signal, `(τq μ + τη s)/(τq + τη)` (p.16). -/
noncomputable def postMean (τq τη μ s : ℝ) : ℝ := (τq * μ + τη * s) / (τq + τη)

/-- **Conjugacy, as completing the square.**  The exponent of prior kernel times
likelihood kernel is `(τq+τη)(q - postMean)²` plus a term free of `q`: the
posterior on `q` is normal with mean `postMean` and precision `τq + τη`
(pp.15-16, "is also normally distributed"). -/
theorem complete_square (τq τη μ s q : ℝ) (hq : 0 < τq) (hη : 0 < τη) :
    τq * (q - μ) ^ 2 + τη * (s - q) ^ 2
      = (τq + τη) * (q - postMean τq τη μ s) ^ 2
        + τq * τη / (τq + τη) * (s - μ) ^ 2 := by
  unfold postMean
  have h : τq + τη ≠ 0 := by positivity
  field_simp
  ring

/-- The same identity at the level of the Gaussian kernels. -/
theorem posterior_kernel (τq τη μ s q : ℝ) (hq : 0 < τq) (hη : 0 < τη) :
    Real.exp (-(τq * (q - μ) ^ 2) / 2) * Real.exp (-(τη * (s - q) ^ 2) / 2)
      = Real.exp (-((τq + τη) * (q - postMean τq τη μ s) ^ 2) / 2)
        * Real.exp (-(τq * τη / (τq + τη) * (s - μ) ^ 2) / 2) := by
  rw [← Real.exp_add, ← Real.exp_add]
  congr 1
  linear_combination (-1 / 2 : ℝ) * complete_square τq τη μ s q hq hη

/-- Optimal evaluation, eq. (4) (p.16): `v = postMean - c`. -/
noncomputable def eval (τq τη μ c s : ℝ) : ℝ := postMean τq τη μ s - c

/-- The evaluation is strictly increasing in the signal and in the prior mean
(p.16, "strictly increasing in `s₁` and `μ̂_g`"). -/
theorem eval_strictMono_prior (τq τη c s : ℝ) (hq : 0 < τq) (hη : 0 < τη) :
    StrictMono fun μ => eval τq τη μ c s := by
  intro μ₁ μ₂ h
  unfold eval postMean
  have : (τq * μ₁ + τη * s) / (τq + τη) < (τq * μ₂ + τη * s) / (τq + τη) :=
    div_lt_div_of_pos_right (by nlinarith) (by positivity)
  linarith

/-- **Eq. (5)** (p.16).  Initial discrimination is independent of the signal:
`D(h₁,s₁) = τq/(τq+τη) (μ̂_M - μ̂_F) + c_F`. -/
theorem disc_initial (τq τη μM μF cF s : ℝ) (h : 0 < τq + τη) :
    eval τq τη μM 0 s - eval τq τη μF cF s = τq / (τq + τη) * (μM - μF) + cF := by
  unfold eval postMean
  field_simp
  ring

/-- **Proposition 1, belief part** (p.16).  With belief-based partiality
`μ̂_F < μ̂_M`, initial discrimination is strictly decreasing in `τη`. -/
theorem prop1_decreasing (τq μM μF cF s τη₁ τη₂ : ℝ) (hq : 0 < τq) (h₁ : 0 < τη₁)
    (h12 : τη₁ < τη₂) (hμ : μF < μM) :
    eval τq τη₂ μM 0 s - eval τq τη₂ μF cF s < eval τq τη₁ μM 0 s - eval τq τη₁ μF cF s := by
  rw [disc_initial _ _ _ _ _ _ (by linarith), disc_initial _ _ _ _ _ _ (by linarith)]
  have : τq / (τq + τη₂) < τq / (τq + τη₁) :=
    div_lt_div_of_pos_left hq (by linarith) (by linarith)
  nlinarith

/-- **Proposition 1, otherwise** (p.16).  Without belief-based partiality initial
discrimination is `c_F`, constant in `τη`. -/
theorem prop1_constant (τq τη μ cF s : ℝ) (h : 0 < τq + τη) :
    eval τq τη μ 0 s - eval τq τη μ cF s = cF := by
  rw [disc_initial _ _ _ _ _ _ h]; ring

/-- **Proposition 1, limit** (p.16 and its proof, p.47): as `τη → ∞`, initial
discrimination tends to `c_F`; so it survives perfect objectivity iff `c_F ≠ 0`. -/
theorem prop1_limit (τq μM μF cF : ℝ) :
    Tendsto (fun τη => τq / (τq + τη) * (μM - μF) + cF) atTop (𝓝 cF) := by
  have h1 : Tendsto (fun τη : ℝ => τq + τη) atTop atTop :=
    tendsto_atTop_add_const_left _ _ tendsto_id
  have h2 : Tendsto (fun τη : ℝ => τq / (τq + τη)) atTop (𝓝 0) :=
    tendsto_const_nhds.div_atTop h1
  simpa using (h2.mul_const (μM - μF)).add_const cF

/-- **Footnote 9** (p.16).  A belief-partial evaluator (`μ̂_F < μ̂_M`, `c = 0`) and a
preference-partial one (`μ̂_F = μ̂_M`, `c_F = τq(μ̂_M - μ̂_F)/(τq+τη)`) give every
female worker the same evaluation, for every signal. -/
theorem fn9_equivalent (τq τη μM μF s : ℝ) (h : 0 < τq + τη) :
    eval τq τη μF 0 s = eval τq τη μM (τq * (μM - μF) / (τq + τη)) s := by
  unfold eval postMean
  field_simp
  ring

/-- **Aggregate discrimination** (p.17): over any finite set of types with weights
`π`, `D = τq/(τq+τη) E_π[μ̂_M - μ̂_F] + E_π[c_F]`.  (The paper introduces this
formula on p.17 as an extension; eq. (5) on p.16 is the single-evaluator case.)
The weights need not sum to one. -/
theorem disc_aggregate {ι : Type*} (S : Finset ι) (π μM μF cF : ι → ℝ) (τq τη s : ℝ)
    (h : 0 < τq + τη) :
    ∑ i ∈ S, π i * (eval τq τη (μM i) 0 s - eval τq τη (μF i) (cF i) s)
      = τq / (τq + τη) * ∑ i ∈ S, π i * (μM i - μF i) + ∑ i ∈ S, π i * cF i := by
  simp_rw [disc_initial _ _ _ _ _ _ h, mul_add, Finset.sum_add_distrib, Finset.mul_sum]
  congr 1
  refine Finset.sum_congr rfl fun i _ => ?_
  ring

/-- **Eq. (7)** (p.20): heuristic type (weight `p`, female prior `μ̂¹_F`) and
impartial type (weight `1-p`, `μ̂²_F = μ̂_M`), no tastes. -/
theorem disc_eq7 (τq τη μM μ1F p s : ℝ) (h : 0 < τq + τη) :
    p * (eval τq τη μM 0 s - eval τq τη μ1F 0 s)
      + (1 - p) * (eval τq τη μM 0 s - eval τq τη μM 0 s)
      = τq / (τq + τη) * (p * (μM - μ1F)) := by
  rw [disc_initial _ _ _ _ _ _ h, disc_initial _ _ _ _ _ _ h]; ring

/-! ## Dynamics: exogenous versus endogenous informational content -/

/-- `τq ≡ τa τε/(τa+τε)` (p.15; eq. (8), p.47 at period `t`). -/
noncomputable def tauQ (τa τε : ℝ) : ℝ := τa * τε / (τa + τε)

/-- `τεη ≡ τε τη/(τε+τη)`, the precision of `s | a` (p.47). -/
noncomputable def tauEE (τε τη : ℝ) : ℝ := τε * τη / (τε + τη)

/-- **Eq. (6)** (p.18): the signal required to receive evaluation `v` given prior
mean `μ`, `s(v,μ) = (τq+τη)/τη · v - τq/τη · μ`. -/
noncomputable def reqSignal (τq τη μ v : ℝ) : ℝ := (τq + τη) / τη * v - τq / τη * μ

/-- `reqSignal` inverts the (taste-free) evaluation. -/
theorem reqSignal_inverts (τq τη μ v : ℝ) (hq : 0 < τq) (hη : 0 < τη) :
    postMean τq τη μ (reqSignal τq τη μ v) = v := by
  unfold postMean reqSignal
  have : τq + τη ≠ 0 := by positivity
  field_simp
  ring

/-- The required signal is strictly decreasing in the prior mean (p.18): a lower
prior needs a higher signal for the same evaluation. -/
theorem reqSignal_anti (τq τη v μ₁ μ₂ : ℝ) (hq : 0 < τq) (hη : 0 < τη) (h : μ₁ < μ₂) :
    reqSignal τq τη μ₂ v < reqSignal τq τη μ₁ v := by
  unfold reqSignal
  have : τq / τη * μ₁ < τq / τη * μ₂ := mul_lt_mul_of_pos_left h (by positivity)
  linarith

/-- Posterior mean of ability after observing the *signal* `s` (exogenous
informational content, footnote 10). -/
noncomputable def nextMeanExo (τa τε τη μ s : ℝ) : ℝ :=
  (τa * μ + tauEE τε τη * s) / (τa + tauEE τε τη)

/-- Posterior mean of ability after observing the *evaluation* `v`, which the
evaluator inverts through the prior mean (Lemma 1, pp.47-49; Lemma 2, p.49). -/
noncomputable def nextMean (τa τε τη μ v : ℝ) : ℝ :=
  (τa * μ + tauEE τε τη * reqSignal (tauQ τa τε) τη μ v) / (τa + tauEE τε τη)

lemma tauEE_pos {τε τη : ℝ} (hε : 0 < τε) (hη : 0 < τη) : 0 < tauEE τε τη := by
  unfold tauEE; positivity

lemma tauQ_pos {τa τε : ℝ} (ha : 0 < τa) (hε : 0 < τε) : 0 < tauQ τa τε := by
  unfold tauQ; positivity

/-- **Footnote 10** (p.19), exogenous case: the gap between two posterior means is
the prior gap times `τa/(τa+τεη) ∈ (0,1)`.  Posterior means are increasing in the
prior mean and never reverse -- "immediately", as the paper says. -/
theorem exo_gap (τa τε τη μ₁ μ₂ s : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    nextMeanExo τa τε τη μ₁ s - nextMeanExo τa τε τη μ₂ s
      = τa / (τa + tauEE τε τη) * (μ₁ - μ₂) := by
  have := tauEE_pos hε hη
  unfold nextMeanExo
  field_simp
  ring

/-- **Eq. (12)** (p.50), the endogenous case of Proposition 2: two opposing
channels, `τa/(τa+τεη)` (the prior, channel (i) of p.19) and
`-τεη τq/((τa+τεη)τη)` (the required signal, channel (ii)). -/
theorem endo_gap_eq12 (τa τε τη μ₁ μ₂ v : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    nextMean τa τε τη μ₁ v - nextMean τa τε τη μ₂ v
      = (τa / (τa + tauEE τε τη)
          - tauEE τε τη * tauQ τa τε / ((τa + tauEE τε τη) * τη)) * (μ₁ - μ₂) := by
  have := tauEE_pos hε hη
  have := tauQ_pos ha hε
  unfold nextMean reqSignal
  field_simp
  ring

/-- **The contraction factor of eq. (12) is exactly `τa/(τa+τε)`.**  The belief gap
between a man and a woman with the same evaluation contracts by `τa/(τa+τε)`,
independently of the evaluation `v` and of the signal precision `τη`. -/
theorem endo_gap (τa τε τη μ₁ μ₂ v : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    nextMean τa τε τη μ₁ v - nextMean τa τε τη μ₂ v = τa / (τa + τε) * (μ₁ - μ₂) := by
  have := tauEE_pos hε hη
  unfold nextMean reqSignal tauQ tauEE
  field_simp
  ring

/-- **The two channels of p.19**: channel (i) is positive, channel (ii) negative,
and "the first effect dominates": their sum is `τa/(τa+τε) > 0`. -/
theorem channels (τa τε τη : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    0 < τa / (τa + tauEE τε τη)
    ∧ -(tauEE τε τη * tauQ τa τε / ((τa + tauEE τε τη) * τη)) < 0
    ∧ τa / (τa + tauEE τε τη) - tauEE τε τη * tauQ τa τε / ((τa + tauEE τε τη) * τη)
        = τa / (τa + τε) := by
  have h1 := tauEE_pos hε hη
  have h2 := tauQ_pos ha hε
  refine ⟨by positivity, by
    have : 0 < tauEE τε τη * tauQ τa τε / ((τa + tauEE τε τη) * τη) := by positivity
    linarith, ?_⟩
  unfold tauQ tauEE
  field_simp
  ring

/-- Posterior mean of ability is strictly increasing in the prior mean in the
endogenous case too (p.19, "the posterior mean is increasing in the prior mean"). -/
theorem endo_strictMono (τa τε τη v : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    StrictMono fun μ => nextMean τa τε τη μ v := by
  intro μ₁ μ₂ h
  have e := endo_gap τa τε τη μ₂ μ₁ v ha hε hη
  have : 0 < τa / (τa + τε) * (μ₂ - μ₁) := mul_pos (by positivity) (by linarith)
  simp only at *
  linarith

/-- **"This speeds up the mitigation of discrimination"** (p.2): endogenous
informational content contracts the belief gap strictly faster than exogenous
content, `τa/(τa+τε) < τa/(τa+τεη)`, because `τεη < τε`. -/
theorem endo_faster (τa τε τη : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) :
    τa / (τa + τε) < τa / (τa + tauEE τε τη) := by
  have hpos := tauEE_pos hε hη
  have hlt : tauEE τε τη < τε := by
    unfold tauEE
    rw [div_lt_iff₀ (by positivity)]
    nlinarith [mul_pos hε hε]
  exact div_lt_div_of_pos_left ha (by linarith) (by linarith)

/-! ## Proposition 2 along a history -/

/-- The evaluator's precision on ability after `n` evaluations, `τa + n τεη`
(eq. (11), p.49). -/
noncomputable def precA (τa τε τη : ℝ) (n : ℕ) : ℝ := τa + n * tauEE τε τη

/-- Posterior mean of ability along a fixed evaluation history `v`, starting from
the prior mean `μ` (Lemma 1, p.47, taste `c = 0`). -/
noncomputable def belief (τa τε τη : ℝ) (v : ℕ → ℝ) (μ : ℝ) : ℕ → ℝ
  | 0 => μ
  | n + 1 => nextMean (precA τa τε τη n) τε τη (belief τa τε τη v μ n) (v n)

lemma precA_pos {τa τε τη : ℝ} (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη) (n : ℕ) :
    0 < precA τa τε τη n := by
  unfold precA; have := tauEE_pos hε hη; positivity

/-- The belief gap after `n` evaluations is the prior gap times
`∏_{j<n} τa(j)/(τa(j)+τε)`. -/
theorem belief_gap (τa τε τη : ℝ) (v : ℕ → ℝ) (μM μF : ℝ) (ha : 0 < τa) (hε : 0 < τε)
    (hη : 0 < τη) (n : ℕ) :
    belief τa τε τη v μM n - belief τa τε τη v μF n
      = (∏ j ∈ range n, precA τa τε τη j / (precA τa τε τη j + τε)) * (μM - μF) := by
  induction n with
  | zero => simp [belief]
  | succ n ih =>
      simp only [belief]
      rw [endo_gap _ _ _ _ _ _ (precA_pos ha hε hη n) hε hη, ih, Finset.prod_range_succ]
      ring

/-- Discrimination in period `n+1` (after `n` evaluations), eq. (14), p.51:
`D = τq(n)/(τq(n)+τη) (μ̂_M(h) - μ̂_F(h))`, the difference of evaluations at a
common signal `s`. -/
noncomputable def discAt (τa τε τη : ℝ) (v : ℕ → ℝ) (μM μF s : ℝ) (n : ℕ) : ℝ :=
  eval (tauQ (precA τa τε τη n) τε) τη (belief τa τε τη v μM n) 0 s
    - eval (tauQ (precA τa τε τη n) τε) τη (belief τa τε τη v μF n) 0 s

/-- **Lemma 3** (p.50) in closed form: discrimination is a positive multiple of the
belief gap, so discrimination reverses iff beliefs reverse. -/
theorem discAt_eq (τa τε τη : ℝ) (v : ℕ → ℝ) (μM μF s : ℝ) (ha : 0 < τa) (hε : 0 < τε)
    (hη : 0 < τη) (n : ℕ) :
    discAt τa τε τη v μM μF s n
      = tauQ (precA τa τε τη n) τε / (tauQ (precA τa τε τη n) τε + τη)
        * (belief τa τε τη v μM n - belief τa τε τη v μF n) := by
  have := tauQ_pos (precA_pos ha hε hη n) hε
  unfold discAt
  rw [disc_initial _ _ _ _ _ _ (by positivity)]
  ring

/-- **Proposition 2, "never reverses"** (p.18).  With a single type, belief-based
partiality `μ̂_F < μ̂_M` and no taste, women face discrimination after every
evaluation history, at every signal. -/
theorem prop2_no_reversal (τa τε τη : ℝ) (v : ℕ → ℝ) (μM μF s : ℝ) (ha : 0 < τa)
    (hε : 0 < τε) (hη : 0 < τη) (hμ : μF < μM) (n : ℕ) :
    0 < discAt τa τε τη v μM μF s n := by
  rw [discAt_eq _ _ _ _ _ _ _ ha hε hη, belief_gap _ _ _ _ _ _ ha hε hη]
  have hp : ∀ j ∈ range n, 0 < precA τa τε τη j / (precA τa τε τη j + τε) := fun j _ => by
    have := precA_pos ha hε hη j; positivity
  have := Finset.prod_pos hp
  have := tauQ_pos (precA_pos ha hε hη n) hε
  have : 0 < μM - μF := by linarith
  positivity

/-- The algebraic core of "discrimination decreases": for any two ability
precisions `a, a' > 0`,
`τq(a')/(τq(a')+τη) · a/(a+τε) < τq(a)/(τq(a)+τη)`. -/
lemma decr_core (a a' τε τη : ℝ) (ha : 0 < a) (ha' : 0 < a') (hε : 0 < τε) (hη : 0 < τη) :
    tauQ a' τε / (tauQ a' τε + τη) * (a / (a + τε)) < tauQ a τε / (tauQ a τε + τη) := by
  have e : ∀ x : ℝ, 0 < x → tauQ x τε / (tauQ x τε + τη)
      = x * τε / (x * τε + τη * (x + τε)) := fun x hx => by
    unfold tauQ; field_simp
  rw [e a' ha', e a ha, div_mul_div_comm, div_lt_div_iff₀ (by positivity) (by positivity)]
  have key : a * τε * ((a' * τε + τη * (a' + τε)) * (a + τε))
      - a' * τε * a * (a * τε + τη * (a + τε))
      = a * τε * (a' * τε ^ 2 + a * τη * τε + τη * τε ^ 2) := by ring
  have : 0 < a * τε * (a' * τε ^ 2 + a * τη * τε + τη * τε ^ 2) := by positivity
  nlinarith

/-- **Proposition 2, "decreases across periods"** (p.18).  Discrimination at a
fixed signal strictly decreases from one period to the next along any history.
The paper's proof (p.51) displays the period-`t+1` expression and stops; this is
the missing comparison. -/
theorem prop2_decreasing (τa τε τη : ℝ) (v : ℕ → ℝ) (μM μF s : ℝ) (ha : 0 < τa)
    (hε : 0 < τε) (hη : 0 < τη) (hμ : μF < μM) (n : ℕ) :
    discAt τa τε τη v μM μF s (n + 1) < discAt τa τε τη v μM μF s n := by
  rw [discAt_eq _ _ _ _ _ _ _ ha hε hη, discAt_eq _ _ _ _ _ _ _ ha hε hη]
  have hg : belief τa τε τη v μM (n + 1) - belief τa τε τη v μF (n + 1)
      = precA τa τε τη n / (precA τa τε τη n + τε)
        * (belief τa τε τη v μM n - belief τa τε τη v μF n) := by
    simp only [belief]
    exact endo_gap _ _ _ _ _ _ (precA_pos ha hε hη n) hε hη
  rw [hg]
  have hgap : 0 < belief τa τε τη v μM n - belief τa τε τη v μF n := by
    rw [belief_gap _ _ _ _ _ _ ha hε hη]
    have hp : ∀ j ∈ range n, 0 < precA τa τε τη j / (precA τa τε τη j + τε) :=
      fun j _ => by have := precA_pos ha hε hη j; positivity
    have := Finset.prod_pos hp
    have : 0 < μM - μF := by linarith
    positivity
  have := decr_core (precA τa τε τη n) (precA τa τε τη (n + 1)) τε τη
    (precA_pos ha hε hη n) (precA_pos ha hε hη (n + 1)) hε hη
  calc tauQ (precA τa τε τη (n + 1)) τε / (tauQ (precA τa τε τη (n + 1)) τε + τη)
          * (precA τa τε τη n / (precA τa τε τη n + τε)
            * (belief τa τε τη v μM n - belief τa τε τη v μF n))
        = (tauQ (precA τa τε τη (n + 1)) τε / (tauQ (precA τa τε τη (n + 1)) τε + τη)
            * (precA τa τε τη n / (precA τa τε τη n + τε)))
            * (belief τa τε τη v μM n - belief τa τε τη v μF n) := by ring
    _ < _ := mul_lt_mul_of_pos_right this hgap

/-! ## Proposition 3: the two-type model (pp.19-21, proof pp.51-54)

After the first evaluation `v₁`, the impartial type (prior `μ̂²_F = μ̂_M`) does not
know whether a woman was evaluated by a heuristic type (probability `p`, required
signal `s¹₁`) or an impartial one (required signal `s²₁ < s¹₁`).  Its posterior
mean is a mixture of the two conditional posterior means `m₁ > m₂` with weights
proportional to `p A` and `(1-p) B`, where `A, B > 0` are the normalising
constants (`C₁D₁`, `C₂D₂` in the paper, p.53).  `m₂` is also the posterior mean
about a man, and `f₁` is the heuristic type's posterior mean about the woman.
Aggregate second-period discrimination is, up to the positive factor
`τq,2/(τq,2+τη)`, `m₂ - p f₁ - (1-p) γ` (p.54). -/

/-- The impartial type's mixture posterior mean `γ`. -/
noncomputable def mixMean (p A B m₁ m₂ : ℝ) : ℝ :=
  (p * A * m₁ + (1 - p) * B * m₂) / (p * A + (1 - p) * B)

/-- Aggregate second-period discrimination (p.54), without the positive factor. -/
noncomputable def disc2 (p A B m₁ m₂ f₁ : ℝ) : ℝ := m₂ - p * f₁ - (1 - p) * mixMean p A B m₁ m₂

/-- **The impartial type discriminates against men** (p.20: "the impartial type's
posterior belief about average ability immediately favors females ... this type
discriminates against males in the second period").  For every `p ∈ (0,1)` and all
positive weights. -/
theorem impartial_favors_female (p A B m₁ m₂ : ℝ) (hp0 : 0 < p) (hp1 : p < 1) (hA : 0 < A)
    (hB : 0 < B) (hm : m₂ < m₁) : m₂ < mixMean p A B m₁ m₂ := by
  unfold mixMean
  have hW : 0 < p * A + (1 - p) * B := by
    have : 0 < 1 - p := by linarith
    positivity
  rw [lt_div_iff₀ hW]
  have : 0 < p * A * (m₁ - m₂) := by
    have : 0 < m₁ - m₂ := by linarith
    positivity
  nlinarith

/-- The inputs of `impartial_favors_female` come from the model: the heuristic
type's required signal exceeds the impartial type's (p.51, "Note `s¹₁ > s²₁`"),
so the heuristic-branch posterior mean `m₁` exceeds `m₂`. -/
theorem m1_gt_m2 (τa τε τη μ μM μ1F v : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη)
    (hμ : μ1F < μM) :
    nextMeanExo τa τε τη μ (reqSignal (tauQ τa τε) τη μM v)
      < nextMeanExo τa τε τη μ (reqSignal (tauQ τa τε) τη μ1F v) := by
  have hs := reqSignal_anti (tauQ τa τε) τη v μ1F μM (tauQ_pos ha hε) hη hμ
  have := tauEE_pos hε hη
  unfold nextMeanExo
  apply div_lt_div_of_pos_right _ (by positivity)
  nlinarith

/-- `disc2` factors as `p · L(p) / W(p)` with `L` affine in `p`:
`L(p) = (1-p) L₀ + p L₁`, `L₀ = B(m₂-f₁) - A(m₁-m₂)`, `L₁ = A(m₂-f₁)`. -/
theorem disc2_factor (p A B m₁ m₂ f₁ : ℝ) (hW : p * A + (1 - p) * B ≠ 0) :
    disc2 p A B m₁ m₂ f₁
      = p * ((1 - p) * (B * (m₂ - f₁) - A * (m₁ - m₂)) + p * (A * (m₂ - f₁)))
        / (p * A + (1 - p) * B) := by
  unfold disc2 mixMean
  field_simp
  ring

/-- Endpoints (p.54): `D = 0` at `p = 0` (no partiality) and `D > 0` at `p = 1`
(Proposition 2). -/
theorem disc2_endpoints (A B m₁ m₂ f₁ : ℝ) (hA : 0 < A) (hB : 0 < B) (hf : f₁ < m₂) :
    disc2 0 A B m₁ m₂ f₁ = 0 ∧ 0 < disc2 1 A B m₁ m₂ f₁ := by
  refine ⟨?_, ?_⟩
  · unfold disc2 mixMean; field_simp; ring
  · unfold disc2 mixMean
    have : (1 : ℝ) - 1 = 0 := by ring
    simp only [this, zero_mul, add_zero, sub_zero, one_mul]
    linarith

/-- **Proposition 3, exact sign.**  For `p ∈ (0,1)`, aggregate second-period
discrimination is negative (a reversal) iff `(1-p) L₀ + p L₁ < 0`. -/
theorem prop3_sign (p A B m₁ m₂ f₁ : ℝ) (hp0 : 0 < p) (hp1 : p < 1) (hA : 0 < A)
    (hB : 0 < B) :
    disc2 p A B m₁ m₂ f₁ < 0
      ↔ (1 - p) * (B * (m₂ - f₁) - A * (m₁ - m₂)) + p * (A * (m₂ - f₁)) < 0 := by
  have hW : 0 < p * A + (1 - p) * B := by
    have : 0 < 1 - p := by linarith
    positivity
  rw [disc2_factor _ _ _ _ _ _ hW.ne', div_neg_iff]
  constructor
  · rintro (⟨h1, h2⟩ | ⟨h1, _⟩)
    · linarith
    · by_contra hc
      rw [not_lt] at hc
      nlinarith [mul_nonneg hp0.le hc]
  · intro h
    right
    exact ⟨by nlinarith, hW⟩

/-- **Proposition 3, the cut-off** (p.21 and p.54).  If the derivative condition
`B(m₂-f₁) < A(m₁-m₂)` holds (and `f₁ < m₂`), aggregate discrimination reverses
exactly for `p` below the explicit cut-off `p̄ = L₀/(L₀ - L₁) ∈ (0,1)`; if it fails,
no `p ∈ (0,1)` gives a reversal. -/
theorem prop3_cutoff (A B m₁ m₂ f₁ : ℝ) (hA : 0 < A) (hB : 0 < B) (hf : f₁ < m₂) :
    (B * (m₂ - f₁) < A * (m₁ - m₂) →
      let L₀ := B * (m₂ - f₁) - A * (m₁ - m₂)
      let L₁ := A * (m₂ - f₁)
      0 < L₀ / (L₀ - L₁) ∧ L₀ / (L₀ - L₁) < 1
        ∧ ∀ p, 0 < p → p < 1 → (disc2 p A B m₁ m₂ f₁ < 0 ↔ p < L₀ / (L₀ - L₁)))
    ∧ (A * (m₁ - m₂) ≤ B * (m₂ - f₁) → ∀ p, 0 < p → p < 1 → 0 ≤ disc2 p A B m₁ m₂ f₁) := by
  have hL1 : 0 < A * (m₂ - f₁) := mul_pos hA (by linarith)
  refine ⟨fun hc => ?_, fun hc p hp0 hp1 => ?_⟩
  · intro L₀ L₁
    have hL0 : L₀ < 0 := by simp only [L₀]; linarith
    have hL1' : 0 < L₁ := hL1
    have hden : L₀ - L₁ < 0 := by linarith
    refine ⟨div_pos_of_neg_of_neg hL0 hden, (div_lt_one_of_neg hden).2 (by linarith),
      fun p hp0 hp1 => ?_⟩
    rw [prop3_sign _ _ _ _ _ _ hp0 hp1 hA hB, lt_div_iff_of_neg hden]
    change (1 - p) * L₀ + p * L₁ < 0 ↔ p * (L₀ - L₁) > L₀
    constructor <;> intro h <;> nlinarith
  · by_contra hneg
    rw [not_le] at hneg
    rw [prop3_sign _ _ _ _ _ _ hp0 hp1 hA hB] at hneg
    have : 0 ≤ (1 - p) * (B * (m₂ - f₁) - A * (m₁ - m₂)) :=
      mul_nonneg (by linarith) (by linarith)
    nlinarith

/-- **The derivative condition in the paper's form** (p.54): substituting the model
values `m₂ - f₁ = τa/(τa+τε) · g` (by `endo_gap`) and
`m₁ - m₂ = τεη τq/(τη(τa+τεη)) · g` (by `reqSignal`), with `g = μ̂_M - μ̂¹_F > 0`,
the condition `B(m₂-f₁) < A(m₁-m₂)` is the paper's
`1 < τε²/((τε+τη)(τa+τε)) · (1 + A/B)`. -/
theorem prop3_condition_iff (τa τε τη A B g : ℝ) (ha : 0 < τa) (hε : 0 < τε) (hη : 0 < τη)
    (hA : 0 < A) (hB : 0 < B) (hg : 0 < g) :
    B * (τa / (τa + τε) * g)
        < A * (tauEE τε τη * tauQ τa τε / (τη * (τa + tauEE τε τη)) * g)
      ↔ 1 < τε ^ 2 / ((τε + τη) * (τa + τε)) * (1 + A / B) := by
  have e1 : tauEE τε τη * tauQ τa τε / (τη * (τa + tauEE τε τη))
      = τa * τε ^ 2 / ((τa + τε) * (τa * τε + τa * τη + τε * τη)) := by
    unfold tauEE tauQ; field_simp
  have e2 : τε ^ 2 / ((τε + τη) * (τa + τε)) * (1 + A / B)
      = τε ^ 2 * (B + A) / ((τε + τη) * (τa + τε) * B) := by
    field_simp
  rw [e1, e2, one_lt_div (by positivity)]
  have hQ : 0 < τa * τε + τa * τη + τε * τη := by positivity
  rw [show B * (τa / (τa + τε) * g) = (B * (τa * τε + τa * τη + τε * τη)) * (τa * g)
        / ((τa + τε) * (τa * τε + τa * τη + τε * τη)) by field_simp,
      show A * (τa * τε ^ 2 / ((τa + τε) * (τa * τε + τa * τη + τε * τη)) * g)
        = (A * τε ^ 2) * (τa * g) / ((τa + τε) * (τa * τε + τa * τη + τε * τη)) by
          field_simp,
      div_lt_div_iff_of_pos_right (by positivity),
      mul_lt_mul_iff_of_pos_right (by positivity)]
  constructor <;> intro h <;> nlinarith
end Literature.BohrenImasRosenberg
