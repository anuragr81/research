import Mathlib

/-! Bohren, Imas and Rosenberg, "The Dynamics of Discrimination: Theory and
Evidence", working paper of January 2019, Section 2 and Appendix A.1, in the
paper's notation. Ability `a ∼ N(μ_g, 1/τa)`, quality `q_t = a + ε_t` with
precision `τε`, signal `s_t = q_t + η_t` with precision `τη`. The prior
precision of quality is `τq = τa τε/(τa + τε)`, and `τεη = τε τη/(τε + τη)` is
the precision of `s` about `a`. An evaluator of gender-taste `c_g` (with
`c_M = 0`) and subjective mean `μ̂_g` reports eq. (4). We write `muM`, `muF`
for `μ̂_M`, `μ̂_F`, and `tq`, `te`, `ta`, `tn`, `ten` for `τq`, `τε`, `τa`,
`τη`, `τεη`. The normal posteriors themselves are not formalised. The
formulas below are the paper's closed forms, and the theorems are the
algebra of its Propositions 1 and 2 and of Lemma 2. -/

namespace BohrenEtAl2019

open Filter Topology

/-- Eq. (4). The first-period evaluation of a worker of gender `g` with
subjective mean `μ̂_g`, signal `s` and taste parameter `c_g`. -/
noncomputable def v (tq tn mu s c : ℝ) : ℝ := (tq * mu + tn * s) / (tq + tn) - c

/-- Eq. (5). Initial discrimination, `D(h₁, s₁) = τq/(τq + τη) (μ̂_M − μ̂_F) + c_F`. -/
noncomputable def D (tq tn muM muF cF : ℝ) : ℝ := tq / (tq + tn) * (muM - muF) + cF

/-- Eq. (5) from eq. (4) and eq. (3). With `c_M = 0`, the difference between
the evaluations of a man and a woman with the same signal is `D` and does not
depend on the signal. -/
theorem eq5 {tq tn : ℝ} (muM muF cF s : ℝ) (h : tq + tn ≠ 0) :
    v tq tn muM s 0 - v tq tn muF s cF = D tq tn muM muF cF := by
  unfold v D; field_simp; ring

/-- Proposition 1, first part. With belief-based partiality, `μ̂_F < μ̂_M`,
initial discrimination is strictly decreasing in the signal precision `τη`. -/
theorem prop1_decreasing {tq n1 n2 muM muF cF : ℝ} (hq : 0 < tq) (h1 : 0 < n1)
    (h12 : n1 < n2) (hb : muF < muM) : D tq n2 muM muF cF < D tq n1 muM muF cF := by
  unfold D
  have : tq / (tq + n2) < tq / (tq + n1) :=
    div_lt_div_of_pos_left hq (by linarith) (by linarith)
  nlinarith

/-- Proposition 1, second part. Without belief-based partiality, initial
discrimination is `c_F` at every precision. -/
theorem prop1_constant {tq tn mu cF : ℝ} : D tq tn mu mu cF = cF := by
  unfold D; ring

/-- Proposition 1, the limit. As the signal becomes perfectly objective,
`τη → ∞`, initial discrimination tends to `c_F`, so it vanishes if and only if
there is no preference-based partiality. -/
theorem prop1_limit (tq muM muF cF : ℝ) :
    Tendsto (fun tn => D tq tn muM muF cF) atTop (𝓝 cF) := by
  have h1 : Tendsto (fun tn : ℝ => tq + tn) atTop atTop := tendsto_atTop_add_const_left _ _ tendsto_id
  have h2 : Tendsto (fun tn : ℝ => tq / (tq + tn)) atTop (𝓝 0) :=
    (tendsto_const_nhds (x := tq)).div_atTop h1
  have h3 := (h2.mul_const (muM - muF)).add_const cF
  simpa [D] using h3

/-- Footnote 9. Initial evaluations do not identify the source. An evaluator
with beliefs `μ̂¹_M > μ̂¹_F` and no taste gives every woman the evaluation that
an evaluator with beliefs `μ̂²_F = μ̂²_M = μ̂¹_M` and taste
`c²_F = τq (μ̂¹_M − μ̂¹_F)/(τq + τη)` gives her. -/
theorem fn9_equivalence {tq tn : ℝ} (muM muF s : ℝ) (h : tq + tn ≠ 0) :
    v tq tn muF s 0 = v tq tn muM s (tq * (muM - muF) / (tq + tn)) := by
  unfold v; field_simp; ring

/-- Eq. (6). The signal a worker of prior mean `μ̂` must produce to receive
evaluation `v₁`, when the taste parameter is zero. -/
noncomputable def sReq (tq tn mu v1 : ℝ) : ℝ := (tq + tn) / tn * v1 - tq / tn * mu

/-- The Bayesian update of the mean of ability after a signal `s` of precision
`τεη`, from Lemma 1. -/
noncomputable def post (ta ten mu s : ℝ) : ℝ := (ta * mu + ten * s) / (ta + ten)

/-- Eq. (12). After a common evaluation, the gap in posterior means is the
prior gap times `k = τa/(τa + τεη) − τεη τq/((τa + τεη) τη)`. -/
theorem eq12 {ta ten tq tn : ℝ} (muM muF v1 : ℝ) (hn : tn ≠ 0) (ha : ta + ten ≠ 0) :
    post ta ten muM (sReq tq tn muM v1) - post ta ten muF (sReq tq tn muF v1)
      = (ta / (ta + ten) - ten * tq / ((ta + ten) * tn)) * (muM - muF) := by
  unfold post sReq; field_simp; ring

/-- The factor of eq. (12) with `τq` and `τεη` written out, as in eq. (13). -/
noncomputable def k (ta te tn : ℝ) : ℝ :=
  ta / (ta + te * tn / (te + tn)) -
    te * tn / (te + tn) * (ta * te / (ta + te)) / ((ta + te * tn / (te + tn)) * tn)

/-- The factor of eq. (12) simplifies to `τa/(τa + τε)`. The signal precision
`τη` cancels. The paper states only that eq. (13) lies in `(0, 1)`; the closed
form is ours. -/
theorem k_eq {ta te tn : ℝ} (ha : 0 < ta) (he : 0 < te) (hn : 0 < tn) :
    k ta te tn = ta / (ta + te) := by
  have h1 : te + tn ≠ 0 := by positivity
  have h2 : ta + te ≠ 0 := by positivity
  have h3 : ta + te * tn / (te + tn) ≠ 0 := by positivity
  unfold k; field_simp; ring

/-- Lemma 2, the core inequality. For positive precisions the factor lies in
`(0, 1)`, as the paper shows from eq. (13). -/
theorem k_mem {ta te tn : ℝ} (ha : 0 < ta) (he : 0 < te) (hn : 0 < tn) :
    0 < k ta te tn ∧ k ta te tn < 1 := by
  rw [k_eq ha he hn]
  exact ⟨div_pos ha (by linarith), (div_lt_one (by linarith)).2 (by linarith)⟩

/-- Lemma 2. With a single type, no taste, and `μ̂_F(h_t) < μ̂_M(h_t)`, a common
evaluation leaves the posterior means in the same order and closer together. -/
theorem lemma2 {ta te tn : ℝ} (ha : 0 < ta) (he : 0 < te) (hn : 0 < tn) {gap : ℝ}
    (hg : 0 < gap) : 0 < k ta te tn * gap ∧ k ta te tn * gap < gap := by
  obtain ⟨h0, h1⟩ := k_mem ha he hn
  exact ⟨mul_pos h0 hg, by nlinarith⟩

/-- Proposition 2. Along any history the posterior precision of ability
`τa(t)` stays positive, so with a single type the gap in means is multiplied
each period by a factor in `(0, 1)`. Starting from `μ̂_F < μ̂_M`, the gap stays
positive (no reversal, Lemma 3) and strictly decreases. -/
theorem prop2 {te tn : ℝ} (he : 0 < te) (hn : 0 < tn) (ta : ℕ → ℝ) (hta : ∀ t, 0 < ta t)
    (gap : ℕ → ℝ) (h0 : 0 < gap 0) (hstep : ∀ t, gap (t + 1) = k (ta t) te tn * gap t) :
    ∀ t, 0 < gap t ∧ gap (t + 1) < gap t := by
  have hpos : ∀ t, 0 < gap t := by
    intro t
    induction t with
    | zero => exact h0
    | succ n ih => rw [hstep n]; exact (lemma2 (hta n) he hn ih).1
  intro t
  refine ⟨hpos t, ?_⟩
  rw [hstep t]; exact (lemma2 (hta t) he hn (hpos t)).2

/-- Control: preference-based partiality does not vanish with objectivity.
With `μ̂_F = μ̂_M` and `c_F > 0`, initial discrimination is `c_F > 0` at every
precision, so the vanishing in `prop1_limit` needs `c_F = 0`. -/
theorem control_taste_persists {tq mu cF : ℝ} (hc : 0 < cF) :
    ∀ tn, 0 < D tq tn mu mu cF := by
  intro tn; rw [prop1_constant]; exact hc

/-- Control: Proposition 2 needs a single type with common knowledge. An
impartial evaluator (prior `μ̂_M` for both genders) who reads a woman's
evaluation as given by a heuristic evaluator with prior `μ̂¹_F < μ̂_M` backs
out a higher signal for her (eq. 6) and so ends with a higher posterior mean
for the woman than for a man with the same evaluation, the reversal the paper
describes before Proposition 3. -/
theorem control_reversal_misspecified {ta ten tq tn muM muF1 v1 : ℝ} (ha : 0 < ta)
    (hen : 0 < ten) (hq : 0 < tq) (hn : 0 < tn) (hb : muF1 < muM) :
    post ta ten muM (sReq tq tn muM v1) < post ta ten muM (sReq tq tn muF1 v1) := by
  unfold post sReq
  rw [div_lt_div_iff_of_pos_right (by linarith)]
  have : tq / tn * muF1 < tq / tn * muM :=
    mul_lt_mul_of_pos_left hb (div_pos hq hn)
  nlinarith

/-! ### Section 2.1, primitives and definitions -/

/-- An evaluator type `θ_i`. Taste `c_{iF}` (with `c_{iM} = 0`) and subjective
means `μ̂_{iF}`, `μ̂_{iM}`. Her belief `π̂_i` about other types is carried
separately where it matters. -/
structure EvalType where
  c : ℝ
  muF : ℝ
  muM : ℝ

/-- Definition 1. Preference-based partiality towards men. -/
def prefPartial (θ : EvalType) : Prop := 0 < θ.c

/-- Definition 2. Belief-based partiality towards men. -/
def beliefPartial (θ : EvalType) : Prop := θ.muF < θ.muM

/-- Definition 2. The partiality is unbiased when both subjective means are
the true ones. -/
def unbiased (θ : EvalType) (μF μM : ℝ) : Prop := θ.muF = μF ∧ θ.muM = μM

/-- Definition 3. A woman faces discrimination when `D > 0`, a man when
`D < 0`. -/
def againstWomen (Dv : ℝ) : Prop := 0 < Dv
def againstMen (Dv : ℝ) : Prop := Dv < 0

/-- Definition 4. A reversal between an earlier and a later history. -/
def reversal (D0 D1 : ℝ) : Prop := againstWomen D0 ∧ againstMen D1

/-- Eqs. (1) and (2). With posterior mean `m` and variance `V` of quality,
the expected loss of reporting `v` is `(v − (m − c))² + V`. -/
theorem eq1_expected_loss (v c m V : ℝ) :
    (v + c) ^ 2 - 2 * (v + c) * m + (m ^ 2 + V) = (v - (m - c)) ^ 2 + V := by ring

/-- Eq. (2). The report `m − c` maximises the expected payoff. -/
theorem eq2_optimal (c m V : ℝ) :
    ∀ v, -((v - (m - c)) ^ 2 + V) ≤ -(((m - c) - (m - c)) ^ 2 + V) := by
  intro v; nlinarith [sq_nonneg (v - (m - c))]

/-- The prior precision of quality, `τq = τa τε/(τa + τε)`, adds variances,
`1/τq = 1/τa + 1/τε`. -/
theorem tq_inv {ta te : ℝ} (ha : 0 < ta) (he : 0 < te) :
    1 / (ta * te / (ta + te)) = 1 / ta + 1 / te := by
  field_simp; ring

/-- The normal update behind eq. (4), at the level of log densities. The
prior kernel `τq (q − μ̂)²` plus the signal kernel `τη (q − s)²` is a normal
kernel with precision `τq + τη` and mean `(τq μ̂ + τη s)/(τq + τη)`, plus a
term free of `q`. -/
theorem normal_update {tq tn : ℝ} (h : tq + tn ≠ 0) (mu s q : ℝ) :
    tq * (q - mu) ^ 2 + tn * (q - s) ^ 2
      = (tq + tn) * (q - (tq * mu + tn * s) / (tq + tn)) ^ 2
        + tq * tn / (tq + tn) * (mu - s) ^ 2 := by
  field_simp; ring

/-- Section 2.3, aggregate initial discrimination over a finite set of types,
`D = τq/(τq + τη) E_π[μ̂_M − μ̂_F] + E_π[c_F]`. -/
theorem agg_initial {ι : Type*} [Fintype ι] (tq tn : ℝ) (π : ι → ℝ) (θ : ι → EvalType) :
    ∑ i, π i * D tq tn (θ i).muM (θ i).muF (θ i).c
      = tq / (tq + tn) * ∑ i, π i * ((θ i).muM - (θ i).muF) + ∑ i, π i * (θ i).c := by
  simp only [D, Finset.mul_sum, ← Finset.sum_add_distrib]
  refine Finset.sum_congr rfl (fun i _ => ?_); ring

/-- Section 2.5.2, heterogeneous preference-based partiality. With correct
beliefs for every type, aggregate initial discrimination is `E_π[c_F]` at
every precision `τη`, so it cannot be positive under subjective judgement
and zero under objective judgement. -/
theorem heterogeneous_taste {ι : Type*} [Fintype ι] (tq tn : ℝ) (π : ι → ℝ)
    (θ : ι → EvalType) (μF μM : ℝ) (hc : ∀ i, unbiased (θ i) μF μM) :
    ∑ i, π i * D tq tn (θ i).muM (θ i).muF (θ i).c
      = tq / (tq + tn) * (∑ i, π i) * (μM - μF) + ∑ i, π i * (θ i).c := by
  rw [agg_initial]
  congr 1
  rw [mul_assoc, Finset.sum_mul]
  congr 1
  refine Finset.sum_congr rfl (fun i _ => ?_)
  rw [(hc i).1, (hc i).2]

/-- Eq. (7). With a heuristic type of share `p` and an impartial type that
shares the heuristic type's `μ̂_M`, aggregate initial discrimination is
`τq/(τq + τη) p (μ̂_M − μ̂¹_F)`. -/
theorem eq7 (tq tn p muM muF1 : ℝ) :
    p * D tq tn muM muF1 0 + (1 - p) * D tq tn muM muM 0
      = tq / (tq + tn) * (p * (muM - muF1)) := by
  unfold D; ring

/-! ### Appendix A.1, Lemmas 1 to 3 and Proposition 2 -/

/-- Eq. (9) inverted. A report `v` from an evaluator with taste `c` and prior
mean `m` reveals the signal `s = (τq + τη)/τη (v + c) − τq/τη m`. -/
theorem invert {tq tn m s c : ℝ} (hn : tn ≠ 0) (h : tq + tn ≠ 0) :
    (tq + tn) / tn * (v tq tn m s c + c) - tq / tn * m = s := by
  unfold v; field_simp; ring

/-- Lemma 1, the recursion. `muSeq n` is the mean of ability after `n`
revealed signals, updated with precision `τεη` each time, and `τa(n) =
τa + n τεη`. -/
noncomputable def muSeq (ta ten mu0 : ℝ) (s : ℕ → ℝ) : ℕ → ℝ
  | 0 => mu0
  | n + 1 => ((ta + n * ten) * muSeq ta ten mu0 s n + ten * s n) / (ta + n * ten + ten)

/-- Lemma 1, eqs. (10) and (11). The recursion has the closed form
`μ̂(h_{n+1}) = (τa μ̂ + τεη Σ_{k<n} s_k)/(τa + n τεη)`. -/
theorem lemma1 {ta ten : ℝ} (ha : 0 < ta) (hen : 0 ≤ ten) (mu0 : ℝ) (s : ℕ → ℝ) :
    ∀ n : ℕ, muSeq ta ten mu0 s n
      = (ta * mu0 + ten * ∑ k ∈ Finset.range n, s k) / (ta + n * ten) := by
  intro n
  induction n with
  | zero => simp [muSeq]; field_simp
  | succ n ih =>
    have h1 : 0 < ta + n * ten := by positivity
    have h2 : 0 < ta + n * ten + ten := by positivity
    simp only [muSeq, ih, Finset.sum_range_succ, Nat.cast_succ]
    field_simp
    ring

/-- Lemma 3. With no taste, period-`t` discrimination is the positive weight
`τq(t)/(τq(t) + τη)` times the gap in posterior means, so a discrimination
reversal is exactly a belief reversal. -/
theorem lemma3 {w0 w1 g0 g1 : ℝ} (h0 : 0 < w0) (h1 : 0 < w1) :
    reversal (w0 * g0) (w1 * g1) ↔ 0 < g0 ∧ g1 < 0 := by
  unfold reversal againstWomen againstMen
  constructor
  · rintro ⟨a, b⟩
    exact ⟨pos_of_mul_pos_right a h0.le, neg_of_mul_neg_right b h1.le⟩
  · rintro ⟨a, b⟩; exact ⟨mul_pos h0 a, mul_neg_of_pos_of_neg h1 b⟩

/-- Proposition 2, that discrimination decreases. The paper ends its proof at
the display of `D(h_{t+1})`. With `τq(t) = τa(t) τε/(τa(t) + τε)` rising in
`t`, the weight rises but the gap shrinks by `τa(t)/(τa(t) + τε) = τq(t)/τε`
(`k_eq`), and the product falls. -/
theorem prop2_D_decreases {ta ta1 te tn g : ℝ} (ha : 0 < ta) (ha1 : 0 < ta1) (he : 0 < te)
    (hn : 0 < tn) (hg : 0 < g) :
    let tq0 := ta * te / (ta + te)
    let tq1 := ta1 * te / (ta1 + te)
    tq1 / (tq1 + tn) * (ta / (ta + te) * g) < tq0 / (tq0 + tn) * g := by
  intro tq0 tq1
  have hq0 : 0 < tq0 := by positivity
  have hq1 : 0 < tq1 := by positivity
  have hq0e : tq0 < te := by
    simp only [tq0]; rw [div_lt_iff₀ (by positivity)]; nlinarith
  have hq1e : tq1 < te := by
    simp only [tq1]; rw [div_lt_iff₀ (by positivity)]; nlinarith
  have hk : ta / (ta + te) = tq0 / te := by
    simp only [tq0]; field_simp
  rw [hk]
  have key : tq1 * (tq0 + tn) < te * (tq1 + tn) := by nlinarith
  rw [show tq1 / (tq1 + tn) * (tq0 / te * g) = (tq1 * tq0 * g) / ((tq1 + tn) * te) by
        field_simp]
  rw [show tq0 / (tq0 + tn) * g = (tq0 * g) / (tq0 + tn) by ring]
  rw [div_lt_div_iff₀ (by positivity) (by positivity)]
  have : 0 < tq0 * g := by positivity
  nlinarith [mul_lt_mul_of_pos_left key this]

/-- Proposition 3, the boundary `p = 0`. Without heuristic evaluators there is
no initial discrimination. The interior result is not formalised. -/
theorem prop3_p_zero (tq tn muM muF1 : ℝ) :
    0 * D tq tn muM muF1 0 + (1 - 0) * D tq tn muM muM 0 = 0 := by
  unfold D; ring

/-- Proof of Proposition 3, "Note `s¹₁(v₁) > s²₁(v₁)`". The signal backed out
under the heuristic type's lower prior exceeds the one backed out under the
impartial prior. -/
theorem prop3_signal_order {tq tn muM muF1 v1 : ℝ} (hq : 0 < tq) (hn : 0 < tn)
    (hb : muF1 < muM) : sReq tq tn muM v1 < sReq tq tn muF1 v1 := by
  unfold sReq
  have : tq / tn * muF1 < tq / tn * muM := mul_lt_mul_of_pos_left hb (div_pos hq hn)
  linarith

/-! ### Appendix A.3.1, coarse evaluations -/

/-- The first-period upvote cut-off `s(μ̂_g, c_g) = c_g (τq + τη)/τη − μ̂_g τq/τη`. -/
noncomputable def sCut (tq tn mu c : ℝ) : ℝ := c * ((tq + tn) / tn) - mu * (tq / tn)

/-- Eq. (23) in period 1. The evaluator upvotes if and only if the signal
reaches the cut-off. -/
theorem coarse_upvote_iff {tq tn mu c s : ℝ} (hq : 0 < tq) (hn : 0 < tn) :
    c ≤ (mu * tq + s * tn) / (tq + tn) ↔ sCut tq tn mu c ≤ s := by
  unfold sCut
  rw [le_div_iff₀ (by linarith)]
  constructor <;> intro h
  · have : c * ((tq + tn) / tn) - mu * (tq / tn) = (c * (tq + tn) - mu * tq) / tn := by
      field_simp
    rw [this, div_le_iff₀ hn]; linarith
  · have : c * ((tq + tn) / tn) - mu * (tq / tn) = (c * (tq + tn) - mu * tq) / tn := by
      field_simp
    rw [this, div_le_iff₀ hn] at h; linarith

/-- Coarse evaluations, initial discrimination. The cut-off rises in the taste
and falls in the prior mean, so a woman faces a higher cut-off if and only if
`c_F > 0` or `μ̂_M > μ̂_F` (with the other weakly in the same direction). -/
theorem coarse_cut_mono {tq tn : ℝ} (hq : 0 < tq) (hn : 0 < tn) (mu mu' c c' : ℝ)
    (hmu : mu' ≤ mu) (hc : c ≤ c') : sCut tq tn mu c ≤ sCut tq tn mu' c' := by
  unfold sCut
  have h1 : 0 < (tq + tn) / tn := by positivity
  have h2 : 0 < tq / tn := by positivity
  nlinarith [mul_le_mul_of_nonneg_right hc h1.le, mul_le_mul_of_nonneg_right hmu h2.le]

/-- Coarse evaluations. As `τη → ∞` the cut-off tends to `c_g`, so initial
discrimination survives objectivity only through a taste. -/
theorem coarse_cut_limit (tq mu c : ℝ) :
    Tendsto (fun tn => sCut tq tn mu c) atTop (𝓝 c) := by
  have e : ∀ tn : ℝ, 0 < tn → sCut tq tn mu c = c + (c * tq - mu * tq) * tn⁻¹ := by
    intro tn hn; unfold sCut; field_simp; ring
  have h : Tendsto (fun tn : ℝ => c + (c * tq - mu * tq) * tn⁻¹) atTop (𝓝 c) := by
    have := (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul (c * tq - mu * tq)
    simpa using this.const_add c
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop 0] with tn hn
  exact (e tn hn).symm

/-- Lemma 4, the step `τx ≥ 1`. With `τx = τa τη/(τq τεη)`,
`τx = (τa + τε)(τη + τε)/τε² ≥ 1`. The bound on the normal hazard terms that
the proof takes from Stack Exchange is not formalised. -/
theorem lemma4_tau_x {ta te tn : ℝ} (ha : 0 < ta) (he : 0 < te) (hn : 0 < tn) :
    ta * tn / ((ta * te / (ta + te)) * (te * tn / (te + tn))) = (ta + te) * (tn + te) / te ^ 2 ∧
      1 ≤ (ta + te) * (tn + te) / te ^ 2 := by
  constructor
  · field_simp; ring
  · rw [le_div_iff₀ (by positivity)]; nlinarith [mul_pos ha hn, mul_pos ha he, mul_pos hn he]

/-! ### Appendix A.3.2, shifting standards -/

/-- Eq. (27). The benchmark `c(r)` enters both genders' evaluations and cancels
from discrimination. -/
theorem shifting_cancels (tq tn muM muF s cr cF : ℝ) :
    ((tq * muM + tn * s) / (tq + tn) - cr - 0) - ((tq * muF + tn * s) / (tq + tn) - cr - cF)
      = (tq * muM + tn * s) / (tq + tn) - (tq * muF + tn * s) / (tq + tn) + cF := by ring

/-- The signal required for evaluation `v` under eq. (27) at prior-quality
precision `τq,t`, prior mean `μ̂` and total benchmark `b = c(r) + c_g`. -/
noncomputable def sShift (tq tn mu b v : ℝ) : ℝ := (tq + tn) / tn * (v + b) - tq / tn * mu

/-- BIR-D8. The paper says that `s(v, h₂, g) − s(v, h₁, g)` does not depend on
`v`. Under eq. (27) the difference has slope `(τq,2 − τq,1)/τη` in `v`, which is
not zero once `τq,t` rises with the history. -/
theorem control_shifting_difference_depends_on_v {tq1 tq2 tn : ℝ} (hn : 0 < tn)
    (h12 : tq1 ≠ tq2) (mu1 mu2 b1 b2 : ℝ) :
    sShift tq2 tn mu2 b2 1 - sShift tq1 tn mu1 b1 1
      ≠ sShift tq2 tn mu2 b2 0 - sShift tq1 tn mu1 b1 0 := by
  unfold sShift
  intro h
  have : (tq2 - tq1) / tn = 0 := by
    have e : (tq2 - tq1) / tn = ((tq2 + tn) / tn * (1 + b2) - tq2 / tn * mu2 -
        ((tq1 + tn) / tn * (1 + b1) - tq1 / tn * mu1)) -
        ((tq2 + tn) / tn * (0 + b2) - tq2 / tn * mu2 -
        ((tq1 + tn) / tn * (0 + b1) - tq1 / tn * mu1)) := by
      field_simp; ring
    rw [e, h]; ring
  rw [div_eq_zero_iff] at this
  rcases this with h' | h'
  · exact h12 (by linarith)
  · linarith

/-! ### Appendix A.2.2, binary ability and quality -/

/-- `φ(p) = ρ_H p + ρ_L (1 − p)`, the probability of high quality. -/
def phi (rH rL p : ℝ) : ℝ := rH * p + rL * (1 - p)

/-- `φ` is strictly increasing in `p` when `ρ_H > ρ_L`, so `v(s, p)` is
increasing in the belief `p`. -/
theorem phi_strictMono {rH rL : ℝ} (h : rL < rH) : StrictMono (phi rH rL) := by
  intro p p' hp; unfold phi; nlinarith

/-- The inversion after eq. (19). From `log(v/(1 − v)) = μ s − μ²/2 + log(φ/(1 − φ))`,
`s(v, p) = (1/μ) log(v/(1 − v)) − (1/μ) log(φ/(1 − φ)) + μ/2`. Written with
`L = log(v/(1 − v))` and `Λ = log(φ/(1 − φ))`. -/
theorem binary_signal {μ L Λ s : ℝ} (hμ : μ ≠ 0) (h : L = μ * s - μ ^ 2 / 2 + Λ) :
    s = 1 / μ * L - 1 / μ * Λ + μ / 2 := by
  rw [h]; field_simp; ring

/-! ### Appendix C, representativeness -/

/-- Eq. (30). The stereotyped belief of Bordalo et al. as the paper writes it. -/
noncomputable def stereo {T : Type*} [Fintype T] (π π' : T → ℝ) (θ : ℝ) (t : T) : ℝ :=
  π t * (π t / π' t) ^ θ / ∑ s, π s * (π s / π' s) ^ θ

/-- Eq. (30). The stereotyped belief is a distribution. -/
theorem stereo_sum {T : Type*} [Fintype T] (π π' : T → ℝ) (θ : ℝ)
    (h : ∑ s, π s * (π s / π' s) ^ θ ≠ 0) : ∑ t, stereo π π' θ t = 1 := by
  unfold stereo; rw [← Finset.sum_div]; exact div_self h

/-- Eq. (30). With no distortion, `θ = 0`, the stereotype is the true
distribution. -/
theorem stereo_zero {T : Type*} [Fintype T] (π π' : T → ℝ) (hπ : ∑ s, π s = 1) (t : T) :
    stereo π π' 0 t = π t := by
  unfold stereo; simp [hπ]

/-! ### Section 3.3 and Appendix B.2, the regressions

Each table regresses the outcome on `Male`, a second dummy (`Question` or
`Advanced`) and their product. The model is saturated, so the coefficients are
differences of the four cell means, and the single-sample columns are sums of
the pooled coefficients. -/

/-- The saturated two-by-two model `y = β₀ + β₁ Male + β₂ X + β₃ Male·X` fitted
to cell means `m(male, x)`. -/
theorem saturated (m00 m10 m01 m11 : ℝ) :
    let b0 := m00
    let b1 := m10 - m00
    let b2 := m01 - m00
    let b3 := m11 - m01 - m10 + m00
    b0 = m00 ∧ b0 + b1 = m10 ∧ b0 + b2 = m01 ∧ b0 + b1 + b2 + b3 = m11 ∧
      b1 + b3 = m11 - m01 := by
  intro b0 b1 b2 b3; refine ⟨rfl, ?_, ?_, ?_, ?_⟩ <;> simp only [b0, b1, b2, b3] <;> ring

/-- Tables 1 and 5, the printed pooled columns reproduce the single-sample
columns exactly. Table 1: `−1.38 + 4.24 = 2.86`, `4.60 + 0.08 = 4.68`,
`−0.31 + 0.89 = 0.58`, `0.79 + 0.09 = 0.88`. Table 5: `−1.15 + 3.32 = 2.17`,
`3.55 − 0.42 = 3.13`, `−0.28 + 0.72 = 0.44`, `0.70 − 0.13 = 0.57`. Table 2:
`2.86 − 6.02 = −3.16`, `4.68 + 2.33 = 7.01`, `0.58 − 1.20 = −0.62`. Table 6:
`2.17 − 4.75 = −2.58`, `3.13 + 1.64 = 4.77`, `0.44 − 0.95 = −0.51`. -/
theorem tables_exact :
    (-138 + 424 : ℤ) = 286 ∧ (460 + 8 : ℤ) = 468 ∧ (-31 + 89 : ℤ) = 58 ∧ (79 + 9 : ℤ) = 88 ∧
    (-115 + 332 : ℤ) = 217 ∧ (355 - 42 : ℤ) = 313 ∧ (-28 + 72 : ℤ) = 44 ∧ (70 - 13 : ℤ) = 57 ∧
    (286 - 602 : ℤ) = -316 ∧ (468 + 233 : ℤ) = 701 ∧ (58 - 120 : ℤ) = -62 ∧
    (217 - 475 : ℤ) = -258 ∧ (313 + 164 : ℤ) = 477 ∧ (44 - 95 : ℤ) = -51 := by
  norm_num

/-- BIR-D9. Four printed constants differ by one in the last digit from the sum
of the pooled coefficients, which rounding explains. Table 2 net votes
`0.88 + 0.49 = 1.37` against 1.38, Table 3 `0.81 + 0.17 = 0.98` against 0.97,
Table 4 `0.57 − 1.20 = −0.63` against −0.64, Table 6 net votes
`0.57 + 0.35 = 0.92` against 0.93. -/
theorem tables_rounding :
    (88 + 49 : ℤ) = 138 - 1 ∧ (81 + 17 : ℤ) = 97 + 1 ∧ (57 - 120 : ℤ) = -64 + 1 ∧
      (57 + 35 : ℤ) = 93 - 1 := by
  norm_num

/-! ### Further controls -/

/-- Control for `prop2_D_decreases`. If the gap did not shrink, the rising
weight `τq(t)/(τq(t) + τη)` alone would raise discrimination. The decrease
needs the factor `τa(t)/(τa(t) + τε)` of Lemma 2. -/
theorem control_D_rises_without_shrinking {ta ta1 te tn g : ℝ} (ha : 0 < ta) (h1 : ta < ta1)
    (he : 0 < te) (hn : 0 < tn) (hg : 0 < g) :
    let tq0 := ta * te / (ta + te)
    let tq1 := ta1 * te / (ta1 + te)
    tq0 / (tq0 + tn) * g < tq1 / (tq1 + tn) * g := by
  intro tq0 tq1
  have hq0 : 0 < tq0 := by positivity
  have h01 : tq0 < tq1 := by
    simp only [tq0, tq1]
    rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith [mul_pos he he]
  have : tq0 / (tq0 + tn) < tq1 / (tq1 + tn) := by
    rw [div_lt_div_iff₀ (by linarith) (by linarith)]; nlinarith
  exact mul_lt_mul_of_pos_right this hg

/-- Control for the coarse model. With a taste `c_F > 0` and equal priors the
woman's cut-off exceeds the man's at every precision. -/
theorem control_coarse_taste {tq tn : ℝ} (hq : 0 < tq) (hn : 0 < tn) (mu cF : ℝ) (hc : 0 < cF) :
    sCut tq tn mu 0 < sCut tq tn mu cF := by
  unfold sCut
  have : 0 < cF * ((tq + tn) / tn) := by positivity
  linarith

/-- Control for eq. (30). With `θ = 1` the stereotype of a two-type group with
true shares `(1/2, 1/2)` against a comparison group `(1/4, 3/4)` puts `3/4` on the
first type, not `1/2`. -/
theorem control_stereo_distorts :
    stereo (T := Fin 2) ![1 / 2, 1 / 2] ![1 / 4, 3 / 4] 1 0 = 3 / 4 := by
  simp [stereo, Fin.sum_univ_two]
  norm_num

end BohrenEtAl2019
