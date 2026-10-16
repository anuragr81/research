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

end BohrenEtAl2019
