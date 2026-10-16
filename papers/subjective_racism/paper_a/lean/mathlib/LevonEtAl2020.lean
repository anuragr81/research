import Mathlib

/-! Levon, Sharma, Watt and Perry, *Accent Bias in Britain. Attitudes to
Accents in Britain and Implications for Fair Access*, project report,
February 2020.

The report states its findings in words and figures and states no model. What
follows formalises **our reading** of the structure the report attributes to
listeners, and not a model of the report's.

* The report says that accents "trigger social stereotypes" and that a
  stereotype becomes discrimination when it is used "to judge unrelated traits,
  like intelligence, competence or trustworthiness" (PDF p. 8). We read the
  stereotype as a listener's prior `π a` that a speaker of accent `a` is
  competent.
* The design plays identical answers in different accents (PDF pp. 5, 12).
  We read an answer as a signal `s` with `p = P(good answer | competent)` and
  `q = P(good answer | not competent)`. A listener who rates by the posterior
  `post (π a) p q` rates identical good answers differently whenever the priors
  differ and the answer does not reveal competence.
* Lawyers "rate responses according to whether the response was of marginally
  higher or lower quality, regardless of accent" (PDF p. 22). We read this as a
  rating that depends on the answer alone.

The five accents of the voice studies are RP, EE, MLE, GNE and UWYE (PDF
p. 12). -/

namespace LevonEtAl2020

/-- The five accents of the real-voice and recruiter studies (PDF p. 12). -/
inductive Accent | RP | EE | MLE | GNE | UWYE
  deriving DecidableEq

/-- A listener's posterior that a speaker is competent after a good answer,
given the prior `π` attached to the speaker's accent. -/
noncomputable def post (π p q : ℝ) : ℝ := π * p / (π * p + (1 - π) * q)

/-- The accent penalty on an identical good answer, the difference in the
posterior between the RP prior `πR` and another accent's prior `πa`. -/
noncomputable def penalty (πR πa p q : ℝ) : ℝ := post πR p q - post πa p q

/-- Identical answers, different ratings. With a prior gap in favour of RP and
an answer that does not reveal competence (`q > 0`), the RP speaker is rated
higher for the same good answer, the finding the report states on PDF p. 5. -/
theorem identical_answer_penalty {πa πR p q : ℝ} (hp : 0 < p) (hq : 0 < q)
    (ha : 0 < πa) (hgap : πa < πR) (hR : πR < 1) :
    post πa p q < post πR p q := by
  unfold post
  have da : 0 < πa * p + (1 - πa) * q := by nlinarith
  have dR : 0 < πR * p + (1 - πR) * q := by nlinarith
  rw [div_lt_div_iff₀ da dR]
  have : πa * (1 - πR) * q < πR * (1 - πa) * q := by nlinarith
  nlinarith

/-- An answer that reveals competence (`q = 0`, no incompetent speaker gives
it) is rated the same in every accent with a positive prior. The penalty is
zero. -/
theorem revealing_no_penalty {πa πR p : ℝ} (hp : 0 < p) (ha : 0 < πa) (hR : 0 < πR) :
    penalty πR πa p 0 = 0 := by
  unfold penalty post
  have h1 : πR * p ≠ 0 := by positivity
  have h2 : πa * p ≠ 0 := by positivity
  simp [h1, h2]

/-- An answer that carries no information (`p = q`) leaves the rating at the
prior, so the penalty is the gap in the priors, the reading of the accent-label
survey, in which no speech is heard. -/
theorem uninformative_is_label {π p : ℝ} (hp : 0 < p) :
    post π p p = π := by
  unfold post
  have : π * p + (1 - π) * p = p := by ring
  rw [this]
  field_simp

/-- The lawyers' reading. A rating that depends on the answer alone gives every
accent the same rating for the same answer. -/
theorem quality_only_no_penalty (r : Accent → Bool → ℝ)
    (h : ∃ f : Bool → ℝ, ∀ a s, r a s = f s) (a b : Accent) (s : Bool) :
    r a s = r b s := by
  obtain ⟨f, hf⟩ := h
  rw [hf, hf]

/-- Control: drop the condition that the answer reveals competence. With
`q = 1/10` the penalty on a good answer is positive. -/
theorem control_partial_signal :
    penalty (3 / 4) (1 / 2) (9 / 10) (1 / 10) ≠ 0 := by
  unfold penalty post
  norm_num

/-- Control: the report finds the gap smaller with real voices than with
labels (PDF p. 18). That is not implied by the Bayesian reading. With a small
prior gap and an informative answer, the posterior gap exceeds the prior gap,
so the finding is an empirical one. -/
theorem control_signal_can_widen_gap :
    post (2 / 100) 1 (1 / 100) - post (1 / 100) 1 (1 / 100) > 2 / 100 - 1 / 100 := by
  unfold post
  norm_num

/-- Control: drop the condition that the rating depends on the answer alone.
An additive accent effect `β` survives at every answer quality, so showing a
good answer does not remove it. -/
theorem control_additive_accent_effect (f : Bool → ℝ) {β : ℝ} (hβ : β ≠ 0) (s : Bool) :
    (fun (a : Accent) (s : Bool) => f s - if a = Accent.RP then 0 else β) Accent.RP s
      - (fun (a : Accent) (s : Bool) => f s - if a = Accent.RP then 0 else β) Accent.MLE s
      ≠ 0 := by
  simp
  exact hβ

end LevonEtAl2020
