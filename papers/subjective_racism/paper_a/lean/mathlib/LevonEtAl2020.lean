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

/-! ### The sample of the real-voice study (PDF pp. 11, 12)

The nations add to the stated sample, and ten speakers, two per accent, give
the ten clips each listener hears, two versions of each accent. -/

/-- England 890, Wales 51, Scotland 90 and Northern Ireland 31 make 1062. -/
theorem voices_sample : (890 + 51 + 90 + 31 : ℕ) = 1062 := by norm_num

/-- Two native speakers of each of five accents make ten speakers, and two
versions of each accent make the ten clips a listener hears. -/
theorem voices_design : (2 * 5 : ℕ) = 10 := by norm_num

/-! ### Expert content (PDF p. 19)

"Expert content improves the judgement of all accents." We read an expert
answer as one more likely from a competent speaker, a larger `p` at the same
`q`. -/

/-- An answer more likely from a competent speaker raises the rating at every
prior strictly between 0 and 1, so it raises every accent's rating. -/
theorem expert_raises_all {π p p' q : ℝ} (h0 : 0 < π) (h1 : π < 1) (hq : 0 < q)
    (hp : 0 < p) (hpp : p < p') : post π p q < post π p' q := by
  unfold post
  have d1 : 0 < π * p + (1 - π) * q := by nlinarith
  have d2 : 0 < π * p' + (1 - π) * q := by nlinarith
  rw [div_lt_div_iff₀ d1 d2]
  have : 0 < π * (1 - π) * q * (p' - p) := by
    have : 0 < 1 - π := by linarith
    have : 0 < p' - p := by linarith
    positivity
  nlinarith

/-! ### Cognitive control (PDF pp. 20 to 24)

The report's account of the lawyers, of listeners with high motivation to
control a prejudiced response (MCPR) and of the awareness intervention is that
a rater can attend to content rather than to the stereotype (PDF pp. 5, 23).
We read a rating as a weighted mix, `γ` on content and `1 − γ` on the
stereotype attached to the accent. -/

/-- A rating with weight `γ` on content and `1 − γ` on the stereotype. -/
def rating (γ stereo content : ℝ) : ℝ := γ * content + (1 - γ) * stereo

/-- For an identical answer, the accent penalty is `(1 − γ)` times the gap in
stereotypes. -/
theorem rating_penalty (γ sR sA c : ℝ) :
    rating γ sR c - rating γ sA c = (1 - γ) * (sR - sA) := by
  unfold rating; ring

/-- More attention to content gives a smaller penalty, the reading of the
awareness intervention (PDF p. 23). -/
theorem penalty_antitone_in_control {γ₁ γ₂ sR sA c : ℝ} (hs : sA < sR) (hγ : γ₁ < γ₂) :
    rating γ₂ sR c - rating γ₂ sA c < rating γ₁ sR c - rating γ₁ sA c := by
  rw [rating_penalty, rating_penalty]
  nlinarith

/-- Full attention to content gives no penalty, the lawyers' reading
(PDF p. 22). -/
theorem full_control_no_penalty (sR sA c : ℝ) :
    rating 1 sR c - rating 1 sA c = 0 := by
  rw [rating_penalty]; ring

/-- Control: drop full attention to content. With a stereotype gap the penalty
is positive at every `γ < 1`. -/
theorem control_partial_control {γ sR sA c : ℝ} (hs : sA < sR) (hγ : γ < 1) :
    0 < rating γ sR c - rating γ sA c := by
  rw [rating_penalty]
  have : 0 < 1 - γ := by linarith
  have : 0 < sR - sA := by linarith
  positivity

/-! ### Life stage against cohort (PDF p. 18)

Older listeners rate the accents lower. The authors read this as a life-stage
effect rather than change over time, because Coupland and Bishop (2007) found
the same age pattern. We read a cohort model as a rating that depends on the
year of birth, `f (t − a)` in year `t` at age `a`, and a life-stage model as a
rating `g a` that depends on age alone. -/

/-- The cross-section by age in year `t` under a cohort model. -/
def cohortSection (f : ℝ → ℝ) (t a : ℝ) : ℝ := f (t - a)

/-- Under a cohort model the cross-section `d` years later is the earlier one
shifted by `d` years of age. -/
theorem cohort_shifts (f : ℝ → ℝ) (t d a : ℝ) :
    cohortSection f (t + d) a = cohortSection f t (a - d) := by
  unfold cohortSection; ring_nf

/-- Under a cohort model with a strictly monotone profile, two cross-sections
`d ≠ 0` years apart differ at every age, so the same age pattern in both
surveys rules the model out. -/
theorem cohort_sections_differ {f : ℝ → ℝ} (hf : StrictMono f) {d : ℝ} (hd : d ≠ 0)
    (t a : ℝ) : cohortSection f (t + d) a ≠ cohortSection f t a := by
  unfold cohortSection
  intro h
  have := hf.injective h
  apply hd; linarith

/-- Control: drop the strict monotonicity. A flat cohort profile gives the
same cross-section in every year, so the inference needs a profile that varies
across cohorts. -/
theorem control_flat_cohort (k t d a : ℝ) :
    cohortSection (fun _ => k) (t + d) a = cohortSection (fun _ => k) t a := rfl

/-! ### RP likeability among lawyers (PDF p. 22)

One reading the authors offer for the lawyers' marginally lower likeability of
RP is "a higher expectation of quality answers for RP speakers". We read this
as a rating relative to the expectation attached to the accent. -/

/-- A rating of an answer of quality `s` relative to the expectation `E`. -/
def relRating (s E : ℝ) : ℝ := s - E

/-- A higher expectation for RP rates the same answer lower in RP. -/
theorem higher_expectation_lower_rating {s ER EA : ℝ} (h : EA < ER) :
    relRating s ER < relRating s EA := by
  unfold relRating; linarith

/-- Control: drop the gap in expectations. With equal expectations the same
answer is not rated lower in RP. -/
theorem control_equal_expectation (s E : ℝ) : ¬ relRating s E < relRating s E :=
  lt_irrefl _

end LevonEtAl2020
