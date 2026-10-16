# Bohren, Imas and Rosenberg (2019), reconstruction

J. Aislinn Bohren, Alex Imas and Michael Rosenberg, "The Dynamics of
Discrimination: Theory and Evidence", working paper, January 2019. Published
as *American Economic Review* 109, 2019, pp. 3395–3436 (as Bohren, Hull and Imas cite it), which is not read
(BIR-D1).

## Source read

- The working paper of January 2019, 83 PDF pages. PDF page 1 is the title
  page with the abstract, and the printed page number is the PDF page minus 1
  from PDF page 2 on. Cached at
  `~/.cache/paper_a/sources/bohren_imas_rosenberg_2019_wp.pdf`, sha256
  `aa697aa3…3639a`. Text layer at
  `~/.cache/paper_a/sources/bohren_imas_rosenberg_2019_wp.txt`, sha256
  `227c28d7…29ba3`, with an 84th, empty page.
- Read in full on 16 October 2026 from the text layer, every page, so the
  evidence tag is [F]. Page images at 110 dpi were read for PDF pages 26 and
  55, where the text layer could not settle a symbol. Figures 1 to 7 are
  images and their plotted values are not attributed.

## The model, from its own primitives

1. **Worker** (PDF p. 10). Visible gender `g ∈ {F, M}` and unseen ability
   `a ∼ N(μ_g, 1/τa)`. Each task has hidden quality `q_t = a + ε_t`,
   `ε_t ∼ N(0, 1/τε)`.
2. **Evaluators** (PDF pp. 11–12). One evaluator per task sees `g`, the
   history of past evaluations `h_t = (v_1, …, v_{t−1})` and a signal
   `s_t = q_t + η_t` with precision `τη`. Lower `τη` is read as more subjective
   judgement. Type `θ_i` fixes a taste `c_{ig}` (with `c_{iM} = 0`), a
   subjective mean `μ̂_{ig}` that may differ from `μ_g`, and a belief `π̂_i`
   about the distribution of types. The payoff is `−(v − (q − c_{ig}))²`, so
   the evaluation is `E[q_t | h_t, s_t, g] − c_{ig}` (eq. 2).
3. **Partiality and discrimination** (PDF pp. 11–13). Preference-based
   partiality is `c_{iF} > 0` (Definition 1). Belief-based partiality is
   `μ̂_{iM} > μ̂_{iF}`, unbiased when both equal the true means (Definition 2).
   Discrimination is `D_i(h, s) = v_i(h, s, M) − v_i(h, s, F) > 0` (Definition
   3), and a reversal is a sign change of `D` along a history (Definition 4).
   Partiality is a property of primitives and discrimination of behaviour
   (BIR-Q4).
4. **Initial discrimination** (PDF pp. 16–18). With `τq = τa τε/(τa + τε)`,
   eq. (4) gives `v = (τq μ̂_g + τη s)/(τq + τη) − c_g`, and eq. (5)
   `D(h₁, s₁) = τq/(τq + τη)(μ̂_M − μ̂_F) + c_F`. At one precision a taste and
   a belief gap give the same evaluations (footnote 9, BIR-Q6). Proposition 1
   separates them by precision. Belief-based discrimination falls in `τη` and
   vanishes as `τη → ∞`, and preference-based discrimination does not
   (BIR-Q7 to BIR-Q9).
5. **Dynamics** (PDF pp. 18–23). With no taste, one type and common knowledge,
   a common evaluation is a stronger signal for a woman (eq. 6), but the
   posterior means keep their order and move closer (Lemma 2), so
   discrimination falls and never reverses (Proposition 2, BIR-Q10). With a
   heuristic type of share `p` who believes everyone shares his bias and an
   impartial type who knows of the heuristic type, the impartial type's
   posterior favours women after one evaluation (BIR-Q12), and for low `p`
   aggregate discrimination reverses (Proposition 3). A reversal is evidence
   of misspecification (BIR-Q11).
6. **Robustness and alternatives** (PDF pp. 23–27). Other ability families
   satisfying MLRP (numerical), coarse votes (Proposition 4), shifting
   standards, differential attrition, a gender difference in the variance of
   ability (BIR-Q13), heterogeneous tastes, and self-fulfilling beliefs.
7. **Experiment** (PDF pp. 27–37). Mathematics Stack Exchange. 280 new
   question accounts, half male and half female usernames, half left Novice
   and half built to a reputation of at least 100 (mean 155.23) and then
   re-randomised in gender. 273 questions analysed. A second set of 140
   Novice accounts posted answers, 135 analysed. Outcomes are the reputation
   earned on the post (∆Rep) and net votes, after seven days.
8. **Results** (PDF pp. 32–37, Tables 1 and 2). No significant gender gap on
   answers (Male coefficient on ∆Rep −1.38, s.e. .97). Novice questions favour
   men (Male 2.86, s.e. 1.32, about 0.4 standard deviations, BIR-Q14).
   Advanced questions favour women (Male −3.16, s.e. 1.37, about 0.6 standard
   deviations, BIR-Q15). Male × Question 4.24 (s.e. 1.64) and Male × Advanced
   −6.02 (s.e. 1.91). The authors read this as belief-based discrimination
   with bias.
9. **Observational data** (PDF pp. 38–41, Appendix B.4). 315,792 users from
   July 2010 to March 2017, gender inferred for 55 percent. No differential
   attrition by gender, no gender difference in the variance of answer
   evaluations (Levene's test, p = .41), no negative autocorrelation, and the
   same pattern of signs as the experiment.
10. **Stereotypes** (PDF pp. 81–83, Appendix C). Under Bordalo et al.'s
    representativeness with distortion `θ = 2.5`, a small gender gap in mean
    answer quintile (2.97 against 2.87) roughly triples.

## Derivation chain of what Paper A1 uses

- Eq. (5) is eq. (4) at `c_M = 0` differenced across genders, and does not
  depend on the signal (`eq5`).
- Proposition 1 follows from eq. (5). The weight `τq/(τq + τη)` on the belief
  gap falls in `τη` and tends to 0, while `c_F` does not move
  (`prop1_decreasing`, `prop1_constant`, `prop1_limit`).
- Footnote 9 is the same algebra read the other way (`fn9_equivalence`).
- Lemma 2 follows from eq. (6) and the normal update of Lemma 1. The gap in
  posterior means is the prior gap times the factor of eq. (12) (`eq12`). We
  find that the factor equals `τa(t)/(τa(t) + τε)`, in which `τη` cancels
  (`k_eq`, BIR-D6), so it lies in `(0, 1)` (`k_mem`, `lemma2`).
- Proposition 2 is Lemma 2 applied period by period, with Lemma 3 turning the
  order of means into the sign of `D` (`prop2`).
- That discrimination also falls, which the paper leaves at a display, follows
  because the weight `τq(t)/(τq(t) + τη)` rises more slowly than the gap
  shrinks (`prop2_D_decreases`, with `control_D_rises_without_shrinking`).
- The step that makes Proposition 3 possible is that an impartial evaluator
  who attributes an evaluation to a biased evaluator backs out a higher
  signal for a woman (`control_reversal_misspecified`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every numbered result, and the gaps are these.

- Normal densities and integrals are not formalised. Eq. (4) and Lemma 1
  are checked as identities of log kernels and of the update recursion
  (`normal_update`, `lemma1`).
- Proposition 3 in general. The mixture posterior and the derivative in `p`
  (PDF pp. 52–55) are not formalised. The boundary `p = 0`, the order of the
  backed-out signals and the degenerate case in which the impartial evaluator
  is sure the first evaluator was heuristic are.
- Proposition 4 and Lemma 5, which need preservation of the monotone
  likelihood ratio property under convolution (Keilson and Sumita), and the
  hazard-rate bound in Lemma 4, which the paper takes from Stack Exchange.
  Only the step `τx ≥ 1` of Lemma 4 is formalised.
- Appendix A.2, whose checks are numerical with code in an unread
  supplement. For the binary model, the monotonicity of `φ` and the inversion
  of eq. (19) are formalised.
- The claim after Definition 6 fails as printed (BIR-D8).
- The regression estimates themselves and eqs. (28), (29). Only the internal
  arithmetic of Tables 1 to 6 is checked.
- The figures, whose plotted values are not in the text layer.
