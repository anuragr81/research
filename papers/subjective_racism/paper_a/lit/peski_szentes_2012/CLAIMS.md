# Pęski and Szentes (2012), claims

Evidence tag [F]: the BFI working paper of 7 May 2012, all 34 PDF pages,
read on 16 October 2026 from the text layer, with PDF pages 6 to 13 and 25
to 28 also read as images for the equations. Pages are PDF pages (printed page plus one).
Quotations are checked against the cached text layer.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PS-Q1 | 4 | "That is, an agent’s physical colour reveals absolutely nothing about his potential as a business partner" | Colour carries no information about any payoff-relevant attribute. Nothing is inferred from it, unlike Paper A1's marker |
| PS-Q2 | 4 | "Therefore, the discrimination in our model is not statistical discrimination." | The authors place their mechanism outside statistical discrimination, where Paper A1's inference sits |
| PS-Q3 | 4 | "and have no intrinsic preferences for one colour over another." | No preference over partners of any kind. Paper A1's engagement utility depends on the partner's class |
| PS-Q4 | 5 | "Such a phenomenon is inconsistent with statistical discrimination because the signal value of colour must be the same for any employer, regardless of his own colour." | The claim Paper A1's contribution 1 qualifies. Under class homophily the posterior is common to all observers but the decision it supports depends on the observer's own class |
| PS-Q5 | 7 | "Prior to making a decision, an employer observes the type of the worker, but nothing else." | The whole type is observed. There is no unseen type in the model |
| PS-Q6 | 7 | "Note that the social colour of an agent carries information about his past employees." | The only signal in the model is about past partners, not about the agent's own attributes |
| PS-Q7 | 7 | "In fact, this is true even if agents have physical colours but no social colours" | Without social colour hiring is unconditional. Formalised as `PeskiSzentes2012.control_no_social_colour` |
| PS-Q8 | 8 | "Therefore, while the best-response [...] of an agent may depend on his physical colour, it cannot depend on his social colour." | Lemma 1. The cutoff depends on the employer's own physical colour. Formalised as `PeskiSzentes2012.cutoff_indep_own_social_colour` |
| PS-Q9 | 9 | "can be interpreted as a bias the agent has towards his own physical colour." | The source of own-colour dependence is the value of one's own social colour, not a preference over the partner |
| PS-Q10 | 11 | "A notable feature of the best response function is that it does not depend on the distribution of social colours." | The equilibrium condition is on cutoffs. No group statistic has to equal an imputed one |
| PS-Q11 | 19 | "First, the white population discriminates strongly against the black population, while black agents do not discriminate at all." | Theorem 1's one-sided equilibrium. Its existence above the threshold is `PeskiSzentes2012.prop1_ii` |
| PS-Q12 | 24 | "In addition, in our model, agents would never discriminate if they knew that their interactions were not observed" | The authors' own test. Discrimination vanishes when partnerships are unobserved, not when an attribute is shown |
| PS-Q13 | 24 | "white employers hiring white workers with black social colours earn more than those who hire whites with white social colours." | A second prediction specific to the association channel |
| PS-Q14 | 24 | "This is inconsistent with the theory of statistical discrimination, according to which the hiring decision of an employer should not depend on his own skin-colour." | The same claim as PS-Q4, restated in the empirical section |
| PS-Q15 | 3 | "being labelled as someone who associates with one’s own physical colour becomes valuable through the equilibrium play." | Information about whom an agent has met acquires value in equilibrium |
| PS-Q16 | 24 | "Therefore, people are more likely to discriminate in smaller communities, like villages, where people are better able to observe the actions of others" | Discrimination rises with how visible contacts are |
| PS-Q17 | 25 | "Hence, a policy intervention which would reduce the incentive to discriminate might involve increasing the fraction of the population whose social colours are" | The authors' policy acts on the distribution of labels, not on a ban |
| PS-Q18 | 19 | "each race discriminates strongly against the other." | Theorem 1's mutual equilibrium. Avoidance runs both ways |
| PS-Q19 | 9 | "This bias may well be negative, that is, an agent is better [...] if his physical colour does not coincide with his social colour." | Footnote 5. An agent may gain from association with the other colour. The elided word is "off" (PS-D2) |
| PS-Q20 | 12 | "then black agents are better [...] having a white social colour than a black one." | When one's own colour does not discriminate and the other does, association with the other colour pays. The elided word is "off" |
| PS-Q21 | 21 | "This makes it more expensive to discriminate against whites, and cheaper to discriminate against blacks." | Section 6.1. Group shares move the intensity of discrimination |

## Lean results

`lean/mathlib/PeskiSzentes2012.lean`, in the paper's notation with `λ`
written `lam`. A colour-`c` employer's social colour is coded `true` when it
equals his physical colour.

| Theorem | What it checks |
|---|---|
| `PeskiSzentes2012.hire_iff` | Lemma 1 for a `(b, w)` worker. Hiring beats not hiring exactly when `s ≥ γα(V_{w,w} − V_{w,b})` |
| `PeskiSzentes2012.cutoff_indep_own_social_colour` | The employer's own social colour cancels from (1) minus (2) |
| `PeskiSzentes2012.lemma1_hire` | Lemma 1 for every worker type. The comparison depends only on the cutoff, whatever the employer's social colour |
| `PeskiSzentes2012.lemma1_cutoffs` | Lemma 1. The four cutoffs are `γ(V_{c,c} − V_{c,−c})` times `1`, `1 − α`, `α`, `0` |
| `PeskiSzentes2012.eq3` | Eq. (3) after truncation at zero |
| `PeskiSzentes2012.worker_gap` | Eqs. (6) and (7). The gap between the two worker values |
| `PeskiSzentes2012.lemma2` | Lemma 2 and eq. (8) from the printed bias line, with `K = Mδγ/2r` |
| `PeskiSzentes2012.lemma2_our_reading` | Lemma 2 on our reading, with `K = Mδγ/(2r + δγ)` (PS-D3) |
| `PeskiSzentes2012.bias_with_employer_branch` | Our reading of the bias equation, `D = δMA/(2r + δγ)` (PS-D3) |
| `PeskiSzentes2012.bias_as_printed` | The printed line gives `D = Mδ A/2r` |
| `PeskiSzentes2012.lemma3_i_strictMono` | Lemma 3 (i). `b̃^c` is strictly increasing in the own cutoff |
| `PeskiSzentes2012.lemma3_i_concave` | Lemma 3 (i). `b̃^c` is concave in the own cutoff |
| `PeskiSzentes2012.lemma3_ii` | Lemma 3 (ii). `b^c(0, y) = 0` for `y ≥ 0` |
| `PeskiSzentes2012.lemma3_iii` | Lemma 3 (iii), the inequality `b^c(x, y) ≤ b^c(x, 0)` |
| `PeskiSzentes2012.G_shift_tendsto` | `G(αy) − G(y) → 0` as `y → ∞`, for `α > 0` |
| `PeskiSzentes2012.lemma3_iii_limit` | Lemma 3 (iii), the equality `b^c(x, 0) = lim_{y→∞} b^c(x, y)`, for `α > 0` |
| `PeskiSzentes2012.lemma4_i` | Lemma 4 (i). Above the threshold `b^c(·, 0)` has exactly one positive fixed point |
| `PeskiSzentes2012.lemma4_ii` | Lemma 4 (ii). At or below the threshold, `b^c(x, 0) < x` for every `x > 0` |
| `PeskiSzentes2012.fp_chord` | Strict concavity of `Kμ_c(1 − e^{−λ(1−α)x}) − x` along a chord from the origin |
| `PeskiSzentes2012.fp_root_unique` | That function has at most one positive root |
| `PeskiSzentes2012.prop1_zero` | Proposition 1. `(0, 0)` is an equilibrium |
| `PeskiSzentes2012.prop1_i` | Proposition 1 (i). Below the threshold for both colours, `(0, 0)` is the only equilibrium |
| `PeskiSzentes2012.prop1_ii` | Proposition 1 (ii). Above the threshold there is `x > 0` with `(x, 0)` an equilibrium |
| `PeskiSzentes2012.remark1` | Remark 1. That `x` is unique |
| `PeskiSzentes2012.compstat_one_sided` | Section 6.1 for one-sided equilibria. The positive cutoff rises with `Kμ_c` and with `λ(1 − α)` |
| `PeskiSzentes2012.lemma5_shift` | Proof of Lemma 5. `B^{x^{−c}} = B^0 + H(x^{−c})` |
| `PeskiSzentes2012.lemma5_H_neg` | Proof of Lemma 5, property (6). `H < 0` for `x^{−c} > 0` |
| `PeskiSzentes2012.lemma5_H_limits` | Proof of Lemma 5, property (7). `H → 0` at infinity and `H(0) = 0` |
| `PeskiSzentes2012.lemma5_B_zero` | Proof of Lemma 5, property (4). `B^{x^{−c}}(0) = H(x^{−c})` |
| `PeskiSzentes2012.lemma6` | Lemma 6 on `0 ≤ ξ ≤ log 2` (PS-D7) |
| `PeskiSzentes2012.eq15_term` | The factorisation behind eq. (15) |
| `PeskiSzentes2012.eq15` | Eq. (15) |
| `PeskiSzentes2012.Pf_ge_sq` | Inequality (16) |
| `PeskiSzentes2012.lemma7_both_pos` | Lemma 7, both cutoffs positive, with the constant `2ψ₀` (PS-D8) |
| `PeskiSzentes2012.lemma7_one_zero` | Lemma 7, one cutoff zero, with the threshold `2/(Kμ_w(1 − α))` (PS-D8) |
| `PeskiSzentes2012.lemma7` | Lemma 7 for every equilibrium, corrected constants |
| `PeskiSzentes2012.slope_at_zero` | The slope of `b̃^c(·, 0)` at 0 is `Kμ_c(1 − α)λ` |
| `PeskiSzentes2012.drift_neg_below` | Eq. (12) on the axis, at or below the threshold. The drift is negative |
| `PeskiSzentes2012.drift_pos_near_zero` | Eq. (12) on the axis, above the threshold. The drift is positive near zero |
| `PeskiSzentes2012.control_no_social_colour` | Control. With `γ = 0`, so `K = 0`, no colour discriminates in any profile |
| `PeskiSzentes2012.control_below_threshold_no_discrimination` | Control. Proposition 1 (ii) fails at or below the threshold |
| `PeskiSzentes2012.control_lemma3_iii_needs_alpha` | Control. With `α = 0` the limit in Lemma 3 (iii) is not `b̃^c(x, 0)` |
| `PeskiSzentes2012.control_lemma6_needs_nonneg` | Control. Lemma 6 as printed fails at `ξ = −1` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Lemma 1 | `PeskiSzentes2012.lemma1_hire`, `PeskiSzentes2012.lemma1_cutoffs` | general | |
| Eq. (3) | `PeskiSzentes2012.eq3` | general | |
| Lemma 2, eqs. (4) to (9) | `PeskiSzentes2012.lemma2` | general | Proved from the printed bias line. Our reading of that line changes `K` (`PeskiSzentes2012.lemma2_our_reading`, PS-D3) |
| Lemma 3 (i) | `PeskiSzentes2012.lemma3_i_strictMono`, `PeskiSzentes2012.lemma3_i_concave` | general | Proved for `b̃^c` on the whole line, which gives the local statement wherever `b^c > 0` |
| Lemma 3 (ii) | `PeskiSzentes2012.lemma3_ii` | general | |
| Lemma 3 (iii) | `PeskiSzentes2012.lemma3_iii`, `PeskiSzentes2012.lemma3_iii_limit` | general | The equality needs `α > 0` and `λ > 0`, which the paper leaves implicit (PS-D10) |
| Lemma 4 (i) | `PeskiSzentes2012.lemma4_i` | general | |
| Lemma 4 (ii) | `PeskiSzentes2012.lemma4_ii` | general | |
| Proposition 1, `(0, 0)` | `PeskiSzentes2012.prop1_zero` | general | |
| Proposition 1 (i) | `PeskiSzentes2012.prop1_i` | general | |
| Proposition 1 (ii) | `PeskiSzentes2012.prop1_ii` | general | |
| Remark 1 | `PeskiSzentes2012.remark1` | general | |
| Lemma 5 | `PeskiSzentes2012.lemma5_shift`, `PeskiSzentes2012.lemma5_H_neg`, `PeskiSzentes2012.lemma5_H_limits`, `PeskiSzentes2012.lemma5_B_zero` | special case | The shift and properties (4), (6) and (7) are proved. Properties (3), (5), (8), (9), the two root branches and their shapes are not. They need a root-counting argument on a family of concave functions |
| Proposition 2 | | not formalised | Rests on Lemmas 7 to 9 and limits in `λ`. Lemma 7 is formalised, Lemmas 8 and 9 are not |
| Definition 1 | | not formalised | Stability of best-response dynamics over distributions of cutoffs |
| Eq. (12) | `PeskiSzentes2012.slope_at_zero`, `PeskiSzentes2012.drift_neg_below`, `PeskiSzentes2012.drift_pos_near_zero` | special case | The sign of the drift on the axis `x^{−c} = 0`. The two-dimensional flow is not formalised |
| Theorem 1 | | not formalised | Needs Proposition 2 and the Hartman–Grobman theorem, which Mathlib lacks |
| Theorem 2 (i) | `PeskiSzentes2012.drift_neg_below` | special case | The drift along the axis only, not Lyapunov stability in the plane |
| Theorem 2 (ii) | `PeskiSzentes2012.drift_pos_near_zero`, `PeskiSzentes2012.prop1_ii` | special case | The direction that makes `(0, 0)` unstable (Case 1 of the proof of Theorem 1) and the existence of a discriminating equilibrium. Its stability is not formalised |
| Theorem 3 | | not formalised | A general concave `G` is not set up. Everything formalised uses the exponential `G` |
| Section 6.1, comparative statics | `PeskiSzentes2012.compstat_one_sided` | special case | One-sided equilibria `(x^c, 0)` only, for parameters entering through `Kμ_c` and `λ(1 − α)`. The mutual equilibrium is not covered. The paper omits the proof |
| Lemma 6 | `PeskiSzentes2012.lemma6` | general | On `0 ≤ ξ ≤ log 2`. The printed domain `ξ ≤ log 2` is too wide (PS-D7, `PeskiSzentes2012.control_lemma6_needs_nonneg`) |
| Eq. (15) | `PeskiSzentes2012.eq15` | general | |
| Lemma 7 | `PeskiSzentes2012.lemma7` | general | With corrected constants, `2ψ₀` and `λ > 2/(Kμ_c(1 − α))` (PS-D8) |
| Lemma 8 | | not formalised | Asymptotic in `λ`. Not attempted in this pass |
| Lemma 9 | | not formalised | Asymptotic in `λ`. Not attempted in this pass |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PS-D1 | The version read is the BFI working paper of 7 May 2012, cited as such, not the AER 2013 article | Only the working paper is cached. The key is PeskiSzentes2012 |
| PS-D2 | The text layer renders the ligatures ff, fi and fl as `¤`, `…` and `‡` (for example `payo¤`, `pro…table`, `in‡uence`) | Quotations avoid such words or elide them, as in PS-Q8 where the elided word is "cutoff" |
| PS-D3 | The bias equation below eq. (7) is read with the employer half carrying `(1 − γ)(V_{c,c} − V_{c,−c})`, which gives `K = Mδγ/(2r + δγ)` against the printed `Mδγ/2r` | Only the persistence event, of probability `1 − γ`, makes an employer's value depend on his own social colour. The difference rescales `K`, which is positive in both readings, and changes no result we use |
| PS-D4 | The heuristic equation above eq. (5) writes `Π^s_{c1,c2}` where eq. (5) writes `Π^l_{c1,c2}` | A typo. Both denote the worker's value |
| PS-D5 | The text before Lemma 5 refers to "part (iv) of Lemma 3" on PDF p. 14, and Lemma 3 has three parts | The limit it cites is part (iii) |
| PS-D6 | The prose after Theorem 1 says the colour-blind equilibrium is unique "if and only if the slope of the best response function is larger than one at the origin" (PDF p. 20) | Proposition 1 and Theorem 2 state the condition as `λ ≤ 1/(K(1 − α)μ_c)`, a slope of at most one. We follow the propositions |
| PS-D7 | Lemma 6 is read on `0 ≤ ξ ≤ log 2` | The printed "For all ξ ≤ log 2" (PDF p. 27) is false for negative `ξ`, for example at `ξ = −1`. Every use in the paper has `ξ ≥ 0` |
| PS-D8 | Lemma 7's constants are read as `2ψ₀` in the first case and `λ > 2/(Kμ_w(1 − α))` in the second | On PDF p. 28 the chain bounds `max{x^b, x^w}` by `x^b + x^w`, which loses a factor 2, and writes `x^w ≥ 2Kμ_w(1 − α)λx^w` where Lemma 6 gives `½Kμ_w(1 − α)λx^w`. The lemma holds with the corrected constants, and Proposition 2 needs only some constants |
| PS-D9 | Property (2) in the proof of Lemma 5 needs `λ > λ₀` strictly | The proof lists it for `λ ≥ 1/(K(1 − α)μ_c)` (PDF p. 26), and at equality the slope of `B^0` at zero is 0 |
| PS-D10 | The equality in Lemma 3 (iii) is read with `α > 0` | With `α = 0` the limit of `G(αy) − G(y)` is `−1`, not 0 (`PeskiSzentes2012.control_lemma3_iii_needs_alpha`). The paper treats `α` as a probability without excluding 0 |
