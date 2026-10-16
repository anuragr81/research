# Pęski and Szentes (2012), claims

Evidence tag [F]: the BFI working paper of 7 May 2012, all 34 PDF pages,
read on 16 October 2026 from the text layer, with PDF pages 6 to 13 also read
as images for the equations. Pages are PDF pages (printed page plus one).
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

## Lean results

`lean/mathlib/PeskiSzentes2012.lean`, in the paper's notation with `λ`
written `lam`.

| Theorem | What it checks |
|---|---|
| `PeskiSzentes2012.hire_iff` | Lemma 1 for a `(b, w)` worker. Hiring beats not hiring exactly when `s ≥ γα(V_{w,w} − V_{w,b})` |
| `PeskiSzentes2012.cutoff_indep_own_social_colour` | The employer's own social colour cancels from (1) minus (2) |
| `PeskiSzentes2012.lemma3_ii` | Lemma 3 (ii). `b^c(0, y) = 0` for `y ≥ 0` |
| `PeskiSzentes2012.lemma3_iii` | Lemma 3 (iii), the inequality `b^c(x, y) ≤ b^c(x, 0)` |
| `PeskiSzentes2012.lemma4_ii` | Lemma 4 (ii). At or below the threshold, `b^c(x, 0) < x` for every `x > 0` |
| `PeskiSzentes2012.prop1_i` | Proposition 1 (i). Below the threshold for both colours, `(0, 0)` is the only equilibrium |
| `PeskiSzentes2012.prop1_ii` | Proposition 1 (ii) and Remark 1. Above the threshold there is `x > 0` with `(x, 0)` an equilibrium |
| `PeskiSzentes2012.bias_with_employer_branch` | Our reading of the bias equation, `D = δMA/(2r + δγ)` (PS-D3) |
| `PeskiSzentes2012.bias_as_printed` | The printed line gives `D = Mδ A/2r` |
| `PeskiSzentes2012.control_no_social_colour` | Control. With `γ = 0`, so `K = 0`, no colour discriminates in any profile |
| `PeskiSzentes2012.control_below_threshold_no_discrimination` | Control. Proposition 1 (ii) fails at or below the threshold |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PS-D1 | The version read is the BFI working paper of 7 May 2012, cited as such, not the AER 2013 article | Only the working paper is cached. The key is PeskiSzentes2012 |
| PS-D2 | The text layer renders the ligatures ff, fi and fl as `¤`, `…` and `‡` (for example `payo¤`, `pro…table`, `in‡uence`) | Quotations avoid such words or elide them, as in PS-Q8 where the elided word is "cutoff" |
| PS-D3 | The bias equation below eq. (7) is read with the employer half carrying `(1 − γ)(V_{c,c} − V_{c,−c})`, which gives `K = Mδγ/(2r + δγ)` against the printed `Mδγ/2r` | Only the persistence event, of probability `1 − γ`, makes an employer's value depend on his own social colour. The difference rescales `K`, which is positive in both readings, and changes no result we use |
| PS-D4 | The heuristic equation above eq. (5) writes `Π^s_{c1,c2}` where eq. (5) writes `Π^l_{c1,c2}` | A typo. Both denote the worker's value |
| PS-D5 | Proposition 2's text refers to "part (iv) of Lemma 3" on PDF p. 14, and Lemma 3 has three parts | The limit it cites is part (iii) |
| PS-D6 | The prose after Theorem 1 says the colour-blind equilibrium is unique "if and only if the slope of the best response function is larger than one at the origin" (PDF p. 20) | Proposition 1 and Theorem 2 state the condition as `λ ≤ 1/(K(1 − α)μ_c)`, a slope of at most one. We follow the propositions |
