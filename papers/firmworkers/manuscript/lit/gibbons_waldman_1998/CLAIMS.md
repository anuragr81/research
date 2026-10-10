# Claims about Gibbons and Waldman (1999)

**Source.** NBER Working Paper 6454, March 1998, "A Theory of Wage and Promotion
Dynamics in Internal Labor Markets", read in full [F]; the *QJE* version (1999,
"... Inside Firms") is **not** read, so the ledger row is unverified for that
version. See `RECONSTRUCTION.md` for the scan, its sha256, the OCR text and its
sha256. Cited in `claims.yaml` as row L2 and in `refs.bib` as
`GibbonsWaldman1999`. Pages are the working paper's printed pages. Quotations
are checked verbatim against the OCR text of the page block that carries their
page by `verify_gibbons_waldman_1998.py`. Results are proved in
`lean/Lit/GibbonsWaldman1998.lean` (see `LEAN`), namespace
`GibbonsWaldman1998`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| GW-1 | 3 | "In this paper we develop a model that integrates job assignment, on-the-job human-capital acquisition, and learning." | L2 as stated in the ledger |
| GW-2 | 8 | "A worker's effective ability is a function of the worker's innate ability and the worker's labor-market experience." | Equation (1) |
| GW-3 | 8 | "Workers and firms are risk-neutral and have a discount rate of zero." | Spot contracts suffice |
| GW-4 | 10 | "in equilibrium there is a job ladder that workers climb as they gain labor-market experience" | Proposition 1 |
| GW-5 | 10 | "There are no demotions in equilibrium because effective ability increases monotonically as a worker gains labor-market experience" | No demotions under full information |
| GW-6 | 10 | "For the same reason, a worker's wage rises every period." | Rising wages under full information |
| GW-7 | 11 | "there is serial correlation in wage increases because high-ability workers have faster growth in effective ability at all experience levels" | Serial correlation under full information |
| GW-8 | 12 | "The wage increase at promotion is the sum of two parts" | The decomposition of the raise at promotion |
| GW-9 | 15 | "Proposition 2 says that wages and job assignments are now determined by expected effective ability" | Proposition 2 |
| GW-10 | 15 | "because output is a linear function of effective ability on each job" | Why only the expectation matters |
| GW-11 | 19 | "Any demotions that occur are associated with wage decreases." | Corollary 2.3 |
| GW-12 | 19 | "a worker's expected innate ability can fall substantially from one period to the next" | Source of wage decreases under learning |
| GW-13 | 20 | "there can be a positive frequency of wage decreases but no demotions" | Wage decreases without demotions |
| GW-14 | 24 | "performance evaluations measure expected innate ability" | Section V.A reading of evaluations |
| GW-15 | 29 | "our model offers a basic framework upon which to build more accurate models of careers in organizations" | The authors' own scope |
| GW-16 | 32 | "a sufficiently strong signal can move the market's belief arbitrarily far" | Proof of Corollary 2.3 |
| GW-17 | 34 | "But beliefs are a martingale" | Proof of Corollary 2.1 |

## Results in Lean (`lean/Lit/GibbonsWaldman1998.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Thresholds η′, η″, p.8 | `GibbonsWaldman1998.prefer_higher_iff`, `GibbonsWaldman1998.prefer_lower_iff` | the higher job is preferred exactly from its threshold up |
| Proposition 1 | `GibbonsWaldman1998.prop1_job1`, `GibbonsWaldman1998.prop1_job2`, `GibbonsWaldman1998.prop1_job3` | the wage max_j(d_j + c_jη) equals the output of the job Proposition 1 assigns |
| No demotions, rising wages, p.10 | `GibbonsWaldman1998.wage_strictMono`, `GibbonsWaldman1998.full_info_wage_rises`, `GibbonsWaldman1998.jobOf_mono`, `GibbonsWaldman1998.demotion_implies_wage_decrease` | the wage is strictly increasing in η; job level is monotone in η; a demotion forces a wage decrease |
| Serial correlation and promotion, pp.11–12 | `GibbonsWaldman1998.serial_correlation_full_info`, `GibbonsWaldman1998.high_ability_promoted_no_later` | θ_H workers get larger within-job raises and are never at a lower job than θ_L workers of the same experience |
| Raise at promotion, pp.12–13 | `GibbonsWaldman1998.promotion_raise_decomposition`, `GibbonsWaldman1998.promotion_raise_exceeds_within_job` | the raise is c₁Δη plus a non-negative reassignment gain |
| (A1), (A2), learning | `GibbonsWaldman1998.likelihoodRatio_A2`, `GibbonsWaldman1998.likelihoodRatio_strictAnti`, `GibbonsWaldman1998.likelihoodRatio_affine`, `GibbonsWaldman1998.posterior_strictAnti_ratio`, `GibbonsWaldman1998.posterior_strictMono_prior`, `GibbonsWaldman1998.posterior_strictMono_signal` | the two lines of (A2) agree; the ratio falls in z; the posterior rises in z and in the prior |
| Beliefs move arbitrarily far, p.32 | `GibbonsWaldman1998.belief_tends_to_high`, `GibbonsWaldman1998.belief_tends_to_low` | the posterior tends to 1 as z → ∞ and to 0 as z → −∞ |
| Martingale, p.34 | `GibbonsWaldman1998.beliefs_martingale` | with finitely many signal values, the expected posterior equals the prior |
| (A4), Corollary 2.4 | `GibbonsWaldman1998.promotion_probability_A4`, `GibbonsWaldman1998.promotion_probability_increasing` | the clearing probability is linear in p with positive slope |
| Corollary 2.3 | `GibbonsWaldman1998.ratio_increasing_of_concave`, `GibbonsWaldman1998.wage_decrease_threshold_persists`, `GibbonsWaldman1998.no_wage_decrease_before_threshold`, `GibbonsWaldman1998.wage_decrease_possible` | f(x)/f(x+1) rises under concavity; the threshold x* persists; no wage decrease before x*; a decrease is possible after |
| Tie rule | `GibbonsWaldman1998.tie_rule_reading` | at the threshold the two jobs give equal output |
| Controls | `GibbonsWaldman1998.control_prefer_needs_order`, `GibbonsWaldman1998.control_wage_mono_needs_positive_slopes`, `GibbonsWaldman1998.control_posterior_needs_interior_prior`, `GibbonsWaldman1998.control_ratio_needs_concavity` | each conclusion fails when its hypothesis is dropped |

Not formalised: Corollaries 2.1 and 2.2, the stochastic-dominance part of
Corollary 2.4, and every BGH statistic.

## Readings recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| GW-D1 | 8, 10, 31 | The assignment at a threshold | p.8 uses strict inequalities and leaves ties unassigned; Proposition 1 (p.10) and footnote 3 put ties in the higher job; the proof on p.31 puts them in the lower job ("job 1 if η ≤ η′"). At a threshold the two jobs give equal output (`tie_rule_reading`), so wages agree. We use Proposition 1 |
| GW-D2 | 8, 29 | `f'' ≤ 0` on p.8, "concave (f''<0)" on p.29 | We use weak concavity, which is all `ratio_increasing_of_concave` needs |
