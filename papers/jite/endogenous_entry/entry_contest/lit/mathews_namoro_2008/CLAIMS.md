# Claims about Mathews and Namoro (2008)

**Source.** Timothy Mathews and Soiliou Namoro, "Participation incentives
in rank order tournaments with endogenous entry", working paper of 29 March
2005, 31 pages, uploaded by the author on Drive; read in full [F]. The published version is *Journal of
Economics* 95(1), 1-23 (2008); it is not checked, and the pagination used here
is the working paper's printed pages. Cited in `skeleton_who_pays.tex` (row L23) and
in `refs.bib` as `MathewsNamoro2008`. Quotations are checked verbatim, on the
page named, against the extracted text by `verify_mathews_namoro.py`. The
results we use are proved in `lean/mathlib/MathewsNamoro.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MN-1 | 1 | "Of particular interest is the possibility that a field of higher quality may be attracted to an event with smaller prizes." | Their question: which of two events a stronger entrant joins |
| MN-2 | 4 | "Consider a series of two tournaments, denoted Event 1 and Event 2, each of which has a field limited to two entrants." | The field size is fixed at two; there is no count of entrants to determine |
| MN-3 | 4 | "Assume each agent wishes to participate in one and only one of these two tournaments." | Entry is a choice between two events, not between entering and staying out |
| MN-4 | 5 | "It is assumed that the entry decision of the agents is sequential, with the agent of higher ability making his choice first." | Sequential entry, the higher ability first |
| MN-5 | 5 | "an agent i will only wish to enter an event if his expected payoff from doing so exceeds a potentially positive reservation wage" | The only place a cost of taking part appears (footnote 5); it is not modelled |
| MN-6 | 7 | "These restrictions on the base probabilities will be referred to as the intuitive restrictions." | omega > lambda > 1/2 and omega > rho > 1/2 |
| MN-7 | 12 | "Allocations a, b, or c may possibly be realized, but allocation d will never be realized." | Observation 1 |
| MN-8 | 13 | "Event 2 is able to attract a field of higher quality by offering smaller prizes." | The counterintuitive allocation c |
| MN-9 | 14 | "Thus, Event 2 attracts a stronger field by offering smaller prizes." | The numerical example, 30,000 against 25,000 and 24,000 |
| MN-10 | 16 | "For this condition to hold for all z ∈ [0, 1], Condition 2 must be satisfied; for this condition to hold for any z ∈ [0, 1], Condition 3 must be satisfied." | When c can never arise |
| MN-11 | 16 | "the possibility of allocation c ever arising can more easily be ruled out when performance is less dependent upon effort" | The role of z |
| MN-12 | 17 | "each of these three outcomes is possible" | The three cases for the sign of eta |
| MN-13 | 19 | "a monopolist event promoter wishing to realize a separating allocation while minimizing the total payments across the two events may need to choose prizes so that allocation c results." | Appendix B: the cheapest separating plan can be a c plan |
| MN-14 | 20 | "first order stochastic dominance is not sufficient to rule out the possibility of allocation c arising" | Section 6.1 |
| MN-15 | 22 | "As a result, when performance is drawn from power functions, allocation c can never arise." | Section 6.2 |
| MN-16 | 23 | "However, it is shown that a field of higher quality may be attracted to an event offering smaller prizes." | Their own summary |

## Results in Lean (`lean/mathlib/MathewsNamoro.lean`, namespace `MathewsNamoro2008`)

| Result | Lean | What is proved |
|---|---|---|
| The tournament (Section 2, Appendix A) | `topPay_eq`, `botPay_eq`, `csf_self`, `csf_homogeneous`, `csf_z_zero`, `csf_swap`, `payB_eq_payA_swap`, `foc_iff`, `single_payoff_A`, `single_payoff_B` | with the ratio form of the success probability and c(e) = e, the interior first-order conditions hold iff both efforts equal z p (1-alpha) delta (1-delta), and the equilibrium payoffs are p top(delta) and p bot(delta) |
| Equilibrium, not only first-order | `csf_tangent`, `payA_best_reply`, `payB_best_reply`, `single_nash`, `payA_z_zero` | for 0 < z <= 1 the equal-effort pair is a Nash equilibrium: no effort, zero included, pays more; at z = 0 the best reply is zero effort |
| Payoff bounds | `topPay_pos`, `botPay_pos`, `topPay_strictMono` | q^2 <= top(q) and (1-q)^2 <= bot(q); top is strictly increasing in q for 0 <= z <= 1 |
| Condition 1 (p.9) | `cond1_factor`, `cond1_z_one`, `cond1_zero_at_half`, `cond1_pos`, `topPay_sub_botPay`, `cond1_eq_A_B`, `cond1_control` | the margin factors as (rho + lambda - 1)(1 + z(rho - lambda)), equals rho^2 - (1 - lambda)^2 at z = 1, is positive on the intuitive restrictions; the control is a case with lambda = 0.9 and rho = 0.05 where it is negative |
| The printed value of Condition 1 at z = 1 | `cond1_printed_wrong` | the expression printed on p.9 is not the value (MN-D1) |
| The four allocations (p.10-12) | `outcome_b_iff`, `outcome_a_iff`, `outcome_c_iff`, `outcome_ne_d` | a iff tau >= max(Psi, Theta); b iff tau < Psi; c iff Psi <= tau < Theta; d never |
| The cutoffs (footnotes 9, 10) | `psi_lt_theta_iff_eta_neg`, `eta_eq`, `eta_mul`, `eta_zero`, `eta_one`, `etaB_eq`, `eta_factor`, `etaPrime_eq`, `etaPrime_z_zero`, `etaPrime_z_one`, `etaC_z_one`, `etaPrime_dz` | Psi(1) = Theta(1) = 1; Psi > 1 and decreasing and convex, and Theta > 1 and decreasing and convex for omega > lambda (footnotes 9 and 10); eta is the printed quadratic alpha^2 eta_a - alpha eta_b + eta_c; eta'(1) in closed form and its slope in z, rho(1-rho) - omega(1-omega) |
| Section 5: the prize cut that attracts the stronger field | `lower_ratio_attracts`, `c_arises_iff` | if c arises at tau then b arises at some 1 < tau' < tau; c arises at some tau iff Psi(alpha) < Theta(alpha) |
| Section 5.1, Conditions 2 and 3 | `condition2_iff`, `condition3_iff`, `condition2_restated`, `condition3_restated`, `condition2_imp_condition3`, `c_never_iff`, `eta_nonneg_iff`, `eta_nonneg_iff_etaPrime` | c never arises iff eta'(1) <= 0; for every z this is lambda >= (omega^2 - rho^2 + 1)/2 (Condition 2), for some z lambda >= (omega - rho + 1)/2 (Condition 3), and Condition 2 implies Condition 3 |
| Section 5.2, the three outcomes | `outcome_i`, `outcome_ii`, `outcome_iii`, `outcomes_exhaust`, `quad_cutoff`, `etaC_at_lam0_nonneg`, `etaC_mono`, `etaPrime_pos_of_etaC_neg` | the three cases exhaust; in (ii) there is a unique alpha-hat with c possible exactly for alpha <= alpha-hat; in (iii) c is possible for every alpha |
| The numerical examples (pp.12, 14, 16) | `example_Psi`, `example_Theta`, `example_regime`, `example_b`, `example_c`, `example_i`, `example_ii`, `example_iii` | Psi = 223001/182201 and Theta = 320000/223001; 30,000 against 25,000 gives b and against 24,000 gives c; the three parameter sets of p.16 give outcomes (i), (ii), (iii) |
| Section 6.1, first-order dominance | `fosd_order`, `fosd_endpoints`, `fosd_omega`, `fosd_lambda`, `fosd_rho`, `fosd_example_c`, `integral_density` | the printed F_H <= F_L <= F_N are distribution functions ordered pointwise; the probabilities computed from them as integrals are omega = 1 and lambda = rho = 251/400, and at alpha = 1/2, z = 1, tau = 5/4 the allocation is c |
| Section 6.2, power functions | `prob_power`, `lamPow_eq`, `rhoPow_eq`, `omPow_eq`, `power_condition2_iff`, `power_poly_nonneg`, `power_example`, `power_functions_no_c` | for F = x^H, x^L, x^N with 0 < N < L < H the probabilities are H/(L+H), L/(N+L), H/(N+H); Condition 2 reduces to a polynomial that is non-negative; c never arises |
| Appendix B, the organiser | `example_payoffs`, `b_cost_lower`, `b_plan`, `b_min_cost`, `c_cost_lower`, `sqrt_90440_bounds`, `c_plan`, `c_cost_approx`, `c_min_cost`, `c_cheaper_than_b` | in the example the cheapest plan realising b costs 1076/279, about 3.8566, the cheapest realising c costs (2 sqrt(90440) + 618)/319, about 3.8228, and c is cheaper |

Not formalised: the printed piecewise linear programmes of Appendix B (only the
two minima and their comparison are proved); the claims that the power
functions are ordered by the monotone hazard-rate and likelihood-ratio
conditions and that the first-order example is not (the orders are not
defined in the file); Table 1's payoffs beyond those the backward induction of
Section 2 uses; and the printed footnotes 13 and 14 of Appendix B (further
requirements on the plans), which are not encoded beyond the conditions the
file lists in its Section 7 header.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| MN-D1 | 9 | "Note that λ(1 − λ) − [1 − ρ2 ] is increasing in both ρ and λ" | At z = 1 the margin (rho + lambda - 1) + [lambda(1-lambda) - rho(1-rho)] is rho^2 - (1-lambda)^2 (`cond1_z_one`). The printed form lambda(1-lambda) - [1 - rho^2] differs from it by 1 - lambda: at rho = lambda = 1/2 it is -1/2, where the text says 0 and the true value is 0 (`cond1_zero_at_half`, `cond1_printed_wrong`). Page 11 uses rho^2 > (1-lambda)^2, which is the corrected form. The conclusion that Condition 1 always holds is right (`cond1_pos`) |
| MN-D2 | 11 | "[pρ + αp(1 − ρ)] − [p(1 − λ)αpλ] = p(1 − α) [λ + ρ − 1] > 0." | A plus sign is missing in the second bracket: L's expected prize against H in Event 1 is p(1-lambda) + alpha p lambda, and the difference is then p(1-alpha)(lambda + rho - 1), as printed on the right |
| MN-D3 | 11 | "Thus, for any 0 < α < 0, the pooling allocation" | The range is 0 < alpha < 1. Nothing else on the page uses the printed range |
