# Claims about Schroyen & Treich

**Paper.** Fred Schroyen and Nicolas Treich, "The Power of Money: Wealth
Effects in Contests." `refs.bib` key `SchroyenTreich2016` (*Games and Economic
Behavior* 100, 46–68).

**Source read.** Drive file `stroyen_treich_weatlh_effects.pdf` (id
`1-V3T-xuauv6garL3e145yokjxOs0-Aw3`, 48 PDF pages), identified from its title
pages on 2026-10-06. PDF page 1 is the Toulouse School of Economics cover,
"WORKING PAPERS N° 16-699, September 2016", with the title printed as “The
Power of Money: Wealth Effects in Contest” (singular). PDF pages 2–48 are the
journal manuscript, stamped "*Manuscript" by the Elsevier submission system,
titled "The Power of Money: Wealth Effects in Contests", dated April 11, 2016,
with a first footnote thanking "three anonymous reviewers". The manuscript is
therefore a post-review revision submitted to *GEB*, not the typeset published
article. Printed page *n* of the manuscript is PDF page *n*+1. Every page
locator below is a printed page. Evidence level **[F]** (full read, WP
version). The published article has **not** been line-checked (TODO S1 stays
open). Theorem numbers below are the manuscript's.

Notation in the source differs from `LITERATURE.tex` in two symbols. The
win probability is written `Π_i` and the CRRA coefficient `ρ`, where
`LITERATURE.tex` writes `π_i` and `γ`.

## Claims as stated in `LITERATURE.tex` (§ "The closest paper on the affordability channel") and `PROOFS.tex` (Payoffs; "Why the extensive margin is the point")

| ID | Claim | Source locator |
|---|---|---|
| ST-1 | The three models are distinguished by whether rent and effort are commensurable with wealth inside `u`: privilege `U_i = u(w_i - x_i) + pi_i r`; ability `U_i = pi_i u(w_i + r) + (1-pi_i) u(w_i) - c(x_i)`; rent-seeking `U_i = pi_i u(w_i + r - x_i) + (1-pi_i) u(w_i - x_i)`. | eq. (4) p.9; p.12; eq. (5) p.14; restated p.31 (A.3); prose p.8 |
| ST-2 | Theorem 3: in the symmetric privilege contest, the sign of the quadratic form is positive iff `2A(1 - m^2) > P`, where `A = -u''/u'` and `P = -u'''/u''`. | Theorem 3, eq. (A.16), p.35; proof in A.7, pp.43–44 |
| ST-3 | Under quadratic utility the condition reduces to `m < 1`. | text after (A.16), p.35; also p.11 |
| ST-4 | Under CARA the condition reduces to `m < 2^(-1/2) ~ 0.707`. | text after (A.16), p.35; also p.11 |
| ST-5 | Under CRRA with coefficient `gamma` the condition reduces to `gamma*(1/2 - m^2) > 1/2`. | p.36 and p.11 (the source writes `ρ`) |
| ST-6 | The result is **local**: a second-order effect of a *small* MPS evaluated at a symmetric equilibrium, obtained as a quadratic form in the Hessian of the best-response function (their Theorem 2). | Theorem 2, eq. (A.9), p.27; Lemma 1 and footnote 8, p.28 |
| ST-7 | Its sign turns on the CSF decisiveness parameter `m` **and on the third derivative of `u`**. | (A.16) via `P`, p.35 |
| ST-8 | Multiplying (A.16) by `(w-x)` replaces `A` and `P` by relative risk aversion and relative prudence. | p.36 |
| ST-9 | Our model is a *privilege* contest: entry cost paid out of resources, `V` separable and exogenous, so only the marginal-cost channel is live. | comparison against eq. (4) p.9; prose p.8 |
| ST-10 | Making `V` money added to the resource would move the model to their *rent-seeking* contest, where the two wealth effects oppose and under CARA exactly cancel. | p.8; `H_RS`, `H'_RS` p.15; p.19 |
| ST-11 | They list an arbitrary number of players as an extension not undertaken. | conclusion, p.20 |
| ST-12 | **Separation claim.** P9-gen's sign rule needs no third derivative, no prudence condition, no decisiveness parameter and no functional-form restriction, and is global rather than a local second-order expansion. | our own claim, contrasted with ST-2/6/7 |

## Derived statement, not made in our documents

| ID | Statement | Basis |
|---|---|---|
| ST-12b | The separator in ST-12a does not depend on the choice `m = 1/2`. For every `m` with `0 < m < 2^(-1/2)`, CARA (where `P = A`) satisfies (A.16) and log utility (where `P = 2A`) violates it, at the same `A`. Above `2^(-1/2)` both violate it, so the witness `m` does work. | (A.16) p.35 with `P = A` and `P = 2A` |

## What is checkable here

ST-2 through ST-8 and ST-12a are arithmetic and are checked in SymPy in
`verify_st.py`. ST-12a is the falsifiable form of ST-12. It holds if two
utilities with the **same** `A` at a point but different `P` give opposite
signs at the same `m`. If no such pair existed, "no third derivative" would
be an empty distinction.

`SchroyenTreich.lean` (core Lean 4, no Mathlib) formalises ST-1, ST-2, ST-3,
ST-4, ST-5, ST-6, ST-7, ST-8, ST-9, ST-10, ST-12a and ST-12b. Analytic inputs
(derivatives of `u` and of the CSF) enter as named hypotheses, and
`verify_st.py` checks in SymPy that each such hypothesis is the true
derivative (checks ST-AP, ST-2b, ST-2c, ST-2d, ST-10).

ST-11 is a reading of prose and has no check. It is quoted verbatim in
`NOTES.md`.

## Lean theorems by claim

| Claim | Theorems in `SchroyenTreich.lean` |
|---|---|
| ST-1 | `privilege_rent_margin`, `privilege_cost_margin_depends_on_wealth`, `ability_cost_margin`, `ability_rent_margin_depends_on_wealth`, `rentSeeking_rent_margin_depends_on_wealth`, `rentSeeking_cost_margin_depends_on_wealth` |
| ST-2 | `gap_alt`, `thm3_iff_alt`, `thm3U_iff_thm3`, `thm3_proof_chain`, `thm3_proof_sign`, `thm3_from_A2`, `thm3_from_A2_sign`, `thm3_end_to_end`, `wtp_concave_mps_negative`, control `thm3_from_A2_sign_needs_m_pos` |
| ST-3 | `quadratic_P_zero`, `quadratic_reduction` |
| ST-4 | `cara_AP`, `cara_reduction`, `cara_boundary_707`, control `cara_reduction_needs_pos_A` |
| ST-5 | `crra_AP`, `crra_identity`, `crra_reduction` |
| ST-6 | `fn8_hessian_form`, `a9_at_symmetric_privilege`, `lemma1_first_order_cancels`, `lemma1_sign` |
| ST-7 | `m_dependence` |
| ST-8 | `gap_scale`, `relative_measures_same_sign` |
| ST-9 | `privilege_rent_margin`, `privilege_cost_margin_depends_on_wealth` |
| ST-10 | `rentSeeking_rent_margin_depends_on_wealth`, `rentSeeking_cost_margin_depends_on_wealth`, `rentSeeking_cara_identity`, `rentSeeking_cara_no_wealth_effect`, controls `rentSeeking_crra2_no_cancel` and `rentSeeking_cancel_needs_cara` |
| ST-12a | `separator_values`, `separator_AP`, `separator`, `separator_derivs` |
| ST-12b | `separator_interval`, control `separator_fails_above_boundary` |
| Arithmetic support | `two_mul_int`, `three_mul_int`, `four_mul_int` (numeral elimination inside the `poly` tactic, used by ST-2, ST-4, ST-5, ST-6, ST-8, ST-10, ST-12b), `pos_mul_iff`, `neg_mul_iff`, `sq_pos_of_ne`, `sq_nonneg_int` (sign of a product, used by ST-2, ST-3, ST-4, ST-6, ST-8, ST-12b), `sq_lt_sq_iff` (used by ST-3) |
