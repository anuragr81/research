# Utility inventory, pass 1, 17 October 2026

A planning note, not part of the paper. The author asked what the basis of
the engagement utility is, how utility varies in the literature, and how
the choice affects A1's conclusions, in several passes. Pass 1 is this
inventory of the objective of the agent who decides about another person,
in each paper read. Every entry rests on a quotation ID of the paper's
`CLAIMS.md`, a verbatim run of its cached text, or a Lean name that exists.
The reports were assembled by three readers working from the records and
checked against them.

## 1. The inventory

| Paper | Objective of the deciding agent | What it depends on | Own type of the decider | Penalty on dispersion | Stated basis |
|---|---|---|---|---|---|
| Phelps (1972) | Expected profit, modelled as the least-squares predictor of qualification (eq. 5′, `Phelps1972.pred`) | Posterior mean of the hidden type | No | None. Variances set the weights (PH-Q4) | "maximize expected profit", no derivation |
| Aigner and Cain (1977) | `U(q) = a − b e^{−cq}`, so `E(q|y) − k Var(q|y)` (eqs. 7, 8, `AignerCain1977.eq8_order`) | Posterior mean and variance | No | Yes, a risk premium (AC-Q6) | Cited to Parkin, chosen because it depends on the variance |
| Rothschild and White (1993) | Benefit rising in the pool's mean ability and falling in its variance (RW-Q7, `collegeBenefit`) | Pool mean and variance, seen | No | Yes, supposed as a teaching cost (RW-Q18) | A production cost, not a preference |
| Pęski and Szentes (2012) | Money, hire iff the shock covers the change in the own future value (`hire_iff`) | The worker's seen type through the own label | Yes, own colour (PS-Q8) | None | Built to exclude taste and inference (PS-Q2) |
| Fryer and Jackson (2008) | `EU(a, o) = U(a, θ̄(f(o)))`, categories minimise within variation (eqs. 3 to 5, `eu_iff_var`) | The category mean of seen attributes | No (`meanDev_eq`) | Within-category variation, a filing loss | Accuracy of prediction |
| Currarini, Jackson and Pin (2008) | `U(s, d)` over counts of same and different friends (eq. 2) | Counts by seen type | No, U is not indexed by type (CJP-Q6) | None | Same and different only, citing Marsden (1988) |
| Mailath and Postlewaite (2006) | Concave `U` of joint income plus the children's average utility | The partner's seen income, vertical (MP-Q5) | Own income, through the concavity of the sum | None | Joint consumption |
| Cole, Mailath and Postlewaite (1991) | `u(c) + j`, linear in the partner's endowment | The partner's seen endowment, vertical (CM-Q2) | Own capital, additively | None (`spread_blind`) | "to avoid distributional issues" (CM-Q15) |
| Pesendorfer (1995) | `ν u(i, h) + (1 − ν) u(i, ℓ)` less the price (eq. 5) | The pool's share of high types | Yes, a complementarity (PF-Q7, `single_crossing`) | None | Complementary inputs (PF-Q8), linear for simplicity (PF-Q9) |
| Corneo and Jeanne (1994) | `u(c) + E(a(r) | δ)` (eqs. 1, 2) | The public's belief about rank, vertical | Reader no (CJ-Q3), signaller through `u` | None | Interpretive |
| Moav and Neeman (2012) | `(y − x)^{1−λ} ỹ(h, x)^λ` (eq. 6) | The social belief about income | Reader no, signaller yes | None | Tractability, p. 10 |
| Charles, Hurst and Roussanov (2007) | `u(y − c) + α u(c) + s(c, μ_G)` (eq. 2) | Inferred income relative to the group mean | Reader no, signaller yes | Named and left out (CH-Q4) | Duesenberry and Ireland |
| Austen-Smith and Fryer (2003) | Group: `b` from a reliable member, `−Bκ` otherwise (p. 11). Firm: posterior mean of output (eq. 8) | Posterior on the person's type | No, the group has no type | None | Reliability under repeated interaction (AS-Q3) |
| Eeckhout (2006) | Prisoner's dilemma payoffs, eq. (1) | The partner's action | Same or different colour, with no payoff weight | None | Trust in repeated interaction |
| Eguia (2013) | `ψ(e, s) + v(s, e_J, s_J) − a d c(s)` (eq. 1) | Group means, vertical (EG-Q6) | Yes, an assumed complementarity (EG-Q8) | Named and left out (EG-Q11) | Interaction within shared norms (EG-Q17) |
| Craig and Fryer (2017) | `κ χ_q − (1 − κ) χ_u` for the firm, likewise for the worker | Posterior that the other invested | No (CF-Q10, `match_gain_indep_own`) | None | Nests Coate and Loury |
| Kim and Loury (2009, 2012, 2014) | `X_q P(qualified|θ) − X_u P(unqualified|θ)`, or a wage `ω` times the posterior | Posterior from signal and reputation | No, identical employers (KLA-Q3) | None | Coate and Loury |
| Bohren, Imas and Rosenberg (2019) | `−(v − (q − c_ig))²` (eq. 1) | Posterior mean of quality, possibly misspecified | Taste and belief parameters, not own group (BIR-Q16) | None, the variance is a constant | Not given |
| Bohren, Hull and Imas (2024) | A reduced-form action rule | Signal and group | No | None | A choice not to model the problem |
| Laouénan and Rathelot (2022) | Guests' demand `Q^β/(P^κ Γ^m)`, hosts maximise profit | Posterior mean of quality | Guest no | None | "a simple conceptual framework" |
| Hitsch, Hortaçsu and Ariely (2006) | `x_w′β + (|x_w − x_m|₊)^α γ⁺ + (|x_w − x_m|₋)^α γ⁻ + Σ ϑ^{kl} + ε` with `α = 2` (eq. 5, HHA-Q11) | Seen attributes, vertically and by distance | Yes, signed squared distances and own-by-mate pairs | None | Identification, the distance terms enter squared |
| McPherson et al. (2001), Chetty et al. (2022b) | No utility. A homophily measure, and friending bias as one minus a share ratio | Sameness, or the high-SES share | By conditioning | None | Measurement |
| **Akerlof (1997)** | `Σ_j e/((f + d₀)(g + |x − x₀ⱼ|)) − a x² + b x + c` (eq. 6). Also the quadratic `−d(x − x̄)²` of eq. (5) as "a special case" (AK-Q9) | The distance between known positions | **Yes, own position, as a distance** (AK-Q16) | None. The benefit is convex in distance, so dispersion can help (`contrast_spread_helps_distant`) | Gravity models of trade, "common practice" (AK-Q11) |
| **A1** | `v − κ(c − y)²` against `ū`, with `κ = a²β²/(8(a + h))` (A14, M19) | Posterior on group, through the standing and dispersion of each group | **Yes, own class, as a distance** (M18) | **Yes, the dispersion enters every observer's expected distance** | **Derived from separate budgets and one shared activity (M19)** |

## 2. What the inventory shows

1. **Three kinds of objective.** Most papers value the other on a vertical
   scale that every decider shares, a posterior mean of productivity,
   quality, income or rank. Two value the other through a complementarity
   with the own type, Pesendorfer and Eguia. Two value the other by a
   distance from the own position, Akerlof and A1, and Hitsch, Hortaçsu
   and Ariely estimate distance terms alongside a vertical one.
2. **The dispersion.** Only Aigner and Cain and Rothschild and White penalise a
   dispersion, the first as a risk premium and the second as a teaching cost.
   Charles, Hurst and Roussanov and Eguia name the dispersion and leave it out.
   In A1 the dispersion enters through the quadratic loss, and under Akerlof's
   convex benefit it would enter with the opposite sign for distant
   observers.
3. **The basis.** Most forms are chosen for tractability or by citation.
   Akerlof takes his from trade models by convention. A1 is the only one
   whose distance form is derived from something below it, the budgets.
   Hitsch, Hortaçsu and Ariely's squared distances are the nearest
   empirical support for the quadratic, and their own-income terms are small
   (HHA-Q3, L12).

## 3. Passes still to do

- **Pass 2, Akerlof.** Done on 17 October 2026 (`lit/akerlof_1997/`, D11).
- **Pass 3, robustness in Lean.** Done on 17 October 2026 (D12). Row M21,
  `Robustness.lean`, under A16, carries headline K9 and conclusion C25. The
  plan as first written follows. Which statements of M18 hold for any
  benefit that falls in distance. Pass 2 already proved the two ends
  (`Akerlof1997.contrast_shift_up_above`, `contrast_shift_up_below`) and the
  failure in between (`control_benefit_range_differs`). The pass would move
  these into a model row, so that a headline can rest on them, and add the
  general statement for the dispersion, that its sign follows the curvature of
  the benefit in distance.
- **Pass 4, the subsection.** Done on 18 October 2026 (D13). Literature,
  subsection The engagement utility, written from this table and the rows,
  with new rows L35 to L39. Restructured on 19 October 2026 (D16) into
  three parts, with the two distances and Table tab:levels.
