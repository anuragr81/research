# Claims about Morgan, Tumlinson and Vardy (2018 working paper; 2022 JET)

**Source.** IMF WP/18/231 (2018), read in part [F for Sections I to IV]; the
2022 *JET* version is not checked. Cited in `MANUSCRIPT.tex` (row L20, C12)
and in `refs.bib` as `MorganTumlinsonVardy2022`. Pages are the working
paper's. `lean/mathlib/MeritocracyLogistic.lean` (see `LEAN`) proves an exact
instance of their Proposition 1 in their own model, with controls.

## Quotations

| ID | Page | Quotation | What we rely on | Verified by |
|---|---|---|---|---|
| MTV-1 | 2 | "We show that too much meritocracy, modeled as accuracy of performance ranking in contests, can be a bad thing: in contests with homogeneous agents, it reduces output and is Pareto inefficient." | Their meritocracy is the accuracy with which the costly choice is ranked | quotation |
| MTV-2 | 4 | "at a critical point, competition becomes so intense that contestants start ‘dropping out,’ i.e., they put in zero effort" | Participation falls as ranking becomes more accurate | quotation; Lean `mixed_region`, `participation_strictMonoOn` |
| MTV-3 | 14 | "the intensity of competition is high, and contestants compete away all rents" | Why: effort competes the rents away, so some must drop out | quotation |
| MTV-4 | 15 | "contestants start dropping out with positive probability" | The same direction, at the critical level | quotation |
| MTV-5 | 5 | "once it kicks in, attrition proceeds from the bottom of the ability distribution" | With heterogeneous ability, participation is a threshold in ability; here it is a threshold in wealth (M5) | quotation |
| MTV-6 | 8 | "they also find that the contest organizer may prefer a noisier winner selection mechanism. They relate this to a trade-off between, on the one hand, encouraging entry and, on the other, more competitive bidding for a given number of entrants." | Their report of Fu, Jiao and Lu (2015): with effort among entrants, more noise can encourage entry | quotation (a report of FJL, not checked in FJL) |

## The instance in Lean (`lean/mathlib/MeritocracyLogistic.lean`)

Their system on p.13 (FOC (3), PC binding in the mixed region, MCC) with
logistic noise and their leading cost `c(x) = k·e^{αx}` (p.9) gives, by our
derivation, participation `γ = m/(1 − σα)` in the drop-out region:

| Lean | Statement |
|---|---|
| `hasDerivAt_logistic`, `hazard_eq` | the logistic density is the derivative of the CDF, and its hazard is the CDF itself |
| `mixed_region` | FOC, binding PC and MCC force `F(z) = σα` and `γ = m/(1 − σα)` |
| `mixed_region_exists` | for `σα < 1 − m` the three conditions have an explicit solution, with `γ ∈ (m, 1)` |
| `soc_mixed` | their SOC (4) holds there, since `f'/f = 1 − 2F` (`hasDerivAt_logisticDensity`) |
| `participation_strictMonoOn`, `participation_deriv_pos` | participation strictly rises with the noise `σ` |
| `participation_tendsto`, `participation_at_critical` | it tends to m as `σ → 0` and reaches 1 at `σ = (1 − m)/α`, where dropping out begins |
| `control_constant_hazard_no_solution` | with a constant hazard the system has no solution unless `σα` equals it, so the rising hazard of log-concave noise is load-bearing |
| `control_soc_fails` | the SOC fails once `σα ≥ 1` |

## Discrepancy recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| MTV-D1 | 13 | The SOC in the displayed system reads `(1/σ) f'/f > c''/c'` | Equation (4) on p.11 and the sentence before it give `c''/c' > −(1/σ) f'/f`, which is what the calculus gives; the display drops the minus sign. `soc_mixed` uses (4) |

## The comparison with M28

In their sense, meritocracy is the precision with which the costly choice is
ranked. In the model here the costly choice is the bought component, and its
weight relative to the base score is `t = (1−μ)/μ`, so their "more
meritocracy" is a rise in t, a fall in `μ`. M28 says the count does not fall
as t rises. Proposition 1 says participation falls as their precision rises
past the critical level. The signs are opposite. The difference is the effort
margin: there, accuracy raises effort until rents are competed away and some
must drop out; here entry is a fixed fee with no effort to escalate, and
non-payers still compete, so accuracy only raises the return to paying.

## What is not claimed

- That their model contains M28, or M28 theirs.
- Anything from Section V, the proofs, or the published version.
- The Fu, Jiao and Lu result itself (MTV-6 is MTV's report of it).
