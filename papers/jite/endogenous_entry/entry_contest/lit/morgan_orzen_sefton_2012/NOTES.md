# Notes — Morgan, Orzen & Sefton (2012)

Running `python3 lit/morgan_orzen_sefton_2012/verify_mos.py` from the paper
root reports **12 checks, 0 failures** (six SymPy checks MOS-1 to MOS-6 from
pass 1, six Lean checks MOS-L from pass 2).

**Scope.** Our reading is arithmetically consistent with the source. One
survey figure was under-qualified (now fixed in `LITERATURE.tex`), one
substantive asymmetry was omitted, and pass 2 adds two locator corrections,
one dropped-hypothesis finding and an assessment of "private random delays".
Nothing here reproves any result of the paper, and **none of the experimental
statistics were re-analysed**.

---

# Pass 1 (SymPy)

## The theory reads cleanly and is fully reproducible

MOS-1 derives both `x_n* = (n−1)P/n²` and `π_n* = w + P/n²` from the stated
payoff `π_i = w + x_i P/X − x_i` rather than taking them on trust, and MOS-2
gets `√(P/F)` from *"the largest integer such that `P/n*² > F`"*.

**MOS-3 is the strongest check here.** All twelve entries of their Table 2
(p.445) investment column reproduce from `(n−1)P/n²`:

| | n=1 | n=2 | n=3 | n=4 | n=5 | n=6 |
|---|---|---|---|---|---|---|
| `P=50` | 0.00 | 12.50 | 11.11 | 9.38 | 8.00 | 6.94 |
| `P=200` | 0.00 | 50.00 | 44.44 | 37.50 | 32.00 | 27.78 |

matching the printed table throughout, and MOS-4 gives `⌊√(50/10)⌋ = 2`,
`⌊√(200/10)⌋ = 4` as printed. Recovering a published design table from the
model is much better evidence the section was read correctly than restating it.
(The values above are the derived ones to two decimals. Table 2 prints one
decimal, for instance 11.1 and 9.4.)

---

## Imprecision: "observed was 2.5 and 3.7"

The survey reported these without qualification. They are the **second-half**
figures (rounds 26–50). The full picture, from the text of p.452:

| treatment | rounds 1–25 | rounds 26–50 | prediction |
|---|---|---|---|
| small prize | 2.7 | **2.5** | 2 |
| large prize | 3.6 | **3.7** | 4 |

The numbers quoted are right; they are post-learning averages, and should be
labelled as such if cited.

**Status 2026-10-06.** `LITERATURE.tex` lines 477–479 now label 2.5 and 3.7 as
rounds 26–50 and give 2.7 and 3.6 for rounds 1–25, so the survey carries the
qualification. The means are printed in the text of p.452 and not in Table 5,
which gives the distribution of the number of entrants (finding F2).

---

## Omission: the two treatments deviate in OPPOSITE directions

This is the more substantive point, and the survey's phrasing hid it.

- Small prize: **2.5 against a prediction of 2** — *excess* entry, significant
  in both halves (p = 0.004, 0.012).
- Large prize: **3.7 against a prediction of 4** — a *shortfall*, differing from
  Nash only at the 6% and 12.5% levels.

MOS-5 records the sign difference. The paper makes a great deal of it, because
both deviations run *against* what within-contest behaviour predicts: with
negative returns to two-person competition one would expect *fewer* than two
entrants, and with under-investment one would expect *more* than four. Their
words: *"we observe precisely the opposite"* — in both cases.

Anyone citing this as "observed entry exceeds prediction" would be wrong for
half the experiment. If the survey's sentence is carried into `PROOFS.tex` or
the welfare section, it needs the directions attached.

---

## MOS-G confirmed verbatim

The risk-attitude rejection is exactly as described (§5.2.1, p.455):

> "in the small prize three-player contest subjects over-invest and earn less
> than the outside option, whereas subjects in the large prize three-player
> contest under-invest and earn more. A two-sided two-sample randomization test
> applied at the group level shows the differences in earnings to be
> significant at the 1% level."

The paper then applies the same controlled comparison to a "contest premium"
explanation, which also fails it. Worth knowing that two behavioural
explanations, not one, are ruled out this way.

---

## Third confirmation of the identity problem — and P5's place

This is the third independent source for the identity problem, after
Levin–Smith (fn. 2, mixed entry) and Fu–Lu (fn. 6, sequential entry). MOS use
private random delays (Proposition 1), so that arrival order breaks the tie.

**All three devices are properties of the protocol, not of the agents.** P5 is
the fourth and the only one where entrant identity is a property of the agents
themselves. MOS-6 makes the contrast arithmetic: with `N = 6` and `n* ∈ {2,4}`
there are `C(6,2) = 15` and `C(6,4) = 15` distinct equilibrium entrant sets,
against exactly **one** under P5 given the wealth profile.

**Status 2026-10-06 (finding F11).** The sentence "against exactly one under
P5" is stale. `LITERATURE.tex` lines 370–380 now say heterogeneous costs do not
by themselves pin which agents enter, with `Δ = (12,10,1)` and `κ = (2,5,8)`
giving three equilibrium entrant sets. The MOS side of the contrast (15 sets
per treatment) stands and is proved in MOS-L6. The P5 side is not checked in
this directory, and the MOS-6 detail line in `verify_mos.py` no longer asserts
it.

---

## A point worth adding to the positioning

**The experiment deliberately switches off the variable our model is about.**
Table 2 (p.445) holds `w = 100` for every subject by design, so there is no
wealth heterogeneity and hence no `κ(w)` channel. The survey notes the design
fact (MOS-D) but does not draw the conclusion: the one experimental paper in
this literature cannot speak to P5's mechanism, in either direction. That is
worth a sentence, because it forecloses "has this been tested?" cleanly rather
than leaving it open.

---

## Pass 1 results

| ID | Result |
|---|---|
| MOS-1 | CONSISTENT. `x_n*` and `π_n*` both derived from the payoff function. |
| MOS-2 | CONSISTENT. The threshold solves to `√(P/F)`. |
| MOS-3 | CONSISTENT. All twelve Table 2 entries reproduce. |
| MOS-4 | CONSISTENT. `n* = 2` and `4` as printed. |
| MOS-5 | CONSISTENT, and it corrects an omission — the gaps are `+0.5` and `−0.3`. |
| MOS-6 | CONSISTENT. 15 equilibrium entrant sets per treatment. |

---

# Pass 2 (Lean), 2026-10-06

## Source

Google Drive file `endogenous_entry_contests.pdf`, id
`1ULmE3gykmeN7eYNdNa6V9FQm-Zc43-xP`, in folder
`1r7J3xJ2rotpmlRaBANCNhviy2d8a1bx_`. The title page reads "Econ Theory (2012)
51:435–463, DOI 10.1007/s00199-010-0544-z, SYMPOSIUM, Endogenous entry in
contests, John Morgan · Henrik Orzen · Martin Sefton", open access. This is
the published version. All 29 pages (pp.435–463) were read in full from the
text layer, lines 1 to 4187 of the extraction.

Two limits of the text layer matter here. The display that defines `n*` on
p.441 lost its radical and floor glyphs (the layer reads "n∗ = P F
participants will opt into the contest (where.denotes the integer floor
function)"), so `⌊√(P/F)⌋` is fixed by the floor-function gloss, by Q6 below,
and by Table 2's entrant column, all three of which the Lean file reproduces.
Table 1 is broken into single tokens and was used only for its caption.

## Verbatim quotes used below

| Q | Page, locator | Quote |
|---|---|---|
| Q1 | p.441, §3 | "All decisions are publicly observable at the instant they are made. Once an individual has made the decision to opt-in or out, he or she cannot subsequently reverse that decision." |
| Q2 | p.441, §3 | "The probability that player i wins the contest is given by the contest success function x_i/X." |
| Q3 | p.441, §3 | "the expected earnings of a contestant who invests x_i when total investment by all contestants is X is given by π_i = w + x_i P/X − x_i." |
| Q4 | p.441, §3.1 | "Suppose that n agents have entered the contest. The unique symmetric Nash equilibrium entails investments equal to x*_n = (n − 1)P/n² yielding an equilibrium expected payoff of π*_n = w + P/n² to everyone who opted into the contest. Clearly, payoffs decrease with the number of individuals entering the contest." |
| Q5 | p.441, §3.1 | "We assume that P > F > P/N², so that there is an incentive for at least one player to enter the contest, but the outside option is more attractive than a contest involving all players. We also focus on generic parameter values where √P/F is not integer valued." |
| Q6 | p.441, §3.1 | "This implies that in any pure strategy equilibrium, the equilibrium number of participants, n*, occurs when one additional entrant would reduce the expected payoff from the contest below that of the outside option. That is, in equilibrium, n* is the largest integer such that P/n*² > F." |
| Q7 | p.441, §3.1 | "This suggests that n* = ⌊√(P/F)⌋ participants will opt into the contest (where ⌊.⌋ denotes the integer floor function) and subsequently play the unique symmetric equilibrium of the rent-seeking game." (display glyphs restored, see Source) |
| Q8 | p.441, §3.1 | "Of course, since all of the players are identical in the model, the identity of the players choosing to opt into the contest is not uniquely determined. We can, however, determine the timing of entry decisions. As Proposition 1 below shows, all entry occurs at time close to t = 0." |
| Q9 | pp.441–442, §3.1 | "Assume that when the clock starts, each player is subject to a private random delay time, d_i, after which entry is possible. The realization d_i is drawn from a CDF F having support on [0, ε] with positive density everywhere, where ε < 1. The introduction of this delay parameter serves to avoid the consideration of events where individuals make choices at exactly the same time." |
| Q10 | p.442, §3.1 | "Without loss of generality, order the indices of the contestants such that d_1 < d_2 < ··· < d_N." |
| Q11 | p.442, Proposition 1 | "Given the continuation payoffs from above, in the unique perfect Bayesian equilibrium (PBE), contestants i = 1, 2,..., n* enter the contest at time d_i. All others remain out of the contest." |
| Q12 | p.442, proof of Prop 1, property 1 | "In any PBE, players only enter if the current number of entrants is n* −1 or fewer. Since the continuation payoffs from additional entry when there are already n* or more entrants are lower than the outside option, staying out of the contest is more profitable." |
| Q13 | p.442, proof of Prop 1, property 2 | "In any PBE, exactly n* players enter. If fewer than n* players enter, a contestant scheduled not to enter can profitably deviate by entering." |
| Q14 | p.442, proof of Prop 1, property 3 | "contestant i can profitably deviate by entering at time d_i and, since j is informed about this entry, he will optimally choose not to enter." |
| Q15 | p.445, Table 2 | Caption "Experimental design and equilibrium benchmarks". Rows "Small prize 100 6 10 50 0.0 12.5 11.1 9.4 8.0 6.9 2" and "Large prize 100 6 10 200 0.0 50.0 44.4 37.5 32.0 27.8 4". Columns Endowment (w), Players (N), Outside pay (F), Prize (P), Individual investment for 1 to 6 entrants, Entrants (n*). Note "a Conditional on number of entrants (x*_n)". |
| Q16 | p.456, §5.2.2 | "Second, we assume an exogenous random sequential ordering of entry decisions. Our theory section outlined a model where such an assumption is appropriate to describe equilibrium behavior." |
| Q17 | p.442, §3.1 | "Somewhat related is Fullerton and McAfee (1999), who study contests where agents are heterogeneous and entry is determined by an auction." |
| Q18 | p.442, §3.1 | "Essentially, the model is a slightly modified version of that analyzed in Corcoran (1984) as well as Corcoran and Karels (1985)." |
| Q19 | p.439, Table 1 | Caption "Summary of previous contest experiments". |
| Q20 | p.452, Table 5 | Caption "Distribution of number of entrants", note "The italicized entries correspond to the relevant equilibrium prediction". The italic entries sit at 2 entrants for the small prize (34.2, 52.4) and at 4 entrants for the large prize (35.6, 29.3). |
| Q21 | p.445, §4 | "In both treatments, if only one person entered a contest, that person received the prize and no contest was conducted." |
| Q22 | pp.444–445, §4 | "During this time they could see how many members of their group had chosen A, how many had chosen B, and how many had not yet chosen." |

## What the Lean file proves and what it takes as given

`MorganOrzenSefton.lean` is core Lean 4 (no Mathlib), `namespace
MorganOrzenSefton`, 47 theorems, no `sorry`, no user `axiom`, no
`native_decide`, no comments. Rationals are avoided by clearing denominators,
so each contest-stage statement is the paper's equation multiplied through by
a positive denominator. The clearing is an equivalence only when the
denominator is positive, which for the contest stage means `n ≥ 2` and
`x > 0`. At `n = 1` the contest success function `x_i/X` of Q2 is undefined
(`X = 0`), and the cleared payoff identity reduces to `0 = 0`. The paper's
experiment settles the one-entrant case by rule (Q21).

The file takes four things as given without proof. They are the necessity of
the first-order condition at an interior optimum, the cap `x_i ≤ w`, the
uniqueness of the perfect Bayesian equilibrium in Proposition 1 (property 3
and the argument against a common extra delay `τ`), and the reduction of the
paper's continuous-time entry game to the one-shot entry stage `PureEq`
(finding F10).

## Theorem table

| Theorem | Claim | Page, locator | Verbatim quote |
|---|---|---|---|
| `foc_symmetric` | MOS-L1 | p.441, §3.1 | Q4 "The unique symmetric Nash equilibrium entails investments equal to x*_n = (n − 1)P/n²" |
| `foc_symmetric_unique` | MOS-L1 | p.441, §3.1 | Q4 "The unique symmetric Nash equilibrium" |
| `best_response` | MOS-L1 | p.441, §3 | Q3 "π_i = w + x_i P/X − x_i" |
| `symmetric_best_response` | MOS-L1 | p.441, §3.1 | Q4 "entails investments equal to x*_n = (n − 1)P/n²" |
| `equilibrium_payoff` | MOS-L2 | p.441, §3.1 | Q4 "yielding an equilibrium expected payoff of π*_n = w + P/n²" |
| `sq_mono` | MOS-L3 (helper) | p.441, §3.1 | Q4 "Clearly, payoffs decrease with the number of individuals entering the contest." |
| `largest_iff_floor` | MOS-L3 | p.441, §3.1 | Q6 "n* is the largest integer such that P/n*² > F" with Q5 "√P/F is not integer valued" |
| `floor_unique` | MOS-L3 | p.441, §3.1 | Q7 "(where ⌊.⌋ denotes the integer floor function)" |
| `rootSearch_spec` | MOS-L3 (helper) | p.441, §3.1 | Q7 "the integer floor function" |
| `floorRoot_spec` | MOS-L3 (helper) | p.441, §3.1 | Q7 "the integer floor function" |
| `floor_iff_root` | MOS-L3 | p.441, §3.1 | Q7 "This suggests that n* = ⌊√(P/F)⌋ participants will opt into the contest" |
| `boundary_exists` | MOS-L3 (helper) | p.441, §3.1 | Q5 "We assume that P > F > P/N²" |
| `count_exists` | MOS-L3 | p.441, §3.1 | Q5 "there is an incentive for at least one player to enter the contest, but the outside option is more attractive than a contest involving all players" |
| `generic_of_bracket` | MOS-L3 (helper for MOS-L4) | p.441, §3.1 | Q5 "generic parameter values where √P/F is not integer valued" |
| `design_hypotheses` | MOS-L4 | Table 2, p.445; p.441 | Q15 "Small prize 100 6 10 50" and "Large prize 100 6 10 200", Q5 "P > F > P/N²" |
| `design_small` | MOS-L4 | Table 2, p.445 | Q15 "Small prize 100 6 10 50 ... 2" |
| `design_large` | MOS-L4 | Table 2, p.445 | Q15 "Large prize 100 6 10 200 ... 4" |
| `design_small_count` | MOS-L4 | Table 2, p.445; p.441 | Q6 "the largest integer such that P/n*² > F", Q15 entrant column "2" |
| `design_large_count` | MOS-L4 | Table 2, p.445; p.441 | Q6 "the largest integer such that P/n*² > F", Q15 entrant column "4" |
| `table2_investment` | MOS-L4 | Table 2, p.445 | Q15 "0.0 12.5 11.1 9.4 8.0 6.9" and "0.0 50.0 44.4 37.5 32.0 27.8" |
| `count_true_add_false` | MOS-L5 (helper) | none | List bookkeeping, no source sentence |
| `mem_true_iff` | MOS-L5 (helper) | none | List bookkeeping, no source sentence |
| `mem_false_iff` | MOS-L5 (helper) | none | List bookkeeping, no source sentence |
| `pure_eq_of_floor` | MOS-L5 | p.441, §3.1 | Q6 "one additional entrant would reduce the expected payoff from the contest below that of the outside option" |
| `pure_eq_iff_floor` | MOS-L5 | p.441, §3.1 | Q6 "in any pure strategy equilibrium, the equilibrium number of participants, n*" |
| `count_pinned` | MOS-L5 | p.441, §3.1 | Q6 "in any pure strategy equilibrium, the equilibrium number of participants, n*" |
| `design_pure_eq_iff` | MOS-L5 | p.441; Table 2, p.445 | Q15 "Large prize ... 4" |
| `design_small_pure_eq_iff` | MOS-L5 | p.441; Table 2, p.445 | Q15 "Small prize ... 2" |
| `entrants_block` | MOS-L6 (helper) | none | List bookkeeping, no source sentence |
| `identity_unpinned` | MOS-L6 | p.441, §3.1 | Q8 "the identity of the players choosing to opt into the contest is not uniquely determined" |
| `mem_profiles` | MOS-L6 (helper) | none | Enumeration completeness, no source sentence |
| `design_eq_sets` | MOS-L6 | p.441; Table 2, p.445 (`N = 6`) | Q8 "since all of the players are identical in the model" |
| `design_roles` | MOS-L6 (helper) | p.441 | Q8 "not uniquely determined" |
| `design_identity_unpinned` | MOS-L6 | p.441; Table 2, p.445 | Q8 "the identity of the players choosing to opt into the contest is not uniquely determined" |
| `seqEntry_take` | MOS-L7 | Proposition 1, p.442 | Q11 "contestants i = 1, 2,..., n* enter the contest at time d_i. All others remain out of the contest." |
| `prop1_count` | MOS-L7 | p.442, property 2 | Q13 "In any PBE, exactly n* players enter." |
| `prop1_step_out` | MOS-L7 | p.442, property 1 | Q12 "the continuation payoffs from additional entry when there are already n* or more entrants are lower than the outside option" |
| `prop1_step_in` | MOS-L7 | p.442, property 2 | Q13 "If fewer than n* players enter, a contestant scheduled not to enter can profitably deviate by entering." |
| `prop1_rule` | MOS-L7 | p.442, property 1 | Q12 "players only enter if the current number of entrants is n* −1 or fewer" |
| `prop1_first_enters` | MOS-L7 | p.442, Proposition 1 | Q10 "order the indices of the contestants such that d_1 < d_2 < ··· < d_N" |
| `prop1_last_out` | MOS-L7 | p.442, Proposition 1 | Q11 "All others remain out of the contest." |
| `design_prop1_orders` | MOS-L7 | p.442; Table 2, p.445 | Q11 "contestants i = 1, 2,..., n* enter the contest at time d_i" |
| `control_floor_needs_generic` | MOS-L8 (control for MOS-L3) | p.441 | Q5 "We also focus on generic parameter values where √P/F is not integer valued." |
| `control_count_needs_generic` | MOS-L8 (control for MOS-L5) | p.441 | Q5, as above |
| `control_count_needs_bound` | MOS-L8 (control for MOS-L3, MOS-L5) | p.441 | Q5 "the outside option is more attractive than a contest involving all players" |
| `control_best_response_needs_foc` | MOS-L8 (control for MOS-L1) | p.441 | Q4 "entails investments equal to x*_n = (n − 1)P/n²" |
| `control_rounding` | MOS-L8 (control for MOS-L4) | Table 2, p.445 | Q15 "11.1 9.4" and "44.4" |

## Statement notes, one per claim

- **MOS-L1.** `foc_symmetric` states `P·((n−1)x) = (nx)²`, which is the
  first-order condition `P(X − x_i)/X² = 1` of Q3 at `X = nx`. `best_response`
  states `(s−Y)(P−s)X ≤ (X−Y)(P−X)s` given `X² = PY`. With `s = z + Y` this
  is the payoff comparison `zP/s − z ≤ x(P/X) − x` multiplied by `sX > 0`.
  The proof uses the identity that the gap equals `X(s−X)²` once `X² = PY`
  holds. `symmetric_best_response` instantiates `Y = (n−1)x` and `X = nx`.
- **MOS-L2.** `equilibrium_payoff` states `n²(xP − x·nx) = P·nx`, which is
  `(xP − xX)/X = P/n²` multiplied by `n²X`.
- **MOS-L3.** `StaysIn P F m` is `F m² < P`, which is `P/m² > F` multiplied by
  `m²`. `IsLargestCount` transcribes Q6. `IsFloorSqrt P F n` is
  `F n² ≤ P < F (n+1)²`, which says `n = ⌊√(P/F)⌋`. `Generic P F` is
  `F k² ≠ P` for every `k`, which says `√(P/F)` is not an integer. `floorRoot`
  is a structurally recursive floor square root, used instead of core
  `Nat.sqrt` because the core lemmas `Nat.sqrt_le` and `Nat.lt_succ_sqrt`
  depend on `Classical.choice`.
- **MOS-L4.** `design_large` is `floorRoot (200 / 10) = 4` over `Nat`, and
  `design_small` is `floorRoot (50 / 10) = 2`. Both divisions are exact.
  `table2_investment` checks `|20(n−1)P − 2tn²| ≤ n²` for each printed value
  `t/10`, which is "`(n−1)P/n²` lies within 0.05 of the printed value".
- **MOS-L5 and MOS-L6.** A profile is a `List Bool` (true for opt in). `PureEq`
  requires that an entrant does not gain by leaving (`P/m² ≥ F`) and that an
  outsider does not gain by entering (`P/(m+1)² ≤ F`), the two halves of Q6.
  `design_eq_sets` shows the 64 enumerated profiles are distinct, and
  `mem_profiles` shows every six-player profile is among them, so "15 of 64"
  is an exact count.
- **MOS-L7.** `seqEntry n 0 σ` lets the players in the delay order `σ` (Q10)
  arrive one by one, and a player enters while fewer than `n` have entered.
  This encodes the strategy of Q11 and Q12 and is a definition, not a derived
  equilibrium. `prop1_rule` shows the threshold `k < n*` is the same as
  `P/(k+1)² > F` under genericity, which ties the encoded strategy to the
  payoffs of properties 1 and 2.

## Controls (non-vacuity, `lit/README.md` rule 3)

Each control instantiates the world in which a hypothesis fails and shows the
conclusion fails with it.

| Control | Hypothesis removed | Parameters | What fails |
|---|---|---|---|
| `control_floor_needs_generic` | genericity (Q5) | `P = 40, F = 10`, `√(P/F) = 2` | the largest integer with `P/n² > F` is 1 while `⌊√(P/F)⌋` is 2, so `largest_iff_floor` fails |
| `control_count_needs_generic` | genericity | `P = 40, F = 10, N = 6` | profiles with 1 entrant and with 2 entrants are both pure equilibria, so `count_pinned` fails |
| `control_count_needs_bound` | `F > P/N²` (Q5) | `P = 200, F = 10, N = 3` | all three players entering is a pure equilibrium with 3 entrants, while `⌊√20⌋ = 4`, so `pure_eq_iff_floor` fails |
| `control_best_response_needs_foc` | `X² = PY` | `P = 4, Y = 1, X = 3, s = 2` | the cleared payoff inequality of `best_response` is false |
| `control_rounding` | printed value correct | 9.3 for 9.375, 44.5 for 44.44 | the rounding test of `table2_investment` rejects both |

`verify_mos.py` adds two controls on itself. A probe theorem proved with
`Classical.em` is audited, and the parser must report `Classical.choice` as
disallowed. Two mutants of the source, with Table 2's 44.4 changed to 44.5 and
with `floorRoot (200 / 10) = 5`, must each be rejected by `lean`. Both controls
pass, so the compile check and the axiom audit can each fail.

## Axiom audit

47 theorems declared, 47 audited, 0 using `sorry`. 7 depend on no axiom, 40
depend only on `propext` or on `propext` and `Quot.sound`, and none depends on
anything else. Compiled with Lean 4.34.1 (the elan default when the suite
ran) and separately with Lean 4.33.1, both with return code 0 and no
warnings.

## Findings

**F1. Design-table locator (corrected in this directory).** Our `CLAIMS.md`
(MOS-D, MOS-E), `RECONSTRUCTION.md` §2, pass-1 notes and `verify_mos.py`
MOS-3 and MOS-4 cited "Table 1" for the design. Table 1 (pp.439–440) is
"Summary of previous contest experiments" (Q19), and the design is Table 2
(p.445, Q15). `LITERATURE.tex` and `PROOFS.tex` cite no table number, so
neither is affected. Falsifier checked and ruled out by Q15, which prints
`w = 100, N = 6, F = 10, P ∈ {50, 200}` and `n* ∈ {2, 4}` under the caption
of Table 2.

**F2. Mean-entry locator (corrected in this directory).** The means 2.7, 2.5,
3.6 and 3.7 are in the text of p.452. Table 5 on p.452 is the distribution of
the number of entrants (Q20), whose italic cells confirm the predictions 2
and 4 independently of Table 2.

**F3. Transcription (corrected).** `verify_mos.py` MOS-3 held 11.11 as the
"printed" small-prize value at `n = 3`. Table 2 prints 11.1. MOS-3 passed
either way at its 0.05 tolerance.

**F4. `LITERATURE.tex` drops two hypotheses of `n* = ⌊√(P/F)⌋`.** Lines
474–476 state the formula "for identical agents with equal endowment w,
outside option F and prize P". The paper derives the formula under
`P > F > P/N²` and for `√(P/F)` not an integer (Q5), and introduces it with
"This suggests" (Q7). The world in which the survey sentence is false is
instantiated in Lean. At `P = 40, F = 10` the paper's own definition (Q6)
gives 1 while the floor gives 2, and two different entrant counts are
equilibria. At `N = 3, P = 200, F = 10` everyone enters, so the count is 3
and not 4. The paper's design satisfies both hypotheses (`design_hypotheses`),
so every number the survey reports is right, and only the general sentence
needs the two conditions.

**F5. Is "private random delays" an accurate description of the mechanism?**
The phrase is the paper's own (Q9), and Proposition 1 does fix who enters
once the delays are drawn. The entrants are the `n*` players with the
shortest delays (Q10, Q11), so the entrant set depends on the delay draw and
not on any trait of the player (MOS-L7, `prop1_first_enters`,
`prop1_last_out`). Three qualifications follow from the text.

1. The paper introduces the delay for timing and to rule out simultaneous
   choices, not to settle identity. Q8 says identity "is not uniquely
   determined" and turns at once to "We can, however, determine the timing of
   entry decisions", and Q9 gives the delay's stated purpose as avoiding
   "events where individuals make choices at exactly the same time".
2. The delays settle identity together with the base model's public and
   irreversible decisions (Q1), not alone. Property 3 of the proof relies on a
   later player being "informed about this entry" (Q14).
3. Before the draw, each player's role is random. The paper's own gloss in
   §5.2.2 is "an exogenous random sequential ordering of entry decisions"
   (Q16).

`LITERATURE.tex` lines 356–359 ("sequential entry with observability, so
arrival order breaks the tie; Morgan et al. add private random delays to make
the order well defined") matches Q1, Q9 and Q11 and is accurate.
`PROOFS.tex` line 575 ("resolved there ... by private random delays") names
the right device but drops the observability that does half the work, and
presents as the paper's resolution a device the paper offers for timing. A
wording the source supports is "by the order of private random delays, with
entry publicly observed". Neither file was edited. The falsifier for "resolved
by delays" would be a Proposition 1 that left the entrant set open given the
delays, and Q11 rules that world out by naming `i = 1, ..., n*`.

Two related observations. The experiment has a public count of choices and a
15-second clock (Q22) but no random delay, so the delay device exists only in
the theory. `PROOFS.tex` lists Fu–Lu under "sequential arrival" and this paper
under "private random delays" as two of three devices, while `LITERATURE.tex`
groups both papers under "sequential entry with observability". On this
paper's side the device is a random arrival order with observed entry, which
belongs to the same family as sequential arrival. Fu–Lu was not re-read in
this pass.

**F6. Novelty lead, unread.** The paper cites Fullerton and McAfee (1999) as
studying "contests where agents are heterogeneous and entry is determined by
an auction" (Q17). Fullerton–McAfee appears nowhere in `LITERATURE.tex`,
`PROOFS.tex` or `refs.bib`. `LITERATURE.tex` lines 363–368 say P5 "differs
from the other three in kind" because the ordering "is a property of the
agents". If the Fullerton–McAfee auction selects entrants by type, that paper
already makes entrant identity a property of the agents, and the claim
narrows to "the only device among the four discussed". Whether it does is not
known until the paper is read. The paper also names Corcoran (1984) and
Corcoran and Karels (1985) as the source of its model (Q18), and neither is
cited in our documents.

**F7. Pass-1 note stale (annotated above).** The "Imprecision" section said
the survey gave 2.5 and 3.7 without qualification. `LITERATURE.tex` lines
477–479 now carry the rounds 26–50 label and the first-half figures.

**F8. One-entrant case.** The contest success function `x_i/X` (Q2) is
undefined at `X = 0`, which is the one-entrant case where `x*_1 = 0`. The
formula `π*_1 = w + P` in Q4 presumes the sole entrant wins, and the
experiment states that rule (Q21). The Lean contest-stage theorems are
meaningful for `n ≥ 2`, and the entry-stage theorems use `π*_m = w + P/m²`
for every `m ≥ 1`, which agrees with Q21 at `m = 1`.

**F9. Notation.** The paper uses `F` both for the outside option (Q5) and for
the CDF of the delay (Q9). The clash is notational only.

**F10. Reduced form is our reading.** Q6 and Q8 are stated for "the model"
before the delay amendment, without naming a game form for the entry
decision. `PureEq` reads Q6 as a one-shot entry stage in which no entrant
gains by leaving and no outsider gains by entering, with continuation payoff
`w + P/m²` from Q4. A different game form (for instance the continuous-time
game of §3 without delays) could have equilibria this stage does not
represent. MOS-L5 and MOS-L6 are statements about the reduced form.

**F11. P5 contrast stale (annotated above).** Pass 1 and `RECONSTRUCTION.md`
§4 say P5 has "exactly one" equilibrium entrant set. `LITERATURE.tex` lines
370–380 now retract that, so the pass-1 sentence and the `RECONSTRUCTION.md`
sentence overstate. The `verify_mos.py` MOS-6 detail line no longer asserts
it. `RECONSTRUCTION.md` §4 was left as written for the author to decide.

## Not formalised, and why

- Uniqueness of the PBE in Proposition 1 (property 3 and the argument against
  a common extra delay `τ`). The argument is about strategies over continuous
  time with private delays, which core Lean without measure theory cannot
  express cleanly. `seqEntry` encodes the prescribed strategy instead.
- Necessity of the first-order condition and uniqueness among asymmetric
  contest profiles. The paper claims uniqueness only among symmetric
  equilibria (Q4). Global optimality of the symmetric point is proved.
- The cap `x_i ≤ w`. Table 2's largest prediction is 50.0 against `w = 100`.
- §3.2 (relative-payoff preferences), which no document of ours attributes.
- MOS-D, MOS-F, MOS-G and the observed means in MOS-E. These are design facts
  and experimental statistics, with no arithmetic content for Lean beyond the
  sign check already in MOS-5.

## Not attempted (unchanged from pass 1)

- **Every regression, randomization test and p-value** — none re-run. All
  statistical claims here rest on the paper's own reporting.
- §2's survey of previous experiments; the investment-dynamics sections; §5.2.1
  beyond the three-player comparison.

Anything leaning on these is **[A]-grade** despite the [F] tag.
