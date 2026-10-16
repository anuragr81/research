# Pesendorfer (1995), claims

Evidence tag [F]: the text of every page (PDF pages 1 to 22) was read on 16
October 2026, with the equations read from page images. Pages are PDF pages.
PDF page `n` is journal page `770 + n`. The cached text sets the two columns
side by side, so each quotation is a run that is contiguous in it, and some
stop at the end of a column line (PF-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PF-Q1 | 1 | "A model of fashion cycles is developed in which designs are used as a signaling device in a “dating game.”" | A costly visible good signals an unseen type in social matching |
| PF-Q2 | 2 | "draw inferences about her type. Thus, if the price of the dress is high enough, it will" | Observers infer the wearer's type from the dress, and the price does the screening. The sentence goes on to PF-Q3 |
| PF-Q3 | 2 | "allow the high types to separate at least" | The continuation of PF-Q2, "partially from the low types". Price screening of an unseen type, formalised statically as `Pesendorfer1995.separating_price_iff` |
| PF-Q4 | 2 | "will insist on dating another “in” person." | The subject, "An “in” person", ends the previous column line. Users of the design choose partners among users |
| PF-Q5 | 2 | "The dress therefore acts as a screening" | The sentence ends "device of potential partners" on the next column line |
| PF-Q6 | 5 | "prefer to be matched with high types" | The subject, "Both types", ends the previous column line. Preferences over partners are vertical, unlike A1's squared gap (`Pesendorfer1995.control_quadratic_low_prefers_low`) |
| PF-Q7 | 5 | "types value a match with high types more" | "high types value a match with high types more than do low types", assumption (1). The value of a pool depends on the chooser's own type (`Pesendorfer1995.single_crossing`) |
| PF-Q8 | 5 | "that the two inputs are complements." | Footnote 6. Assumption (1) is grounded in complementary human capital |
| PF-Q9 | 6 | "The matching interpretation implies that u(i, p) is lin" | Footnote 8, "linear in p", the word broken at the line. Random matching within a pool makes the payoff linear in the pool's share of high types |
| PF-Q10 | 7 | "“in” group will always imply a decrease in the average" | Footnote 9. With a continuum of types a larger in-group lowers the average quality of both groups. The paper does not derive a penalty on a group's spread |
| PF-Q11 | 8 | "histories of individuals are unobservable," | Nothing about a person is seen but the design she uses |
| PF-Q12 | 14 | "meet high types, and low types are always" | Proposition 3. Under competition only high types buy, and matching is fully separated, "matched with low types" |
| PF-Q13 | 15 | "to demand otherwise useless designs is that" | "A necessary condition for consumers to demand otherwise useless designs is that individuals do not know each other's characteristics". Screening has no value when types are seen |
| PF-Q14 | 15 | "her wealth and couples simply combine their" | Complementarity "is less justified if the type of a consumer refers to her wealth and couples simply combine their wealth" |
| PF-Q15 | 16 | "are mostly low types will be less attractive to" | "A designer whose clients are mostly low types will be less attractive to prospective buyers". A seller's value is the average type of its clientele |
| PF-Q16 | 16 | "legislation can be interpreted as a way of" | "In the context of my model, sumptuary legislation can be interpreted as a way of avoiding wasteful fashion cycles". A formal rule by class replaces the purchased signal |
| PF-Q17 | 16 | "well-defined class structure, one would" | "Thus in societies with a well-defined class structure, one would expect sumptuary laws, while in a society for which it is impossible for the lawmaker to identify the efficient matches, one would not expect these laws" |
| PF-Q18 | 15 | "tween the likelihood of meeting an individ" | "This suggests a connection between the likelihood of meeting an individual whose type is unknown and the importance of fashion in a community". Signals matter more where strangers meet |

## Lean results

`lean/mathlib/Pesendorfer1995.lean`, in the paper's notation. Definitions (not
audited by name) are `f` (eq. 4), `meetHigh` (matching technology (i)), `eq5`,
`discounted` (eqs. 6 and 7) and A1's `U`.

| Theorem | What it checks |
|---|---|
| `Pesendorfer1995.in_pool_high` | Matching technology (i). An all-high pool gives a high partner with probability 1 |
| `Pesendorfer1995.out_pool_high` | Outside `[0, q]` the chance is `(α − q)/(1 − q)`, computed from the actual non-users |
| `Pesendorfer1995.in_pool_low` | Inside `[0, q]`, `q > α`, the chance is `α/q` |
| `Pesendorfer1995.eq2` | Eq. (2). A high type pays at most `v^h (1 − α)/(1 − q)` |
| `Pesendorfer1995.eq3` | Eq. (3). A low type pays at most `v^ℓ α/q` |
| `Pesendorfer1995.bandwagon` | Eq. (4) rises on `[0, α]` |
| `Pesendorfer1995.snob` | Eq. (4) falls on `(α, 1]` |
| `Pesendorfer1995.peak` | `f(α) = v^h`, and under complementarity no `q` gives more |
| `Pesendorfer1995.f_one` | `f(1) = α v^ℓ > 0` |
| `Pesendorfer1995.control_no_bandwagon_without_gain` | Control. With `v^h = 0` the demand is flat on `[0, α]` |
| `Pesendorfer1995.single_crossing` | A pool with more high types is worth more to a high type, by complementarity |
| `Pesendorfer1995.separating_price_iff` | A price that keeps high types in and low types out exists if and only if `v^ℓ < v^h` |
| `Pesendorfer1995.control_no_separation_without_complementarity` | Control. Without (1) no price separates |
| `Pesendorfer1995.pool_value_mono` | Eq. (5) rises in the chance of a high partner for every type that prefers one, so avoidance runs one way |
| `Pesendorfer1995.prop1_static_step` | Proposition 1 (i). `(q − q′) f(q)` is maximised at `α` or `1` |
| `Pesendorfer1995.prop1_ii` | Proposition 1 (ii). The lower bound `log(1 − c/α f(α))/log δ` on cycle length |
| `Pesendorfer1995.prop1_iii_step` | Proposition 1 (iii). The high-type value exceeds any bound for large `v^h` |
| `Pesendorfer1995.example1_last` | Example 1. `(1 − δ) f(1/2) = 6` |
| `Pesendorfer1995.example1_second_by_eq9` | Example 1. Eq. (9) at the printed numbers does not give the printed `10.54` |
| `Pesendorfer1995.example2_second` | Example 2. `(1 − δ) f(1/2) + δ · 1 = 4.9` |
| `Pesendorfer1995.example2_last_by_eq9` | Example 2. Eq. (9) at `q = 1` gives `1/2`, not the printed 1 |
| `Pesendorfer1995.prop3_price` | Proposition 3 (ii). The price `v^ℓ (1 − δ^n)` equals a low type's value of the remaining use |
| `Pesendorfer1995.prop3_T_exists` | Proposition 3. A cycle length covering the cost exists when `c < α v^ℓ/N` |
| `Pesendorfer1995.control_no_T_when_costly` | Control. With `c ≥ α v^ℓ/N` none exists |
| `Pesendorfer1995.price_floor` | PDF p. 15. Under competition the price is at least `2c/α` |
| `Pesendorfer1995.control_price_floor_one_designer` | Control. With one designer the floor is only `c/α` |
| `Pesendorfer1995.fn24` | Footnote 24. With enough designers `(v^h − v^ℓ)(1 − α) > v^ℓ/N` |
| `Pesendorfer1995.fn26` | Footnote 26. At `δ = 0` the prices are `v^h` and `v^ℓ` |
| `Pesendorfer1995.observed_type_no_value` | Our reading of PF-Q13. With types observed the design is worth nothing |
| `Pesendorfer1995.control_unobserved_positive` | Control. With types unobserved it is worth `v^h (1 − α)/(1 − q) > 0` |
| `Pesendorfer1995.quadratic_complementarity` | A1's `v − κ(c_i − c_j)²` on two classes satisfies (1) |
| `Pesendorfer1995.control_quadratic_low_prefers_low` | Control. Under A1's utility the low class does not prefer a high partner, against PF-Q6 |
| `Pesendorfer1995.quadratic_avoidance_both_ways` | Under A1's utility the high class seeks high pools and the low class avoids them |
| `Pesendorfer1995.two_type_gap` | With two types, mean gap plus variance equals `1 − π` or `π`, so spread is fixed by mean |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Assumption (1), complementarity | `Pesendorfer1995.single_crossing`, `Pesendorfer1995.separating_price_iff` | general | |
| Matching technology (i) | `Pesendorfer1995.in_pool_high`, `Pesendorfer1995.out_pool_high`, `Pesendorfer1995.in_pool_low` | general | |
| Matching technology (ii) and (iii) | none | not formalised | Conventions for empty pools, used by no formalised result |
| Eq. (2) | `Pesendorfer1995.eq2` | general | |
| Eq. (3) | `Pesendorfer1995.eq3` | general | |
| Eq. (4) and its shape (PDF p. 7) | `Pesendorfer1995.bandwagon`, `Pesendorfer1995.snob`, `Pesendorfer1995.peak`, `Pesendorfer1995.f_one` | general | |
| Footnote 9, the continuum example | none | not formalised | The continuum case is described in words, and its demand is not given |
| Eq. (5) | `Pesendorfer1995.pool_value_mono` | general | |
| Eqs. (6) and (7) | none (definition `discounted`) | general | Definitions only. No result of the paper is stated about them alone |
| Eqs. (8) to (10) | none | not formalised | The designer's dynamic programme and possibly mixed pricing |
| Theorem 1 | none | not formalised | Rests on Gul, Sonnenschein and Wilson (1986) and a generalised maximum theorem |
| Proposition 1 (i) | `Pesendorfer1995.prop1_static_step` | special case | The static step of the proof. The monotone price path is not formalised |
| Proposition 1 (ii) | `Pesendorfer1995.prop1_ii` | special case | The revenue bound is a hypothesis. The claim of a new design every period for small `c` is not formalised |
| Proposition 1 (iii) | `Pesendorfer1995.prop1_iii_step` | special case | The unbounded-gain step only |
| Proposition 1 (iv) | none | not formalised | Needs the equilibrium path |
| Example 1 | `Pesendorfer1995.example1_last`, `Pesendorfer1995.example1_second_by_eq9` | special case | The last price follows from (9). The earlier prices do not, at the printed `q` (PF-D5) |
| Example 2 | `Pesendorfer1995.example2_second`, `Pesendorfer1995.example2_last_by_eq9` | special case | The second price follows from (9) given the third. The third does not (PF-D5) |
| Proposition 2 | none | not formalised | Limits as `δ → 1` |
| Proposition 3 | `Pesendorfer1995.prop3_price`, `Pesendorfer1995.prop3_T_exists` | special case | Price path and cycle length. Subgame perfection is not formalised |
| Footnote 24 | `Pesendorfer1995.fn24` | general | |
| Price at least `2c/α` (PDF p. 15) | `Pesendorfer1995.price_floor` | general | |
| Footnote 26 | `Pesendorfer1995.fn26` | general | |
| Claims 1 and 2, (A1) to (A4) | none | not formalised | Limits as `δ → 1` in the proof of Proposition 2 |
| The necessary condition of PDF p. 15 (PF-Q13) | `Pesendorfer1995.observed_type_no_value` | special case | Our reading, for a high type when matching can condition on type |

Counts. General 10, special case 7, not formalised 7.

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PF-D1 | Quotations are limited to runs contiguous in the cached text, and where a sentence crosses a column line the rest is given in the last column, not in the quotation | The cached text lays the two columns side by side line by line, so a sentence that crosses a line is interleaved with the other column. No `[...]` is used for a line break |
| PF-D2 | In Proposition 3 the creation period is read as `t = 0`, so a new design sells at `v^ℓ(1 − δ^{T+1})` | The statement's formula `v^ℓ(1 − δ^{T+1−t})`, the text on PDF p. 14, and the proof's condition `(1 − δ^{T+1}) α v^ℓ/N > c` agree on this reading. The proof indexes periods as `kT + t'` from `t' = 1`, which would give `v^ℓ(1 − δ^T)` |
| PF-D3 | The OCR renders `ℓ` as `£`, `¢` or `2` and `δ` as `5` or `6`, and the equations are read from page images | Quotations avoid symbols except `u(i, p)` in PF-Q9, which the OCR renders correctly |
| PF-D4 | Eq. (4) jumps down at `q = α`, from `v^h` to `v^ℓ`, and "reaches a maximum" there is read with that jump | Figure 1 on PDF p. 7 shows the jump, and `peak` checks the maximum |
| PF-D5 | The example prices are not all reproduced from eq. (9) at the printed diffusion | Example 1's last price is `(1 − δ) f(1/2) = 6`, but (9) with equality at `q = 0.4135` and resale at 6 gives `10.515`, against the printed `10.54`, and the first price is further off. Example 2's second price `4.9` follows from (9) with resale at the printed third price 1, but (9) at `q = 1` gives `1/2`. The proof of Proposition 1 allows mixing in the first period after innovation, which the examples do not report. "3/5 of the high types" is the printed `0.298/0.5 = 0.596` rounded |
| PF-D6 | Example 2's third price is printed `p_{3t+1} = 1` and read as `p_{3t+3}` | The first price already carries the index `3t + 1` |
