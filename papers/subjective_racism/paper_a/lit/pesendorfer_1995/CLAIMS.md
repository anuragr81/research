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

## Lean results

`lean/mathlib/Pesendorfer1995.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `Pesendorfer1995.eq2` | Eq. (2). A high type pays at most `v^h (1 − α)/(1 − q)` |
| `Pesendorfer1995.eq3` | Eq. (3). A low type pays at most `v^ℓ α/q` |
| `Pesendorfer1995.bandwagon` | Eq. (4) rises on `[0, α]` |
| `Pesendorfer1995.snob` | Eq. (4) falls on `(α, 1]` |
| `Pesendorfer1995.peak` | `f(α) = v^h`, and under complementarity no `q` gives more |
| `Pesendorfer1995.f_one` | `f(1) = α v^ℓ > 0` |
| `Pesendorfer1995.single_crossing` | A pool with more high types is worth more to a high type, by complementarity |
| `Pesendorfer1995.separating_price_iff` | A price that keeps high types in and low types out exists if and only if `v^ℓ < v^h` |
| `Pesendorfer1995.control_no_separation_without_complementarity` | Control. Without (1) no price separates |
| `Pesendorfer1995.control_no_bandwagon_without_gain` | Control. With `v^h = 0` the demand is flat on `[0, α]` |
| `Pesendorfer1995.quadratic_complementarity` | A1's `v − κ(c_i − c_j)²` on two classes satisfies (1) |
| `Pesendorfer1995.control_quadratic_low_prefers_low` | Control. Under A1's utility the low class does not prefer a high partner, against PF-Q6 |
| `Pesendorfer1995.two_type_gap` | With two types, mean gap plus variance equals `1 − π` or `π`, so spread is fixed by mean |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PF-D1 | Quotations are limited to runs contiguous in the cached text, and where a sentence crosses a column line the rest is given in the last column, not in the quotation | The cached text lays the two columns side by side line by line, so a sentence that crosses a line is interleaved with the other column. No `[...]` is used for a line break |
| PF-D2 | In Proposition 3 the creation period is read as `t = 0`, so a new design sells at `v^ℓ(1 − δ^{T+1})` | The statement's formula `v^ℓ(1 − δ^{T+1−t})`, the text on PDF p. 14, and the proof's condition `(1 − δ^{T+1}) α v^ℓ/N > c` agree on this reading. The proof indexes periods as `kT + t'` from `t' = 1`, which would give `v^ℓ(1 − δ^T)` |
| PF-D3 | The OCR renders `ℓ` as `£`, `¢` or `2` and `δ` as `5` or `6`, and the equations are read from page images | Quotations avoid symbols except `u(i, p)` in PF-Q9, which the OCR renders correctly |
| PF-D4 | Eq. (4) jumps down at `q = α`, from `v^h` to `v^ℓ`, and "reaches a maximum" there is read with that jump | Figure 1 on PDF p. 7 shows the jump, and `peak` checks the maximum |
