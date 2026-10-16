# Peng, Zhang and Pan (2010), claims

Evidence tag [F]: the full text of all 26 PDF pages of the published article
was read on 16 October 2026, with PDF pages 3, 4 and 7 also read from 110 dpi
images for eqs. (1), (2), (6) and (9). Pages are PDF pages; PDF page `n` is
journal page `538 + n`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| PZ-Q1 | 1 | "virtual evidence (reflecting the uncertainty one has about a reported observation) and soft evidence (reflecting the uncertainty of an event one observes)" | The paper's two kinds of uncertain evidence, told apart by what the update preserves |
| PZ-Q2 | 2 | "They also showed that a soft evidence can be converted into a virtual evidence, and as the result, belief update with a single soft evidence can be carried out by Pearl's virtual evidence method for both BN and joint distributions." | The paper's account of Chan and Darwiche (2005), its Ref. 3, `eq9_soft_eq_virtual` |
| PZ-Q3 | 2 | "We formally established the equivalence of Jeffrey's rule, I-projection (a central operation of IPFP), and virtual evidence method, when dealing with a single uncertain evidence." | The single-update equivalence that A1's statement rests on. `eq9_soft_eq_virtual`, `virtual_eq_jeffrey` |
| PZ-Q4 | 2 | "We also established that Pearl's virtual evidence method works for multiple virtual evidences but not for multiple soft evidences." | The two rules part only with more than one evidence, `control_two_virtual_not_soft` |
| PZ-Q5 | 3 | "One of the difficulties stems from the fact that the Bayes' rule cannot directly apply here because R(y), although acting as a condition for the update, itself is not an event." | Why a soft evidence needs a rule of its own. Eq. (9) answers it by turning `R(y)` into the likelihood of a virtual event |
| PZ-Q6 | 3 | "keep the conditional distribution of X, given Y ⊆ X , unchanged" | Jeffrey's second principle, the rigidity of `JF-Q4`, `jeffrey_marginal` with eq. (2) |
| PZ-Q7 | 4 | "Note that (6) is exactly the same as (2). This proves the following theorem." | Theorem 1. Jeffrey's rule is the I-projection on the set with marginal `R(Y)` |
| PZ-Q8 | 6 | "The virtual evidence method requires this uncertainty information be given as a likelihood ratio" | A virtual evidence is a likelihood ratio, defined up to scale, `virtual_scale` |
| PZ-Q9 | 7 | "In other words, R ( X i ) is a true (and certain) observation, and this distribution should be preserved in the updated joint distribution" | Soft evidence fixes the marginal, `jeffrey_marginal` |
| PZ-Q10 | 7 | "In fact, a hard evidence, say X i = a , is a special case of soft evidence" | Conditioning is the degenerate soft evidence, as in Jeffrey (JF-Q5) |
| PZ-Q11 | 8 | "when this virtual evidence is applied to P(X), the new distribution is exactly the same as the one obtained by applying R(Y) using the Jeffrey's rule of (2)." | The conversion of eq. (9), cited as Theorem 5 of Ref. 3 and not proved in the paper, `eq9_soft_eq_virtual` (PZ-D5) |
| PZ-Q12 | 8 | "Update cannot be done by first converting se1 and se2 to two virtual evidences and then applying the virtual evidence method with these two virtual evidences." | With two soft evidences the conversion fails, `control_two_virtual_not_soft` |
| PZ-Q13 | 8 | "This problem, known as the commutativity of iterated revisions, has been viewed as a problem for Jeffrey's rule." | The order effect lies in several updates, outside A1's single reading (cf. JF-Q6) |
| PZ-Q14 | 9 | "none of these two soft evidences is satisfied by the resulting distributions." | Table 1, rows 4 and 5, `control_two_virtual_not_soft` |

## Lean results

`lean/mathlib/PengEtAl2010.lean`, in the paper's notation, on a finite set of
joint states.

| Theorem | What it checks |
|---|---|
| `PengEtAl2010.sum_by_cell` | A sum over joint states is a sum over the instantiations `y` of the sums within each |
| `PengEtAl2010.sum_cell` | A function of `y` summed against `P` is summed against `P(y)` |
| `PengEtAl2010.jeffrey_marginal` | Eq. (2) gives `Q(Y) = R(Y)` where `P(y) ≠ 0` |
| `PengEtAl2010.eq9_soft_eq_virtual` | Eq. (9). Bayes with `L(y) = R(y)/P(y)` equals Jeffrey's rule with `R(Y)` |
| `PengEtAl2010.virtual_eq_jeffrey` | The converse. Bayes with any `L` equals Jeffrey's rule with `R(y) = L(y)P(y)/Σ L P` |
| `PengEtAl2010.virtual_soft_sums_to_one` | That `R` is a distribution on `Y` |
| `PengEtAl2010.virtual_scale` | The update with `aL`, `a ≠ 0`, is the update with `L` |
| `PengEtAl2010.virtual_commute` | Two virtual evidences commute, Table 1 rows 4 and 5 |
| `PengEtAl2010.theorem2` | Theorem 2. Jeffrey's rule is at total variation `Σ_y \|P(y) − R(y)\|` from `P`, and no distribution with marginal `R(Y)` is closer |
| `PengEtAl2010.ex1_marginals` | Example 1. `P(B) = (0.44, 0.56)`, `P(C) = (0.45, 0.55)` |
| `PengEtAl2010.ex1_row2` | Table 1, row 2. `Q(C = 1) = 327/770`, printed `0.425` |
| `PengEtAl2010.control_two_virtual_not_soft` | Control, Table 1, row 4. The two converted evidences give `B = 1` the belief `245/344`, not `0.7` |
| `PengEtAl2010.control_dominance_needed` | Control. Where `P(y) = 0 < R(y)`, Jeffrey's rule cannot give `y` its new probability |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PZ-D1 | The definition of inconsistent constraints on PDF p. 14 is read with `S = ∅` | It prints "is said to be inconsistent if S = ∩ P_R(Y^j) ≠ ∅", which is the definition of consistency on p. 5 and contradicts the next sentence |
| PZ-D2 | "0.7.59" on PDF p. 9 is read as `0.7059` | `0.3/0.425 = 0.70588` |
| PZ-D3 | The dominance condition is read as `R(Y) << P(Y)` | Sec. 2.2 has "provided R(Y) << P(Y)", but Sec. 4 and Theorem 6 write `P(Y^j) << R(Y^j)`. Eq. (6) divides by `P(y)` and can only reach `R(y) > 0` where `P(y) > 0` (`control_dominance_needed`) |
| PZ-D4 | Theorem 5's `δ(P, Q*)` is read as `δ(Q_0, Q*)` | The theorem's initial distribution is `Q_0`, and `P` is not defined in it |
| PZ-D5 | The equality of eq. (9)'s virtual evidence with Jeffrey's rule is the paper's citation of Chan and Darwiche (2005), not its own proof | PZ-Q11 cites "Theorem 5 of Ref. 3". Chan and Darwiche is not read. `eq9_soft_eq_virtual` proves the finite case independently |
| PZ-D6 | The converse, that every virtual evidence is a Jeffrey update, is our step, not a statement of the paper | The paper states the equivalence for a single evidence (PZ-Q3) and gives the direction from soft to virtual. `virtual_eq_jeffrey` and `virtual_soft_sums_to_one` give the other direction |
| PZ-D7 | Table 1's printed values are three-decimal roundings | Row 2 is `327/770 = 0.42468` and row 4 is `245/344 = 0.71221` (`ex1_row2`, `control_two_virtual_not_soft`) |
| PZ-D8 | Ref. 12, "The Logic of Decisions", is read as Jeffrey's *The Logic of Decision*, second edition, 1983 | The title has a stray plural; it is the edition read in `jeffrey_1983/` |
| PZ-D9 | Variable `D` of Example 1 is left out of the Lean joint | `D` is a child of `B` and `C` only, so it sums out of every marginal of `B` and `C` that Table 1 reports |
