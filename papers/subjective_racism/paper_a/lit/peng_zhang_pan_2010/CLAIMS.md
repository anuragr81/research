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
| `PengEtAl2010.eq1` | Eq. (1). Jeffrey's rule gives an event `Z` the probability `Σ_y P(Z, y)/P(y) R(y)` |
| `PengEtAl2010.jeffrey_marginal` | Eq. (2) gives `Q(Y) = R(Y)` where `P(y) ≠ 0` |
| `PengEtAl2010.kl_term_ge` | The pointwise bound behind Gibbs' inequality, `a − b ≤ a log(a/b)` |
| `PengEtAl2010.le_cellMass` | A state's probability is at most its cell's |
| `PengEtAl2010.theorem1` | Theorem 1 with Definitions 1 and 3. Jeffrey's rule is the I-projection on `P_{R(Y)}`, for a strictly positive prior |
| `PengEtAl2010.control_theorem1_dominance` | Control. Without dominance a distribution in `P_{R(Y)}` has smaller finite I-divergence |
| `PengEtAl2010.theorem2` | Theorem 2. Jeffrey's rule is at total variation `Σ_y \|P(y) − R(y)\|` from `P`, and no distribution with marginal `R(Y)` is closer |
| `PengEtAl2010.control_dominance_needed` | Control. Where `P(y) = 0 < R(y)`, Jeffrey's rule cannot give `y` its new probability |
| `PengEtAl2010.step_factor` | One IPFP step on `Y^j ⊆ Y` rescales `Q` by a function of `y` |
| `PengEtAl2010.theorem4_finite` | Theorem 4 for every finite iterate. `Q_k(x) = Q_0(x) K(y)`, eq. (8) |
| `PengEtAl2010.theorem4_rigid` | Theorem 4, rigidity form. `Q_k(x) Q_0(y) = Q_0(x) Q_k(y)` |
| `PengEtAl2010.control_theorem4_factor` | Control. A constraint outside `Y` breaks the rigidity |
| `PengEtAl2010.t5_two_steps` | Theorem 5 counterexample. IPFP reaches the uniform `Q*` in two steps |
| `PengEtAl2010.t5_fixed_rows` | `Q*` is fixed by the row constraint |
| `PengEtAl2010.t5_fixed_cols` | `Q*` is fixed by the column constraint |
| `PengEtAl2010.t5_ipfp_limit` | IPFP from `Q_0` stays at `Q*` in any later order, so `Q*` is its converging distribution |
| `PengEtAl2010.theorem5_fails` | Theorem 5 as printed fails. `Q̃ ∈ S` has `δ(Q_0, Q̃) = 4/5 < 28/25 = δ(Q_0, Q*)` |
| `PengEtAl2010.eq9_soft_eq_virtual` | Eq. (9). Bayes with `L(y) = R(y)/P(y)` equals Jeffrey's rule with `R(Y)` |
| `PengEtAl2010.virtual_eq_jeffrey` | The converse. Bayes with any `L` equals Jeffrey's rule with `R(y) = L(y)P(y)/Σ L P` |
| `PengEtAl2010.virtual_soft_sums_to_one` | That `R` is a distribution on `Y` |
| `PengEtAl2010.virtual_scale` | The update with `aL`, `a ≠ 0`, is the update with `L` |
| `PengEtAl2010.virtual_commute` | Two virtual evidences commute, Table 1 rows 4 and 5 |
| `PengEtAl2010.hard_evidence` | PZ-Q10. Hard evidence is the soft evidence with mass one on `y₀`, and Jeffrey's rule then conditions |
| `PengEtAl2010.hard_evidence_virtual` | Hard evidence as a virtual likelihood, the indicator of `y₀`, conditions on `y₀` |
| `PengEtAl2010.hard_evidence_forgets_prior` | Hard evidence forgets the prior's weights across instantiations |
| `PengEtAl2010.control_soft_keeps_prior` | Control. A non-degenerate likelihood keeps the prior's weights |
| `PengEtAl2010.eq13_1` | Eq. (13-1). The projection on the smoothed constraint is a mixture |
| `PengEtAl2010.eqA5` | (A-5). The step to the mixture is `α` times the step to the projection |
| `PengEtAl2010.theorem7_step` | (A-4) with (A-5). One SMOOTH step contracts by `α` |
| `PengEtAl2010.eq14_initial` | Eq. (14) at `k = 0` is `1/(1 + exp(−A))` |
| `PengEtAl2010.control_eq14_printed` | Control. The printed `α₀ = 1/(1 + exp A)` is below `1/2` for `A > 0` (PZ-D10) |
| `PengEtAl2010.table4_inconsistent` | Table 4 at `ε = 3/20` (CONS1) is inconsistent |
| `PengEtAl2010.table4_consistent` | Table 4 at `ε = 4/20` (CONS0) is consistent |
| `PengEtAl2010.ex1_marginals` | Example 1. `P(B) = (0.44, 0.56)`, `P(C) = (0.45, 0.55)` |
| `PengEtAl2010.ex1_row2` | Table 1, row 2. `Q(C = 1) = 327/770`, printed `0.425` |
| `PengEtAl2010.ex1_row3` | Table 1, row 3. `Q(B = 1) = 5/11`, printed `0.455` |
| `PengEtAl2010.ex1_row6` | Table 1, row 6. `Q(B = 1) = 343049/482870`, printed `0.710`, and `Q(C = 1) = 0.3` |
| `PengEtAl2010.control_two_virtual_not_soft` | Control, Table 1, row 4. The two converted evidences give `B = 1` the belief `245/344`, not `0.7` |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Eq. (1), Jeffrey's rule | `PengEtAl2010.eq1` | general | Finite joint states |
| Eq. (2) | `PengEtAl2010.jeffrey_marginal` | general | Lean's `x/0 = 0` is the "otherwise" branch |
| Definition 1, eq. (3) | `PengEtAl2010.theorem1` | special case | Defined as `kl` without the `+∞` branch, so results using it assume dominance (`control_theorem1_dominance`) |
| Definition 2, eq. (4) | `PengEtAl2010.theorem2` | general | `tv` |
| Definition 3, eq. (5) | `PengEtAl2010.theorem1` | general | `IsIProjection` |
| Eq. (6) and Theorem 1 | `PengEtAl2010.theorem1` | special case | Proved for a strictly positive prior with every instantiation of `Y` possible; the general case needs the `+∞` convention of eq. (3) |
| Theorem 2 | `PengEtAl2010.theorem2` | general | Finite joint states, `P(y) > 0` |
| Definition 4, eq. (7) | `PengEtAl2010.theorem4_finite` | general | `ipfp` over a finite list of constraints |
| Theorem 3 (cited, IPFP converges to the I-projection on `S`) | none | not formalised | Needs Csiszár's I-divergence geometry and a limit; a cited result |
| Theorem 4 | `PengEtAl2010.theorem4_finite`, `PengEtAl2010.theorem4_rigid` | special case | Every finite iterate has the form of eq. (8); the passage to the limit `Q*` is not formalised |
| Theorem 5 | `PengEtAl2010.theorem5_fails` | general | Formalised as a counterexample: the statement is false (PZ-D11) |
| Eq. (9) and the cited Theorem 5 of Chan and Darwiche | `PengEtAl2010.eq9_soft_eq_virtual` | general | |
| Single-evidence equivalence, PZ-Q3 | `PengEtAl2010.eq9_soft_eq_virtual`, `PengEtAl2010.virtual_eq_jeffrey` | general | The converse direction is ours (PZ-D6) |
| Hard evidence is a special case of soft evidence, PZ-Q10 | `PengEtAl2010.hard_evidence` | general | |
| Multiple virtual evidences commute, Sec. 3.3 | `PengEtAl2010.virtual_commute` | general | |
| Multiple soft evidences are not multiple virtual evidences, Example 1 | `PengEtAl2010.control_two_virtual_not_soft` | general | |
| Example 1 and Table 1, rows 1 to 6 | `PengEtAl2010.ex1_marginals`, `PengEtAl2010.ex1_row2`, `PengEtAl2010.ex1_row3`, `PengEtAl2010.control_two_virtual_not_soft`, `PengEtAl2010.ex1_row6` | general | Rows 4 and 5 give `C = 1` as `1199/4300`, printed `0.279`; only the `B` value is a theorem |
| Algorithms BN-IPFP-1 and BN-IPFP-2 | `PengEtAl2010.eq9_soft_eq_virtual` | special case | Each BN-IPFP-1 step is an IPFP step by eq. (9); the junction-tree computation is not formalised |
| Theorem 6 | `PengEtAl2010.eq9_soft_eq_virtual`, `PengEtAl2010.theorem4_finite` | special case | The step identities behind parts (1) and (2); convergence is Theorem 3, not formalised |
| Fig. 2's converged likelihood ratios | none | not formalised | Values of an iteration to convergence |
| Eq. (10), SR-IPFP | `PengEtAl2010.eq13_1` | special case | Defined as `srStep`; its convergence claims are cited and not formalised |
| Eq. (11), the I-aggregate | none | not formalised | Defined as `iAggregate`; GEMA's convergence is cited |
| Eqs. (12), (13), (13-1) | `PengEtAl2010.eq13_1` | general | |
| Theorem 7 | `PengEtAl2010.theorem7_step` | special case | The one-step contraction (A-4) with (A-5); the iteration and limit are not formalised (PZ-D12) |
| The conjecture for `m > 2` | none | not formalised | Stated as a conjecture |
| Eq. (14) | `PengEtAl2010.eq14_initial`, `PengEtAl2010.control_eq14_printed` | general | |
| Table 4 and Experiments 5-1, 5-2 (consistency of CONS0 and CONS1) | `PengEtAl2010.table4_consistent`, `PengEtAl2010.table4_inconsistent` | general | |
| Tables 2, 3, 5 to 10 (timings, I-divergences, iteration counts) | none | not formalised | Experimental output of programs not in the paper |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| PZ-D1 | The definition of inconsistent constraints on PDF p. 14 is read with `S = ∅` | It prints "is said to be inconsistent if S = ∩ P_R(Y^j) ≠ ∅", which is the definition of consistency on p. 5 and contradicts the next sentence |
| PZ-D2 | "0.7.59" on PDF p. 9 is read as `0.7059` | `0.3/0.425 = 0.70588`, computed from the rounded `0.425`. From the exact `327/770` the ratio is `231/327 = 0.7064`; row 6 rounds to `0.710` either way (`ex1_row6`) |
| PZ-D3 | The dominance condition is read as `R(Y) << P(Y)` | Sec. 2.2 has "provided R(Y) << P(Y)", but Sec. 4 and Theorem 6 write `P(Y^j) << R(Y^j)`. Eq. (6) divides by `P(y)` and can only reach `R(y) > 0` where `P(y) > 0` (`control_dominance_needed`) |
| PZ-D4 | Theorem 5's `δ(P, Q*)` is read as `δ(Q_0, Q*)` | The theorem's initial distribution is `Q_0`, and `P` is not defined in it |
| PZ-D5 | The equality of eq. (9)'s virtual evidence with Jeffrey's rule is the paper's citation of Chan and Darwiche (2005), not its own proof | PZ-Q11 cites "Theorem 5 of Ref. 3". Chan and Darwiche is not read. `eq9_soft_eq_virtual` proves the finite case independently |
| PZ-D6 | The converse, that every virtual evidence is a Jeffrey update, is our step, not a statement of the paper | The paper states the equivalence for a single evidence (PZ-Q3) and gives the direction from soft to virtual. `virtual_eq_jeffrey` and `virtual_soft_sums_to_one` give the other direction |
| PZ-D7 | Table 1's printed values are three-decimal roundings | Row 2 is `327/770 = 0.42468` and row 4 is `245/344 = 0.71221` (`ex1_row2`, `control_two_virtual_not_soft`) |
| PZ-D8 | Ref. 12, "The Logic of Decisions", is read as Jeffrey's *The Logic of Decision*, second edition, 1983 | The title has a stray plural; it is the edition read in `jeffrey_1983/` |
| PZ-D9 | Variable `D` of Example 1 is left out of the Lean joint | `D` is a child of `B` and `C` only, so it sums out of every marginal of `B` and `C` that Table 1 reports |
| PZ-D10 | Eq. (14)'s initial value is `α₀ = 1/(1 + exp(−A))` | The paper writes `α₀ = 1/(1 + exp(A))` and sets `A = 4.595` for `α₀ ≈ 0.99`. The printed formula gives `0.01`; eq. (14) at `k = 0` gives `0.99` (`eq14_initial`, `control_eq14_printed`) |
| PZ-D11 | Theorem 5 is false as stated | The paper derives it from Theorems 4 and 2, but Theorem 2 gives minimality only over distributions whose marginal on `Y` is `Q*(Y)`, a subset of `S`. With two binary variables, uniform target marginals and `Q_0 = (.01, .09, .09, .81)`, IPFP converges to the uniform distribution while `(.41, .09, .09, .41) ∈ S` is nearer in total variation (`theorem5_fails`). The paper gives no appendix proof of Theorem 5 |
| PZ-D12 | Theorem 7's proof is read as a contraction by `α` per step | The appendix concludes convergence from a strictly decreasing step length, which alone does not imply convergence. (A-4) with (A-5) gives `δ(Q_1, Q_2) ≤ α δ(Q_0, Q_1)`, a geometric decay that does (`theorem7_step`), if (A-4) holds at every step |
