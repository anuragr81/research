# Rothschild and White (1993), claims

Evidence tag [F]. The text layer of the NBER chapter PDF was read in full, PDF
pp. 1 to 33, on 16 October 2026, with the equation pages also read from images
(`RECONSTRUCTION.md`). This is the 1993 conference chapter, not the 1995
*Journal of Political Economy* article on customers as inputs (RW-D1). Pages
are PDF pages. Book page `n` is PDF page `n − 9` (RW-D2).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| RW-Q1 | 4 | "In essence, the incoming students themselves are an important input to (and affect the efficiency of production of) the educational services output of the university, and the university’s admission and pricing policies are likely to reflect this special relationship between input and" | Clients enter as inputs to the venue's output, and admission and price respond to that. The premise of A1's contribution 3, stated for colleges |
| RW-Q2 | 8 | "We can only have a concern for the efficiency of different allocations if students themselves are an important input into the educational process. If students are inputs in this sense, then there are externalities in the higher education industry." | Client composition matters only through peer effects in the output |
| RW-Q3 | 8 | "Allocations differ in efficiency only if there is some synergy." | Formalised as `control_no_synergy` |
| RW-Q4 | 4 | "An efficient university admissions policy would be to encourage a large pool of applicants (e.g., through apparent “underpricing”) and then to accept students selectively and to price selectively (i.e., to practice price discrimination through selective scholarships) so as to achieve the efficiencies that accompany the mix and diversity characteristics." | Pricing on client composition. The venue sets a price below market clearing and then selects, by admission and by personal price |
| RW-Q5 | 9 | "A denial of admission is the same as a price of" | The price is `∞` (read from the image). Exclusion is a price, formalised as `pA_finite_iff` |
| RW-Q6 | 9 | "What is perhaps most strange about the price system equation (1) is that it is not competitive; colleges must collude to implement it." | The personalised price that implements any allocation is not a competitive outcome |
| RW-Q7 | 10 | "college conferred on its students were an increasing function of average ability of the student body and a decreasing function of the variance of ability. An efficient price system would necessarily group students of similar ability together." | A penalty on the variance of a pool's type, supposed rather than derived. Formalised in a special case as `sorted_pairing_efficient` |
| RW-Q8 | 10 | "It is easy to devise a price system that will segregate people of different ability levels." | Price screening on an unseen type by self-selection, by appeal to Spence (1974), not worked out in the chapter |
| RW-Q9 | 10 | "Such markets may lack equilibria-or at least the most obvious kinds of equilibria do not exist." | When colleges compete for students whose mix matters, the structure is that of Rothschild and Stiglitz (1976), with possible non-existence |
| RW-Q10 | 10 | "The prices that companies charge can in some cases depend on the observable characteristics of customers. They cannot depend simply on the identification of customers." | Prices may condition on what is observed about a client, a group marker included |
| RW-Q11 | 11 | "An anonymous price system-that is, one that ignores a person’s observable characteristics-could not accomplish this result." | Formalised in our reading as `anonymous_same_ranking` and `control_personalised_separates` |
| RW-Q12 | 9 | "If, however, students care about both money and about who their classmates are, then stable allocations may not exist." | Footnote 7. With preferences over classmates, stable matchings may fail to exist. A caution for A1's equilibrium with venue choice |
| RW-Q13 | 17 | "If, say, a “desirable” student enhances the educational experience of other students, then those other students should be willing to pay higher tuition to a university that offers this diversity; the externality is internalized." | Clients pay for peer composition through the venue's price |
| RW-Q14 | 10 | "First, people differ in many characteristics, perhaps most importantly in liquidity or wealth." | Wealth enters only as a limit on a price-based allocation of students |
| RW-Q15 | 9 | "The matching literature has generally produced results that state that its most powerful and positive conclusions may not apply when people care about whom they are matched with." | The authors' summary of matching theory under compositional preferences |
| RW-Q16 | 7 | "Prices ration access to many goods in our society. A conspicuous exception is the right to attend the best institutions of higher education. [...] The obvious answer is that it is not fair (in some sense) to let people buy their way into the best universities; access ought to be based on merit." | A norm rules out price as the rationing device for the best venues, and access is rationed by admission instead |
| RW-Q17 | 9 | "However, scholarships determine the net prices that students pay for colleges, and these scholarships depend on a great many personal characteristics." | Net prices are personal and condition on characteristics of the client, the device by which a venue prices on composition |
| RW-Q18 | 9 | "It is hard to teach a class when the students differ greatly in ability and background." | The source's origin of grouping by type is a cost in production, not a preference over partners |

## Lean results

`lean/mathlib/RothschildWhite1993.lean`, in the chapter's notation. The chapter
is verbal except for eq. (1), so the formalisation is of our reading of its
structure.

| Theorem | What it checks |
|---|---|
| `RothschildWhite1993.pA_finite_iff` | Eq. (1). Under `p_A` the only college with a finite price for student `s` is `A(s)` |
| `RothschildWhite1993.anonymous_same_ranking` | PDF p. 11, our reading. Under anonymous prices two students with the same benefits rank colleges alike |
| `RothschildWhite1993.control_personalised_separates` | Control. Personalised prices separate two such students |
| `RothschildWhite1993.pairVar_eq` | The within-college variance of a pair is `(u − v)²/4` |
| `RothschildWhite1993.totalBenefit_eq` | With benefit `mean − k · variance`, `B(A)` is total ability less `k/2` times the sum of squared gaps |
| `RothschildWhite1993.sorted_pairing_efficient` | Section 1.3.2. Grouping similar abilities is efficient when `k ≥ 0` |
| `RothschildWhite1993.sorted_pairing_strict` | Strictly, when `k > 0` and the middle abilities differ |
| `RothschildWhite1993.control_no_synergy` | Control. With `k = 0` every allocation gives the same total |
| `RothschildWhite1993.homogeneous_share_var` | Section 1.3.3. A student in `{x₁, x₂}` bears variance `σ²` |
| `RothschildWhite1993.mixed_share_var` | A student in `{xᵢ, yⱼ}` bears `σ²/2` under independence |
| `RothschildWhite1993.mixed_better` | Mixing lowers the variance of each share when `σ² > 0` |
| `RothschildWhite1993.control_perfect_correlation` | Control. With perfectly correlated inputs mixing gains nothing |
| `RothschildWhite1993.equal_pair_zero_var` | A pair of equal abilities has zero within-college variance |
| `RothschildWhite1993.noCrossSubsidy_iff_no_profitable_entry` | PDF p. 6. No cross-subsidy exactly when no entrant could serve a subset of customers at a profit |
| `RothschildWhite1993.control_singleton_tests_insufficient` | Control, our example. Passing the test against single-product rivals does not imply passing it for every subset (RW-D10) |
| `RothschildWhite1993.all_efficient_without_synergy` | PDF p. 8. Without synergy every allocation is efficient |
| `RothschildWhite1993.choice_monotone` | PDF p. 10. Under strictly increasing differences optimal choices rise with type |
| `RothschildWhite1993.control_choice_not_monotone` | Control. Without increasing differences the choice can fall with type |
| `RothschildWhite1993.tuition_scholarship_neutral` | PDF p. 16. Adding the same amount to tuition and every scholarship changes nothing |
| `RothschildWhite1993.selective_increments_raise_revenue` | PDF p. 16. Selective scholarship increments below the tuition increment raise net revenue |
| `RothschildWhite1993.footnotes_9_10` | Footnotes 9 and 10. 6.6 percent, over 86 percent, and the revenue shares summing to 100 |
| `RothschildWhite1993.table14_numbers` | Table 1.4 and PDF p. 23. The 1,851 institutions, and which series shows over 50 percent growth (RW-D9) |
| `RothschildWhite1993.tuition_salary_slopes` | PDF p. 19. Both slopes are below 0.10 |

## Coverage of the source's results

The chapter is verbal apart from the stand-alone test, `B(A)`, eq. (1) and the
two regressions of PDF p. 19. Every displayed claim it relies on has a row. 22
rows: 10 general, 4 special case, 8 not formalised.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Stand-alone test, PDF p. 6 | `noCrossSubsidy`, `noCrossSubsidy_iff_no_profitable_entry` | general | |
| Survival of joint producers shows no cross-subsidy, PDF p. 6 | `control_singleton_tests_insufficient` | special case | Survival against single-product rivals gives the test on singletons only. The chapter's inference needs rivals for every subset |
| Economies of scope, PDF p. 7 | none | not formalised | Evidence cited, no claim derived |
| `B(A)` and efficiency, PDF p. 8 | `totalSurplus`, `Efficient` | general | |
| No synergy, all allocations efficient, PDF p. 8 | `all_efficient_without_synergy`, `control_no_synergy` | general | |
| Stability under compositional preferences, PDF pp. 8–9 and footnote 7 | none | not formalised | Cited from Roth and from Kelso and Crawford |
| Eq. (1) implements any allocation, PDF p. 9 | `pA`, `pA_finite_iff` | general | |
| Eq. (1) is not competitive, PDF p. 9 | none | not formalised | Verbal, with no model of competition |
| Homogeneity, grouping similar abilities, PDF p. 10 | `totalBenefit_eq`, `sorted_pairing_efficient`, `sorted_pairing_strict` | special case | Four students, two colleges of two, benefit `mean − k · variance`. The general monotone benefit with any number of students and colleges is not formalised |
| Spence, choice rising with type, PDF p. 10 | `StrictIncDiff`, `choice_monotone`, `control_choice_not_monotone` | general | Under strictly increasing differences, the chapter's "mild conditions" |
| A price system that segregates abilities, PDF p. 10 | none | not formalised | The chapter constructs no price system beyond the appeal to Spence |
| Possible non-existence, PDF p. 10 | none | not formalised | Cited from Rothschild and Stiglitz (1976) and Wilson (1977) |
| Diversity, mixing is optimal, PDF p. 11 | `homogeneous_share_var`, `mixed_share_var`, `mixed_better`, `control_perfect_correlation` | special case | Risk aversion is read as caring about the variance of one's share |
| Anonymous prices cannot achieve diversity, PDF p. 11 | `anonymous_same_ranking`, `control_personalised_separates` | special case | A ranking lemma without capacities or the dependence of benefits on the allocation |
| Law of large numbers, PDF p. 11 | none | not formalised | A remark |
| Tuition and scholarship neutrality, PDF p. 16 | `netRevenue`, `tuition_scholarship_neutral` | general | |
| Selective scholarships raise revenue, PDF p. 16 | `selective_increments_raise_revenue` | general | Enrolment held fixed, as in the chapter |
| The externality is internalised in tuition, PDF p. 17 | none | not formalised | Verbal |
| Tuition on expected salary, PDF p. 19 | `tuition_salary_slopes` | general | The comparison with 10 percent; the regressions are not re-estimated |
| Rank correlations, PDF pp. 19–20 | none | not formalised | Ties in the ranks and columns lost in the text layer. Not relevant to A1 |
| Footnotes 9 and 10 | `footnotes_9_10` | general | |
| Table 1.4 and PDF p. 23 | `table14_numbers` | general | |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| RW-D1 | The source read is the 1993 NBER conference chapter, "The University in the Marketplace: Some Insights and Some Puzzles", in Clotfelter and Rothschild (eds.), pp. 11–42. It is not Rothschild and White (1995), the *Journal of Political Economy* article on pricing services whose customers are inputs, which is not read and whose title is not checked here | Nothing in this record is attributed to the 1995 article. The chapter states the customers-as-inputs idea for colleges (RW-Q1, RW-Q2) but has no model of it beyond section 1.3 |
| RW-D2 | Pages are PDF pages. PDF page 1 is the NBER cover sheet, PDF page `n` from 2 to 33 is book page `n + 9`, and segment 33 of the text is empty | The running heads |
| RW-D3 | PDF pp. 28 (from "Comment") to 33, book pp. 37–42, are Martin Feldstein's comment on the chapter. They were read and nothing in them is attributed to Rothschild and White | The chapter PDF includes the comment |
| RW-D4 | In eq. (1) (PDF p. 9) the printed `x_s ≤ ∞` and "`∞` if `A(s) ≠ c'`" are read as `x_s < ∞` and "`∞` if `A(s) ≠ c`" | Read from the image. Otherwise the price system would not implement `A` |
| RW-D5 | `B(A) = Σ_{s=0}^{S} W_A[A(S)]` (PDF p. 8) is read as the sum over `s ∈ S = {1, …, N}` of `W_s[A(s)]` | Read from the image. The index set is given on PDF p. 7 |
| RW-D6 | In section 1.3.3 both `x` students bring the same random variable `x̃` and both `y` students `ỹ`, as printed | If each student brought an independent draw, every allocation would give the same variance and the example would have no point. `homogeneous_share_var` uses this reading |
| RW-D7 | The variance penalty of section 1.3.2 is a supposition ("Suppose the benefits ..."), and the Lean file takes the special case `mean − k · variance` | The chapter states only that the benefit rises with the mean and falls with the variance |
| RW-D8 | The text extraction renders `∞` as "a", drops `Σ` and the tildes, and breaks words with spaces ("o u t p ~ t") | Equations were read from images of PDF pp. 6, 8, 9 and 10. Quotations avoid the garbled passages, which is why RW-Q1 and RW-Q5 stop short |
| RW-D9 | In Table 1.4 the text layer prints 221 private two-year institutions in 1949–50; the image shows 227 (PDF p. 22) | With 227 the four counts sum to the 1,851 of PDF p. 23 (`table14_numbers`). The "over 50 percent" growth of private four-year institutions holds on the 1989–90 count including branches and not on the series excluding them |
| RW-D10 | `control_singleton_tests_insufficient` is our example, not the chapter's | It shows what the chapter's application of the stand-alone test leaves unchecked |
