# Edelman, Luca and Svirsky (2017), claims

Evidence tag [F]. The text layer of the accepted version, marked "forthcoming,
American Economic Journal: Applied Economics", was read in full, PDF pp. 1 to
36, on 16 October 2026, with the two figure pages also read from images
(`RECONSTRUCTION.md`). Pages are PDF pages, which equal the printed page
numbers of this version. The published pagination is not read (ELS-D6).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ELS-Q1 | 1 | "applications from guests with distinctively African-American names are 16% less likely to be accepted relative to identical guests with distinctively White names." | The headline. A host decides on a guest from a name marker alone, and the penalty is 8 points on a base of about 50 (`pooled_gap`) |
| ELS-Q2 | 3 | "When we restrict our analysis to hosts who have had an African-American guest in the recent past, discrimination disappears" | Table 5, column 5. The penalty is concentrated among hosts with no African-American guest in their reviews (`gap_vanishes_iff`) |
| ELS-Q3 | 7 | "To avoid the confounds that would result from pictures, we use only names; our Airbnb profiles include no picture of the putative guest." | The marker is the name, and the profiles are otherwise identical. The host's information is the marker alone, A1's group sufficiency |
| ELS-Q4 | 2 | "We only observe a gap between distinctively white and distinctively African-American names, which differ not only by suggested ethnicity but also potentially by socioeconomic status (Fryer and Levitt, 2004)." | The marker carries class as well as race. The measured penalty may be a penalty on inferred class, A1's channel |
| ELS-Q5 | 17 | "Our findings cannot identify whether the discrimination is based on race, socioeconomic status, or a combination of these two." | The authors do not separate the race channel from the class channel. A1's test of contribution 4 is the missing separation |
| ELS-Q6 | 13 | "renters with African-American names face a penalty, but that the penalty decreases if the email sent to a landlord signals higher social class." | Their account of Hanson and Hawley (2011), not read here [C]. The nearest precedent for contribution 4 found so far, in rental housing |
| ELS-Q7 | 9 | "If discrimination is driven by homophily (in-group bias), then the host’s race should matter." | The homophily the source tests is homophily in race, read from the host's own race, not homophily in class |
| ELS-Q8 | 10 | "Thus, with the exception of African-American females, the data is inconsistent with homophily." | African-American male hosts also accept African-American guests less (Table 4). A penalty shared by hosts of both races fits inference about the guest, and does not fit in-group taste |
| ELS-Q9 | 11 | "Column 1 of Table 6 shows that, overall, listings above the median price are more likely to reject inquiries. However, discrimination remains both among more expensive and less expensive listings." | The interaction with price above the median is 0.01 (0.03). If listing price proxies the host's class, the penalty does not vary with it |
| ELS-Q10 | 18 | "Furthermore, we find that discrimination is not sensitive to a measure of proximity between the host and guest. Both findings are in tension with pure taste-based discrimination." | Shared and entire properties show the same gap (Table 5, column 1) |
| ELS-Q11 | 18 | "This suggests that, at the very least, hosts are using different statistical models as they evaluate potential guests." | The authors' reading of the review-history result. Hosts' inference differs with their own experience |
| ELS-Q12 | 14 | "One limitation of our experiment is that we do not observe the effect of past reviews on discrimination." | Whether reviews remove the penalty is not tested |
| ELS-Q13 | 14 | "If our findings are driven by statistical discrimination, positive reviews from previous hosts may reduce the extent of discrimination." | The authors state the information prediction without testing it, and argue that reviews would be an incomplete remedy |
| ELS-Q14 | 16 | "The median host who rejects a guest because of race is turning down between $65 and $100 of revenue." | Hosts discriminate at a cost in net revenue (`cost_identity`, `cost_numbers`) |
| ELS-Q15 | 8 | "we restrict our attention to the simplest response – “Yes” – though all of our results are robust to using “No” instead" | Read with ELS-D1. The 50 and 42 percent rates follow from Appendix Table 4 only when the "Yes" categories are pooled |

## Numbers A1 would cite

About 6,400 inquiries to one listing per host in Baltimore, Dallas, Los
Angeles, St. Louis and Washington, D.C., sent 7 to 30 July 2015 from twenty
accounts identical except for the name, ten distinctively African-American and
ten distinctively White (PDF pp. 2, 5, 7, 8). Table 2 (PDF p. 27), 6,235
observations, the guest coefficient −0.08 (0.02) with constant 0.49, and −0.09
(0.02) with host and listing controls. Table 3 (PDF p. 28), host
African-American × guest African-American 0.01 (0.05) overall, −0.11 (0.08)
for male hosts, 0.11 (0.06) for female hosts. Table 5 (PDF p. 30), shared
property × guest −0.02 (0.03); host with a review from an African-American
guest × guest 0.06 (0.02), implied gap −0.04 (0.03). Table 6 (PDF p. 31),
price above median × guest 0.01 (0.03), tract share African-American × guest
0.02 (0.08). Cost of a rejection, $64.19 to $98.19 (PDF p. 16).

## Lean results

`lean/mathlib/EdelmanEtAl2017.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `EdelmanEtAl2017.gap_with_trait` | The "implied coefficient" rows. The race gap among hosts with the trait is `b + i` |
| `EdelmanEtAl2017.gap_without_trait` | The race gap among hosts without the trait is `b` |
| `EdelmanEtAl2017.gap_vanishes_iff` | The gap vanishes among hosts with the trait exactly when `i = −b` |
| `EdelmanEtAl2017.control_no_interaction` | Control. With no interaction a nonzero gap cannot vanish among hosts with the trait |
| `EdelmanEtAl2017.white_total` | Appendix Table 4. 3,167 responses to White guests |
| `EdelmanEtAl2017.aa_total` | Appendix Table 4. 3,090 responses to African-American guests |
| `EdelmanEtAl2017.pooled_yes_rates` | The six "Yes" categories pooled give 0.496 and 0.415 |
| `EdelmanEtAl2017.pooled_gap` | The pooled gap is between 8 and 9 points and between 16 and 17 percent |
| `EdelmanEtAl2017.control_simple_yes_not_fifty` | Control. The simple "Yes" alone gives rates below 0.37 and 0.31 (ELS-D1) |
| `EdelmanEtAl2017.response_shares` | PDF p. 13. The 44, 11 and 46 percent shares, with "Not sure" counted as no response (ELS-D2) |
| `EdelmanEtAl2017.same_points_not_same_percent` | Footnote 11. An 8-point gap is 16 percent of 50 and under 14 percent of 60 |
| `EdelmanEtAl2017.cost_identity` | PDF p. 15. `(p − f) − π(p − f) = (1 − π)(p − f)` |
| `EdelmanEtAl2017.cost_numbers` | PDF p. 16. $64.19 and $98.19 to the cent |
| `EdelmanEtAl2017.cost_pos` | The cost is positive when `π < 1` and `p > f` |
| `EdelmanEtAl2017.control_sure_replacement_costless` | Control. With `π = 1` the cost is zero |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| ELS-D1 | The outcome "Yes" of Tables 2 to 7 is read as the six "Yes" categories of Appendix Table 4 pooled, although PDF p. 8 says the analysis uses "the simplest response – 'Yes'" | Pooled, the counts give 0.496 and 0.415, which match the 50 and 42 percent and the constant 0.49 of Table 2. The simple "Yes" gives 0.364 and 0.304 (`pooled_yes_rates`, `control_simple_yes_not_fifty`). Figure 2's "Yes" bars (about 1,150 and 940, read from the image) are the simple "Yes" |
| ELS-D2 | "44% of guests were rejected or received no response" (PDF p. 13) is read as including "Not sure or check back later" | Only with those 329 responses does the share reach 0.439 (`response_shares`) |
| ELS-D3 | Sample sizes differ across the paper, about 6,400 (text), 6,392 (Table 1), 6,390 (Table 7), 6,257 (Appendix Table 4) and 6,235 (regressions) | Not explained in the text. No row relies on the difference |
| ELS-D4 | In Table 1 (PDF p. 26), "Host has multiple listings" has mean 0.16 but group means 0.32 and 0.33, and bedrooms and bathrooms have nearly identical rows | The text says "Roughly a sixth of Airbnb hosts manage multiple properties" (PDF p. 10), so 0.16 is read as right. Not relied on |
| ELS-D5 | Implied coefficients differ from the sum of the rounded coefficients by 0.01, for example Table 5, column 5, `−0.09 + 0.06` printed as −0.04, and Table 3, male hosts, `−0.09 − 0.11` printed as −0.19 | Rounding of unrounded estimates. The sentence that the zero gap among hosts with an African-American guest is "only significant at the 10% level" (PDF p. 13) is ambiguous and is not relied on |
| ELS-D6 | The version read is the accepted manuscript marked "forthcoming". The published article is in *American Economic Journal: Applied Economics* 9(2), 2017 | The cached file has no journal pagination. The issue number and pages of the published article are not checked against a copy of it |
| ELS-D7 | "six groups" of responses (PDF p. 8) and "11 categories" (PDF p. 9) are read as the coarse coding and the categories of Appendix Table 4 | Appendix Table 4 lists eleven |
