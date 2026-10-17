# Hanson and Hawley (2011), claims

Evidence tag [F]. The working paper, Georgia State University Experimental
Economics Center WP 2011-05, PDF pp. 1 to 58, was read in full on 17 October
2026 (`RECONSTRUCTION.md`). Pages are PDF pages. The paper's printed page is the
PDF page less two, for PDF pp. 3 to 35. An elision `[...]` stands for words left
out between two parts of the quotation.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| HH-Q1 | 2 | "Generally, discrimination occurs against African American names; however, when the content of the e-mail messages insinuates home-seekers with high social class, discrimination is non-existent." | The authors' statement of the finding A1 links to. Its scope is HH-D1 |
| HH-Q2 | 4 | "When e-mail inquiries imply the African American is of higher social class, racial discrimination is small and not statistically different than zero- a unique finding in the literature." | The authors say the finding is new in the literature they review. See HH-Q20 and HH-Q21 |
| HH-Q3 | 10 | "Specifically, they may also infer the social class of the individual. This is particularly problematic for studying racial discrimination using names if African American sounding names are associated with a lower class, as this would bias the results toward finding racial discrimination." | Class inferred from a group marker, the channel of A1, named as the reason for the class treatment |
| HH-Q4 | 10 | "There is substantial agreement, however, that the stratifications of class are not by definition those of race, religion, and ethnic origin" | The authors' definition of class, from Gordon (1949), keeps class apart from group, as A1 does |
| HH-Q5 | 11 | "High-class e-mails suggest that the references they have are “good” and the credit report is “recent”." | The class signal includes creditworthiness. It is the landlord's question whether the tenant can pay |
| HH-Q6 | 10 | "we intend for landlords to infer information (besides race) specific enough for them to believe an individual would be a better (high-class) or worse (low-class) tenant for the property, yet vague enough not to be associated with a particular characteristic such as income, occupation, or education." | Class here is a vertical ranking of tenants, not a distance between peers |
| HH-Q7 | 19 | "Furthermore, we cannot reject the null hypothesis that the net level of discrimination between African American and white home-seekers equals zero when both inquiries use a high-class" | The test behind HH-Q1, the landlord-level net measure. The quotation ends where the text breaks "e-mail" across a line. Formalised as `table6_high_high`, `net_eq_rate_diff` |
| HH-Q8 | 19 | "Comparing the response rates shows that high-class e-mails from African American sounding names perform better than low-class e-mails from the same set of names, but still not as well as white sounding names from either class type." | Over all e-mails the penalty does not vanish. A high-class African American name trails a low-class White name. Formalised as `table6_gaps`, `table6_gap_halves_not_zero` |
| HH-Q9 | 21 | "In Column (4), the marginal effect of sending an e-mail from an African American sounding name is a 4 percentage point reduction in the likelihood of response, statistically significant at conventional levels." | With class coded one for a low-class e-mail, this is the race effect on a high-class e-mail (HH-D1) |
| HH-Q10 | 33 | "Under statistical discrimination, a landlord discriminates because they have imperfect information about potential tenants and use observable characteristics to make inference about the type of tenant the applicant will be." | The authors' reading of the result as inference from a marker |
| HH-Q11 | 34 | "As we demonstrate, when a landlord is offered something else to infer characteristics from (how an e-mail is written), and the information is positive, they do not base their response on the race associated with our inquiries." | The property, stated for positive information only. Our threshold reading is `threshold_equal_above` |
| HH-Q12 | 23 | "If landlords advertising apartments or condominiums are concerned with being able to rent additional units in the same building and they believe current or future tenants are prejudice, then they may be more likely to discriminate." | A landlord screens on behalf of the tenants who will live in contact. Formalised as `table9_nets` |
| HH-Q13 | 34 | "In the case of landlords who operate in apartments or condominiums, they bear the full costs of having an African American tenant move in if the other tenants are displeased (neighbors may move out or rents may decrease); while landlords of single family homes may share the cost with other landlords in the neighborhood." | The venue operator bears the cost of its clients' contact preferences |
| HH-Q14 | 34 | "This suggests landlords discriminate to prevent white residents from leaving the neighborhood." | The authors' reading of the tipping result |
| HH-Q15 | 22 | "For both the response rate and landlord level tests, we find that discrimination is most severe in neighborhoods that are between 80 and 95 percent white." | Discrimination is largest at an intermediate share. Formalised as `tipping_iff_minority`, `table8_band_largest`. Table 8 is inconsistent (HH-D4, HH-D5) |
| HH-Q16 | 25 | "The net difference in landlords describing the unit as available between whites and African Americans is not statistically different that zero for the entire sample [...] we cannot reject the null hypothesis that there is no net discrimination in inviting home-seekers to view the unit for the full sample and almost all cities individually" | Once a landlord writes back, the reply treats the two names alike. The difference is in whether the landlord replies |
| HH-Q17 | 31 | "we find slightly less (0.39 percentage points) net discrimination at the landlord level, and slightly more (0.08 percentage points) discrimination in response rates." | Landlords behind an anonymous address do not discriminate more |
| HH-Q18 | 12 | "Craigslist puts a notice on every advertisement posted on the site warning that stating a discriminatory preference is illegal and allows users to easily notify them of any such advertisements." | The venue forbids stating a preference. The experiment measures what remains |
| HH-Q19 | 33 | "Either type of discrimination is illegal in the United States under the Civil Rights Act of 1968." | Both taste-based and statistical discrimination are banned in this market |
| HH-Q20 | 7 | "Ahmed, Andersson, and Hamarstedt (2010) find that while information (including marital status, employment information, age, and education level) does increase the response to minority applicants, it does not decrease the difference in response between native Swedes and the Muslim minority." | A study the authors cite in which shown information does not close the gap |
| HH-Q21 | 8 | "including positive information does not affect the response rate difference between races." | Ewens, Tomlin and Wang (2009), as the authors report it. A second such study |
| HH-Q22 | 8 | "Although, we cannot prevent landlords from inferring traits besides race about the names in our study, we attempt to influence them into inferring something about the social class of the auditors and test how this matters for response." | The class treatment is an attempt to move an inference, not a shown fact |
| HH-Q23 | 17 | "Although we count these landlords to represent equal treatment, it is possible that they do not infer anything about race from the names we use." | Equal treatment in the landlord measure includes landlords who did not read the marker |
| HH-Q24 | 33 | "Discrimination goes away when both e-mail inquiries use a high-class e-mail, and is stronger when both e-mail inquiries use a low-class e-mail." | The conclusion's statement of the finding |

## Lean results

`lean/mathlib/HansonHawley2011.lean`. The arithmetic is the paper's. The
predictor of quality and the threshold are our reading.

| Theorem | What it checks |
|---|---|
| `HansonHawley2011.table6_gaps` | Table 6. Race gaps of 4.01 (high) and 8.59 (low) points, class gaps of −0.97 (White) and 3.61 (African American), and 4.98 between a low-class White and a high-class African American name |
| `HansonHawley2011.table6_class_moves_one_group` | Table 6. The high-class e-mail raises the African American rate and not the White rate |
| `HansonHawley2011.table6_gap_halves_not_zero` | Table 6. The high-class race gap is positive and less than half the low-class gap |
| `HansonHawley2011.table6_emails` | Tables 5 and 6. The e-mail counts add up to 9,456 |
| `HansonHawley2011.table6_audits` | Tables 3, 5 and 6. Six pairings add to 4,728 audits and four cross-race pairings to 3,153 |
| `HansonHawley2011.table6_high_high` | Table 6, high and high. Counts add to 790 and each share, and the net 1.77, is its count over 790 |
| `HansonHawley2011.table6_low_low` | Table 6, low and low. 788 audits and a net share of −5.96 percent |
| `HansonHawley2011.table6_high_low` | Table 6, White high and African American low. 788 audits and a net share of 8.76 percent |
| `HansonHawley2011.table6_aa_row_short` | HH-D2. The counts of the African American high and low row add to 760, the shares to 96.56, and the shares are over 787 |
| `HansonHawley2011.table6_wl_ah_neither` | HH-D3. 266 of 787 is not the printed 36.34 percent, and 286 of 787 is |
| `HansonHawley2011.table5_all` | Table 5. 3,153 audits and a net share of 4.54 percent |
| `HansonHawley2011.fe_effect_discordant` | PDF p. 20. 394 and 251 of 645 landlords, a difference of 0.222 |
| `HansonHawley2011.net_eq_rate_diff` | On the same paired audits the net share equals the difference in reply rates |
| `HansonHawley2011.control_pooled_rates_differ` | Control. Table 6's pooled rate gap, 4.01, is not the net share of its paired audits, 1.77 |
| `HansonHawley2011.gap_common_weight` | Our reading. With a common weight `k` on the e-mail, the group gap in predicted quality is `(1 − k)(μW − μA)` |
| `HansonHawley2011.gap_vanishes_full_signal` | Our reading. The gap is zero when the e-mail shows class fully, `k = 1` |
| `HansonHawley2011.control_partial_signal_gap` | Control. With `k < 1` and different group means the gap is not zero |
| `HansonHawley2011.threshold_equal_above` | Our reading. With a partial signal, a signal that puts both groups over the threshold gives equal replies although the predictions differ |
| `HansonHawley2011.control_threshold_between` | Control. A threshold between the two predictions gives unequal replies |
| `HansonHawley2011.high_signal_clears` | Our reading. With `k > 0` a high enough signal clears any threshold for both groups |
| `HansonHawley2011.control_zero_weight` | Control. With `k = 0` the e-mail moves no prediction |
| `HansonHawley2011.tipping_iff_minority` | Note 26. Eighty to 95 percent White is a minority share of five to 20 percent |
| `HansonHawley2011.table8_band_largest` | Table 8. The net share is largest in the tipping band |
| `HansonHawley2011.table8_emails_all` | HH-D4. Table 8's e-mails add to 9,456, against 3,060 units matched to a tract |
| `HansonHawley2011.table8_counts_not_shares` | HH-D5. Table 8's landlord counts are not the numerators of its shares |
| `HansonHawley2011.table9_nets` | Table 9. Net shares of 0.48 (single-family), 6.09 (apartments) and 11.88 (condominiums) percent |

## Coverage of the source's results

The source numbers tables, not propositions. Every table and every number in
the text that follows from other printed numbers has a row. Definitions are
cited without the module prefix.

| Result (as the source numbers it) | Lean name | Status | Reason |
|---|---|---|---|
| Matrix 1 and Table 2, the twelve audit types | none | not formalised | A design, not a result |
| Table 1, names and frequencies | none | not formalised | Counts of use only |
| Table 3, audits and reply rates by city | `HansonHawley2011.table6_audits` | special case | The total of 4,728. Rates by city are from the data |
| Table 4, unit and neighbourhood characteristics | none | not formalised | Means from the data |
| Table 5, every row's net share | `HansonHawley2011.table5_all` | special case | The full sample. Cities are from the data in the same way |
| Table 5, the tests of proportions | none | not formalised | p-values need the data |
| Table 6, rates and gaps | `rWH`, `rWL`, `rAH`, `rAL` (definitions), `HansonHawley2011.table6_gaps`, `HansonHawley2011.table6_class_moves_one_group`, `HansonHawley2011.table6_gap_halves_not_zero`, `HansonHawley2011.table6_emails` | general | Two printed gaps differ from the rates by rounding (HH-D10) |
| Table 6, landlord level | `HansonHawley2011.table6_audits`, `HansonHawley2011.table6_high_high`, `HansonHawley2011.table6_low_low`, `HansonHawley2011.table6_high_low`, `HansonHawley2011.table6_aa_row_short`, `HansonHawley2011.table6_wl_ah_neither` | special case | Five of six rows. The White high and low row is not formalised. Two rows are inconsistent (HH-D2, HH-D3) |
| Table 6, the landlord-level measure | `Pairs` (definition), `HansonHawley2011.net_eq_rate_diff`, `HansonHawley2011.control_pooled_rates_differ` | general | |
| Table 7, probits | `HansonHawley2011.fe_effect_discordant` | special case | The 0.222 of column (3) only. The rest are estimates |
| Note 27, the tipping probits | none | not formalised | The equation is garbled in the PDF (HH-D8) and the results are not printed |
| Table 8, by percentage White | `tipping` (definition), `HansonHawley2011.tipping_iff_minority`, `HansonHawley2011.table8_band_largest`, `HansonHawley2011.table8_emails_all`, `HansonHawley2011.table8_counts_not_shares` | special case | The ordering of column (8) and two inconsistencies (HH-D4, HH-D5) |
| Table 9, by unit type | `HansonHawley2011.table9_nets` | special case | Three of six rows |
| Table 10, interactions | none | not formalised | Estimates from the data |
| Tables 11 and 12, availability and viewing | none | not formalised | Shares and tests from the data |
| Tables 13 to 16, robustness | none | not formalised | Re-estimates on subsamples |
| Table 17, anonymous addresses | none | not formalised | From the data |
| Table 18 and the reweighting of PDF p. 33 | none | not formalised | The weights are not printed |
| PDF p. 35, consumer surplus from Bradburd et al. (2005) | none | not formalised | From a model not read |
| Figure 1 | none | not formalised | No values printed |
| PDF pp. 33–34, statistical discrimination as the reading | `predict`, `answers` (definitions), `HansonHawley2011.gap_common_weight`, `HansonHawley2011.gap_vanishes_full_signal`, `HansonHawley2011.control_partial_signal_gap`, `HansonHawley2011.threshold_equal_above`, `HansonHawley2011.control_threshold_between`, `HansonHawley2011.high_signal_clears`, `HansonHawley2011.control_zero_weight` | special case | Our reading. The paper states the reading in words and no model |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| HH-D1 | "Discrimination is non-existent" (HH-Q1) and "goes away" (HH-Q24) hold for one measure, the net share of landlords between two high-class e-mails, 1.77 percent with p = 0.2485. Over all e-mails the race gap between high-class e-mails is 4.01 points with p = 0.0058 (Table 6). With class coded one for a low-class e-mail, Table 7's race coefficient is the effect on a high-class e-mail, −0.0401 with p = 0.002 in column (4) and −0.119 with p = 0.003 with landlord effects in column (6) (HH-Q9) | The pooled rates of Table 6 are over every e-mail of a type, from all pairings, and not over the paired audits of the row (`control_pooled_rates_differ`). The paper does not reconcile the measures |
| HH-D2 | Table 6's row "African American, High / African American, Low" prints counts 320, 300, 87 and 53, adding to 760, and shares adding to 96.56 percent. The shares are over 787 audits | `table6_aa_row_short`. 27 audits are missing from the columns. Not relied on beyond the net share |
| HH-D3 | Table 6's row "White, Low / African American, High" prints 266 for "Respond to Neither" and 36.34 percent. 286 gives 36.34 percent of 787 | `table6_wl_ah_neither`. Read as a misprint of 286 |
| HH-D4 | Table 8's e-mail counts add to 9,456, every e-mail sent, while PDF p. 16 matches 3,060 units, 6,120 e-mails, to a tract. Table 10's neighbourhood rows have 5,908 | `table8_emails_all`. Table 8 cannot be over tract-matched units only. The tipping result is relied on as the authors state it, not for its numbers |
| HH-D5 | Table 8's landlord counts in columns (4) to (7) are not the numerators of the printed shares | `table8_counts_not_shares`. Not relied on |
| HH-D6 | PDF p. 1 is a cover sheet the repository writes at download, with the download date | A fresh download has a different sha256. The pin is to the cached copy of 17 October 2026 |
| HH-D7 | The working paper is read, not the article in the *Journal of Urban Economics* | The article was not available. The working paper is after review (note 32). Numbers in the article may differ |
| HH-D8 | The probit equation of note 27 (PDF p. 22) is garbled in the PDF, not only in the text layer | Read from the image. Not relied on |
| HH-D9 | Table 7 prints the p-value of the class coefficient in column (4) as "[.01293]", with five digits where every other has three | It may be 0.1293 or 0.01293. Not relied on |
| HH-D10 | Table 6 prints 3.62 for 52.62 − 49.01 = 3.61 and −8.60 for 49.01 − 57.60 = −8.59 | Rounding from unrounded rates. `table6_gaps` uses the printed rates |
| HH-D11 | The reference list gives "Milton, M. Gordon. 1949" for Milton M. Gordon | Typo. HH-Q4 is the authors' quotation of Gordon, cited as such |
