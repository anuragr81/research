# Dahis, Nix and Qian (2019), claims

Evidence tag [F]. The full text of NBER Working Paper 26465, in its June 2020
revision, 79 PDF pages, was read on 16 October 2026 from its text layer
(DNQ-D1). Pages are PDF pages. The printed page is the PDF page minus 2 on
PDF pp. 3 to 51.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| DNQ-Q1 | 2 | "racial identity is neither entirely exogenous nor fixed over the lifetime, and responds to incentives." | The abstract's conclusion. The marker is chosen by some men, as in A1's passing |
| DNQ-Q2 | 5 | "An observed change in racial classification in the census required a change in lifestyle and situation so that a person would be accepted as white by those he encountered." | Passing is read as a change of the marker that others see, not of a record. The marker is what observers accept |
| DNQ-Q3 | 6 | "middle class African Americans were more likely to pass" | The authors' own reading of their correlates. Selection into passing by class |
| DNQ-Q4 | 12 | "In the modern U.S. context, sociologists have documented that the perception of whether an individual is black is positively correlated with socio-economic status" | Footnote 26. The perceived marker responds to class, which A1's group sufficiency rules out within a group. Cited studies are [C] |
| DNQ-Q5 | 12 | "separation became the key to whiteness. People who had associated with whites must be whites themselves, just as people who had associated with blacks had to be black" | Gross (2009) as quoted by the authors, [C] for Gross. The marker was inferred from contact, the reverse of A1's inference of class from the marker |
| DNQ-Q6 | 12 | "passing required a person to have physical features that are commonly shared by whites, to behave and dress like a white person, and to associate with white people." | Passing needs contact with the other group. Marker choice and venue choice are joint |
| DNQ-Q7 | 17 | "16.6% of black males with 2SUP links passed for white" | The headline rate within the linked sample, Table 1 |
| DNQ-Q8 | 18 | "If we make the extreme assumption that unlinked individuals never pass, the population rate of passing for black men under age 55 would be 1.4% (0.086 × .166)." | The lower bound, `lower_bound` and `lower_bound_value` |
| DNQ-Q9 | 34 | "conditional on the educational and income opportunities of his county, a man with a higher occupational income score is more likely to pass for white." | Table 7, column 3. Positive selection into passing on an income proxy, the premise of `left_behind_lower` |
| DNQ-Q10 | 34 | "We find that being married and having more children are negatively associated with passing for white (as is having a distinctively black name)." | Table 7, column 4. The cost of passing falls on ties to the group |
| DNQ-Q11 | 35 | "Column (1) shows that an individual who has passed for white earned a higher income" | Table 8, column 1. The return to the marker, 3.017 occupational score points given the base-year score |
| DNQ-Q12 | 36 | "behavior and success can influence the perception of race [...] a better dressed or educated individual is more likely to be categorized as mulatto by the enumerator." | The authors' reading of the mulatto coefficient in Table 8. The recorded marker carries class cues |
| DNQ-Q13 | 68 | "Interpreting these extrapolations assumes that selection (i.e., who decides to pass for white) is based on observables." | The weighted rates of Appendix F rest on selection on observables |
| DNQ-Q14 | 35 | "Column (2) shows that an individual who passed is 48% more likely to move counties or states than one who did not pass." | Read as 47.6 percentage points (DNQ-D2) |
| DNQ-Q15 | 34 | "bling the share of immigrants is associated with a 34.5 percentage-point higher probability of passing for white." | The text's "Doubling", hyphenated across a line, read per unit of share (DNQ-D3) |

## Lean results

`lean/mathlib/DahisEtAl2019.lean`. The linking rule and the extrapolation are
the paper's. The composition step is our reading.

| Theorem | What it checks |
|---|---|
| `DahisEtAl2019.twoSided_oneSided` | Every 2SUP link is a one-sided Unique Perfect link |
| `DahisEtAl2019.twoSided_unique_back` | A record of year `t + 10` is the 2SUP link of at most one record of year `t` |
| `DahisEtAl2019.figA4a_abe_linked` | Figure A.4a. The one-sided link joins the black Abe to a white Abe |
| `DahisEtAl2019.figA4b_abe_dropped` | Figure A.4b. 2SUP links the black Abe to no one |
| `DahisEtAl2019.figA4b_elijah_kept` | Figure A.4b. 2SUP keeps Elijah's link to a white Elijah |
| `DahisEtAl2019.figA4b_samuel_kept` | Figure A.4b. 2SUP keeps Samuel, who stays black |
| `DahisEtAl2019.figA4a_two_pass` | Appendix C.1. The one-sided link records two of the three black men as passing |
| `DahisEtAl2019.figA4b_one_pass` | Appendix C.1. 2SUP records one |
| `DahisEtAl2019.lower_bound` | Section 5.1.1. With the unlinked passing at a rate `u ≥ 0`, `l r` bounds the population rate |
| `DahisEtAl2019.lower_bound_value` | `0.086 × 0.166 = 0.014276`, and `1.4%/0.25% = 5.6` |
| `DahisEtAl2019.scale_up` | `30,239 × 10 = 302,390` |
| `DahisEtAl2019.adjusted_weighted` | Appendix F. `0.16 × (1 − 0.38) = 0.0992` |
| `DahisEtAl2019.table8_col4_sum` | Table 8, column 4. `3.178 − 1.301 + 0.454 = 2.331` |
| `DahisEtAl2019.left_behind_lower` | Our reading. If those who pass have a mean above the group's, those who remain have a mean below it |
| `DahisEtAl2019.left_behind_mean` | Our reading. The mean of those who remain is `(m − s mp)/(1 − s)` |
| `DahisEtAl2019.control_backward_needed` | Control. Without the backward condition the black Abe counts as passing |
| `DahisEtAl2019.control_bound_strict` | Control. If some unlinked men pass, the bound is strict |
| `DahisEtAl2019.control_no_selection` | Control. If those who pass have the group's mean, the mean of those who remain is unchanged |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| DNQ-D1 | The version read is the June 2020 revision of NBER Working Paper 26465, cited as the 2019 working paper with its revision date | The cover (PDF p. 1) is dated November 2019, the abstract page (PDF p. 2) reads "November 2019, Revised June 2020", and the PDF was created on 11 June 2020. No published version is read |
| DNQ-D2 | "48% more likely to move" (DNQ-Q14) is read as 47.6 percentage points | Table 8, column 2, is a linear probability coefficient of 0.476 on a dummy whose mean is 0.48 |
| DNQ-D3 | "Doubling the share of immigrants" (DNQ-Q15) is read as a rise of one in the share | Table 7, column 5, gives 0.345 on the county immigrant share. A doubling from a share `x` raises the probability by `0.345 x`, which is 34.5 points only at `x = 1` |
| DNQ-D4 | The age window "aget + 7 ⩽ aget + 10 ⩾ aget + 13" (PDF p. 58) is read with both inequalities as `⩽` | The text (PDF p. 16), Appendix C.3 and Figure A.5 give a window of three years either side |
| DNQ-D5 | The note to Table A.4 refers to "Appendix Section D" for the linking methods. They are in Section C.4 | Section D is on name similarity (PDF p. 65) |
| DNQ-D6 | A perfect spelling match is "a score of two" on PDF p. 59 and a Jaro-Winkler score of 0 on PDF p. 63 | Two conventions for the same rule. No row relies on the score |
| DNQ-D7 | The regressions of Tables 7 and 8 use base years 1910 to 1930 and men aged 15 to 54 | Their notes. The headline rate of Table 1 uses base years 1880 to 1930 and men under 55 |
| DNQ-D8 | Selection on education rests on the county education index and the own occupational score, not on own literacy | Table 7, column 3. Own literacy is −0.00361 (s.e. 0.00566). The text (PDF p. 33) cites Mill and Stein (2016) for more educated mulattoes passing |
| DNQ-D9 | The Mill and Stein (2016) cited here is a later version than the December 2012 working paper read in `lit/mill_stein_2012/` | The 10% to 13% rate the authors attribute to it (PDF p. 7) matches the 2012 version. Nothing is attributed to the 2016 version |
| DNQ-D10 | Quotations of Gross (2009), Packard (2003), Sharfstein (2011) and others are the authors' quotations | Tagged [C] for those sources. DNQ-Q5 is such a quotation |
| DNQ-D11 | "16.6% per census interval on average" (PDF p. 4) is the pooled 2SUP rate over five intervals, one of them twenty years | Table A.6 gives 22.4% for 1880 to 1900 and 15.0% to 18.1% for the ten-year intervals |
| DNQ-D12 | The sum of three coefficients on PDF p. 34 compares mulatto passers with black non-passers | The gain of a mulatto passer over a mulatto non-passer is `3.178 − 1.301 = 1.877`. Both are positive |
