# Dahis, Nix and Qian (2019), reconstruction

Ricardo Dahis, Emily Nix and Nancy Qian, "Choosing Racial Identity in the
United States, 1880-1940", NBER Working Paper 26465, November 2019, revised
June 2020. 79 PDF pages.

## Source read

- Cached at `~/.cache/paper_a/sources/dahis_nix_qian_2019.pdf` (4,040,274
  bytes), sha256 `11cca560…0bec9a`, with the text layer at
  `~/.cache/paper_a/sources/dahis_nix_qian_2019.txt`, sha256 `fc187e4f…0fbc`.
  The text has 80 form-feed segments. Segment `n − 1` is PDF page `n`, and
  segment 79 is empty.
- The version is the June 2020 revision of the working paper. PDF page 1 is
  the NBER cover dated November 2019. PDF page 2, the abstract page, reads
  "November 2019, Revised June 2020", and the PDF was created on 11 June 2020
  (DNQ-D1). The printed page is the PDF page minus 2 on PDF pp. 3 to 51. The
  Online Appendix restarts at printed page 1 on PDF p. 52.
- Every page, 1 to 79, was read from the text layer on 16 October 2026, so the
  evidence tag is [F]. The tables of PDF pp. 44 to 50 and 69 to 74 are legible
  in the text layer. The figures of PDF pp. 51 and 75 to 79 were read from
  their extracted labels and captions, and nothing is attributed to them
  beyond what the text says of them. Pages are PDF pages throughout.

## The study, from its own primitives

1. **Data** (PDF pp. 14 to 15). The full-count US censuses of 1880 to 1940,
   digitised by Ancestry and supplied through the NBER. Men under 55 in the
   base year. The enumerator records race. "Black" includes the mulatto
   category in the years that have it.
2. **Passing** (PDF pp. 5, 15). A change in the census classification from
   black to white between consecutive censuses. The authors read it as an
   active choice, because the classification needed the man to be accepted as
   white by those he met, the enumerator among them (DNQ-Q2). Passing required
   white features, white behaviour and dress, and association with whites
   (DNQ-Q6).
3. **Linking** (PDF pp. 16 to 17, Appendix C, Figures A.4 and A.5). A 10%
   random sample of black men in year `t` is matched to all men in year
   `t + 10` by Phonex name, an age window of three years either side and
   birth place, then by perfect spelling. A Unique Perfect link needs one
   candidate going forwards. A 2-Sided Unique Perfect (2SUP) link also needs
   the record found in `t + 10` to have one candidate going backwards. The
   backward condition drops the false link of Figure A.4, where a black Abe who
   left the data is joined to the future self of one of many white Abes
   (`figA4a_abe_linked`, `figA4b_abe_dropped`, `control_backward_needed`).
4. **Rates** (PDF pp. 17 to 21, Tables 1, 2, A.6). 16.6% of black men with 2SUP
   links pass for white (DNQ-Q7), 30,239 men in the 10% sample, about 302,390
   in the population (`scale_up`). 8.6% of black men are linked. Assuming that
   the unlinked never pass gives the population lower bound of 1.4% per census
   interval (DNQ-Q8, `lower_bound`, `lower_bound_value`). The one-sided link
   gives 26.8% with 14.1% linked. In the transition matrix of 1920 to 1940,
   0.7% of white men become black, 8.2% of Chinese and 7.0% of Japanese men
   become white, 27.8% of Native American men become white, and passing across
   the three East Asian categories is 0.1% to 0.4%. Of those who pass, 30%
   reverse-pass to black in the census after.
5. **Falsification** (PDF pp. 19 to 22, Table A.3). Literate-to-illiterate
   changes and children's ages are similar for passers and non-passers.
6. **Descriptive patterns** (PDF pp. 23 to 36). Men classified mulatto pass
   slightly more often than men classified black (Table 3). Men with
   distinctively black names pass at 7.4% and with distinctively white names
   at 24.3%. Nearly every man who becomes married to a white woman passes,
   and nearly no man married to a black woman does (Table 4). The same holds
   for the race of children in the household (Table 5). Passers move more and
   move to whiter counties (Table 6, Figure 1). Passing is higher among men
   born or living in the North.
7. **Base-year correlates** (PDF pp. 30 to 33, Table 7). Passing is lower where
   intermarriage is legal (−0.319), higher with the Democratic vote share,
   higher in Southern counties with better black schooling (0.0219), higher
   with the man's own occupational income score (0.000946, standardised
   0.0219), lower for married men, for men with more children and for men
   with distinctively black names, and higher in urban counties and counties
   with more immigrants. Own literacy has a coefficient of −0.00361 with a
   standard error of 0.00566 (DNQ-D8).
8. **Outcomes** (PDF pp. 33 to 34, Table 8). Conditional on base-year county,
   occupational score, urban residence, marriage, literacy and age, a man who
   passed has an occupational score 3.017 points higher in `t + 10` (mean
   16.4). The gain is larger for passers who moved. Men classified mulatto
   who did not pass score 0.454 higher than men classified black who did
   not, and their gain from passing is 1.301 smaller (`table8_col4_sum`,
   DNQ-D12).
9. **Weights** (Appendix F, PDF pp. 66 to 68). Population weights give 16% and
   probit weights 16.8%. Adding back the 38% of the population whose cell has
   no linked man, assumed never to pass, gives 9.9% (`adjusted_weighted`) and
   6.8%. The authors state that the extrapolations assume selection on
   observables (DNQ-Q13).

## Who passes, and what the paper says of those who remain

The paper's evidence on selection is Table 7. Conditional on county
characteristics, a higher own occupational income score goes with more
passing (DNQ-Q9), and marriage, children and a distinctively black name go
with less (DNQ-Q10). The authors read the pattern as middle-class men being
more likely to pass (DNQ-Q3). Own literacy is not a significant correlate in
the column where the occupational score is (DNQ-D8).

The paper does not state what passing does to the class composition of the
black men who remain. It says that spouses and children who cannot pass are
left behind (PDF p. 13, Tables 4 and 5). The step to composition is ours. If
those who pass have a mean above the group's, those who remain have a mean
below it (`left_behind_lower`, `left_behind_mean`), and without that selection
the mean is unchanged (`control_no_selection`). Table 7 supports the premise
only as a conditional correlation, not as a comparison of the means of
passers and non-passers.

The paper also reports that the classification of race responds to class
cues. A better dressed or educated man is more likely to be recorded as
mulatto (DNQ-Q12), and modern studies find that the perception of whether a
person is black correlates with socio-economic status (DNQ-Q4). In the legal
cases the authors cite, race was decided by association, by whom a person
danced and mixed with (DNQ-Q5).

## Derivation chain of what Paper A1 would use

- The population rate is `l r + (1 − l) u`. Setting `u = 0` gives the lower
  bound `l r`, which is `0.086 × 0.166 = 0.014276` (`lower_bound`,
  `lower_bound_value`). The bound is strict whenever some unlinked men pass
  (`control_bound_strict`).
- A 2SUP link is a one-sided link with a backward uniqueness condition
  (`twoSided_oneSided`), and a record of `t + 10` is the 2SUP link of at most
  one record of `t` (`twoSided_unique_back`). Figure A.4 counts two passers
  under the one-sided link and one under 2SUP (`figA4a_two_pass`,
  `figA4b_one_pass`).
- The composition step, `m = s mp + (1 − s) ms` with `mp > m` gives `ms < m`,
  is our reading and not the paper's statement.

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every result. What is short of general
is the following.

- The estimated rates of Tables 1 to 5 and A.2 to A.8, the coefficients of
  Tables 7 and 8, and the segregation regressions of PDF p. 30. The linked
  microdata are not available here, so the estimates are taken as printed.
  Their specifications are formalised (`lin`, `lin_change`) and so are the
  numbers the text derives from them.
- Figure A.4 is formalised as the example it is.
- The probit of Appendix F and the adjusted probit rate of 6.8%. The weight is
  formalised as a function of the propensity (`probitWeight`), not estimated.
- Whether the 8.6% link rate of the 10% base sample is the right share for the
  whole black population, which the lower bound of 1.4% assumes.
