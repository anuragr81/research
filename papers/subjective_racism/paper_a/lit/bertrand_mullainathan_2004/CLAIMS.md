# Bertrand and Mullainathan (2004), claims

Evidence tag [F]. The article, journal pp. 991 to 1013 (PDF pp. 1 to 23), was
read in full on 16 October 2026, with Tables 6 and 8 also read from images
(`RECONSTRUCTION.md`). The publisher's cited-by list on PDF pp. 24 to 162 is not
part of the paper (BM-D8). Pages are journal pages.

The cached text interleaves the two columns line by line. An elision `[...]` in
these quotations stands for the other column's text, or for a word broken by a
hyphen at a line end, never for words left out of the sentence otherwise. The
broken words are named in the last column and in BM-D1.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BM-Q1 | 991 | "We also find little evidence that employers are inferring social class from the names." | The authors' summary of their class tests. Read with BM-Q8 for its scope |
| BM-Q2 | 991 | "White names receive 50 percent more callbacks for interviews. Callbacks are also more [...] to resume quality for White names than for African-American ones." | The gap and the lower return to credentials. Broken word "responsive". Formalised as `table1_gap`, `table4_returns` |
| BM-Q3 | 995 | "We chose name over other potential manipulations of [...] race, such as affiliation with a minority group, because we [...] felt such affiliations may especially convey more than race." | A chosen affiliation carries more than the group. The design avoids the kind of marker A1's passing concerns |
| BM-Q4 | 1003 | "There is no evidence that [...] benefit any more than Whites from [...] living in a Whiter, more educated zip code." | A shown proxy for class does not narrow the gap. Broken word "African-Americans" |
| BM-Q5 | 1007 | "When employers read a name [...] like “Tyrone” or “Latoya,” they may assume [...] that the person comes from a disadvantaged [...] background." | The class channel as the authors state it, an inference of background from a name |
| BM-Q6 | 1007 | "In the extreme form of this social [...] background interpretation, employers do not [...] care at all about race but are discriminating only [...] against the social background conveyed by the [...] names we have chosen." | The hypothesis the tests address, background conveyed by the name |
| BM-Q7 | 1007 | "If the African-American [...] names we have chosen mainly signal negative [...] social background, one might have expected the [...] estimated name gap to be lower for better [...]" | The prediction of the address test. Broken word "addresses". In a linear predictor with a common weight the gap does not depend on the address (`gap_indep_address`) |
| BM-Q8 | 1007 | "For this social class [...] effect to be something of independent interest, one must [...] assert that African-Americans with the African-American [...] names we have selected are from a lower social background [...] than the average African-American and/or that Whites with [...] the White names we have selected are from a higher social [...] background than the average White." | The tests are of class conveyed by the name beyond race. Class conveyed by race itself, A1's channel, is set aside. Formalised as `control_group_sufficiency_zero` |
| BM-Q9 | 1007 | "Also, if the names mainly signal social background, one might have expected the name gap to be higher for jobs that rely more on soft [...] skills or require more interpersonal interactions. [...] We found no such evidence in Table 7." | The soft-skills test |
| BM-Q10 | 1008 | "“Correlation” reports the Spearman rank order correlation between callback rate and mother education within each race-gender group as well as the p-value for the test of independence." | The statistic of the Table 8 test. Formalised as `table8_aa_male`, `table8_aa_female` |
| BM-Q11 | 1009 | "Consistent with a social background [...] the African-American names we have [...] chosen fall below the African-American [...]" | The names do carry background. Broken words "interpretation," and "average" |
| BM-Q12 | 1009 | "In summary, [...] this test suggests little evidence that social [...] drives the measured race gap." | The authors' conclusion from Table 8. Broken word "background" |
| BM-Q13 | 1010 | "In one class of [...] discrimination models, employers use [...] (observable) race to proxy for unobservable [...] skills" | The Phelps and Arrow reading of the gap. Broken word "statistical" |
| BM-Q14 | 1010 | "Indeed, [...] the added credentials should lead to a larger [...] update for African-Americans and hence greater [...] returns to skills for that group." | That reading's prediction for credentials, contradicted by Table 4 |
| BM-Q15 | 1011 | "One such [...] heuristic could be to simply read no further [...] when they see an African-American name." | Lexicographic search. Under it a shown credential or class is not read |

## Lean results

`lean/mathlib/BertrandMullainathan2004.lean`. The arithmetic and the two
correlations are the paper's. The test logic is our reading.

| Theorem | What it checks |
|---|---|
| `BertrandMullainathan2004.table1_gap` | Table 1. A gap of 3.20 points, a ratio between 1.49 and 1.50, and eight years of experience at 0.4 points a year |
| `BertrandMullainathan2004.table4_returns` | Table 4, Panel A. Gains of 2.29 and 0.51 points from a higher-quality resume |
| `BertrandMullainathan2004.table8_aa_male` | Table 8. The African-American male correlation is `−25/42 ≈ −0.595` |
| `BertrandMullainathan2004.table8_aa_female` | Table 8. The African-American female correlation is `−23/60 ≈ −0.383`, with the callback tie ranked in listed order |
| `BertrandMullainathan2004.background_concordant` | Our reading. A callback strictly increasing in mother's education gives a positive concordance |
| `BertrandMullainathan2004.control_group_sufficiency_zero` | Control. A callback that depends on the group only gives zero concordance, not a positive one |
| `BertrandMullainathan2004.table8_rejects_monotone` | Rasheed and Leroy are discordant, so no strictly increasing map fits African-American males |
| `BertrandMullainathan2004.gap_indep_address` | Our reading. With a common weight on the address, the group gap in predicted class is `(1 − w)(μ_W − μ_B)` at every address |
| `BertrandMullainathan2004.gap_vanishes_class_shown` | The gap is zero when the address shows class fully |
| `BertrandMullainathan2004.control_unequal_weights_gap_moves` | Control. With different weights by group the gap moves with the address |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| BM-D1 | An elision in BM-Q2 to BM-Q15 stands for the other column's text or for a hyphen-broken word. The broken words are "responsive" (Q2), "African-Americans" (Q4), "addresses" (Q7), "interpretation," and "average" (Q11), "background" (Q12) and "statistical" (Q13) | The cached text is a layout extraction. The suite's normalisation rejoins a broken word only when the next line starts flush, and these do not |
| BM-D2 | Table 8's correlations are reproduced from the printed table for African-American males (−0.595) and, with mid-ranks for the tie between Geoffrey and Brett, for White males (−0.0251). For the two female groups they are reproduced only when the tie in printed callback rates (Carrie and Kristen at 13.1, Tamika and Lakisha at 5.5) is ranked in the listed order, giving −0.318 and −0.383 | The authors presumably ranked unrounded rates. Only the two African-American values are in Lean |
| BM-D3 | Table 2 prints "African-Americans Favored" as 3.48 percent, while its parts 2.49, 0.45 and 0.53 sum to 3.47 | Rounding. The counts 33, 6 and 7 give 46 of 1,323 ads, 3.48 percent |
| BM-D4 | The note to Table 2 writes "Ho: WF = WB" for the table's "Ho: WF = BF" | Typo. Not relied on |
| BM-D5 | Page 1000 calls the 2.29-point gain for White names "statistically significant" at p = 0.0557 | Significant at 10 percent only. Not relied on |
| BM-D6 | "Log(median per capital income)" in Tables 3 and 6 is read as per capita income | Typo |
| BM-D7 | In Table 6 the African-American name coefficient in the log income column is −0.112 (0.152), against −0.031 in the other two | It is a level at log income zero, not comparable with the others. Not relied on |
| BM-D8 | PDF pp. 24 to 162 are a publisher's list of citing works, not part of the paper | The suite's range is set to journal pp. 991 to 1013 |
