# Mill and Stein (2012), claims

Evidence tag [F]. The full text of the working paper of December 2012, 60 PDF
pages, was read on 16 October 2026 from its text layer, with PDF pages 33 and
51 to 53 read from page images (MLS-D1). Pages are PDF pages. The printed page
is the PDF page minus 1 from PDF p. 2 on.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MLS-Q1 | 1 | "Passing for White meant that individuals changed their racial affiliation by changing their social ties, while skin color remained unchanged." | Passing changes the marker others see by changing contact, with appearance fixed |
| MLS-Q2 | 5 | "Mulattoes who passed for White earned 31%-42% more than [...] Because passers tended to be more educated than non-passers, the increase in earnings from being classified as White decreases when we control for education, though it remains considerable at 20%-31%." | The return to passing before and after a schooling control, Table 10, columns 1 and 2 |
| MLS-Q3 | 5 | "When comparing siblings we find that passers earned 14%-21% more than their brothers who did not pass." | Table 10, column 4. The labour-income estimate is not significant (MLS-D5) |
| MLS-Q4 | 9 | "the African-American community reached this pooling equilibrium over skin colors [...] Time was when there were blue-vein societies [...] have in great part folded their tents and crept quietly into the ranks of the whites." | The authors' gloss and Bond (1931) as quoted, [C] for Bond. Exit by passing emptied the venues that screened on the marker |
| MLS-Q5 | 9 | "Blue-vein societies, which he mentions in the quotation, were African-American social clubs that accepted only members whose skin was light enough so that their veins could be seen through it." | Footnote 10. A club that admits on a visible marker within the group |
| MLS-Q6 | 9 | "likely to be the outcome of some context-based classification that is more likely to classify children in households of higher status as Mulattoes." | Footnote 11. The recorded marker responds to household status, stated by the authors as unverified |
| MLS-Q7 | 11 | "not everyone could; both light skin color and sufficient skills to “act White” were necessary." | Passing needs a marker one can change and skills, so it selects |
| MLS-Q8 | 11 | "the higher social status he could enjoy in the African-American community relative to in the White community, the economic stability he gained from the relative scarcity of skilled African-American workers, and his richer social life among the higher ranks of African-Americans." | Myrdal's teacher, as the authors report him, [C] for Myrdal. Reasons not to pass that depend on one's own standing in the group |
| MLS-Q9 | 32 | "the racial sorting according to economic outcomes mattered for the long-run development of African-Americans." | The composition of those who remain, stated and not measured |
| MLS-Q10 | 34 | "we should expect those who had higher earning potential to be more likely to pass because the variance of income among Whites was higher." | The Roy argument, formalised in our reading as `passers_upper` |
| MLS-Q11 | 36 | "Selection across families accounts for 1/3-2/3 of the differences between passers and non-passers in the general population." | Computed as 0.30 and 0.63, `selection_share` (MLS-D4) |
| MLS-Q12 | 36 | "the returns to schooling are higher for those who are White." | Table 10, columns 3 and 6. The steeper slope that drives selection in `gain_strictMono` |
| MLS-Q13 | 37 | "We expect those who had more to gain from passing to have been more likely to pass, and indeed we see that passers were positively selected on education." | Positive selection into passing on schooling |
| MLS-Q14 | 3 | "lighter-skinned African-Americans completed 1.1 more years of schooling on average, and their weekly salaries were 11.4% higher." | Footnote 5. The 11.4% does not match Table 6 (MLS-D2) |
| MLS-Q15 | 3 | "which stands at 15 to 30% in our case" | Footnote 4. The match rates of Table 3 are 34% and 11% (MLS-D3) |

## Lean results

`lean/mathlib/MillStein2012.lean`, in the paper's notation. The Roy section is
our reading of the argument on PDF p. 34.

| Theorem | What it checks |
|---|---|
| `MillStein2012.eq_5_2` | Eq. (5.2). The measured difference is the return plus the selection term |
| `MillStein2012.measured_eq_return_iff` | The measured difference is the return exactly when the selection term is zero |
| `MillStein2012.within_family` | Eq. (5.4). Between brothers the family effect cancels |
| `MillStein2012.across_families` | Across families the difference keeps `φ_f − φ_g` |
| `MillStein2012.gain_strictMono` | Our reading. With a steeper White slope, the gain from passing rises in earning potential |
| `MillStein2012.passers_upper` | Our reading. Passers form an upper set in earning potential |
| `MillStein2012.exp_sandwich` | `1 + x + x²/2 + x³/6 ≤ exp x ≤ 1 + x + x²/2 + (2/9) x³` on `[0, 1]` |
| `MillStein2012.col1_percent` | Table 10, column 1. `exp 0.273` lies in `(1.31, 1.32)` and `exp 0.351` in `(1.41, 1.43)` |
| `MillStein2012.col2_percent` | Table 10, column 2. `exp 0.185` lies in `(1.20, 1.21)` and `exp 0.269` in `(1.30, 1.31)` |
| `MillStein2012.col4_percent` | Table 10, column 4. `exp 0.191` lies in `(1.21, 1.22)` and `exp 0.130` in `(1.13, 1.14)` |
| `MillStein2012.selection_share` | `(0.273 − 0.191)/0.273` lies in `(0.30, 1/3)` and `(0.351 − 0.130)/0.351` in `(0.6, 2/3)` |
| `MillStein2012.control_family_effect` | Control. Across families with different effects, the comparison misses `β` |
| `MillStein2012.control_equal_slopes` | Control. With equal slopes, passing is not selected on earning potential |
| `MillStein2012.control_log_not_percent` | Control. `0.351` log points exceed 35.1 percent by more than six points |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MLS-D1 | The version read is the working paper of December 2012, updated 30 December 2012, cited as such | The title page. Dahis, Nix and Qian (2019) cite a 2016 version from Arizona State University, which is not read. Nothing is attributed to it |
| MLS-D2 | The population colour gap in earnings is read from Table 6, column 1, as −0.116 log points | The text gives 10.9% less for darker sons (PDF p. 25), and `exp(−0.116) − 1` is about −0.1095. Footnote 5 gives 11.4% higher for lighter sons (MLS-Q14), which matches neither `exp(0.116) − 1` nor 10.9% |
| MLS-D3 | The match rates are 33.7% (Match 1) and 10.8% (Match 2) | Table 3, 20,574 and 6,613 of 61,055, read from the image. Footnote 4 says 15 to 30% (MLS-Q15), and PDF p. 19 says 34% and 11% |
| MLS-D4 | "1/3-2/3" (MLS-Q11) is read as 0.30 to 0.63 | `selection_share`. The lower end lies just below one third |
| MLS-D5 | Between brothers the passing premium is significant only for total income and only at 10% | Table 10, column 4, 0.191 (s.e. 0.097) and 0.130 (s.e. 0.125). With schooling, column 5 gives 0.054 and 0.001. The text (PDF p. 36) calls these "minor differences" |
| MLS-D6 | With the interaction of White and schooling, the White main effect is the premium at zero schooling | Table 10, columns 3 and 6, −0.041 and −0.442 for total income. The premium works through the schooling slope |
| MLS-D7 | The linking appendix is absent | The text and the note to Table 3 cite Appendix A. PDF p. 46 reads "To be added" |
| MLS-D8 | Some statistical comparisons are announced but absent | Footnote 25 says the neighbours sample is not fully digitised, and footnote 28 that tests of equal sibling and population gaps are in progress |
| MLS-D9 | Typos not relied on | Eq. (5.2) is described with "individual k" while the formula uses `j` (PDF p. 33). A sentence on alleles (PDF p. 28) is garbled. The notes to Tables 6 and 7 describe "difference in schooling" for income columns |
| MLS-D10 | Quotations and paraphrases of Bond (1931), Myrdal (1944) and Stephenson (1910) are the authors' | Tagged [C] for those sources. MLS-Q4 and MLS-Q8 are of this kind |
| MLS-D11 | The passing sample is all sons coded Mulatto in 1910 with brothers aged 3 to 18, matched to 1940, with passers those coded White in 1940 | Table 10, note 2. Footnote 35 assumes every son coded Mulatto could pass |
