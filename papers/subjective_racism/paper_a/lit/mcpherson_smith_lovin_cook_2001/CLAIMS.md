# McPherson, Smith-Lovin and Cook (2001), claims

Evidence tag [F]. The text layer of the published article was read in full,
journal pp. 415 to 444, on 16 October 2026 (`RECONSTRUCTION.md`). Pages are
journal pages. The review reports other studies, and a finding it attributes
to another study is [C] for that study.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MSC-Q1 | 416 | "Homophily is the principle that a contact between similar people occurs at a higher rate than among dissimilar people." | The definition A1 uses for class homophily in contact |
| MSC-Q2 | 415 | "Homophily in race and ethnicity creates the strongest divides in our personal [...] with age, religion, education, occupation, and gender following in roughly that order." | Race is the strongest divide in contact, the fact A1 sets out to explain. The elided word is "environments" (MSC-D2) |
| MSC-Q3 | 419 | "attempting to distinguish [...] between homophily effects that are created by the demography of the potential tie pool as baseline homophily and homophily measured as explicitly over and above the opportunity set as inbreeding homophily" | The two measures. Formalised as `inbreeding` over the baseline share `p`. The elision is the download stamp (MSC-D1) |
| MSC-Q4 | 419 | "in which similarity is based on informal, formal, or ascribed status, and value homophily, which is based on values, attitudes, and beliefs" | Status homophily against value homophily. A1's class homophily is status homophily on an unseen status |
| MSC-Q5 | 419 | "to homophily induced by other dimensions with which the focal dimension is correlated (which Blau 1977 called consolidation), and to homophily induced by personal preferences. Therefore, it does not in any sense indicate choice or agency purified of structural factors." | Inbreeding on race can come from a correlated dimension rather than from a preference over race. Formalised as `consolidation_pos` |
| MSC-Q6 | 420 | "the baseline homophily created by groups of different sizes is combined with the differences in racial/ethnic groups’ positions on other dimensions (e.g., education, occupation, income, religion) and the personal prejudices that often result from the latter" | The review's account of race homophily already names class differences between groups and prejudice that results from them. A1 must cite this and say what its inference adds |
| MSC-Q7 | 421 | "areas where African Americans are a larger part of the population also show larger African-American/Anglo [...] in education, income and other social class variables. Once the extent of these group differences (which Blau calls consolidation) is controlled, the [...] of population distributions again predicts the homogeneity of ties." | Class differences between races account for part of racial homogeneity in intermarriage. [C] for Blau et al. (1982). The elided words are "differences" and "effect" (MSC-D2) |
| MSC-Q8 | 421 | "This sociodemographic feature also leads to the highest level of inbreeding homophily (in-group deviations from a random assortment model) of all the characteristics that researchers have studied." | Race homophily is not only a matter of group sizes |
| MSC-Q9 | 422 | "Marsden (1988) found no significant social distance effect for race, after taking baseline and inbreeding into account. The key distinction appears to be same-different, not any more elaborated form of stratification." | For race the pattern is same-different, not graded. [C] for Marsden (1988). Formalised as `control_same_different_flat` |
| MSC-Q10 | 427 | "People are both more likely to confide in others who share their same educational level and become less and less likely to form such a tie as their difference from others’ achievement increases" | For education the pattern is graded in distance, the shape of A1's `(c_i − c_j)²`. [C] for Marsden (1988). Formalised as `distance_effect` |
| MSC-Q11 | 427 | "Indications are that it is the operation of US colleges as a locus of marriage formation and the cultural aspects of educational and occupational homophily, rather than the economic ones, that drive the structure." | The review's reading runs against an economic account of class homophily such as budget compatibility |
| MSC-Q12 | 435 | "If demographic similarity tends to indicate shared knowledge [...] we would expect people to associate with similar others for ease of communication, shared cultural tastes (Mark 1999), and other features that smooth the coordination of activity and communication." | The nearest statement to A1's inference. Visible demographic similarity indicates an unseen trait. The elision is the citation "(see this argument developed in Mayhew et al 1995)," |
| MSC-Q13 | 436 | "the closer a member is to the edge of a group’s niche (i.e., the more s/he is unlike the other members of the group), the more likely s/he is to leave the group." | Exit from a group rises in distance from its members. [C] for Popielarz and McPherson (1995) |
| MSC-Q14 | 433 | "Feld (1982) found that organizational foci produced ties that were twice as homogeneous as would be expected by chance." | Foci, the review's venues, produce baseline homophily. [C] for Feld (1982) |
| MSC-Q15 | 429 | "it unclear whether this homophily is due to actual political similarity or similarity on other social characteristics that are correlated with political beliefs" | The identification problem A1's test faces, stated for values. The source's "it unclear" is quoted as printed (MSC-D3) |

## Lean results

`lean/mathlib/McPhersonEtAl2001.lean`. The file formalises our reading of
structures the review describes in words. The review proves none of them.

| Theorem | What it checks |
|---|---|
| `McPhersonEtAl2001.baseline_minority` | Under random assortment a member of a category with share below one half has a smaller in-group share than a member of the other |
| `McPhersonEtAl2001.consolidation_inbreeding` | With ties formed only within class, race's in-group share exceeds its baseline by `nH nL (aH − aL)² / ((nH + nL)(nH aH + nL aL))` |
| `McPhersonEtAl2001.consolidation_pos` | That excess is strictly positive when race and class are correlated |
| `McPhersonEtAl2001.control_no_consolidation` | Control. With the same racial mix in both classes there is no inbreeding on race |
| `McPhersonEtAl2001.distance_effect` | A propensity `v − κ d²` with `κ > 0` falls as the difference grows |
| `McPhersonEtAl2001.control_same_different_flat` | Control. A same-different rule gives the same propensity at every nonzero difference |
| `McPhersonEtAl2001.control_distance_needs_kappa` | Control. With `κ = 0` there is no distance effect |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| MSC-D1 | The download stamp "by Columbia University on 02/14/05. For personal use only." sits in the middle of each page's extracted text, and an elision in MSC-Q3 stands for it | It is not part of the article |
| MSC-D2 | An elision in MSC-Q2 and MSC-Q7 stands for a word broken by a hyphen at a line end ("environments", "differences", "effect"), which the suite's normalisation does not rejoin when the next line is indented | No other word is omitted |
| MSC-D3 | MSC-Q15 reads "it unclear", with "is" missing | Quoted as printed. The sense is "it is unclear" |
| MSC-D4 | Footnote 5 ends "selections within voluntary organizations5]" with a stray superscript | Typo. Not relied on |
| MSC-D5 | The reference list has two entries "McPherson JM. 1981", and p. 432 cites "McPherson 1981, pp. 718–20", which matches the *Social Forces* 59 article (pp. 705–21) | Not relied on |
| MSC-D6 | Page 438 reads "discuss above’ the need", a stray quotation mark for a colon | Typo. Not relied on |
