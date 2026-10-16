# Kraus, Park and Tan (2017), claims

Evidence tag [F]: the accepted manuscript, 45 PDF pages, was read in full on 16
October 2026 from its text layer, with pages 43 and 44 also read as images.
Pages are PDF pages, equal to the manuscript's page numbers. The published
version is not read (KPT-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KPT-Q1 | 3 | "Specifically, we theorize that class signals (1) Occur frequently, rapidly, and accurately in the social perception process" | The first of three predictions. Class is read from the person's own signals |
| KPT-Q2 | 8 | "was significantly positively correlated with perceptions of social class made by observers based only on 60s of recorded behavior r(98) = .28, p = .005, or on viewing 20 Facebook photographs r(111) = .27, p = .005. Moreover these associations held after accounting for the race and gender of participants." | Accuracy from behaviour and photographs is a correlation near .27, and survives a control for the target's race |
| KPT-Q3 | 8 | "The observer estimates were considered to be correct if the quartile or half chosen by observers matched that of participants" | The accuracy metric, formalised as `blindHit` |
| KPT-Q4 | 11 | "Observers provided judgments of speaker social class that were significantly positively correlated with speaker social class r(211) = .22, p = .002. Moreover, this association held after accounting for speaker characteristics—race, gender, and vocal pitch—as well as geographic region characteristics" | Seven words give r = .22, and the speech cue carries class beyond the speaker's race |
| KPT-Q5 | 11 | "Perceptions of social class were also significantly positively associated with actual speaker social class in three of the four Census defined regions of the USA (all but in the Midwest" | The accuracy is not uniform across regions |
| KPT-Q6 | 12 | "observers showed above chance accuracy in judging the correct quartile, X2(1) = 15.38, p < .001, and half, X2(1) = 12.21, p < .001, of speaker social class based only on their speaking style" | Above chance, with Figure 2 showing about 36.5% against 25% and 62% against 50% |
| KPT-Q7 | 14 | "When individuals accurately signal and perceive social class in interactions with others, signals that communicate differences in social class are likely to create barriers for relationship formation across class boundaries." | The sorting argument, our formal reading `accurate_signal_blocks`. Class read directly, not through a group |
| KPT-Q8 | 15 | "describing a potential interaction partner using upper class signals (e.g., plans to travel to Europe during the summer) led to greater affiliation motivation among upper-class participants whereas describing a partner using lower class signals" | Côté et al. as cited [C]. Affiliation depends on the observer's own class, `own_class_ranking` |
| KPT-Q9 | 15 | "or who did not (i.e., a member of the Peruvian indigenous population not attending college)" | Bergeron and Zanna as cited [C]. The lower-class condition bundles an ethnic marker with class, so the cited result does not separate them |
| KPT-Q10 | 18 | "How social class group boundaries and stereotypes inform, and are informed by, perceptions of other categories is an important area" | The link between class and race perception is named as future research |
| KPT-Q11 | 21 | "When resources were visible, participants shared their resources with other resource-rich individuals, thereby perpetuating economic inequality. In contrast, when resources were invisible, participants shared regardless of the resources of their partners" | Nishi et al. as cited [C]. Behaviour toward partners differs when type is shown and when it is hidden, `invisible_signal_admits` |
| KPT-Q12 | 13 | "One implication of this logic is that in countries or cultures where societal economic inequality is lower, signals of social class might actually be harder to discern" | A conjecture. Accuracy of class reading depends on the spread of class |
| KPT-Q13 | 19 | "Aspirational motivations of relatively lower-class individuals might lead these individuals to identify less with their social class group or even to strategically adjust their behavior to signal the social class to which they aspire." | Class signals can be changed, unlike the marker in A1. Passing on the class signal rather than on the group |
| KPT-Q14 | 23 | "the stated motivations for school uniform policies, in more than 20% of schools in the USA, include the concealment of visible social class" | Institutions conceal class signals, an instance of class made unseen by design |

## Lean results

`lean/mathlib/KrausEtAl2017.lean`. The chance benchmark is the manuscript's
metric. The sorting theorems are our formal reading of a verbal argument.

| Theorem | What it checks |
|---|---|
| `KrausEtAl2017.blindHit_uniform` | With sample-quantile categories, a stimulus-blind guess is correct with probability `1/n` |
| `KrausEtAl2017.chance_quartile` | The 25% benchmark for quartiles |
| `KrausEtAl2017.chance_half` | The 50% benchmark for halves |
| `KrausEtAl2017.accurate_signal_blocks` | An accurate signal blocks affiliation across a class distance above the threshold |
| `KrausEtAl2017.invisible_signal_admits` | A signal that reads everyone alike admits a partner an accurate signal would block |
| `KrausEtAl2017.own_class_ranking` | Under `−(ĉ − cᵢ)²` the upper-class observer prefers the upper signal and the lower-class observer the lower |
| `KrausEtAl2017.control_nonuniform_categories` | Control. With population classes of unequal size a fixed guess beats 25% |
| `KrausEtAl2017.control_vertical_common_ranking` | Control. With a vertical utility both observers rank partners alike |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KPT-D1 | The version read is the accepted manuscript, cited as the 2017 article with that caveat | Page 1 says the manuscript "may not reflect the version that is eventually published". Numbers and wording are attributed to the manuscript only |
| KPT-D2 | The speaker counts of the seven-word study differ from Kraus et al. (2019) | This manuscript says 213 speakers reported education and occupation (p. 10). Kraus et al. (2019, study 2) use 189 with the class composite and 213 with perceived ratings. The same 568-observer panel underlies both (KT-D7) |
| KPT-D3 | The occupation figures on pp. 11–12 are occupation codes in which a smaller number is a higher status | The bottom half averages 4209.54 and the top half 1477.33, compared with sales (4000) and journalists (1500). The text calls the scheme an "occupation status coding scheme", which reads like a status score; the numbers behave as major-group codes |
| KPT-D4 | Figure 2 values are read from the bars, to about one percentage point | The text gives only χ² statistics |
| KPT-D5 | Citation typos not relied on | "Shirado et al., 2015" (p. 23) for the reference listed as Nishi, Shirado, Rand and Christakis (2015); "Brown-Iannuzi ... 2014" (p. 23) against Brown-Iannuzzi et al. (2015) in the list; "Monisaivis & Drewnowski, 2008" (p. 7) against Monsivais and Drewnowski (2009); Côté, Kraus et al. "2015" (p. 15) against the 2014 working paper in the list; "Sharif" against "Shariff" |
