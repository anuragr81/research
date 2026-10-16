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
| KPT-Q6 | 12 | "observers showed above chance accuracy in judging the correct quartile, X2(1) = 15.38, p < .001, and half, X2(1) = 12.21, p < .001, of speaker social class based only on their speaking style" | Above chance, with Figure 2 showing about 36.5% against 25% and 62% against 50% |
| KPT-Q7 | 14 | "When individuals accurately signal and perceive social class in interactions with others, signals that communicate differences in social class are likely to create barriers for relationship formation across class boundaries." | The sorting argument, our formal reading `accurate_signal_blocks`. Class read directly, not through a group |
| KPT-Q8 | 15 | "describing a potential interaction partner using upper class signals (e.g., plans to travel to Europe during the summer) led to greater affiliation motivation among upper-class participants whereas describing a partner using lower class signals (e.g., plans to get a job over the summer to help with family bills) led to greater affiliation motivation among lower-class participants [...] several correlational studies find that first generation college students report feeling more alienated from university environments than do their continuing generation counterparts" | Côté et al. and the studies of first-generation students as cited [C]. Affiliation depends on the observer's own class in both directions (`own_class_ranking`), the signals used are spending plans and budget constraints, and lower-class individuals in a high-class setting feel apart from it |
| KPT-Q9 | 15 | "or who did not (i.e., a member of the Peruvian indigenous population not attending college)" | Bergeron and Zanna as cited [C]. The lower-class condition bundles an ethnic marker with class, so the cited result does not separate them |
| KPT-Q10 | 18 | "How social class group boundaries and stereotypes inform, and are informed by, perceptions of other categories is an important area" | The link between class and race perception is named as future research |
| KPT-Q11 | 21 | "When resources were visible, participants shared their resources with other resource-rich individuals, thereby perpetuating economic inequality. In contrast, when resources were invisible, participants shared regardless of the resources of their partners" | Nishi et al. as cited [C]. Behaviour toward partners differs when type is shown and when it is hidden, `invisible_signal_admits` |
| KPT-Q12 | 13 | "One implication of this logic is that in countries or cultures where societal economic inequality is lower, signals of social class might actually be harder to discern" | A conjecture. Accuracy of class reading depends on the spread of class |
| KPT-Q13 | 19 | "Aspirational motivations of relatively lower-class individuals might lead these individuals to identify less with their social class group or even to strategically adjust their behavior to signal the social class to which they aspire." | Class signals can be changed, unlike the marker in A1. Passing on the class signal rather than on the group |
| KPT-Q14 | 23 | "the stated motivations for school uniform policies, in more than 20% of schools in the USA, include the concealment of visible social class [...] Do uniforms reduce social class signaling and class conflict, or alternatively, do the myriad and subtle ways in which behaviors signal social class render such uniform policies ineffective in concealing social class?" | A rule that removes one class signal, with the question whether other signals take its place. The shadow-market question, asked of class signals in schools |
| KPT-Q15 | 15 | "Interviews reveal that top-tier business firms rely on social class information in extracurricular activities and club memberships to seek out higher social class applicants" | Rivera as cited [C]. Screening on correlated social information, here memberships, rather than on class itself |
| KPT-Q16 | 21 | "sharing resources with a person with low economic standing may be perceived as risky behavior because such a sharing relationship has few direct economic benefits for the sharer" | The economic motive the manuscript gives for class-directed sharing, a vertical return rather than budget compatibility (`visible_share_rich`) |

## Lean results

`lean/mathlib/KrausEtAl2017.lean`. The chance benchmark is the manuscript's
metric. Every other theorem is our formal reading of a claim the manuscript
makes in words.

| Theorem | What it checks |
|---|---|
| `KrausEtAl2017.blindHit_uniform` | With sample-quantile categories, a stimulus-blind guess is correct with probability `1/n` |
| `KrausEtAl2017.chance_quartile` | The 25% benchmark for quartiles |
| `KrausEtAl2017.chance_half` | The 50% benchmark for halves |
| `KrausEtAl2017.accurate_signal_blocks` | An accurate signal blocks affiliation across a class distance above the threshold |
| `KrausEtAl2017.sorting_symmetric` | Under an accurate signal the sorting runs both ways |
| `KrausEtAl2017.invisible_signal_admits` | A signal that reads everyone alike admits a partner an accurate signal would block |
| `KrausEtAl2017.own_class_ranking` | Under `−(ĉ − cᵢ)²` the upper-class observer prefers the upper signal and the lower-class observer the lower |
| `KrausEtAl2017.accuracy_rises_with_spread` | With noise of fixed size, more spread in class makes the signal a better guide to class |
| `KrausEtAl2017.signal_upgrade_raises_competence` | If competence is judged by an increasing function of perceived class, an upward shift of the signal raises it |
| `KrausEtAl2017.visible_share_rich` | With visible resources the sharer prefers the richer partner |
| `KrausEtAl2017.invisible_share_indifferent` | With invisible resources every partner is read alike |
| `KrausEtAl2017.visible_gap` | An accurate signal shows the actual gap between two people, and a uniform signal shows none |
| `KrausEtAl2017.control_nonuniform_categories` | Control. With population classes of unequal size a fixed guess beats 25% |
| `KrausEtAl2017.control_vertical_common_ranking` | Control. With a vertical utility both observers rank partners alike |
| `KrausEtAl2017.control_noiseless_flat` | Control. Without noise, accuracy does not depend on the spread of class |
| `KrausEtAl2017.control_flat_stereotype` | Control. Without a link from class to competence, an upgraded signal gains nothing |

## Coverage of the source's results

The manuscript numbers three predictions (p. 3). Each has three parts in
Figure 1 (p. 43). The new and re-analysed data give the statistics below.

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Prediction 1, class signals are frequent, rapid and accurate | `blindHit_uniform`, `chance_quartile`, `chance_half`, `control_nonuniform_categories` | special case | The accuracy benchmark is formalised. "Frequent" and "rapid" have no structure beyond their words |
| Two prior studies, `r(98) = .28`, `r(111) = .27`, robust to race and gender | none | not formalised | Data. The `p` values need the `t` distribution |
| Quartile and half re-analysis, `χ²` statistics and Figure 2 | `blindHit_uniform`, `chance_quartile`, `chance_half` | special case | The chance benchmark only. The `χ²` values need the cell counts, which are not printed |
| Seven-word study, `r(211) = .22`, `β = .14`, region `F(3,1632)` | none | not formalised | Data |
| Accuracy harder to discern where inequality is lower (KPT-Q12) | `accuracy_rises_with_spread`, `control_noiseless_flat` | special case | Our reading as a signal-plus-noise model with fixed noise |
| Prediction 2a, sorting (KPT-Q7) | `affiliates`, `accurate_signal_blocks`, `sorting_symmetric`, `invisible_signal_admits` | special case | Our reading with a squared-distance threshold, which is A1's own form |
| Côté et al., own-class affiliation (KPT-Q8) | `own_class_ranking`, `control_vertical_common_ranking` | special case | Two classes, our squared-distance reading |
| Prediction 2b, stereotyping, competence follows status | `signal_upgrade_raises_competence`, `control_flat_stereotype` | special case | A strictly increasing map from perceived class to competence. Warmth is not formalised |
| Prediction 2c, class conflict from visible inequality | `visible_gap` | special case | Visibility of the gap only. The step from a visible gap to conflict has no structure beyond its words |
| Prediction 3a, dehumanisation | none | not formalised | No structure beyond its words |
| Prediction 3b, strategic sharing (KPT-Q11, KPT-Q16) | `visible_share_rich`, `invisible_share_indifferent` | special case | Partners valued by perceived resources |
| Prediction 3c, justifying ideology through dissonance | none | not formalised | No structure beyond its words |
| Pygmalion, aspirational signal change (KPT-Q13) | `signal_upgrade_raises_competence` | special case | The gain only. The cognitive cost the manuscript names (p. 19) is not formalised |
| Definition of class (p. 4) as income, education and occupation | none | not formalised | A definition with no structure used later in the argument |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KPT-D1 | The version read is the accepted manuscript, cited as the 2017 article with that caveat | Page 1 says the manuscript "may not reflect the version that is eventually published". Numbers and wording are attributed to the manuscript only |
| KPT-D2 | The speaker counts of the seven-word study differ from Kraus et al. (2019) | This manuscript says 213 speakers reported education and occupation (p. 10). Kraus et al. (2019, study 2) use 189 with the class composite and 213 with perceived ratings. The same 568-observer panel underlies both (KT-D7) |
| KPT-D3 | The occupation figures on pp. 11–12 are occupation codes in which a smaller number is a higher status | The bottom half averages 4209.54 and the top half 1477.33, compared with sales (4000) and journalists (1500). The text calls the scheme an "occupation status coding scheme", which reads like a status score; the numbers behave as major-group codes |
| KPT-D4 | Figure 2 values are read from the bars, to about one percentage point | The text gives only χ² statistics |
| KPT-D5 | Citation typos not relied on | "Shirado et al., 2015" (p. 23) for the reference listed as Nishi, Shirado, Rand and Christakis (2015); "Brown-Iannuzi ... 2014" (p. 23) against Brown-Iannuzzi et al. (2015) in the list; "Monisaivis & Drewnowski, 2008" (p. 7) against Monsivais and Drewnowski (2009); Côté, Kraus et al. "2015" (p. 15) against the 2014 working paper in the list; "Sharif" against "Shariff" |
