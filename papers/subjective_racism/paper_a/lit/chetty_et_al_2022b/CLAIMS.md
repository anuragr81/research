# Chetty et al. (2022b), Social Capital II, claims

Evidence tag [F]. The Nature article was read in full, PDF pp. 1 to 31, on 16
October 2026, with the figure and table pages read from images. Pages are PDF
pages. PDF pp. 1 to 13 are journal pp. 122 to 134. Quotations are checked
against a reading-order extraction of the pinned PDF (CB-D1). The NBER working
paper 30314 was read for differences only and is not quoted.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CB-Q1 | 1 | "The other half is explained by friending bias—the tendency for people with low SES to befriend people with high SES at lower rates even conditional on exposure." | The empirical hook, in the Nature wording. The motivation scan quotes the NBER wording (CB-D2) |
| CB-Q2 | 2 | "We define a person’s friending bias in each group as one minus the share of friends they make in that group who have high SES divided by the share of people in the group who have high SES." | The measure, eq. (4), the definition `FriendingBias` in the Lean file |
| CB-Q3 | 2 | "the term friending bias should be interpreted in a statistical sense—denoting biased sampling from the pool of available peers—rather than as a normative statement about individuals’ preferences, as bias may be the result of institutional factors such as within-school tracking." | The measure is silent on mechanism. A1 may offer one without contradicting the source |
| CB-Q4 | 3 | "suggests that it is in substantial part determined by the nature of the institutions in which people interact—consistent with Blau’s theory of social structure[...]rather than entirely determined by preferences." | The authors' reading of the setting differences. The elision is a reference number |
| CB-Q5 | 5 | "Equating friending bias mechanically closes the remaining 46% gap in EC between the average person with low versus high SES." | The share of the gap attributed to bias in the exposure-first order, checked in `ChettyEtAl2022b.fig3a_shares` |
| CB-Q6 | 9 | "Friending bias is also higher in more racially diverse schools, as measured by a Herfindahl–Hirschman index (Extended Data Fig. 1e) or the share of white students in the school (Extended Data Fig. 1f)." | The empirical hook. A correlation across schools, with magnitudes only in figures (CB-D7) |
| CB-Q7 | 9 | "One potential explanation for the link between racial diversity and friending bias by SES is that, when low- and high-SES students have different racial and ethnic backgrounds, they are less likely to be friends." | The authors' suggested mechanism runs from race to SES. A1's runs from inferred class to race. The fact does not separate them |
| CB-Q8 | 9 | "higher levels of friending bias are associated with greater racial diversity across colleges and neighbourhoods (Extended Data Fig. 3)" | The gradient is not confined to high schools |
| CB-Q9 | 9 | "The explanatory factors considered in Extended Data Figs. 1–3 are not intended to be exhaustive, and much more remains to be learned about the determinants of friending bias." | The mechanism is open, in the authors' words |
| CB-Q10 | 10 | "feature significant Greek life, where the high costs of fraternity and sorority dues may generate friending bias on campus" | A cost of joining a sub-venue, offered as a suggestion from a student newspaper. Motivation for contributions 2 and 3, not evidence |
| CB-Q11 | 16 | "Because equation (2) is not additive, the share of the total difference attributed to friending bias versus exposure depends on the order in which we conduct each of the steps of the decomposition exercise." | "About half" is order-dependent, formalised as `ChettyEtAl2022b.control_order_matters` |
| CB-Q12 | 12 | "students clustered in one of three lunchrooms depending on their social group or the options for low-cost and free lunch" | A second price example, quoted by the authors from an architect's account. The scan's "only cost evidence" is not exact |
| CB-Q13 | 12 | "creating new programs and venues for cross-SES interaction (for example, through peer mentoring programs or internship programs) could help to reduce friending bias." | Venues as a lever on friending bias. No model of venue choice |
| CB-Q14 | 14 | "We also do not use direct information on individuals’ incomes or wealth, as we do not observe these variables at the individual level in our data" | SES is predicted from markers by the authors. Friending bias is measured on predicted SES, not on what friends see |
| CB-Q15 | 30 | "the tendency for low-SES people to befriend high-SES people at lower rates even conditional on exposure" | The reporting summary carries the wording the motivation scan uses (CB-D2) |

## Lean results

`lean/mathlib/ChettyEtAl2022b.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `ChettyEtAl2022b.eq2` | Eq. (2). Individual EC is the friend-share-weighted sum of exposure times one minus bias, for groups with `w_{H,g} ≠ 0` |
| `ChettyEtAl2022b.eq7` | Eqs. (6) and (7). The log of recomposed EC is the sum of the logs of exposure and of one minus bias |
| `ChettyEtAl2022b.bias_zero_of_random` | Random friending with equal friend counts gives zero bias |
| `ChettyEtAl2022b.eq5` | Eq. (5) follows from the no-homophily share `w x/(w x + 1 − w)`, our reading |
| `ChettyEtAl2022b.eq5_neg` | Eq. (5). The benchmark is negative when `x_g > 1` |
| `ChettyEtAl2022b.benchmark_values` | The printed −0.11 (`x_g = 1.254`) and −0.10 (`x_g = 1.22`) |
| `ChettyEtAl2022b.control_bias_zero_iff_equal_friends` | Control. Under no homophily, bias is zero only when friend counts are equal |
| `ChettyEtAl2022b.control_order_matters` | Control. The amount attributed to exposure depends on the order unless one factor does not differ |
| `ChettyEtAl2022b.fig3a_shares` | Fig. 3a. 12%, 54% cumulative, 46%, and in the other order 34% and 54%. Exposure alone in the first order is 42% |
| `ChettyEtAl2022b.fig3b_average_of_orders` | Fig. 3b. 73% and 16% are the means over the two orders. 11% for friending rates |
| `ChettyEtAl2022b.control_fig3b_single_order` | Control. Neither order alone gives 73% |
| `ChettyEtAl2022b.tcec_max_at_half` | TCEC read with the low-SES share is maximised at a high-SES share of one half |
| `ChettyEtAl2022b.control_tcec_printed` | Control. The printed TCEC, read literally, is higher at one quarter and negative at three quarters |
| `ChettyEtAl2022b.fig6_numbers` | Fig. 6. Marginal bias 0.11, corrected slope 0.61/0.67 ≈ 0.91 |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CB-D1 | Quotations are checked against `chetty_2022_sc2_nature_read.txt`, a reading-order `pdftotext` (25.02) extraction of the pinned PDF made on 16 October 2026, not against the cached `chetty_2022_sc2_nature.txt` | The cached text is a layout extraction that interleaves the two columns line by line. Of sixteen candidate quotations, three were found in it and all sixteen in the reading-order text. The new file is pinned by sha256 and the cached file is untouched |
| CB-D2 | The motivation scan's sentence "The other half is explained by friending bias—the tendency for low-SES people to befriend high-SES people ..." is the NBER abstract (NBER PDF p. 3). The Nature abstract says "people with low SES to befriend people with high SES" (CB-Q1), and the NBER clause appears in Nature only in the reporting summary (CB-Q15) | The substance is the same. A manuscript quotation attributed to Nature should use CB-Q1 |
| CB-D3 | "About half" each for exposure and bias depends on order. Exposure first gives friending rates 12%, exposure 42% and bias 46%, and the main text's 54% for exposure (p. 5) is cumulative with friending rates. Bias first gives bias 34% and exposure 54% (p. 16) | Arithmetic on the bars of Fig. 3a, `fig3a_shares`. The paper states the order dependence (CB-Q11) |
| CB-D4 | The 73% for exposure and 16% for bias across ZIP codes (p. 6) equal the means of the two orders of Fig. 3b (73.0%, 16.0%). Exposure first gives 68.8% and 20.2%, bias first 77.2% and 11.8% | Arithmetic on the bars, `fig3b_average_of_orders` and `control_fig3b_single_order`. Neither version says the figures are averaged, so this is our reading |
| CB-D5 | TCEC on p. 16 is read as `(1 − w_{H,g}) × Exposure_g × (1 − Friending bias_g)`, the low-SES share times EC, as the main text describes it on p. 9 | The printed first factor is `1 − Exposure_g`, and exposure is twice the high-SES share. Taken literally the measure is negative above a 50% share and peaks at 25%, which contradicts the text (`control_tcec_printed`). The NBER version (p. 35) prints the same formula |
| CB-D6 | The diversity index is `1 − Σ_i s_i²` over five race and ethnicity shares, as the caption of Extended Data Fig. 1 (p. 19) defines it, although the text and axis call it a Herfindahl–Hirschman index | One minus the Herfindahl–Hirschman index is a diversity index. The NBER notes (pp. 64, 66) define it as `100 × (1 − Σ s_i²)` while the axis runs from 0 to 0.8, and the axis is read |
| CB-D7 | The racial-diversity gradient is read from binned scatter plots at 110 dpi and is approximate. High schools, parental SES (Extended Data Fig. 1e), from about −1% in the least diverse ventile to about 1.5% to 2% in the most diverse. Own SES (Extended Data Fig. 2e), from about −2.5% to about 4%. Neighbourhoods (Extended Data Fig. 3), from about 2% to about 21%. Colleges (Extended Data Fig. 3), from about −1% to about 4% or 5%. By share white (Extended Data Fig. 1f), an inverted U peaking at about 2% to 3% near 30% to 40% white and falling to about −1.5% near 100% white | No coefficient, standard error or control is reported for racial diversity. The fitted lines are quadratics by OLS on ventile means, weighted by enrolment. No row should cite a slope |
| CB-D8 | NBER only (Supplementary Table 1, NBER p. 56). Across high schools, mean friending bias is 0.01 (s.d. 0.04) with parental SES and 0.03 (s.d. 0.06) with own SES. Across colleges 0.02 (0.03) and 0.04 (0.06) | It scales CB-D7. The high-school gradient of about three points across diversity ventiles is under one cross-school standard deviation. The Nature Supplementary Information, which should hold the same table, is not read |
| CB-D9 | NBER only (Supplementary Fig. 3, NBER p. 76). In colleges, with parental SES, friending bias rises nearly linearly from about 0.6% to about 9% across racial-diversity ventiles, and is an inverted U in the share white peaking near 40% to 55% white | Nature cites this as Supplementary Fig. 3 (p. 10) but it is not in the PDF read |
| CB-D10 | NBER only (SI C.1, NBER p. 45). Counties with more residential income segregation have higher mean friending bias, controlling for exposure, which the authors read as bias arising partly from segregation within ZIP codes | A structural source of measured bias that needs no inference from a marker |
| CB-D11 | NBER only (SI C.4, NBER p. 46). The high bias at Evanston Township High School is linked to accounts of racial and socioeconomic segregation there, and to prom tickets that low-income students cannot afford | A third price example after dues (CB-Q10) and lunch (CB-Q12) |
| CB-D12 | NBER only (introduction, NBER p. 4 of the text, PDF p. 8). Efforts against bias "may be particularly important in environments that have the type of large, diverse populations that we find are otherwise prone toward segregation and friending bias" | The Nature text has no such sentence. Not quoted |
| CB-D13 | Version differences. Phillips Exeter has "almost 80%" high-SES students in Nature (p. 9) and "more than 80%" in NBER (p. 20). Extended Data Fig. 5 reports top-ten-friends and assigned-friends slopes of 0.46 (0.006) and 0.47 (0.007) (p. 24), NBER Fig. 11 0.45 (0.006) and 0.47 (0.006) (p. 73). NBER eq. (20) omits the `1/N_{L,a}` that eq. (19) has | The Nature figures are used. None bears on A1 |
| CB-D14 | EC is 0.776 for low-SES people in the companion paper (p. 2) and 0.826 for the representative low-SES person here (Fig. 3a) | The second uses only the 30% of friendships assigned to groups and multiplies setting means. NBER footnote 9 (PDF p. 44) says so and gives the companion gap as 1.4 − 0.78 = 0.63 |
| CB-D15 | Eq. (5) is read as following from the share `w x/(w x + 1 − w)` | The paper prints the formula without a friending model. The reading reproduces the formula and both printed values (`eq5`, `benchmark_values`) |
| CB-D16 | Friending bias is measured with respect to the authors' predicted SES. Neither version says whether people observe each other's SES, and neither uses individual race | CB-Q14, and racial shares come from NCES, IPEDS and ACS at the group level (p. 19, p. 21). A1's premise that class is unseen is neither supported nor contradicted by the measure |
