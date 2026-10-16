# Chetty et al. (2022a), Social Capital I, claims

Evidence tag [F]. The Nature article was read in full, PDF pp. 1 to 30, on 16
October 2026, with the figure and table pages read from images. Pages are PDF
pages. PDF pp. 1 to 14 are journal pp. 108 to 121. Quotations are checked
against a reading-order extraction of the pinned PDF (CA-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CA-Q1 | 1 | "The share of high-SES friends among individuals with low SES—which we term economic connectedness—is among the strongest predictors of upward income mobility identified to date" | Why cross-class friendship matters, the motivation A1 borrows |
| CA-Q2 | 2 | "There is strong homophily, whereby individuals with higher SES are friends with higher-SES people." | Class homophily in friendship as an outcome. The paper does not say whether it comes from preference, inference or exposure |
| CA-Q3 | 2 | "A one percentile point increase in one’s own SES rank is associated with a 0.44 percentile point increase in the SES rank of one’s friends on average." | The size of class homophily |
| CA-Q4 | 3 | "Relative to that benchmark, low-SES individuals make 30.2% fewer high-SES friends than they would in the absence of homophily." | The benchmark adjusted for friend counts, checked in `ChettyEtAl2022a.fewer_p3` |
| CA-Q5 | 3 | "Connectedness is lower between deciles that are further apart." | Friendship falls with SES distance, the shape of A1's squared-distance utility, checked on the bottom decile in `ChettyEtAl2022a.row1_strictAnti` |
| CA-Q6 | 3 | "echoing Blau's observation that “persons cannot associate without having opportunities for contact”" | Exposure as a precondition, the role the companion paper gives it |
| CA-Q7 | 12 | "Areas with a larger Black population tend to have lower levels of EC (Supplementary Table 3), and this relationship accounts for the negative correlation between the share of Black residents and rates of mobility." | Race and EC are linked at the level of areas. No mechanism is offered |
| CA-Q8 | 9 | "economic connectedness remains highly correlated with economic mobility even conditional on race, which implies that segregation by race is unlikely to be the primary driver of the observed correlation between EC and mobility overall." | Conditioning on race means restricting to areas of one race. There are no individual race data |
| CA-Q9 | 15 | "We do not use any information from an individual’s friends to predict their SES, which ensures that errors in the SES predictions are not correlated across friends" | SES is the authors' prediction, and homophily is measured on it |
| CA-Q10 | 15 | "we estimate a gradient-boosted regression tree to predict these median household incomes using variables observed for all individuals in our sample, such as age, sex, language, relationship status, location information (ZIP code), college, donations, phone model price and mobile carrier" | Class is read from visible correlates by the authors themselves, the econometrician's version of A1's observer inference |
| CA-Q11 | 9 | "If low-SES children were to grow up in counties with EC comparable to the average high-SES child, their incomes would increase on average by 0.636 × 30.7 = 19.5%" | The stakes of EC, checked in `ChettyEtAl2022a.income_p9` |
| CA-Q12 | 12 | "a lack of economic connectedness may be a key reason that upward mobility is lower in areas with larger Black populations and greater inequality" | The authors' own link from race of area to cross-class contact |
| CA-Q13 | 16 | "We define the level of EC in community (county or ZIP code) c as the mean level of individual EC of low-SES (for example, below-median) members of that community" | Eq. (3), a mean over people, formalised with `ChettyEtAl2022a.control_EC_mean_of_ratios` |
| CA-Q14 | 3 | "If high-SES and low-SES individuals were to make friendships independent of SES (that is, there were no homophily by SES) and also were to make the same number of friends on average, then 50% of low-SES individuals’ friends would have high SES." | The benchmark with equal friend counts, `ChettyEtAl2022a.control_equal_friends` |
| CA-Q15 | 3 | "On average, 38.8% of the friends of below-median-SES individuals have above-median SES, whereas 70.6% of the friends of above-median-SES individuals have above-median SES." | The two shares from which EC 0.776 and the gap 0.636 follow, `ChettyEtAl2022a.shares_p3` |

## Lean results

`lean/mathlib/ChettyEtAl2022a.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `ChettyEtAl2022a.shares_p3` | 22.4%, 41.2%, 54.7%, EC 0.776 and the gap 0.636 from the shares 38.8% and 70.6% |
| `ChettyEtAl2022a.decile_p3` | Top-decile friends under-represented by 75% among the bottom decile |
| `ChettyEtAl2022a.benchmark_value` | The benchmark `1.254/(1 + 1.254)` is 55.6% to three figures |
| `ChettyEtAl2022a.fewer_p3` | 30.2% from the rounded benchmark, 30.25% to 30.26% from the unrounded one |
| `ChettyEtAl2022a.control_equal_friends` | Control. With equal friend counts the benchmark is one half and the shortfall 22.4% |
| `ChettyEtAl2022a.benchmark_strictMono` | The benchmark rises with the ratio of friend counts |
| `ChettyEtAl2022a.EC_pooled_of_equal_degree` | Eq. (3) equals the pooled share when every low-SES member has the same number of friends |
| `ChettyEtAl2022a.control_EC_mean_of_ratios` | Control. With unequal friend counts the mean of ratios (0.5) differs from the pooled ratio (0.2) |
| `ChettyEtAl2022a.income_p9` | 19.5%, 6.23 percentiles, about 37% of 17 percentiles, and 17.5% |
| `ChettyEtAl2022a.row1_strictAnti` | Extended Data Table 1A. The bottom decile's friend shares fall across deciles |
| `ChettyEtAl2022a.row5_interior_peak` | Extended Data Table 1A. The fifth decile's friend shares peak at decile 6 and are lower at both ends |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CA-D1 | Quotations are checked against `chetty_2022_sc1_read.txt`, a reading-order `pdftotext` (25.02) extraction of the pinned PDF made on 16 October 2026, not against the cached `chetty_2022_sc1.txt` | The cached text is a layout extraction that interleaves the two columns line by line. The new file is pinned by sha256 and the cached file is untouched |
| CA-D2 | "30.2% fewer" (CA-Q4) is computed from the benchmark rounded to 55.6%. From the unrounded `1.254/2.254` the shortfall is 30.26% | `fewer_p3`. The difference is in the third figure and no row should depend on it |
| CA-D3 | The benchmark `1.254/(1 + 1.254)` is read as the share `x/(1 + x)` of a low-SES person's friends who are high-SES when friending ignores SES and high-SES people have `x` times as many friends | The paper prints only the number. The reading is the one that reproduces eq. (5) of the companion paper at `w = 1/2` |
| CA-D4 | Race enters only through the racial composition of areas | "in the absence of individual-level data on race" (p. 9). The correlations with race-specific mobility are within areas over 80% or 90% one race (Extended Data Table 3) |
| CA-D5 | The analysis sample is 72.2 million here and 70.3 million in the companion paper, and the parent-linked subsample of 19.4 million is "about 27%" here (p. 16) and 28% there | The companion paper keeps only users with at least one friendship assigned to a group. The two percentages use different denominators |
| CA-D6 | "In the presence of homophily, EC ranges from 0 to 1" (p. 16) is read as a statement about the usual case, not a bound | Counties in the top decile have EC of 1.05 or more (p. 3) |
| CA-D7 | The fifth-decile row of Extended Data Table 1A is read as evidence that friend shares by SES are not ordered alike for every own decile | Shares are an outcome of meeting, choice and both sides' consent, and of high-SES people having more friends. The table does not separate preference from opportunity |
