# Chetty et al. (2022a), Social Capital I, reconstruction

Raj Chetty, Matthew O. Jackson, Theresa Kuchler, Johannes Stroebel and 18
co-authors, "Social capital I: measurement and associations with economic
mobility", *Nature* 608, 4 August 2022, pp. 108–121, doi
10.1038/s41586-022-04996-4.

## Source read

- The published open-access article, 30 PDF pages. PDF pp. 1 to 14 are journal
  pp. 108 to 121. PDF pp. 15 to 17 are the online Methods, p. 18 the
  acknowledgements, pp. 19 to 28 the Extended Data figures and tables, and
  pp. 29 to 30 the reporting summary. The Supplementary Information is a
  separate file and is not read. Pages are cited as PDF pages.
- Cached at `~/.cache/paper_a/sources/chetty_2022_sc1.pdf`, sha256
  `16920674a6efccb6c7f95457e757c78da53a01d6cd5f38be8462996440dd927a`.
- Every page was read on 16 October 2026 [F]. The text pages were read from
  the cached text, and the figure and table pages (PDF pp. 3, 7, 10, 19, 21,
  22, 24, 26) from images at 110 dpi.
- As for the companion record, the cached layout text
  `chetty_2022_sc1.txt` (sha256 `6e0035d6…1f90`) interleaves the two columns,
  and quotations are checked against a reading-order `pdftotext` 25.02
  extraction of the same PDF, `chetty_2022_sc1_read.txt`, sha256
  `d374a34c4ce0c43ebd3cc1806edfa171e4dc2eeba38df382db16fb7c47967f16`
  (CA-D1).
- The NBER working-paper version of this paper is not read.

## The measures, from the paper's own primitives

1. **Data** (pp. 1–2, 15). Facebook users aged 25 to 44 on 28 May 2022, in the
   United States, active in the last 30 days, with at least 100 US friends and
   a ZIP code. 72.2 million people, 84% of the US population of that age, and
   about 21 billion friendships among them (p. 16).
2. **SES** (pp. 2, 15). Not observed. The median household income of a
   user's block group, known for users with location history, is predicted by
   a gradient-boosted tree from variables seen for everyone, including phone
   model price and mobile carrier (CA-Q10). Users are ranked within birth
   cohort. Nothing from friends is used, so errors are not correlated across
   friends (CA-Q9). Parental SES is assigned for 31%.
3. **Friend shares and EC** (eqs. 1 to 3, p. 16). `f_{Q,i}` is the share of
   `i`'s friends in quantile `Q`, `IEC_{Q,i} = f_{Q,i}/w_Q`, and the EC of a
   community is the mean of IEC over its low-SES members (CA-Q13). With the
   median split, EC is twice the high-SES share of a low-SES person's friends.
4. **Childhood EC** (p. 16). From high-school friends within three cohorts and
   parental SES, and separately from Instagram users aged 13 to 17 with SES
   from ZIP income and phone price.
5. **Cohesiveness and civic engagement** (p. 17). Clustering, support ratio,
   spectral homophily, volunteering rate and density of civic organisations.

## Derivation chain of what A1 would use

- The shares 38.8% and 70.6% (CA-Q15) give EC of 0.776 for low-SES people,
  under-representation of 22.4%, over-representation of 41.2% for high-SES
  people, an average of 54.7%, and a gap in EC of 0.636 between the two
  groups (`shares_p3`).
- With equal friend counts and no homophily, half of a low-SES person's friends
  would be high-SES (CA-Q14, `control_equal_friends`). With high-SES people
  having 25.4% more friends, the paper's benchmark is `1.254/(1 + 1.254)`,
  55.6% (`benchmark_value`), and low-SES people have 30.2% fewer high-SES
  friends than that (CA-Q4). The 30.2% follows from the rounded benchmark
  (`fewer_p3`, CA-D2).
- Community EC weights people, not friendships. It equals the pooled share
  only when friend counts are equal (`EC_pooled_of_equal_degree`,
  `control_EC_mean_of_ratios`).
- Friend shares fall with SES distance for the bottom decile and peak near
  the own decile for the middle decile (CA-Q5, Extended Data Table 1,
  `row1_strictAnti`, `row5_interior_peak`).

## The empirical results, with the numbers A1 would cite

- **Homophily** (p. 2, Fig. 1). A one-point rise in own SES rank goes with a
  0.44-point rise in friends' mean rank (CA-Q3), 0.41 between the 10th and
  90th percentiles and 0.98 above the 90th. With the ten closest friends 0.46.
  By parental SES among high-school friends 0.31, matching Add Health (0.31).
- **Deciles** (p. 3, Extended Data Table 1). Top-decile friends are
  under-represented among bottom-decile people by 75% (`decile_p3`).
- **Geography** (p. 3). County EC below 0.58 in the bottom decile and 1.05 or
  more in the top decile. 42% of ZIP-level variation in EC is within counties.
  The mean county standard error is 0.004 against a signal s.d. of 0.18
  (p. 16).
- **Mobility** (pp. 6, 8–9). The county correlation of EC with upward mobility
  is 0.65 (s.e. 0.04), and 0.69 across ZIP codes (Extended Data Table 2). An
  EC rise of 0.5 goes with 8.2 percentiles of adult income rank. On the causal
  county effects of Chetty and Hendren, one unit of EC raises adult income by
  9.8 percentiles (s.e. 1.4), 30.7% of mean rank, with a signal correlation of
  0.44 (Extended Data Fig. 5). Growing up at the high-SES child's EC would
  raise a low-SES child's income by 19.5% (CA-Q11, `income_p9`).
- **Race** (pp. 9, 11–12, Table 2, Extended Data Table 3). There are no
  individual race data. In counties and ZIP codes over 80% or 90% of one race,
  EC correlates with race-specific upward mobility at 0.68 to 0.69 for white,
  0.61 to 0.65 for Black and 0.31 to 0.36 for Hispanic residents (CA-Q8).
  Areas with a larger Black population have lower EC, and EC accounts for the
  negative relation between Black share and mobility (CA-Q7). Controlling for
  EC removes most of the association of mobility with income segregation,
  racial segregation and the Gini coefficient (Table 2).
- **Other social capital** (pp. 5–8). Clustering, support ratio and civic
  engagement correlate weakly with mobility once EC is included (Fig. 3b).

## What could not be reconstructed

This list matches the coverage table in CLAIMS.md.

- The correlations and regressions of Figs. 1, 3, 4 and 5, Tables 1 and 2 and
  Extended Data Tables 2 and 3, and the SES model with its bootstrap standard
  errors. They rest on data we do not hold. The arithmetic printed from them is
  formalised (`income_p9`, `mobility_p6`, `scale_numbers`).
- Eq. (5), a mean of eq. (4) over residents, from which nothing printed follows.
- That spectral homophily lies in [0, 1]. Only `|λ| ≤ 1` is proved
  (`stochastic_eig_le_one`).
- The benchmark formula, formalised under our reading (CA-D3).
- Any statement about whether people see each other's SES. The paper measures
  homophily on predicted SES and does not model friendship formation.
- The decomposition into exposure and friending bias, which this paper defers
  to its companion (p. 3) and which is in the record `chetty_et_al_2022b/`.
