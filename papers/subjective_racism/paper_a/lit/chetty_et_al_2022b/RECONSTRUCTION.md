# Chetty et al. (2022b), Social Capital II, reconstruction

Raj Chetty, Matthew O. Jackson, Theresa Kuchler, Johannes Stroebel and 18
co-authors, "Social capital II: determinants of economic connectedness",
*Nature* 608, 4 August 2022, pp. 122–134, doi 10.1038/s41586-022-04997-3.

## Source read

- The published open-access article, 31 PDF pages. PDF pp. 1 to 13 are journal
  pp. 122 to 134. PDF pp. 14 to 17 are the online Methods, p. 18 the
  acknowledgements, pp. 19 to 28 the Extended Data figures and tables, and
  pp. 29 to 31 the reporting summary. The Supplementary Information is a
  separate file and is not read. Pages are cited as PDF pages.
- Cached at `~/.cache/paper_a/sources/chetty_2022_sc2_nature.pdf`, sha256
  `930d3b6496712168305177e1c3e2a21de65cd32736d3c68a50a90439b78b4ee9`.
- Every page was read on 16 October 2026 [F]. The text pages were read from
  the cached text. The figure and table pages (PDF pp. 15, 19 to 22, 25 to 28)
  and the formula for TCEC on p. 16 were read from images at 110 and 150 dpi,
  because their content is not in the text layer.
- The cached text `chetty_2022_sc2_nature.txt` (sha256 `da0264bb…5e6`) is a
  layout extraction that interleaves the two columns line by line, so no
  sentence that runs over a line is contiguous in it. For the quotation check
  this record uses a reading-order extraction of the same PDF made with
  `pdftotext` 25.02 in its default mode, cached as
  `chetty_2022_sc2_nature_read.txt`, sha256
  `108f6c6257f4a197073986ae46a9d17b6d03dc3ec90908a52e1780a6bae98685` (CB-D1).
- The NBER working paper 30314 (July 2022, 81 PDF pages,
  `chetty_2022_sc2.pdf`, sha256 `1eb14349…dcc7`) was also read in full, for
  differences only. No quotation is taken from it. What it has that the Nature
  version read does not is recorded as CB-D8 to CB-D13.

## The measures, from the paper's own primitives

1. **Data** (pp. 2, 14). Facebook users aged 25 to 44 on 28 May 2022, living
   in the United States, active in the last 30 days, with at least 100 US
   friends and a ZIP code, for whom at least one friendship can be assigned to
   a group. 70.3 million people, 82% of the US population of that age.
2. **SES** (p. 14). Not observed. For users with location history, the median
   household income of their census block group is computed. A
   gradient-boosted tree predicts it from variables seen for everyone (age,
   gender, language, relationship status, ZIP code, college, donations, phone
   model price, mobile carrier, Facebook usage). People are ranked within birth
   cohort. Incomes are not observed (CB-Q14), and nothing from a person's
   friends is used. High SES is above
   the median. Parental SES is the same prediction for linked parents, for 31%
   of the sample.
3. **Groups** (pp. 14–15). The six settings are high schools, colleges,
   employers, ZIP codes, religious groups and recreational groups. A friendship is
   assigned to a group when both friends belong to it. About 30% of
   friendships can be assigned.
4. **Individual EC** (eq. 1, p. 15). `IEC_{H,i} = f_{H,i}/w_H` with `w_H = 0.5`.
5. **Exposure** (eq. 3). `Exposure_{H,g} = w_{H,g}/w_H`, twice the share of
   high-SES members of the group.
6. **Friending bias** (eq. 4, CB-Q2). `1 − f_{H,i,g}/w_{H,g}`, one minus the
   ratio of the high-SES share of `i`'s friends in the group to the high-SES
   share of the group. It is a statistical term, not a statement about
   preferences (CB-Q3).
7. **Decomposition** (eq. 2). With `f_{H,i} = Σ_g φ_{i,g} f_{H,i,g}`,
   `IEC_{H,i} = Σ_g φ_{i,g} × Exposure_{H,g} × (1 − Friending bias_{H,i,g})`.
8. **No-homophily benchmark** (eq. 5). If high-SES people have `x_g > 1`
   times as many friends and there is no homophily, friending bias is
   `(1 − x_g)/(1 + (w_{H,g}/(1 − w_{H,g})) x_g) < 0`, which is −0.11 at
   `x_g = 1.254`, `w_{H,g} = 0.5`, and −0.10 at the high-school ratio 1.22
   (p. 16).

## Derivation chain of what A1 would use

- Eq. (2) is an identity given the friend-share weights and `w_{H,g} ≠ 0`
  (`eq2`). The paper excludes groups with `w_{H,g} = 0` (NBER SI B.4).
- Eq. (5) is printed without a friending model. Reading "no homophily" as a
  low-SES person's high-SES friend share of `w x/(w x + 1 − w)` reproduces it
  exactly (`eq5`), and its sign (`eq5_neg`) and printed values
  (`benchmark_values`). Friending bias is zero under random friending only
  with equal friend counts (`control_bias_zero_iff_equal_friends`).
- The decompositions of Fig. 3 use a representative agent (NBER eq. 14), the
  product of setting-level means. Because the product is not additive, the
  shares depend on the order of the steps (CB-Q11,
  `control_order_matters`). The printed shares are checked in
  `fig3a_shares` and `fig3b_average_of_orders` (CB-D3, CB-D4).
- TCEC is printed as `(1 − Exposure_g) × Exposure_g × (1 − Friending bias_g)`.
  Read with the low-SES share in the first factor it is maximised at a
  high-SES share of one half, as the text says (`tcec_max_at_half`). Read
  literally it is not (`control_tcec_printed`, CB-D5).

## The empirical results, with the numbers A1 would cite

- **Exposure and bias by SES** (pp. 4–5, Fig. 3a). Representative EC is
  0.826 for the low-SES and 1.534 for the high-SES person. Equating friending
  rates across settings gives 0.910 (12% of the gap). Then equating exposure
  gives 1.209 (54% cumulative). Equating bias closes the remaining 46%. In the
  other order bias explains 34% and exposure 54% (p. 16).
- **Settings** (p. 3, Extended Data Table 4 on p. 28). Mean friending bias of
  low-SES people is 0.043 in high schools, 0.047 in colleges, 0.054 at
  employers, 0.025 in recreational groups, −0.032 in religious groups and
  0.166 in neighbourhoods. For high-SES people it is negative in every setting
  (−0.282 in neighbourhoods to −0.066 in recreational groups). Low-SES members
  of religious groups show more bias in every other setting (Fig. 2d), which
  the authors read as the setting mattering more than the person (CB-Q4).
- **Areas** (pp. 5–6, Fig. 3b, Extended Data Table 1). Between bottom and top
  ZIP-code quintiles of EC (0.520 and 1.222), 73% is attributed to exposure,
  16% to bias and 11% to friending rates. Bias varies 3.3 times more across
  settings than across counties, exposure 3.3 times more across counties.
- **Mobility** (Table 1, p. 7). Across ZIP codes the elasticity of upward
  mobility is 0.236 on log EC, and 0.248 on log exposure and 0.185 on
  log(1 − bias). With county fixed effects 0.224 and 0.236. Across counties on
  the causal mobility effects of Chetty and Hendren, 0.116 and 0.339.
- **High schools** (pp. 8–9, Fig. 5a). The split-sample reliability of
  school-level bias is 0.58 with parental SES and 0.88 with own SES. Bias is
  higher with more AP and gifted-and-talented enrolment, in larger schools,
  and in schools near half high-SES (Extended Data Fig. 1a–d). **Bias is
  higher in more racially diverse schools**, by a diversity index and by the
  share of white students (CB-Q6, Extended Data Fig. 1e,f), with a suggested
  explanation that students of different racial backgrounds are less likely to
  be friends (CB-Q7). The same holds by own SES (Extended Data Fig. 2), and
  across colleges and neighbourhoods (CB-Q8, Extended Data Fig. 3). The
  magnitudes are given only as binned scatter plots (CB-D7).
- **Colleges** (p. 10). Bias rises with size and racial diversity. Greek life
  and dues are suggested as a source (CB-Q10). Community colleges without
  residents also show high bias.
- **Causal designs** (pp. 10–11, 17, Fig. 6, Extended Data Fig. 4). Within a
  school, cohorts with more top-quintile students have more such friends,
  slope 0.89 (s.e. 0.005), a marginal bias of 0.11 equal to the mean bias of
  0.11. The effect of exposure falls with leave-out bias, slope −0.61
  (s.e. 0.12), 0.91 after correcting for the 0.67 attenuation. By regression
  discontinuity on school-entry dates, EC jumps 0.39 in low-bias and 0.33 in
  high-bias schools, slopes 0.94 and 0.80. Identification is that cohort
  composition is orthogonal to other determinants of friending, and that
  nothing else jumps at the cut-off.
- **Interventions** (p. 12). Tracking and group size (Berkeley High), space
  (Lake Highlands High, where students split by lunchroom "depending on their
  social group or the options for low-cost and free lunch", CB-Q12), and new
  venues (CB-Q13).

## What could not be reconstructed

This list matches the coverage table in CLAIMS.md.

- The estimates themselves. Table 1, the reliabilities, the slopes of Fig. 6
  and Extended Data Fig. 4, and the adjusted R² of Extended Data Table 1 are
  regressions on data we do not hold. Their specifications, identifying
  conditions and the arithmetic read off them are formalised (`spec1`,
  `cohort_slope`, `rd_identifies`, `attenuation`, `variance_ratios`).
- The predictors of bias in Extended Data Figs. 1 to 3, racial diversity among
  them. They are binned scatter plots with no printed coefficient, standard
  error or control set. Only the diversity index is formalised
  (`diversity_le`), and the readings in CB-D7 are from the plots.
- The friending model behind eq. (5). Formalised under our reading (CB-D15).
- The averaging behind the 73% and 16% of Fig. 3b. Formalised as our reading
  (CB-D4).
- TCEC as printed. Formalised with the low-SES share in the first factor
  (CB-D5).
- The authors' race mechanism (CB-Q7). Stated in one sentence. Formalised as
  our two-race model (`raceOnlyBias_formula`, CB-D19).
- The setting benchmarks. The paper prints the benchmark only for a group half
  high-SES and for high schools. Our setting benchmarks use the national
  friend-count ratio (CB-D18).
- Whether the missing cross-SES friendships in diverse schools are cross-race
  friendships. No individual race is used. Racial composition is school-level
  (NCES), college-level (IPEDS) and ZIP-level (ACS).
- The Supplementary Information of the Nature article, which the main text
  cites for Supplementary Figs. 1 to 6 and Tables 1 to 3. The NBER version
  contains these, and its differences are in CB-D8 to CB-D12.
