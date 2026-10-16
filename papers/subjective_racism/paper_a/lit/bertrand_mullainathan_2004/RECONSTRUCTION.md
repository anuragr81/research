# Bertrand and Mullainathan (2004), reconstruction

Marianne Bertrand and Sendhil Mullainathan, "Are Emily and Greg More Employable
Than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination",
*American Economic Review* 94(4), September 2004, pp. 991–1013.

## Source read

- Cached at `~/.cache/paper_a/sources/bertrand_mullainathan_2004.pdf`
  (1,780,171 bytes), sha256 `796b4bd2…a18281`, with the text layer at
  `~/.cache/paper_a/sources/bertrand_mullainathan_2004.txt`, sha256
  `98b570f2…05ac`. The PDF has 162 pages. PDF pages 1 to 23 are the article,
  journal pp. 991 to 1013, ending with the reference list. PDF pages 24 to 162
  are a list headed "This article has been cited by", added by the publisher's
  site, and are not part of the paper (BM-D8). The suite's page range is
  991 to 1013. Segment `n − 1` of the text is PDF page `n`, which is journal
  page `n + 990`.
- Every page of the article, 991 to 1013, was read on 16 October 2026, so the
  evidence tag is [F]. The cached text is a layout extraction that interleaves
  the two columns line by line, so the pages were also read in column order
  from `pdftotext` without layout, as a reading aid in the scratch directory.
  Tables 6 and 8 (pp. 1003 and 1008) were read from 110 dpi images rendered
  with `pdftoppm`. Quotations are checked against the cached layout text
  (BM-D1). Pages are cited as journal pages.

## The experiment

1. **Resumes** (pp. 994–995). Templates from resumes posted on two job sites,
   for sales, administrative support, clerical and customer service jobs in
   Boston and Chicago, swapped between the cities. Each is classed as higher or
   lower quality, and higher-quality ones get some of experience while at
   school, volunteering, computer skills, certification, languages, honours or
   military service.
2. **Names** (p. 995). First names with the highest ratio of frequency between
   races in Massachusetts birth certificates, 1974 to 1979, checked by a street
   survey in Chicago (Table A1). Names rather than affiliations were chosen
   because affiliations "may especially convey more than race" (BM-Q3).
3. **Addresses** (p. 996). Real streets, up to three per zip code, assigned at
   random within a city. So zip code characteristics are independent of race
   and quality by design.
4. **Sending** (pp. 996–997). July 2001 to January 2002 in Boston and to May
   2002 in Chicago. Four resumes per ad, two of each quality, one of each with
   an African-American name. More than 1,300 ads and 4,870 resumes. The outcome
   is a callback by phone or e-mail.

## Results and what each is derived from

- **The gap** (Table 1, p. 997). Callback rates of 9.65 percent for White names
  and 6.45 percent for African-American names, a gap of 3.20 points and a ratio
  of 1.50 (BM-Q2, `table1_gap`). At 0.4 points per year of experience, a White
  name is worth about eight years (p. 998). The gap is about the same in both
  cities and for both sexes.
- **By ad** (Table 2, p. 999). Equal treatment at 88 percent of ads, mostly with
  no callback. Whites favoured at 8.4 percent and African-Americans at 3.5.
- **Returns to quality** (Tables 4 and 5, pp. 1000–1002). White names gain 2.29
  points from a higher-quality resume (p = 0.0557), African-American names 0.51
  points (p = 0.6084) (`table4_returns`). With quality predicted from resume
  characteristics, the ratios are 1.89 and 1.60. The standard deviation of
  predicted callback is 0.062 for White names and 0.037 for African-American
  names.
- **Address** (Table 6, p. 1003). A Whiter, more educated or richer zip code
  raises callbacks, by 0.54 points for ten points more college-educated. The
  interactions with an African-American name are −0.000 (fraction White),
  −0.002 (fraction college) and 0.008 (log income), none significant (BM-Q4).
- **Job and employer** (Table 7, pp. 1004–1006). No requirement predicts the
  gap. Equal Opportunity Employers and federal contractors show if anything a
  larger gap. The gap does not differ across occupations or industries.
- **Social background** (Section IV.B, pp. 1007–1009). The authors consider
  that a name may signal a disadvantaged background (BM-Q5), and in the
  extreme form that employers care only about that background and not about
  race (BM-Q6). Three tests bear on it.
  - The address test. If the names mainly signal background, the gap should be
    smaller at better addresses (BM-Q7). It is not (BM-Q4).
  - The soft-skills test. The gap should be larger in jobs that need
    interpersonal skills. It is not (BM-Q9).
  - The mother's education test (Table 8). For each first name, the share of
    Massachusetts births in 1970 to 1986 whose mother had finished high school.
    The African-American names chosen are below the African-American average
    (BM-Q11), so the names do carry background. Within each race and sex, the
    Spearman correlation between callback rate and mother's education is
    −0.318 (White female), −0.383 (African-American female), −0.0251 (White
    male) and −0.595 (African-American male), none significant at 10 percent
    (BM-Q10, `table8_aa_male`, `table8_aa_female`). The hypothesis predicts a
    positive correlation, and the authors conclude little evidence that
    background drives the gap (BM-Q12).
  - **The scope of the test** (p. 1007, n. 45). The authors note that
    African-Americans as a whole come from more disadvantaged backgrounds, and
    that a social class effect is "of independent interest" only if the names
    chosen carry a lower background than the average African-American (BM-Q8).
    So the test is of class conveyed by the name beyond race, not of class
    conveyed by race.
- **Theories** (Section IV.C, pp. 1010–1011). Customer and co-worker tastes
  should vary across occupations, and the gap does not. Employer tastes cannot
  explain lower returns to credentials. Statistical discrimination in which
  race proxies for unobservable skill (BM-Q13) predicts larger returns to
  credentials for African-Americans (BM-Q14), the opposite of the finding.
  Precision-based versions sit badly with identical, verifiable resumes. The
  authors float lexicographic search, in which an employer stops reading at an
  African-American name (BM-Q15), and coarser categories (n. 51).

## What is formalised, and from what

`lean/mathlib/BertrandMullainathan2004.lean`. The table "Coverage of the
source's results" in `CLAIMS.md` lists every table and every model the paper
reasons with.

- The arithmetic of every row of Table 1, every cell of Tables 2 and 4, the
  resume counts of Tables 1 and 3, and the numbers in the text that follow from
  them (`table1_gap`, `table1_rows`, `table1_counts`, `ads_per_callback`,
  `table2_counts`, `table4_all`, `table6_college`). Two printed values of
  Table 1 disagree with the rates (BM-D9).
- Table 2's classification of ads (`classify`, `table2_classes`).
- Two Spearman correlations of Table 8 from its printed ranks
  (`table8_aa_male` = −25/42, `table8_aa_female` = −23/60, BM-D2).
- Section IV.A, that a race-blind rule with randomised names treats the groups
  equally (`race_blind_equal`, `control_race_used`).
- Section IV.C, the four models the paper weighs against its findings. Employer
  taste (`taste_gap_region`), statistical discrimination after Phelps and Arrow
  and after Aigner and Cain (`phelps_larger_update`,
  `aigner_cain_smaller_update`), and lexicographic search
  (`lexicographic_zero_return`).
- **Our reading** of the Table 8 test (`background_concordant`,
  `control_group_sufficiency_zero`, `table8_rejects_monotone`) and of the
  Table 6 test in a linear predictor of class that the paper does not state
  (`gap_indep_address`, `gap_vanishes_class_shown`,
  `control_unequal_weights_gap_moves`).

## What could not be reconstructed

- The probit estimates of Tables 5 to 7, the tests of proportions and the
  symmetry test of Table 2, the means of Table 3, Table A1 and the "Average"
  and "Overall" rows of Table 8. They come from data that are not available,
  and the Table 8 averages are weighted by births whose numbers are not
  printed.
- The prediction procedure of Table 4, Panel B.
- The White female and White male correlations of Table 8. Both involve ties,
  and the reproduction of the printed values depends on how ties are ranked
  (BM-D2).
- The verbal arguments of Sections IV.A to IV.C on target racial mixes,
  familiarity, reverse discrimination, and customer and co-worker tastes.
- The four models of Section IV.C are formalised only in special cases, a
  deterministic threshold for taste and normal signal extraction for the two
  statistical models.
- The unpublished appendix on occupation and industry gaps (p. 1005), Fryer and
  Levitt (2003) on names and background, and Mullainathan (2003) on categories.
