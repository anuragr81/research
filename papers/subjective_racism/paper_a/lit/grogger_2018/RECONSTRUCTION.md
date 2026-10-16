# Grogger (2018), reconstruction

Jeffrey Grogger, "Speech and Wages", working paper, University of Chicago
Harris School of Public Policy, Version 8, 2 February 2018. 64 PDF pages.

This is not the article Paper A1 cites, Grogger (2011), "Speech Patterns and
Racial Wage Inequality", Journal of Human Resources 46. The paper read extends
that article with a larger sample, a sorting model and new robustness checks
(PDF pp. 2 to 3). What it says of the 2011 article is its author's description
and is tagged [C] for the article (GRG-D1).

## Source read

- Cached at `~/.cache/paper_a/sources/grogger_speech_wp.pdf` (706,674 bytes),
  sha256 `c0909b7b…3b8712994`, with the text layer at
  `~/.cache/paper_a/sources/grogger_speech_wp.txt`, sha256 `7d34ca33…499830c97`.
  The text has 65 form-feed segments. Segment `n − 1` is PDF page `n`, and
  segment 64 is empty. The PDF metadata title is "speech and wages v8", and the
  file was created on 2 February 2018.
- Every page, 1 to 64, was read from the text layer on 16 October 2026, so the
  evidence tag is [F]. The extraction garbles eqs. (1) and (2) and footnotes 19
  and 20 on PDF p. 26, which was read from a 110 dpi image rendered with
  `pdftoppm`. The tables on PDF pp. 41 to 57 are legible in the text layer.
  Pages are PDF pages. The printed page is the PDF page minus 1 on PDF pp. 3 to
  40, the tables carry no printed page, and the Data Appendix restarts at printed page 1 on PDF p. 58.

## The study, from its own primitives

1. **Question** (PDF pp. 2 to 5). How a worker's speech in his native language
   relates to his wages, and why. Dialects are African American Vernacular
   English (AAVE) and Southern American English (SoAE), set against Standard
   American English.
2. **Background** (PDF pp. 5 to 8). Listeners identify a speaker's race from
   very short clips (GRG-Q3). Native accent is acquired from linguistic peers
   during a sensitive period that ends before puberty concludes (GRG-Q4). Black
   and white listeners rate AAVE speakers lower in socioeconomic status,
   intelligence and attractiveness (GRG-Q5). These are the author's summaries
   of other literatures, [C] for those sources (GRG-D12).
3. **Data** (PDF pp. 8 to 12, Data Appendix). NLSY97 Round 15, fielded from
   September 2011 to June 2012. A formal job-search (JS) prompt and an informal
   happiest-moment (HM) prompt. Black and Southern white respondents got both,
   others one at random. Speech data exist for 4,225 respondents. Three NORC
   listeners per audio file report sex, race and region, so a speaker heard on
   both prompts has six reports.
4. **Speech and background** (Table 2, PDF pp. 12 to 13, 42). Among black men,
   those heard as black by no listener average 14.37 years of school, AFQT
   0.07, household income $47,700 and log wage 2.83. Those heard as black by
   all six average 11.46 years, AFQT −0.83, $24,100 and 2.46. Family income
   falls as more listeners hear the speaker as black or Southern (GRG-Q6).
5. **Wage regressions** (Section V, Table 3). Men out of school for two years,
   2005 to 2013, 6,735 observations. Each extra report of black speech lowers
   the log wage by 0.037, and of Southern speech by 0.046 (`table3_col2`). The
   flexible specification shows the penalty at four or more reports. The
   mainstream dummy is fewer than four of six reports (`mainstream`,
   `three_report_mainstream_iff`, `control_doubling_needed`). 29% of black men
   and 68% of Southern whites are mainstream. In column (5) the black dummy is
   −0.152 and black times mainstream is 0.136, so black mainstream speakers
   are 0.016 below comparably skilled whites with a standard error of 0.051
   (GRG-Q7, `table3_col5`). Whites heard as black are 0.162 below whites heard
   as mainstream, 21 workers.
6. **Controls** (Table 4, Appendix Tables 5 and 6). Adding AFQT, family income,
   family structure, maternal education and school type, then state-by-urban
   location effects, leaves the black mainstream premium at 0.109 and 0.119
   (Table 4, columns 1 and 2). The Southern premium falls to 0.034 and loses
   significance with location effects. Personality scales, skin colour,
   industry by occupation, and arrest and incarceration leave the black premium
   between 0.104 and 0.124. The dialect density of grammar and a code-shifting
   measure are insignificant and leave the premium unchanged. The availability
   of wages does not depend on speech (Table 5). The text's numbers for Table 4
   differ from the table's (GRG-D2).
7. **Sorting model** (Section VI.A, PDF pp. 25 to 26). `w_t = α_t + λ_t S + ε_t`
   (eq. 1), with `λ₁ > λ₀ ≥ 0`, and the worker chooses the task-intensive
   sector with probability `F[(α₁ − α₀) + (λ₁ − λ₀) S]` (eq. 2). So high-`S`
   workers sort into sector 1 (`w_diff`, `sorting`, `control_no_sorting`).
   Footnote 19 says that workers in both sectors need `α₁ < α₀` (`fn19`,
   `control_fn19`).
8. **Sorting results** (Table 6, PDF pp. 26 to 29, 47). With interaction
   intensity measured by Deming's social-skill index from O*NET and the
   intensive sector as its top quartile, black mainstream speakers sort into
   it (probit 0.598, s.e. 0.147) and earn 0.228 there (s.e. 0.087) against
   0.070 (s.e. 0.043) elsewhere. For six other tasks there is no premium
   difference between sectors (GRG-Q11).
9. **Reverse causation** (second Section VI, PDF pp. 29 to 31). Of 25 black men
   non-mainstream in 2006, 2 were mainstream in 2011, and of 9 mainstream in
   2006, 4 were not (Table 7, `table7`, GRG-Q12). Sorting into interactive work
   is the same at the first and the last job (Table 8).
10. **Conclusion** (PDF pp. 31 to 33). The author cannot isolate the effect of
    speech (GRG-Q13). He proposes customer and coworker discrimination as one
    explanation (GRG-Q14), alongside speech as a social skill (GRG-Q10).

## Whether speech is read as race, as class or as skill

- **As race.** The measure itself is listeners' reports of race (Section III.C),
  and the penalty falls on whites heard as black as well as on blacks (GRG-Q7).
- **As class.** The paper reports that listeners rate AAVE lower in
  socioeconomic status (GRG-Q5) and that speech correlates with family income,
  schooling and AFQT (GRG-Q6, Table 2). It does not frame the wage penalty as
  an inference about class, and it controls for these variables.
- **As skill.** The premium survives controls for AFQT and schooling (GRG-Q9),
  and it appears only in interaction-intensive work, not in work intensive in
  analytical, informational, reasoning, numerical or routine tasks (GRG-Q11).
  The author reads this against speech as general skill. He leaves open
  whether speech is a social skill or the object of customers' and coworkers'
  taste (GRG-Q10, GRG-Q14).

## Derivation chain of what Paper A1 would use

- The mainstream dummy and its doubling rule are definitions
  (`three_report_mainstream_iff`).
- The sorting prediction follows from `λ₁ > λ₀` and a strictly increasing `F`
  (`sorting`), and fails with equal skill prices (`control_no_sorting`).
- The reported percentages are sums and multiples of log-point coefficients
  (`table3_col2`, `table3_col5`, `table4_col2`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every result. What is short of general
is the following.

- The regression estimates and test statistics. The NLSY97 audio-linked data
  are restricted, and the estimates are taken as printed. The specifications
  and every number the text derives from the tables are formalised.
- Footnote 19 is formalised without the disturbances, and footnote 20 as a
  three-type example (`fn20_downward`). The general downward bias under normal
  errors is not formalised.
- Eq. (2) is proved for any strictly increasing `F`. That the normal cdf is
  strictly increasing is not proved here.
- Which of the two explanations, taste or social skill, holds. The paper does
  not separate them.
