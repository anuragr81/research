# Reconstruction of Laurison and Friedman (2016)

**Source read.** Daniel Laurison and Sam Friedman, "The Class Pay Gap in
Britain's Higher Professional and Managerial Occupations", *American
Sociological Review* 81(4), 668-695. Read in full [F] from the accepted
author version on LSE Research Online (eprints.lse.ac.uk/66753), supplied by
the author on 8 Oct 2026 as `Laurison_Class pay gap_2016.pdf` on Drive. Page
numbers below are those of the accepted version (title page is p. 1); the
published pagination is not checked. The text was extracted from the PDF and
cached at `~/.cache/entry_contest/laurison_friedman_2016.txt`, which
`verify_laurison_friedman.py` reads.

**Why this paper.** The rejected manuscript's claim 7 said the model's class
gap "rising with (1 − μ)" matches Laurison and Friedman's finding that class
pay gaps are large in finance and law and near zero in science. The rebuild
needs to know what they measure before saying anything is consistent with it.

## The object

Two questions about Britain's NS-SEC 1 occupations (higher managerial,
administrative and professional):

1. **Access.** Is upward mobility more common into some NS-SEC 1 occupations
   than others?
2. **The class ceiling.** Once in, do the upwardly mobile earn what the
   intergenerationally stable earn, and if not, is the gap explained by
   education, human capital or work context?

## Data

- UK Labour Force Survey, July-September 2014 quarter, which for the first
  time asked the occupation of the main-earner parent when the respondent
  was 14 (p. 5, p. 9).
- n = 95,950 in the quarter; 43,444 aged 23-69 with origin data; 6,104 in
  NS-SEC 1; 3,510 NS-SEC 1 respondents with earnings after linking four
  quarters under a special licence; 3,377 with all covariates (pp. 9-10).
- Origin: parent's occupation mapped to the eight NS-SEC classes, collapsed
  to four groups: NS-SEC 1 (stable), NS-SEC 2 (short-range mobile), NS-SEC
  3-5 (mid-range), NS-SEC 6-8 (long-range) (p. 9). "Micro-class stable" when
  the respondent is in the parent's occupational group.
- Destinations: NS-SEC 1 as a whole, its sectors 1.1 (managers) and 1.2
  (professionals), and 63 occupations grouped into 15 groups (pp. 8-9).
- Earnings: natural log of weekly gross earnings; coefficients are
  exponentiated and read as percentages (p. 10, note 12).

## Derivation chain

1. **Origins and destinations (Tables 1-2, Figure 1).** Column shares of
   origin within each destination, against population shares.
2. **Earnings by origin (Figure 2).** Geometric mean weekly earnings by
   origin group within NS-SEC 1.
3. **Nested regressions (Table 3).** Model I: demographics, hours, wave.
   Model II adds education. Model III adds job tenure, training, health.
   Model IV adds region, industry, sector, firm size, 1.1 versus 1.2.
   Model V adds dummies for each occupation.
4. **Decomposition (Table 4).** Blinder-Oaxaca, NS-SEC 3-8 origins against
   NS-SEC 1 origins, with Model V's variables.
5. **Disaggregation (Figures 3-7).** The origin coefficients by gender,
   ethnicity, age, sector, and the 15 occupational groups.

## Findings, with the numbers as printed

- NS-SEC 1 origins are 26.6% of NS-SEC 1 against 14.1% of the population;
  routine origins 9.5% against 18.3% (p. 11).
- Recruitment into higher managerial occupations is wider than into the
  higher professions, although managers earn 24% more (p. 11).
- Medicine and law show micro-class reproduction of 21 and 18 times the
  population rate (p. 12, from Figure A1). 53% of doctors (Table 2: 52.3%;
  Table A7: 52.6%) and 16% of senior public-sector managers and
  professionals have NS-SEC 1 origins (p. 12). Fewer than 7% of doctors,
  veterinarians, dentists and physical scientists have routine, semi-routine
  or no-earner origins (p. 12; Table 2 gives 4.5, 5.0, 5.8 and 4.7).
- Stable earners: £844 a week; the long-range mobile earn 83% of that, £141
  a week less, about £7,350 a year (p. 14).
- With all controls the gap is 9-12% for NS-SEC 6-8 origins, about £4,342 a
  year, similar in size to the gender gap, where women earn 88.2% of
  otherwise similar men (p. 15).
- Decomposition: 46% of the gap explained, 54% not (p. 16). Education 45%
  of the explained part; human capital −5%; work context 31%, of which
  region 15%, firm size 10%, occupation 12% (Table 4).
- By occupation, with base controls: pay gaps near zero in science, academia
  and the built environment; around 20% in law, accountancy and finance;
  finance below 75% with full controls (pp. 20-21).

## Mechanisms the authors offer

Sorting into smaller firms and outside London (p. 17, p. 22); recruiters'
evaluation of "talent" by attributes rooted in middle-class socialisation,
and Rivera's cultural matching (p. 17); the mobile specialising in less
lucrative areas or self-excluding; class discrimination or homophily
(p. 18).

## What could not be reconstructed

- The coefficients for individual occupations in Table 3 are not printed
  ("Individual occupation coefficients not shown").
- The micro-class reproduction multiples (21 and 18) come from Appendix
  Figure A1, whose values are not printed.
- The text's firm-size figures, "only 27% of people from working-class
  origins are in 500+ person firms, as compared with 37% of people from
  NS-SEC 1 origins" (p. 17), do not appear in Appendix Table A3, which gives
  31.1% for the long-range mobile, 33.0% mid-range and 39.8% for the stable.
  They may come from the decomposition sample. Recorded as a discrepancy.
- The published pagination and any copy-editing changes.
