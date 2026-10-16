# Mill and Stein (2012), reconstruction

Roy Mill and Luke C. D. Stein, "Race, Skin Color, and Economic Outcomes in
Early Twentieth-Century America", working paper, Stanford University, December
2012 ("This version updated December 30, 2012"). 60 PDF pages.

## Source read

- Cached at `~/.cache/paper_a/sources/mill_stein_2012.pdf` (898,596 bytes),
  sha256 `19df2e69…181bee2`, with the text layer at
  `~/.cache/paper_a/sources/mill_stein_2012.txt`, sha256 `63a7631a…948da61`.
  The text has 61 form-feed segments. Segment `n − 1` is PDF page `n`, and
  segment 60 is empty. The PDF was created on 31 December 2012 and modified
  on 14 January 2013. The printed page is the PDF page minus 1 from PDF p. 2
  on.
- Every page, 1 to 60, was read from the text layer on 16 October 2026, so the
  evidence tag is [F]. The extraction garbles eq. (5.2) on PDF p. 33 and
  renders Tables 1 to 3 on PDF pp. 51 to 53 in a private font encoding. Those
  four pages were read from 110 dpi images rendered with `pdftoppm`. The
  figures on PDF pp. 47 to 50 were read from their captions and labels. Pages
  are PDF pages throughout.
- The linking appendix that the text cites (PDF pp. 17, 52) is not in this
  version. PDF p. 46 reads "To be added" (MLS-D7). Dahis, Nix and Qian (2019)
  cite a 2016 version of the paper, which is not read (MLS-D1).

## The study, from its own primitives

1. **Two forms of race** (PDF pp. 1 to 2). A visual form, skin colour as the
   1910 enumerator coded it, Black or Mulatto, and a social form, racial
   affiliation. Passing changed the social form by changing social ties
   while skin colour stayed the same (MLS-Q1).
2. **Samples** (PDF pp. 13 to 16). All 1910 families in which some sons aged 3
   to 18 were coded Black and others Mulatto, 19,910 sons in 6,047 families
   (BMS), and the analogue for daughters. Benchmarks are a random sample of
   one-colour families and a sample of one-colour neighbours from the same
   enumeration districts. Classification was by appearance. When one parent
   was coded Mulatto and the other Black, 27.5% or 44.2% of the children were
   coded Black (Table 1a, read from the image). The authors note that the
   asymmetry may come from a classification that codes children of higher
   status households as Mulatto (MLS-Q6).
3. **Linking** (PDF pp. 16 to 19, Table 3). Sons are matched to 1940 by name,
   birth year and birth state with a link score. The stricter rule, used for
   all 1940 results, matches 6,613 of 61,055 searched sons. Literate sons and
   sons of literate, home-owning heads match more often.
4. **Colour within families** (Section 4, eqs. 4.1 to 4.3, Tables 4 to 8).
   `y_if = α + β darkskin_if + x_if γ + φ_f + ε_if`. In the population, boys
   coded Black are 9.3 points less literate in 1910 and attain 1.1 fewer years
   of school by 1940. Between brothers the gaps are 1.3 points and 0.2 years.
   The population earnings gap, −0.116 log points, falls to −0.028 with
   schooling, and between brothers it is 0.035 with the wrong sign and a
   standard error of 0.087 (Table 6). The authors conclude that family
   background, not colour, drives the colour gaps (`within_family`,
   `across_families`, `control_family_effect`).
5. **Passing** (Section 5, PDF pp. 30 to 36). Passing required looking and
   acting White, removing doubt about family history, and changing social
   ties, language and cultural references. Between 10% and 13% of sons coded
   Mulatto in 1910 are coded White in 1940, by matching rule. The measured gap
   between passers and non-passers is the return to passing plus selection,
   eq. (5.2) (`eq_5_2`, `measured_eq_return_iff`). Selection is reduced by
   controlling for schooling, by comparing brothers (eq. 5.4), and by both.
6. **Roy selection** (PDF p. 34). Because the variance of income among Whites
   was higher, those with higher earning potential should be more likely to
   pass (MLS-Q10). Our reading formalises this as a White log income steeper in
   earning potential, which makes the gain from passing rise in potential and
   the passers an upper set (`gain_strictMono`, `passers_upper`). With equal
   slopes there is no selection (`control_equal_slopes`).
7. **Results on passing** (Table 10, PDF p. 60). Log income gaps of 0.273
   (total) and 0.351 (labour) across all matched Mulatto sons, read in the text
   as 31% and 42% (`col1_percent`). With schooling, 0.185 and 0.269, read as
   20% and 31% (`col2_percent`). Between brothers, 0.191 (s.e. 0.097) and
   0.130 (s.e. 0.125), read as 21% and 14% (`col4_percent`). Between brothers
   with schooling, 0.054 and 0.001. The interaction of White in 1940 with years
   of schooling is 0.029 and 0.042 across all, and 0.065 and 0.100 between
   brothers (MLS-Q12). Selection across families accounts for "1/3-2/3" of the
   population gap (MLS-Q11), computed as 0.30 and 0.63 (`selection_share`,
   MLS-D4).

## Who passes, and what the paper says of those who remain

Passers were positively selected on education, and their brothers who did not
pass earned more than the average non-passer, so passers came from families
with higher earning potential (MLS-Q13, PDF pp. 35 to 36). Passing needed light
skin and the skills to act White (MLS-Q7). The paper states the consequence for
those who remain as a sentence, not as a measured or modelled result. Racial
sorting by economic outcomes "mattered for the long-run development of
African-Americans" (MLS-Q9). Through Bond (1931), it also gives a contemporary
account of the consequence. The blue-vein societies, clubs that admitted only
the light-skinned (MLS-Q5), disintegrated because those most like the dominant
group passed (MLS-Q4).

## Derivation chain of what Paper A1 would use

- Eq. (5.2) is an identity, adding and subtracting the counterfactual income of
  passers as Black (`eq_5_2`).
- A difference between brothers removes the family effect, and a difference
  across families keeps `φ_f − φ_g` (`within_family`, `across_families`).
- The Roy argument, in our reading, makes passers an upper set in earning
  potential when White income is steeper in it (`passers_upper`).
- The percentages of the text are `exp` of the log-point coefficients, checked
  by bounds from Mathlib's exponential series (`exp_sandwich`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every result. What is short of general
is the following.

- The estimates of Tables 4 to 10. The microdata are not available here, and
  the estimates are taken as printed. The specifications, eqs. (4.1) to (5.4),
  are formalised, and so is every number the text derives from the tables.
- The within-family estimator of eq. (4.2) is formalised for families of two
  brothers without the controls `x` (`within_estimator`).
- The attenuation argument after Griliches (1979) on PDF p. 29, which is
  stated in words without a model.
- The Roy argument as the authors state it, in variances. Our formalisation
  takes log income linear in one potential `s`, so that the higher White
  variance is a steeper White slope (`roy_variance`), which is the form Table
  10, column 3, tests.
- The linking procedure, since its appendix is absent from this version.
