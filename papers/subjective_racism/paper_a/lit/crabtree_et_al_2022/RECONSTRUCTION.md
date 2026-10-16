# Crabtree, Gaddis, Holbein and Larsen (2022), reconstruction

Charles Crabtree, S. Michael Gaddis, John B. Holbein and Edvard Nergård
Larsen, "Racially Distinctive Names Signal Both Race/Ethnicity and Social
Class", *Sociological Science* 9, 2022, pp. 454–472, doi:10.15195/v9.a18.

## Source read

- The published article, 19 PDF pages (journal pp. 454–472, PDF page `n` is
  journal page `n + 453`). Cached at `~/.cache/paper_a/sources/crabtree_2022.pdf`,
  sha256 `5ad9c1fc…7291`, with its text layer `crabtree_2022.txt`, sha256
  `128f7a8a…c408`.
- Every page read on 16 October 2026 from the text layer. Pages 6 to 9, which
  carry Figures 1 to 5, were also read as images at 110 dpi, because the text
  layer has the captions and not the plotted values. Evidence tag [F].

## The studies, from their own primitives

Names are used in audits and conjoint experiments as a treatment for race.
The paper asks whether they also move perceived class, a violation of
excludability or "information equivalence" (CR-Q3).

1. **Samples** (p. 4). Five surveys of U.S. residents. Study 1, Prolific,
   1,004 (February 2021). Study 2, Lucid Marketplace national quota sample,
   2,002 (May 2021). Study 3, Lucid, 1,035 first- or second-generation
   immigrants (May 2021). Study 4, MTurk, 5,631 (2014–2015). Study 5, MTurk,
   1,858 (October 2015). In total 11,530 respondents, 151,899 perceptions,
   1,000 name combinations (CR-Q5).
2. **Names** (p. 5). Studies 1–3 take the 10 most frequent racially
   distinctive first and last names for White, Black, Asian or Pacific
   Islander and Hispanic people, all combinations, 400 names. Study 4 takes
   80 White, 80 Black and 40 Hispanic first names, each with three
   distinctive surnames, 600 names. Study 5 takes a subset of Study 4
   (CR-D1).
3. **Outcomes** (p. 5). Studies 1–3 ask for likely education (four levels)
   and income (three bands), after questions on race and citizenship
   (CR-Q14). Study 4 asks for a free-text class, recoded to
   lower/working, middle, upper. Study 5 shows "[Name] has a [degree]" with
   a high school degree, a public four-year degree or an Ivy League degree,
   and asks for class.
4. **Estimation** (p. 6). OLS of the answer on dummies for the intended
   racial signal, with robust standard errors, plotted as predictions
   (CR-Q7). No covariates of the respondent enter the reported figures, and
   perceived race of each name is asked but not used as the regressor.

## Results

- Figure 1 (p. 6, read from the image). On education, Asian names are
  highest, just above White (both above a bachelor's degree), then Black,
  then Hispanic. On income, White and Asian names sit at about middle
  income, Black below, Hispanic lowest. The ordering holds in each of
  Studies 1–3 (CR-Q6).
- Figure 2 (p. 7). Within each intended group, individual first names
  differ. Richard, Robert and Mary (White), Roosevelt, Latonya and Tanisha
  (Black) and Thanh (Asian) differ from their group mean on education;
  Richard, Mary, Roosevelt, Lakisha and Guadalupe on income (p. 8–9).
- Figures 3 and 4 (pp. 7–8). In Study 4, White names sit between middle and
  upper, Black names between lower/working and middle, Hispanic below. Name
  means spread widely within each group, yet "the vast majority of the White
  distribution does not overlap with the vast majority of the Black
  distribution" (CR-Q8).
- Figure 5 (p. 9, read from the image, on a scale with lower/working = 1,
  middle = 2, upper = 3, values approximate). High school, White about 1.6,
  Black about 1.15. Public four-year, White about 2.0, Black about 1.6. Ivy
  League, White about 2.25, Black about 2.15. The gap narrows at the Ivy
  League degree and remains significant (CR-Q9, CR-Q10).

## Derivation chain of what Paper A1 would use

- The sample and name totals are arithmetic on the stated counts
  (`sample_total`, `names_total`).
- Group sufficiency of a name means names of one intended group are perceived
  alike (`GroupSufficient`), equivalently the perception factors through the
  group (`sufficient_iff_factor`), and then the within-group dispersion of
  name means is zero (`within_dispersion_zero`). Figures 2 and 4 show
  name-level means that differ within a group, which fails group sufficiency
  (`control_name_heterogeneity`).
- Our reading of Study 5. If perceived class mixed the stated education `s`
  with the group mean `μ` by a common weight `w`, the White–Black gap would be
  `(1 − w)(μW − μB)` at every education level (`gap_eq`, `gap_indep_signal`).
  The narrower gap at the Ivy League degree then needs a larger weight on
  that degree (`gap_shrinks`). The gap vanishes only at full weight
  (`gap_zero_iff`) and persists below it (`control_gap_persists`). The paper
  does not state a model of perception. The linear mixture is ours.
- The estimating model. With one dummy per intended group, least squares is
  fitted by the group means (`ols_dummies_group_means`), because residuals from
  group means sum to zero within each group (`resid_sum_zero`) and so are
  orthogonal to any function of the group (`cross_zero`). The plotted
  predictions of Figures 1, 3 and 5 are therefore mean answers by intended
  group (or by cell in Figure 5). Least absolute deviations would not give the
  means (`control_lad_not_mean`).
- Name selection. A name counts as distinctive for a group when its share of
  users in that group meets the threshold. Thresholds above one half make the
  assignment unique (`distinctive_unique`). The 0.45 threshold for Black
  surnames does not, and admits surnames used mostly by White people
  (`control_black_surname_threshold`).
- Study 5's "reduced considerably" compares gaps across education levels,
  which is a difference in differences of four cell means (`gap_change_did`).
  A gap that changes across levels rules out every common-weight mixture
  (`varying_gap_needs_varying_weight`).

## What could not be reconstructed

- The numerical predictions behind the figures. Values above are read from
  the images.
- Robust standard errors, confidence intervals and every `p < 0.05` test.
- Whether perceived class ordering matches the groups' actual class
  distributions. The paper does not compare perceptions with group data and
  says people "often misperceive economic reality" (CR-Q4).
- Respondent heterogeneity. Respondent class, race and other covariates are
  collected in Studies 2–5 (p. 4) but no result by respondent type is
  reported.
- The Study 4 recoding of free text and the question order of Studies 1–3,
  which are design facts with no structure to formalise.
