# Levon, Sharma and Ilbury (2022), reconstruction

Erez Levon, Devyani Sharma and Christian Ilbury, *Speaking Up. Accents and
Social Mobility*, The Sutton Trust, London, November 2022.

## Source read

- The report as a 40-page PDF, downloaded from
  `https://www.suttontrust.com/wp-content/uploads/2022/11/Accents-and-social-mobility.pdf`
  (PDF metadata, created 2 November 2022). Cached at
  `~/.cache/paper_a/sources/sutton_trust_2022.pdf`, sha256 `7e305677…319d`,
  with its text layer at `~/.cache/paper_a/sources/sutton_trust_2022.txt`,
  sha256 `538a65f6…fc02a`.
- Read in full [F] on 16 October 2026, every PDF page from the text layer,
  and from page images rendered at 80 dpi with `pdftoppm` of the cover (PDF
  p. 1, no text layer), the figures on PDF pp. 12, 13, 16 to 19, 33 and 34,
  and the back page (PDF p. 40). The printed page is the PDF page minus 1.
- The contents page has production defects (ST-D1).
- Not read. Levon et al. (2021), Sharma, Levon and Ye (2022), Cardoso et al.
  (2019) and Friedman (2022), which the report cites for its background and
  for the accent-strength finding (ST-Q3).

## The report, from its own primitives

The report states no model. It has two parts.

1. **Background from the Accent Bias Britain studies** (PDF pp. 9 to 14). A
   hierarchy of 14 accent labels over 1969, 2004 and 2019 (Figure 1), a
   real-voice study with five accents in which listeners over 45 rate the
   working-class London accents lower for identical content (Figure 2), and
   four mitigating findings, namely trained lawyers, an awareness text, expert
   content and confidence (PDF p. 14). A strong MLE accent was the lowest
   rated recording and a milder MLE accent was rated well (ST-Q3).
2. **New surveys** (PDF pp. 15, 16). Four surveys, of university applicants
   (N = 511), university students (N = 1029), early-career professionals
   (N = 1014) and senior professionals (N = 1002), the first three by Savanta
   and the fourth by YouGov, representative by age, region and gender.
   Respondents agree or disagree on a 5-point scale with three statements, on
   self-consciousness about their accent, on concern that their accent could
   affect their future success, and on having been mocked, criticised or
   singled out for their accent, in an institution and in a social setting.
   Class is parental socio-economic grade, ABC1 against C2DE. Region is
   recoded to six regions. The analysis is multivariate regression, with only
   results significant at p < 0.05 shown. Respondents with foreign accents are
   excluded. Open-ended responses come from 27 applicants, 178 students, 81
   teachers and 38 lawyers and civil servants, and the authors say they are
   not necessarily representative.

## What each finding rests on

- **Life stages** (PDF pp. 16 to 19, Figures 3 to 6). Self-consciousness is
  24%, 35% and 23% among applicants, students and employees. Career anxiety is
  18%, 33% and 19%, with an overall 24% that is the sample-weighted mean
  (ST-D3). Institutional mockery is 29%, 30% and 25%. Mockery in social
  settings is 40%, 47% and 46%.
- **Applicants** (PDF pp. 20 to 23). Career anxiety is 22% for lower and 19%
  for upper parental grade (Figure 7), and 29% in the North against 13% in the
  South (10% without London). School mockery runs from 24% in the South to 44%
  in the North (Figure 8).
- **Students** (PDF pp. 24 to 29). Career anxiety is 41% in the North and 31%
  in the South (19% without London, Figure 9), and rises with year of study
  from 29% to 36% (Figure 10). Social mockery runs from 42% to 56% by region
  (Figure 11). Grade has no main effect once region and year are controlled,
  but interacts with region.
- **Early career** (PDF pp. 30, 31). Anxiety and workplace mockery are highest
  in the North (34% and 40%, Figures 12 and 13). Anxiety rises with age at
  career start, from 22% to 38% (Figure 14).
- **Senior managers** (PDF pp. 32 to 37). Anxiety is 21% for lower and 12% for
  upper parental grade (Figure 15), and falls with age from 21% to 9%
  (Figure 16). Mockery falls with age, 27, 25, 19, 20 in the workplace and 52,
  40, 33, 23 in social settings (Figure 17, ST-D5). By grade it is 29% against
  22% in the workplace and 55% against 34% in social settings (ST-Q13,
  Figure 18). Region has no main effect at this stage.
- **Accent modification** rests on open-ended responses (ST-Q6, ST-Q7,
  ST-Q9) and on the authors' reading of the senior managers' age pattern
  (ST-Q12). It is not a survey item (ST-D8).

## The structure we attribute, and the Lean

The identities behind the reported shares are formalised (`agree_shares`,
`bars_sum`, `overall_is_weighted`). The authors' inference from the age
pattern to modification is formalised in our reading, as a reported rate
equal to the share who have not modified times the rate at which they are
mocked. More modification lowers the reported rate
(`modification_lowers_reports`), but a fall in the rate of mockery across
cohorts produces the same figure (`control_not_identified`). The authors name
the two rival readings themselves, that younger people meet more bias or
perceive more of it, before preferring modification (PDF p. 33).

## What could not be reconstructed

- No coefficient, standard error or model specification is reported, so the
  statements of significance cannot be checked.
- The share of any group that has modified its accent is not measured.
- The accent-strength finding (ST-Q3) is cited from Cardoso et al. (2019),
  which is not read, and the report gives no numbers for it.
