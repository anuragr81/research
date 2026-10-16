# Levon, Sharma, Watt and Perry (2020), reconstruction

Erez Levon, Devyani Sharma, Dominic Watt and Christina Perry, *Accent Bias in
Britain. Attitudes to Accents in Britain and Implications for Fair Access*,
project report, Queen Mary University of London and the University of York,
first published February 2020. ESRC-funded.

## Source read

- The report as a 28-page PDF, downloaded from
  `https://accentbiasbritain.org/wp-content/uploads/2020/02/Accent-Bias-Report-2020.pdf`
  (PDF metadata, created 26 February 2020 in Microsoft Word). Cached at
  `~/.cache/paper_a/sources/accent_bias_britain_2020.pdf`, sha256
  `a2bb6ae7…4941`, with its text layer at
  `~/.cache/paper_a/sources/accent_bias_britain_2020.txt`, sha256
  `27bf7e07…e48c`.
- Read in full [F] on 16 October 2026, every PDF page from the text layer.
  The figure pages (PDF pp. 17 to 21) and the pages with no text (PDF pp. 2,
  26, 27) were also read from page images rendered at 80 dpi with `pdftoppm`.
  PDF p. 1 is the cover, PDF p. 3 the contents, PDF p. 28 the back cover. The
  printed page is the PDF page minus 2.
- Not read. The journal articles on the same studies, Sharma et al. (2019),
  Levon et al. (2021) and Sharma, Levon and Ye (2022), and the March 2020 file
  that the Sutton Trust report cites (AB-D1).

## The studies, from the report's own primitives

The report states no model. It reports four studies.

1. **Accent labels** (PDF p. 11). 827 respondents from a market research
   firm, aged 18 to 79, representative of the four nations, balanced for
   gender, rate 38 accent labels on a 1 to 7 scale for prestige and for
   pleasantness, then give background and attitude measures, among them the
   motivation to control a prejudiced response (MCPR).
2. **Real voices** (PDF pp. 11 to 13). 1062 members of the public hear ten
   20-second mock answers to law-firm interview questions, in a formal
   register with standard grammar, recorded by ten men aged 18 to 25, two
   native speakers of each of RP, Estuary English (EE), Multicultural London
   English (MLE), General Northern English (GNE) and Urban West Yorkshire
   English (UWYE). Some questions are expert and some are not. Each listener
   hears two versions of each accent and no answer or speaker twice, and rates
   five items on a 10-point scale (quality, expert knowledge, success as a
   lawyer, would like to work with, overall).
3. **Recruiters** (PDF p. 13). 61 lawyers and graduate recruiters in leading
   UK-based international law firms rate the same stimuli, with a quality
   manipulation that makes some answers subtly better than others.
4. **Interventions** (PDF pp. 14, 15). 480 members of the public rate RP,
   MLE and EE candidates, each answering one question, after one of five
   interventions (raising awareness, identifying irrelevant information,
   committing to objective criteria, accountability, multiculturalism) or a
   control.

The report gives findings in words and figures, with no coefficients or
tests (AB-D4).

## What each finding rests on

- **The hierarchy of labels** (PDF pp. 17, 18, Figure 1) rests on study 1
  compared with Giles (1970) and Coupland and Bishop (2007). Standard and
  middle-class accents are rated highest for prestige, Birmingham lowest, and
  working-class and ethnic-minority accents fill the bottom ten (AB-Q6).
- **Smaller gaps with real voices** (PDF p. 18, AB-Q15) rests on study 2.
  Older listeners rate every accent lower and rate EE and MLE lower than other
  accents. Younger listeners do not distinguish the accents. The authors read
  the age pattern as a life-stage effect, since Coupland and Bishop found it
  too (Figure 2).
- **Expert content** (PDF p. 19, Figure 3) raises the rating of every accent
  (AB-Q7). For older Southern and Midlands listeners EE and MLE stay lowest
  with expert answers, and the bias is "particularly prevalent" among
  listeners of higher class (AB-Q8, Figure 4).
- **MCPR** (PDF pp. 20, 21, Figure 5). Listeners with higher MCPR rate every
  accent higher, and among older Southern and Midlands listeners only those
  with low MCPR show the dispreference for EE and MLE (AB-Q9).
- **Recruiters** (PDF pp. 21 to 23, Figure 6) rate by quality regardless of
  accent, matching a pre-test of the written answers (AB-Q10). EE and MLE
  still get the lowest single ratings of high-quality answers (AB-Q11), and
  lawyers rate RP marginally lower on likeability (AB-Q12). Age, region, class,
  MCPR and years in law have no effect on their competence ratings. MCPR
  raises their likeability ratings.
- **Interventions** (PDF pp. 23, 24). Raising awareness narrows the gap
  between MLE or EE and RP most consistently. The report says all
  interventions are beneficial (AB-D7).

## The structure we attribute, and the Lean

The report's premise is that accent reflects social background and not
competence (AB-Q3), and that listeners use the stereotype of the accent's
group to judge competence (AB-Q2, AB-Q4). We read this in two ways, both ours.
As inference, a prior on competence attached to the accent and an answer read
as a signal. Identical answers are then rated differently when the priors
differ and the answer does not reveal competence
(`identical_answer_penalty`), expert content raises every accent
(`expert_raises_all`), the penalty is zero when the answer reveals competence
(`revealing_no_penalty`), and the label survey is an uninformative signal
(`uninformative_is_label`). As attention, a rating that puts weight `γ` on
content and `1 − γ` on the stereotype. The penalty is then `(1 − γ)` times the
stereotype gap (`rating_penalty`), which falls as attention rises
(`penalty_antitone_in_control`) and vanishes for the lawyers
(`full_control_no_penalty`, `quality_only_no_penalty`). The authors' reading of
the age pattern as life stage rather than cohort is formalised against a
cohort model (`cohort_sections_differ`), and one reading of the lawyers' lower
likeability of RP as a rating relative to expectation
(`higher_expectation_lower_rating`). The printed sample identities are checked
(`voices_sample`, `voices_design`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every finding. Those not formalised,
and the special cases, are these.

- No estimate, standard error or test can be reconstructed, because the
  report gives none (AB-D4). The label hierarchy and its stability, the lowest
  single ratings of EE and MLE among lawyers, and the ranking of the five
  interventions rest on figures with no printed values (AB-D3), so none is
  formalised.
- The larger bias among listeners of higher class (AB-Q8) is a finding with no
  stated structure, and is not formalised.
- The solidarity reading of the lawyers' likeability ratings has no stated
  structure, and is not formalised.
- The inference reading uses two competence types, a special case of a
  graded competence the report does not specify.
- The report does not separate class from ethnicity. The bottom of the label
  ranking mixes working-class accents with ethnic-minority accents (AB-Q6),
  and the PDF p. 7 box describes MLE as originating among working-class
  young people "of different ethnicities". Both EE and MLE are working-class
  London accents, so the contrast between them is not a class contrast.
- The report rates competence and likeability. It does not measure contact,
  avoidance or venue choice, and it says it does not cover informal
  interaction (AB-Q17).
