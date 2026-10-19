# Language vignette: what Lazear (1999) says and how it bears on the model (19 Oct 2026)

Exploration for a future vignette. No ledger row rests on this note.

## Source read

Lazear, Edward P. "Culture and Language." *Journal of Political Economy* 107,
no. 6, pt. 2 (December 1999): S95–S126. Read in full, 32 pages, through a local
OCR (`lit/ocr_pages.py`, rapidocr) of the journal PDF supplied by the author,
which has no text layer (Drive id 1kpQLzHMN6RVM4Q9R1LHZguoXG06QVVvs; cached as
`~/.cache/firmworkers/lazear_1999.pdf`, sha256 044b4d960ce095f1…; OCR in
`lazear_1999.ocr.txt`). The NBER working paper 5249 (1995) was also supplied
(Drive id 1TeDz7SeWtkuVJNcMYGemfh2ju9jk7kKN, sha256 87ac20f74cbc1ee6…); its embedded text
layer is garbled and Drive's OCR repeats it, so it was OCRed locally
(`lazear_1995_wp.ocr.txt`) but not read. Quotations below are from the OCR and
carry its errors; a record would check them against the page images.

## The model and results

- Random encounters, one per period; a trade worth one occurs only between
  individuals sharing a culture. Majority A has share p_a, minority B share p_b.
  Expected gains R_a = p_a, R_b = p_b, so "Individuals from the majority are
  richer than those from the minority." (S98)
- Learning the other culture costs t_i, distributed G. "the B acquires the A
  culture if and only if 1 − t_i > p_b", so a share G(1 − p_b) assimilates
  (eqs. 2, 3, S98). "the proportion of a minority group that becomes
  assimilated into the majority culture is decreasing in the proportion of the
  population comprising the minority group" (S99).
- Encounters within circles: "this is handled by defining the relevant group
  for analysis as that set of individuals with whom random encounters are
  equally (and most) likely." (S99)
- Language learning is human capital: "older immigrants are less likely to be
  fluent in English than younger ones. This is a direct prediction of Becker's
  (1975) human capital theory." (S105)
- Evidence, 1900 and 1990 U.S. censuses: fluency falls with the share of the
  county born in one's native country (Tables 2, 3). In 1900 the effect runs
  through learning in the United States (negative interaction with years in the
  US); in 1990 no interaction, which "favors the sorting interpretation"
  (S104): the non-fluent move to enclaves.
- Extensions: fragmentation keeps societies fragmented (S108–S109); neutral
  languages; critical mass and tipping, with an epidemic threshold analogue
  (S110–S113); optimal assimilation with a tax k_b = t_b − 1 + p_b on those who
  do not learn (S115); chauvinism (S116); voluntary migration and selection of
  slow learners into enclaves (S117–S119); ghettos efficient in one period,
  V_c > V_nc for every within-ghetto trade value below one (S120), but not
  necessarily across generations (S121–S122); government transfers: "a
  government transfer program places a floor on the amount that an individual
  earns ... Reducing the size of government transfers would increase the rate
  of assimilation in the society." (S122)

## Bearing on the model and the vignette

What Lazear already has, so the vignette cannot claim it:
1. A minority's incentive to acquire the majority skill rises as its share
   falls, from random encounters; this is the "joint draw" logic.
2. Closure as a restricted circle of encounters: his relevant group (S99) is
   the precedent for reach restricted to the own group, M64's σ = 1 case.
3. Language learning as human capital, and the enclave effect on it, with
   census evidence and the sorting caveat.
4. A floor that reduces investment in the human-capital channel (transfers,
   S122). The model has the same mechanism in another form: the bottom rung is
   a floor below every lottery prize, which makes the lottery dominate the
   downgrade (M33) and, where the return costs more than it regains, the return
   too (`top_displaced_prefer_lottery`).

What Lazear does not have, which is where the vignette's novelty lies:
1. A second channel. In Lazear the only way into the majority is to learn;
   contact is the payoff (trade), not a route. In the model the register is
   acquired only through contact with those above, and contact is the lottery
   for jobs.
2. Rank. Lazear has no ladder, no displacement and no firm; the model's claim
   that classes alone do not move the ranks below (V − L)/(1 + δ), because their
   own job is worth less than the lottery at any retraining price (M63), has no
   counterpart.
3. The persistence of a rank gap between groups under partial closure (M49,
   M51, the half-life once proved). Lazear's dynamics are of language shares
   across generations and tipping, not of positions on a ladder.
4. A firm that prices or subsidises the human-capital channel (M61–M63);
   Lazear's subsidy is a government's, set for the social optimum.

One tension to state: Lazear's ghettos raise one-period welfare because
within-group trades are worth something; the model's closure has no such gain,
since the draw only reallocates jobs. The vignette should not import his
efficiency result.

## Other papers on Drive for the vignette, not read

- Coate and Loury (1993), AER: group identity and human-capital investment
  through employers' beliefs; the likeliest answer to the human-capital
  objection.
- Laurison and Friedman (2016): the class pay gap within elite occupations in
  the UK; a test of M47's "the firm does not price the status group".
