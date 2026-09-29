# Asch, S. E. (1946), "Forming Impressions of Personality"

*Journal of Abnormal and Social Psychology* 41, 258-290.

**Source.** `asch1946.pdf` is an original scan, 33 pages covering journal pp.
258-290. It was read in full on 2026-09-29. Tables 7 and 8 were transcribed from
the rendered pages (PDF p.14 = p.271 and PDF p.15 = p.272), not from the text
layer, because the text layer scrambles the table columns.

## Claims formalized

Asch's paper is empirical and makes no formal claim. The closest it comes is
Propositions I, Ia, Ib and II, the "Impression = a + b + …" schemata (pp.
258-259, 286-287). These are verbal contrasts between additive and structural
accounts, not statements with content that could be proved. **No Lean file is
written.** What can be checked is the data the project quotes. Those data are
the Experiment VI order comparison in Table 7 and the ranking in Table 8.

* **Experiment VI** (p.270). Group A (N = 34) heard `intelligent - industrious -
  impulsive - critical - stubborn - envious`. Group B (N = 24) heard the same six
  terms in reverse order.
* **Response instrument.** Check List I (Table 1, p.262) has 18 pairs, "mostly
  opposites". "From each pair of terms … the subject was instructed to select
  the one that was most in accordance with the view he had formed" (p.262).
  Table 7 gives the percentage choosing the listed member of each pair (p.263).
* **Table 7** (p.271) has four columns: Experiment VI I→E (N=34) and E→I
  (N=24), and Experiment VII I→Evasive (N=46) and Evasive→I (N=53).
* **Table 8** (p.272) gives the rank of "envious" in importance under the two
  Experiment VI orders.

## Result

`sympy/check_table7.py` holds my transcription of all 72 cells of Table 7 and all
of Table 8 (16/16 PASS).

* **Every number the project quotes is correct and in the right column:**
  * restrained 64/9
  * good-looking 74/35
  * serious 97/100
  * persistent 82/87
  * reliable 84/91
  * humorous 52/21
  * good-natured 18/0
  * important 85/90

  In each pair the first number is I→E and the second is E→I. My transcription
  agrees cell for cell with the audit's table in
  `notes/citation_audit/verify_hogarth_asch.md` §2.
* **Summary of the Experiment VI order effect across the 18 traits.** Take
  `d = I→E − E→I`, in percentage points.
  * `d > 0` for 14 traits and `d < 0` for 4. None is zero.
  * Mean `d` = 14.7, mean `|d|` = 16.9, median `|d|` = 14.5. The range is −7 to
    +55 (restrained).
  * The four traits with `d < 0` are exactly the four that the project says
    "barely move": reliable −7, important −5, persistent −5 and serious −3. All
    four move toward the envious-first order.
* **Size relative to sampling noise.** One E→I subject is worth 4.2 points and
  one I→E subject is worth 2.9. I ran an exact two-sided Fisher test on counts
  reconstructed as `round(p·N)`. This is a reconstruction, because denominators
  vary (see below).
  * Six traits reach p < .05: restrained (p ≈ 1e−5), good-looking (.003), happy
    (.010), humorous (.016), sociable (.031) and good-natured (.037).
  * The four "barely move" traits all have p ≥ .69. Their differences amount to
    one or two subjects.
* **Table 8.**
  * The counts sum to 34 and 24.
  * The modal rank of "envious" is 6th under I→E (13/34 = 38%) and 1st under E→I
    (7/24 = 29%).
  * In the I→E column the printed percentages 11, 11 and 39 do not match
    rounded counts (4/34 → 12, 13/34 → 38). Correct rounding would make the
    column total 101, so Asch evidently forced it to 100.
* **Denominators.**
  * 12 of the 18 E→I cells cannot be written as `round(100k/24)` for any whole
    number `k`. Examples are 91, 90, 87, 35 and 9.
  * 5 of the 18 I→E cells cannot be written as `round(100k/34)`.
  * Per-item non-response must therefore be common. Asch mentions it for
    Experiment I on p.263.
  * So the nominal N is only an approximate base for any single cell.
* **Experiment VII** (Table 7, right half). `d` is positive for 12 traits,
  negative for 5 and zero for 1. The mean is only +0.7. One trait reverses
  strongly: serious is 44 under I→Evasive against 100 under Evasive→I.

## What transcribing revealed

1. **The contrast the project draws within Table 7 is partly noise.** Four
   large swings are clearly beyond sampling error: restrained, good-looking,
   humorous and good-natured. The four "barely move" traits sit within one or
   two subjects of zero, and all four sit on the other side of zero. What the
   table supports is a contrast between large shifts and differences
   indistinguishable from zero. It does not show a graded unevenness measured
   to the percentage point.
2. **The order effect is not a uniform shift.** It is favourable to
   intelligent-first on 14 traits and slightly unfavourable on the four
   character traits. This fits Asch's Experiment I finding that reliability,
   importance, persistence and seriousness were "not affected by the transition
   from 'warm' to 'cold'" (p.264). Two of the traits Asch lists there as
   unaffected, good-looking (11) and restrained (14), are the largest swings in
   Experiment VI.
3. **Experiment VII barely replicates the aggregate direction.** Its mean `d` is
   +0.7, against +14.7 in Experiment VI. Asch himself calls its results "less
   clear" (p.272). He also reports subjects for whom "the final term completely
   undid their impression" (p.273).

## Bearing on Paper B

Asch is Paper B's oldest witness that reading order moves check-list marginals,
and he is a sound witness for that. He does not support either endpoint of the
ω family, for three reasons.

* His account is structural. "It is not the sheer temporal position of the item
  which is important as much as the functional relation of its content to the
  content of the items following it" (p.272).
* His footnote 5 says primacy "should be abolished — or reversed — if it does
  not stand in a fitting relation to the succeeding qualities, or if a certain
  quality stands out as central despite its position".
* His stimulus terms (6 cues) and his response items (18 check-list pairs) do
  not overlap. So Experiment VI has no cue-to-attribute correspondence of the
  kind Paper B's attribute locality assumes.

Hogarth and Einhorn classify Asch as simple, short series, EoS, primacy
(Appendix A row 14, T-p.31). They note that their model "shows that change of
meaning is not necessary for primacy" (T-p.29). In their model that primacy
comes from EoS anchoring on the first item (see
`literature/hogarth_einhorn1992`).

## Audit findings (2026-09-29)

This section summarises `notes/citation_audit/verify_hogarth_asch.md` §2 and
checks it against the transcription above.

* **A1-A10 (VERIFIED)** are confirmed. The trait list and order match, all eight
  quoted pairs are correct, and the columns are assigned correctly. The audit's
  side remark under A10 is also confirmed and extended: the four "barely move"
  traits all move toward envious-first, they are exactly the four traits with
  `d < 0`, and none differs significantly between orders.
* **A11 (VWC)** is confirmed. The check-list is a forced choice within a pair of
  "mostly opposites" (p.262). It never asks whether a single trait "fits".
* **A13 (CONTRADICTED)**, "the association cannot be formed from his data"
  (RL:539-540, CP:810-811). Confirmed as stated:
  * Each subject completed all 18 pairs.
  * Asch conditions on one check-list item in Experiment II (p.265), splitting
    subjects by whether they chose "warm" or "cold".
  * He notes that "the individual responses exhibit much stronger trends in a
    consistently positive or negative direction" (pp.264-265).

  What cannot be formed is the association from his *published* tables, which
  for Experiment VI are marginals only. A subject's *credence* about
  co-occurrence cannot be formed from any of his data, and neither can a prior.
* **A15 (VWC)**, "no account offered". Asch gives no trait-by-trait account of
  Experiment VI. He does give a content-based account of uneven effects in
  Experiment I (p.264, and footnote 2).
* **A18 (WRONG-LOCATION)** is confirmed. The quoted sentence begins on p.271
  ("When the subject hears the first term, a broad, …") and the quoted words are
  on p.272. Nothing is on p.273.
* **A20/A21** are confirmed. Footnote 5 gives two conditions for abolishing or
  reversing primacy: lack of fitting relation, and centrality. Its "(see
  Table 1)" should read Table 3.
* **A23 (VWC)** is confirmed with the numbers above. The ranks of "envious"
  (6th vs 1st) are modal ranks held by 38% and 29% of subjects. The printed 39%
  is forced rounding.
* **A24/A26 (VWC)**, "early terms dominate" and "what a later cue delivers
  depends on its position". Confirmed. The qualifications are:
  * 10 of 24 within-subject subjects reported no change (p.271).
  * For some Experiment VII subjects the final term "completely undid their
    impression" (p.273).
  * Footnote 5 applies.
  * The effect depends on the "functional relation of its content", not "sheer
    temporal position" (p.272).
* **A27 (CONTRADICTED)**, "one cue per trait across eighteen traits"
  (CP:1314-1315). Confirmed. There are 6 cues and 18 response items, and the
  two sets do not overlap.
* **A28 (CONTRADICTED)**, "those studies elicit a single evaluative level"
  (QA:63-65). Confirmed for Asch. He collects:
  * check-lists (Tables 2, 7, 9, 10 and 11);
  * free sketches;
  * importance rankings (Tables 3, 4, 5 and 8);
  * synonyms (Table 6);
  * resemblance judgments (Tables 12 and 13).
* **H16 (CONTRADICTED) for Asch**. The citation is to the "position channel, in
  which the observer weights the later cue less" (IO:62-65). Asch explicitly
  rejects position as such (p.272), so `Asch1946` should be removed from that
  citation.

### Proposed corrected wording (not applied)

* **CP:805-811 and CP:838-839, 849-851**. Replace "the percentage of subjects
  judging it to fit" / "the proportion of subjects judging it to fit the person
  described" with:
  > the percentage of subjects who, choosing within a pair of opposites, picked
  > the named trait as the one more in accordance with their impression

  Replace "the check-list asks whether a trait fits and never whether two traits
  go together, so the (prior) association cannot be formed from such data at
  all" with:
  > the published tables report only these marginals; subjects were never asked
  > how likely two traits are to go together, and nothing was elicited before the
  > list was read, so neither a credence about co-occurrence nor a prior
  > association can be recovered from them
* **CP:816-819 / CP:842-847** (the unevenness sentence). Keep the numbers, but
  replace "Some statistics swing enormously and others scarcely move, with no
  account offered of the difference" with:
  > Some statistics swing enormously; others differ by one or two subjects, and
  > those move slightly the other way. Asch offers no trait-by-trait account for
  > this experiment, though in Experiment~I he attributes uneven effects to the
  > content of the traits.
* **CP:1314-1319** (proposed 6.A text). Replace "\citet{Asch1946} is evidence
  that order moves marginals, one cue per trait across eighteen traits, and not
  evidence for either endpoint: his own account is that early terms set a
  direction for the reading of later ones, so that what a later cue delivers
  depends on its position" with:
  > \citet{Asch1946} is evidence that reading order moves marginals (eighteen
  > check-list items, after six cues read in either order), and not evidence for
  > either endpoint: his own account is that early terms set a direction into
  > which later ones are fitted, so that what a later cue delivers depends on how
  > its content relates to that direction rather than on its position as such
* **CP:298-299**. Replace "\citet{Asch1946} reports that early terms dominate"
  with:
  > \citet{Asch1946} reports that early terms set a direction for the impression
  > in most subjects, though not in all, and not where a later term fails to fit
  > or is central
* **QA:63-65**. Replace "those studies elicit a single evaluative level" with:
  > those studies report marginals (a single evaluative level in
  > \citet{HogarthEinhorn1992}; check-list percentages and rankings in
  > \citet{Asch1946})
* **RL:574-577**. Change the page of the "broad, uncrystallized" quotation from
  "pp.272-273" to "pp.271-272".
* **RL:591-594**. Replace "6th under `intelligent->envious`, 1st under the
  reverse" with "modally 6th under `intelligent->envious` (38% of subjects), 1st
  under the reverse (29%)".
* **IO:62-65 / CP:203-205**. Remove `Asch1946` from the position-channel
  citation (see `literature/hogarth_einhorn1992/README.md` for replacement
  wording).

## Not formalized

No Lean file is written, because Asch states no formal claim. The following
were not transcribed:

* Tables 2-6 and 9-13, beyond reading them for the audit.
* The free sketches.
* The Experiment VIII combination question (32 of 52 reported difficulty,
  p.274).
