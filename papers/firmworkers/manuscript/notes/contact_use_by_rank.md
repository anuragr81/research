# Research note: who uses contacts, by rank (19 Oct 2026)

Exploration only. No ledger row rests on this note. Sources were triaged from
abstracts and from the accessible full text of one review; none has a `lit/`
record yet.

## The claim under test

MICRO_REVIEW claim 4 (bottom rung, `return_set_interval`, `top_displaced_prefer_lottery`):
the lottery takes the bottom (the return only regains a poor job, worth less than
the lottery's expected prize) and the top (regaining the job costs more than it is
worth), while the middle returns. Prediction: contact use is U-shaped in rank. The
earlier phrasing "the lowest invest in contact because credentials cannot bridge
their gap" was wrong about the mechanism and is withdrawn.

## Evidence read (Ioannides and Datcher Loury, JEL 42(4), 2004, pp. 1056–1093)

Verbatim, with pages:

- p.1058: "More educated job-seekers were also less likely to use friends and
  relatives. From appendix table 1, 20.8 percent of the unemployed job-searchers
  who check with friends and relatives have more than twelve years of schooling,
  and 23.3 percent had at most eight years. The respective percentages for those
  searching on the job are 44.8 and 11.6 percent. Clearly, these particular
  patterns are unlikely to be two sides of the same phenomenon. Indeed, different
  forces may be affecting workers with more schooling."
- p.1058: "James Elliott (1999) looks at workers with no more than twelve years
  of schooling and finds that those in high-poverty neighborhoods were
  substantially more likely (88 percent) to use informal job-search methods than
  those from low-poverty neighborhoods (74 percent)."
- p.1058: "about half of all workers heard about their current job through a
  friend or relative (Corcoran et al. 1980)"; "Truman Bewley (1999) estimated
  that 30 to 60 percent of jobs were found through friends or relatives."
- p.1065: "Marsden and Gorman present evidence that contacts are more likely to
  be used when filling managerial, professional, or sale/service positions and
  are less likely to be used in the public sector and in establishments that are
  part of multi-site firms."
- p.1065: "Elliott (1999) found differences in informal contacts by occupation.
  General laborers (defined as all nontechnical positions lacking managerial
  authority, 1990 Census Occupation Codes 243-902) and those in managerial
  positions who found their jobs through informal contacts had significantly
  lower wages than those who found their jobs without an active search."

Supporting (Dustmann, Glitz and Schönberg, NY Fed Staff Report 568, footnote 39):
"Corcoran et al. (1980), Datcher (1983), Marx and Leicht (1992), all report higher
usage for less educated job seekers. ... Rees and Schultz (1970) and Corcoran et
al. (1980) both find that informal search methods are used more often for
blue-collar than for white-collar occupations."

## Computation on the review's appendix table 1 (PSID 1993; illustration)

The table gives the schooling composition of the users of each search method and
of all searchers. Use rate of "checked with friend or relative" by schooling =
method frequency × (share of its users with that schooling) / (share of all
searchers with that schooling).

| schooling | unemployed searchers | on-the-job searchers |
|---|---|---|
| 8 years or fewer | 21.2% | 11.9% |
| 9 to 11 | 14.2% | 8.6% |
| 12 | 14.0% | 7.1% |
| 12 plus non-academic | 13.0% | 7.3% |
| 13 to 15 | 24.2% | 9.3% |
| BA or more | 10.0% | 16.5% |

Both columns are high at the bottom, low in the middle, and high again toward
the top; among the unemployed the rise is at some college rather than at the BA,
and the top cells are small (1.7% and 5.2% of searchers).

## Verdict

Consistent with the U-shape, not established. What is established in the
literature read: contact use is higher among the less educated and in
blue-collar occupations, and contacts are more used when filling managerial and
professional positions. No single study read reports use across the whole wage or
education range with the middle identified. The computed table does, for one
survey year, with small top cells and with "checked with a friend" as a search
method rather than the method that produced the job.

Two mismatches with the model: the data cover all job seekers, the model only the
displaced; and the model's "lottery" is the route that produced the job, while
the table counts a method used.

## Language side (abstracts only)

- NCES 1994–95: adults with a high school diploma took ESL classes at 14 percent
  against 8 percent for those without, so the less educated buy fewer classes.
- Chiswick and Miller (IZA DP 449, 2002): living among co-linguists lowers English
  proficiency, and lower proficiency and larger concentrations each lower earnings.
  A later study of German refugees with a residential natural experiment finds no
  enclave effect on language learning once sorting is handled.

## What a claim would need

1. A record with Lean for Ioannides and Datcher Loury (2004), and the appendix
   table pinned, so the computation above becomes an audited computation on
   recorded data.
2. A microdata test on the displaced alone: share of post-displacement jobs
   found through contacts, by pre-displacement wage decile. JLS's data have no
   search method; the CPS Displaced Worker Supplement has none either, as far as
   abstracts show. The 1992 and 1995 CPS job-search supplements (Bortnick and
   Ports 1992; Ports 1993) report methods by demographic group and may allow a
   cut by education.
3. The top of the U needs Granovetter's occupational figures (65.4% managerial,
   56.1% professional and technical, secondary sources) checked in the book.

## What the U-shape rests on (19 Oct, after the author's question)

Three bands, three different grounds:

1. **The bottom gambles** because the lottery is a random reshuffle of the
   entrants' jobs: its expected prize is the pool's mean, so anyone below the
   mean gains in expectation and anyone above loses (M13, `poolGain_pos_iff`).
   This is structural (M36, uniform displacement M39) and holds for any
   nondecreasing utility where the lottery dominates (M33); under curvature it
   needs risk neutrality or near it. Robust.
2. **The middle returns** only if the return can beat the lottery somewhere,
   which needs `b ≥ 1` for the band to be an interval (`return_set_interval`)
   and a return value above the lottery's. At the source's own parameters,
   `κ = 1`, `b = 2`, income in units of `x_0`, the return is worth at most `5/4`
   (`no_middle_band_at_source_parameters`), while the lottery is worth the
   ladder's mean `2` less `L`; so unless `L > 3/4` **there is no middle band
   and the whole ladder gambles**. The band exists with a cheaper return
   (`control_middle_band_with_cheap_return`, `κ = 1/4`). Fragile: it is a
   statement about `κ`, `L` and the unit (issue 1), not about the model's form.
3. **The top gambles** because the convex cost makes regaining a top job worth
   less than the bottom rung (`top_displaced_prefer_lottery`), which needs
   `κ(x − x_0)^{b−1} ≥ 1`, unit-dependent (M35), and the bottom rung as the
   fallback (M32); under a proportional step the top refuses the lottery (M44)
   and under a scale-free cost the top returns (`scale_free_return_beats_step`).
   Assumed, twice over.

So "contact at the bottom" follows from the structure; "credentials in the
middle" and "contact at the top" follow from the convex, unit-dependent cost and
the bottom-rung fallback, and the middle is empty at the source's parameters.
The resemblance to a class system is not a prediction the model makes on its
own; it is what the model says once those assumptions are granted and `κ` is
small enough relative to `L`.

## Primary sources read (19 Oct, supplied by the author)

1. **Corcoran, Datcher and Duncan (1980)**, "Information and Influence Networks
   in Labor Markets", ch. 1 of *Five Thousand American Families*, vol. VIII
   (Ann Arbor: ISR), pp. 1–37. Read in full, through Drive's OCR of the scanned
   volume (`~/.cache/firmworkers/psid_vol8_1980.drive_ocr.txt`; Drive id
   1A3--iSRCC4DV8mTUzKTJmsbJqdWpxtYt). PSID 1978, heads and wives under 45 who
   worked 250 hours or more in 1977; N = 1,499 white men, 988 white women,
   667 black men, 605 black women. Page numbers below are from the OCR's page
   markers and must be checked against the page images before a row rests on
   them; table numbers are unambiguous.
2. **Ioannides and Datcher Loury (2004)**, accepted manuscript of 29 June 2004
   (Drive id 1NBUHWLmoWfteOdLTLCjcbLQJ_B4ecolu, sha256 416cf4cafe43da8b…), 62 pages.
   Read: the stylised facts (section 2), the sociology of networks (3.1), the
   appendix tables. Not read: the theory sections 3.2 to 5. Every passage
   quoted above matches the published version (pp.1056–1093) word for word,
   apart from "From Table 1 Appendix A" for "From appendix table 1".

## What Corcoran, Datcher and Duncan show

Share of white men who heard about their current job from a friend or relative
(Table 1.7), by schooling: 8 or fewer grades 69.3%, 9–11 59.5%, 12 64.2%,
12 plus non-academic 51.8%, 13–15 53.4%, BA 38.5%, advanced degree 22.8%; mean
52.0%. By occupation (Table 1.6): professional 31.9%, manager 48.1%, clerical
and sales 51.6%, foremen and craftsmen 58.1%, other operatives 61.2%, transport
operatives 69.8%, labourers and household workers 74.6%. White-collar 41.6%
against blue-collar 62.0%. First jobs (Table 1.8) show the same pattern more
steeply: professional 20.0%, advanced degree 19.4%, unskilled labour and
service 68.4%.

Verbatim:
- "Regardless of race/sex group, workers with college and advanced degrees and
  professional workers were considerably less likely than the average worker to
  report having used informal information and influence channels in obtaining
  their current jobs (Tables 1.6 and 1.7)." (p.17)
- "Perhaps informal information and influence networks tend to substitute for
  the more formal credentials used by well-educated and professional workers."
  (p.17)
- "White men in blue collar occupations were more likely than other white men
  to have heard about their jobs from a friend, to have known someone on the
  Job, to have been helped by someone, to have both known and been helped by
  someone and to have been helped by someone with much say" (p.17)
- On Rees and Schultz's Chicago study: "Informal sources such as referrals from
  employees or other employers accounted for about half of all white collar
  hires and four-fifths of all blue collar hires." (p.3)
- On Granovetter's 282 professional and technical men in Newton: "About 57
  percent of these men had heard about their current job from a personal
  contact" (p.4); a sample of job changers in one suburb, not comparable with
  the PSID's 31.9% for professionals.
- Occupation explains much of the schooling gradient for white men: "When
  occupation is controlled (Table 1.10) the magnitude of the negative
  association between schooling and the information and influence measures for
  first and current jobs dropped sharply for white men." (p.25)
- Wages: knowing someone on the job gives white men about 9 percent more at
  first, "declining to zero within nine years for white men" (p.33); hearing
  from a friend has no significant wage effect except for black men.

## Revised verdict

The worker-side primary evidence shows contact use falling with schooling and
rank almost monotonically: highest for labourers and the least schooled, lowest
for professionals and the most schooled, managers in between and below the
mean. **There is no rise at the top.** The rise in the computed table from
Ioannides and Datcher Loury's appendix concerns on-the-job searchers' search
methods in 1993 (a method tried, not the source of the job), with small top
cells; Marsden and Gorman's employer-side finding that contacts are used to fill
managerial and professional positions is reported secondhand. Neither outweighs
Corcoran, Datcher and Duncan's tables for the source of the current job.

So the evidence matches the model's robust part, the bottom gambles, and does
not support its assumption-driven part, the top gambles. The pattern the data
show is "contact at the bottom, credentials at the top", with the two channels
as substitutes, which is Corcoran, Datcher and Duncan's own reading (p.17) and
the model's two channels exactly. In model terms it would hold if the
return were cheap enough at the top, that is under a scale-free cost
(`scale_free_return_beats_step`) or a small `κ` (issue 1), and not under the
unit-dependent convex cost that produces the top band. The data therefore bear
on issue 1: they favour a cost that does not make regaining a top job dearer
than the bottom rung.

Mismatches remain: the data cover all workers under 45, not the displaced, and
"heard about the job from a friend" is a source of information, not the
model's lottery.
