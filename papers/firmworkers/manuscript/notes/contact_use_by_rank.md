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
