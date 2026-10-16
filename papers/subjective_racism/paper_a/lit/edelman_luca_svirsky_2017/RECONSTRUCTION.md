# Edelman, Luca and Svirsky (2017), reconstruction

Benjamin Edelman, Michael Luca and Dan Svirsky, "Racial Discrimination in the
Sharing Economy: Evidence from a Field Experiment", *American Economic Journal:
Applied Economics* 9(2), 2017. The version read is the accepted manuscript
marked "forthcoming", cited as such (ELS-D6).

## Source read

- 36 PDF pages, cached at
  `~/.cache/paper_a/sources/edelman_luca_svirsky_2017.pdf` (348,609 bytes),
  sha256 `b28a4e16…6179ac77`, with its text layer at
  `~/.cache/paper_a/sources/edelman_luca_svirsky_2017.txt`, sha256
  `bd6388cb…c3469`. The text has 37 form-feed segments. Segment `n − 1` is PDF
  page `n`, which is also the printed page number. Segment 36 is empty.
- Every page, 1 to 36, was read from the text on 16 October 2026, so the
  evidence tag is [F]. PDF pages 24 and 25, Figures 1 and 2, have no text
  beyond their titles and were read from 80 dpi images rendered with
  `pdftoppm`. Nothing beyond these pages is attributed.
- The layout. Text PDF pp. 1–20 (with the invited postscript on pp. 19–20),
  references pp. 21–23, figures pp. 24–25, Tables 1–7 pp. 26–32, Appendix
  Tables 1–4 pp. 33–36.

## The design

1. **Market** (PDF pp. 2, 4–5). Airbnb hosts decide whether to accept a guest
   after seeing a name and often a picture. A guest writes to the host to ask
   whether a listing is available.
2. **Sample** (PDF pp. 5–6). All listings in Baltimore, Dallas, Los Angeles,
   St. Louis and Washington, D.C. as of July 2015, one listing per host chosen
   at random. Host race, gender and age were coded from the profile picture by
   Mechanical Turk workers, past guests' race from review photos by Face++ and
   workers, and listing and census-tract variables were linked.
3. **Treatment** (PDF p. 7). Twenty guest accounts identical except for the
   name, five African-American female, five African-American male, five White
   female and five White male names from Bertrand and Mullainathan (2004), with
   no picture. A survey confirms that the names signal race (Appendix Table 1).
   Each host got one message from one random account about a weekend eight
   weeks ahead.
4. **Procedure** (PDF p. 8). About 6,400 messages from 7 to 30 July 2015.
   Responses over 30 days were coded into categories. The outcome is
   `1(Host Accepts)`, read as the pooled "Yes" categories (ELS-D1).
5. **Estimating equation** (Tables 2–6). A linear probability model of
   acceptance on a guest dummy, host or listing traits, and their
   interactions, with standard errors clustered by guest name and city. The
   race gap among hosts with a trait is the sum of the main and interaction
   coefficients (`gap_with_trait`).

## The results, and what each rests on

- **Main effect** (PDF pp. 8–9, Table 2). Acceptance about 50 percent for White
  names and 42 for African-American names. The 8-point gap is 16 percent of
  the White rate (`pooled_gap`). It is stable under host and listing controls.
- **Host's race** (PDF pp. 9–10, Tables 3, 4, Appendix Table 2). The
  interaction of host and guest race is 0.01 (0.05) overall and noisy by host
  gender. African-American male hosts also accept African-American guests
  less. Only African-American female hosts favour African-American female
  guests. The authors read this as inconsistent with homophily in race.
- **Host and listing traits** (PDF pp. 10–11, Tables 5, 6, Appendix Table 3).
  The gap does not vary with shared versus entire property, multiple listings,
  ten or more reviews, apparent age, price above median, the predicted chance
  the listing is filled, the tract's share African-American, or listings per
  tract, and it is present in every city.
- **Names** (PDF p. 12, Table 7). The best-treated African-American name in each
  gender is below the worst-treated White name.
- **Review history** (PDF pp. 12–13, Table 5 column 5). Among hosts with at
  least one review from an African-American guest the gap is −0.04 (0.03), not
  distinguishable from zero. 29 percent of hosts have such a review.
- **Profiles and reviews** (PDF pp. 13–14). Rejection rates match Airbnb's
  internal rate for ordinary users, so bare profiles do not drive the result.
  The effect of reviews on discrimination is not observed.
- **Cost** (PDF pp. 14–16). With replacement probability `π` the forgone net
  revenue is `(1 − π)(p − f)` (`cost_identity`). With the median price $163
  and a 3 percent fee it is $64.19 for `π = .594` and $98.19 for `π = .379`
  (`cost_numbers`).
- **Discussion** (PDF pp. 17–20). The design cannot tell race from
  socioeconomic status, nor taste from statistical discrimination. The authors
  propose concealing names or expanding Instant Book.

## What could not be reconstructed

The coverage table in `CLAIMS.md` has one row per result. What is short of
general is this.

- The regressions themselves, their standard errors and significance, the
  balance tests of Table 1, the probit behind Table 6 column 4, and the power
  analysis of footnote 9. They need the data or numbers not printed.
- The pooled outcome of ELS-D1 is inferred from Appendix Table 4. Table 7 uses a
  base that matches neither reading (ELS-D9), and Table 5 column 4 does not add
  up (ELS-D8).
- The paper has no model. The remedy of PDF p. 18 is formalised in our reading,
  with acceptance a function of what the platform displays
  (`gap_zero_all_rules_iff`).
