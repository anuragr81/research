# Hanson and Hawley (2011), reconstruction

Andrew Hanson and Zackary Hawley, "Do Landlords Discriminate in the Rental
Housing Market? Evidence from an Internet Field Experiment in U.S. Cities",
Georgia State University, Experimental Economics Center Working Paper 2011-05.
Published in the *Journal of Urban Economics* 70(2), September 2011,
pp. 99–114.

## Source read

- The working paper, from ScholarWorks@GSU (handle 20.500.14694/6123), cached
  at `~/.cache/paper_a/sources/hanson_hawley_2011_wp.pdf` (252,920 bytes, 58 PDF
  pages), sha256 `02e1f339…d289c2`, with the text layer at
  `~/.cache/paper_a/sources/hanson_hawley_2011_wp.txt`, sha256 `3b8d71df…62df`.
  The text has 59 form-feed segments. Segment `n − 1` is PDF page `n`, and
  segment 58 is empty. Pages are cited as PDF pages.
- PDF page 1 is a cover sheet that the repository adds at download, with the
  date of the download. A fresh download is a different file with a different
  sha256 (HH-D6). The pin is to the cached copy.
- PDF page 2 is the title page and abstract. PDF pages 3 to 35 are the paper,
  numbered 1 to 33. PDF pages 36 and 37 are the references. PDF pages 38 and 39
  are the appendix of e-mail examples, printed twice. PDF pages 40 to 57 are
  Tables 1 to 18 and PDF page 58 is Figure 1.
- The version is after review. Note 32 thanks "a particularly thoughtful
  referee". The published article was not available and is not read (HH-D7).
- Every page, 1 to 58, was read on 17 October 2026, so the evidence tag is [F].
  PDF page 22, where the probit equation of note 27 is garbled, and PDF pages 45
  and 46, Tables 6 and 7, were also read from images rendered with `pdftoppm`.
  The equation of note 27 is garbled in the PDF itself (HH-D8).

## The experiment

1. **Units** (PDF pp. 12–13). Every advertisement posted on Craigslist on
   Tuesdays from 22 July to 7 October 2009 in Atlanta, Boston, Chicago, Dallas,
   Washington, Houston, Los Angeles, New York, Seattle and San Francisco. Repeat
   landlords are removed. In Boston and New York, only the no-fee pages are
   used.
2. **Race** (PDF pp. 9–10). Each inquiry carries a first and last name from
   Bertrand and Mullainathan (2004), nine White and nine African American
   (Table 1). The name appears in the address, the inbox name-plate and the
   signature.
3. **Class** (PDF pp. 10–11, 38). Each inquiry is a "high-class" or "low-class"
   e-mail. Both ask about the unit and offer references and credit information.
   The low-class e-mail has a spelling error, informal or ungrammatical
   sentences and an informal valediction ("C U Later"). The high-class e-mail
   calls the references "good" and the credit report "recent" (HH-Q5). The class
   meant is the landlord's view of the applicant as a tenant, vague enough not to
   be read as income, occupation or education (HH-Q6). The authors take class,
   after Gordon (1949), as not by definition race (HH-Q4).
4. **Design** (PDF pp. 11–12). Each landlord receives two e-mails, one of the
   six pairings of {White, African American} × {high, low}, in both orders, so
   twelve audit types (Table 2). Assignment is random and balanced by city and
   day. Same-race pairs use different names.
5. **Sending** (PDF p. 14). Gmail addresses of the form
   `firstname.lastname.###@gmail.com`, sent by hand on Wednesday mornings, one
   to three hours apart.
6. **Outcomes** (PDF pp. 17, 24–25). Whether the landlord replies. For
   landlords who reply to both, whether the reply says the unit is available,
   and whether it invites a viewing.

## Results and what each is derived from

- **Sample** (Table 3, PDF p. 42). 4,728 audits and 9,456 e-mails. 53.9 percent
  of e-mails get a reply and 63.7 percent of landlords reply to at least one.
- **Race** (Table 5, PDF pp. 17–18, 44). Over all e-mails, 57.12 percent of White names
  and 50.81 percent of African American names get a reply, a gap of 6.30 points.
  Over the 3,153 cross-race audits, 12.50 percent of landlords answer only the
  White name and 7.96 percent only the African American name, a net 4.54
  percent (`table5_all`). The gap is larger in Boston and Los Angeles.
- **Race and class** (Table 6, PDF pp. 18–19, 45). Reply rates are 56.63 (White,
  high), 57.60 (White, low), 52.62 (African American, high) and 49.01 (African
  American, low) percent (`table6_gaps`).
  - Between two high-class e-mails, the net landlord measure is 1.77 percent,
    p = 0.2485 (HH-Q7, `table6_high_high`).
  - Between two low-class e-mails it is 5.96 percent, p = 0.0001
    (`table6_low_low`).
  - Over all e-mails of each type, the race gap is 4.01 points between
    high-class e-mails, p = 0.0058, and 8.60 between low-class ones
    (`table6_gap_halves_not_zero`).
  - The class of the e-mail moves the reply rate for African American names,
    by 3.62 points (p = 0.0129), and not for White names, where the high-class
    rate is 0.97 points lower (p = 0.5029) (`table6_class_moves_one_group`).
  - A high-class African American name gets 4.98 points fewer replies than a
    low-class White name, p = 0.0006, while the net landlord measure for that
    pairing is 1.65 percent, p = 0.2355 (HH-Q8).
- **Probits** (Table 7, PDF pp. 20–21, 46). With the class dummy equal to one
  for a low-class e-mail, the race coefficient is the effect of an African
  American name on a high-class e-mail. It is −0.0401, p = 0.002, without
  landlord effects (column 4) and −0.119, p = 0.003, with them (column 6)
  (HH-Q9). The fixed-effects race effect in column (3), −0.222, is the
  difference between the 61 and 39 percent of the 645 landlords who answer one
  e-mail only (`fe_effect_discordant`).
- **Neighbourhoods** (Tables 8 and 10, PDF pp. 21–24, 47, 49). The net measure
  is largest, 7.02 percent, where the neighbourhood is 80 to 95 percent White,
  the "tipping" band (HH-Q15, `tipping_iff_minority`, `table8_band_largest`).
  The interaction of an African American name with a tipping neighbourhood is
  −0.0525, p = 0.028. The interaction with the percentage White is 0.0035,
  p = 0.938. Table 8's counts are inconsistent with its shares and with the
  number of units matched to a tract (HH-D4, HH-D5).
- **Units** (Tables 9 and 10, PDF pp. 23–24, 48). No net discrimination for
  single-family homes, duplexes or townhouses. Net shares of 6.09 percent for
  apartments and 11.88 for condominiums (`table9_nets`). The authors read this
  as landlords who bear the cost of other tenants' displeasure (HH-Q12,
  HH-Q13).
- **Intensity** (Tables 11 and 12, PDF pp. 24–25, 50–51). Among landlords who
  reply to both, there is no net difference in saying the unit is available or
  in inviting a viewing, except Seattle, where African American names are
  invited more (HH-Q16).
- **Robustness** (Tables 13 to 16, PDF pp. 25–30, 52–55). Removing the names of Muslim
  origin raises the gap. Removing the most common and most unique names lowers
  it by up to 1.5 points but leaves it significant. Removing names that perform
  unusually in a city leaves it as it was.
- **Anonymity** (Table 17, PDF pp. 31, 56). Landlords using Craigslist's anonymous
  address discriminate slightly less at the landlord level, 0.39 points, and
  slightly more in reply rates, 0.08 points (HH-Q17).
- **Reweighting** (Table 18, PDF pp. 32–33, 57). Reweighted to the unit types of
  the American Housing Survey, the reply gap is 8.48 points and the net share
  5.58 percent.
- **Interpretation** (PDF pp. 33–34). The authors read the class result and
  the variation by unit and neighbourhood as statistical discrimination, the
  name used to infer the kind of tenant, with customer prejudice in larger
  buildings and tipping neighbourhoods (HH-Q10, HH-Q11, HH-Q14).

## What the module formalises

- The arithmetic of Tables 5, 6 and 9, and of the 0.222 of Table 7, column (3).
- The identity behind the landlord-level measure. On the same paired audits the
  net share equals the difference in reply rates (`net_eq_rate_diff`), and
  Table 6's pooled rates are not over those audits
  (`control_pooled_rates_differ`).
- Our reading of the class manipulation, a linear predictor of quality from the
  name's group mean and the e-mail, with a threshold for a reply
  (`gap_common_weight`, `gap_vanishes_full_signal`,
  `control_partial_signal_gap`, `threshold_equal_above`,
  `control_threshold_between`, `high_signal_clears`, `control_zero_weight`).
  The paper states no model. The predictor and the threshold are ours.

## What could not be reconstructed

- The probit estimates, their standard errors and every p-value.
- The models of note 27, whose equation is garbled (HH-D8).
- Figure 1, whose bars have no printed values.
- Bradburd et al. (2005), Card, Mas and Rothstein (2008) and the other cited
  studies, which are not read.
