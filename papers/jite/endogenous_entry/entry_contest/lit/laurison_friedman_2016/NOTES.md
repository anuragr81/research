# Notes — Laurison and Friedman (2016), pass 1 (read in full, accepted version)

Run `python3 lit/laurison_friedman_2016/verify_laurison_friedman.py` from the
repository root. The suite needs the extracted text at
`~/.cache/entry_contest/laurison_friedman_2016.txt` (not committed; export it
from the Drive PDF). It matches every quotation in `CLAIMS.md` against that
text, compiles `LaurisonFriedman2016.lean` with core Lean, audits its axioms,
and runs a fabricated quotation as a control.

## What was verified

1. Every quotation LF-1 to LF-15 is in the text at the stated page.
2. The arithmetic behind the headline numbers: £844 less £141 is £703, which
   is 83.3% of £844; £141 a week is £7,332 a year against "about £7350"; 0.0677/0.147 is 46%;
   26.6/14.1 lies between 1.8 and 2; the four occupations named at p. 12 are
   under 7% in Table 2; London 26.7% against 16.0% is more than 1.5 times.
3. Two discrepancies between text and tables (LF-D1, LF-D2), both small and
   both recorded rather than resolved. Neither touches what we rely on.

## What the paper is, for our purposes

Their object is a pay gap *after entry*, within occupations, by class
origin. Their first finding, access, is a by-product that they themselves
say the mobility literature has over-studied; their contribution is the
second finding, the class ceiling, and their methodological point (LF-3) is
that access and class position must not be conflated.

The model is an access model: one prize, awarded by rank, with wealth acting
only at the gate. So:

1. **Claim 7 of the rejected paper was mis-aimed.** It read the class *pay*
   gap (large in finance and law, near zero in science) as the model's class
   gap and tied it to (1 − μ). The pay gap is not a model object. The
   rejected paper's mapping also conflicts with their access data: science
   has a near-zero pay gap (LF-13) but is among the closed professions in
   access (LF-7), so "science = high μ" cannot be read off their results.
2. **What the model can be compared with is access.** The share of the
   prize taken by the paying class (the share row) is a closure measure in
   their sense, and it rises as the rule weights the bought component more
   (M28 and the share's monotonicity). Their own mechanism (LF-11, "talent"
   judged by class-rooted attributes; Rivera's cultural matching) is a rule
   with μ < 1. The sentence the manuscript may carry: the pattern that the
   traditional professions are the most closed is what the model predicts
   for occupations whose rule weights the bought component more; the weight
   itself is not measured by them or by us.
3. **A warning the paper gives, which the model must respect.** Managers
   earn more than professionals and are less closed (LF-5, LF-6). In the
   model a larger prize alone raises the count and so the paying class's
   share. So the cross-occupation pattern cannot be attributed to V; only a
   difference in μ fits it. This is the falsifier the writing discipline asks
   for: if closure tracked pay across occupations, V would do; it does not,
   so the rule weight has to carry the explanation.
4. **What is outside the model.** The ceiling after entry, the sorting into
   firms and regions, discrimination, and self-exclusion (pp. 17-18). The
   manuscript should say the model stops at entry, in LF-3's words.

## For the manuscript

- Row L17: the access finding (LF-6 as the quote), linked to the share row
  when it lands and to M5 and M28; `\unv` for the published version.
- Appendix D or the conclusions: the model stops at entry; the class
  ceiling of LF-1 and LF-2 is not addressed.
- The claim-7 replacement: consistency with their *access* pattern under
  the reading of LF-11, not with their pay gap.
