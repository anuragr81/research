# Pre-registration: the testing thread's comparison, fourteenth file

Written and committed 24 October 2026, after the author's price files for
the new parents of results_match.md were checked for coverage, and before
any volatility is computed or any model is run on them. It amends
PREREG_comparison_12.md in the items below.

## What was seen before this file

- The author downloaded 19 of the 21 files by browser (24 October 2026)
  and put them in the Drive folder; Stooq then refused further downloads
  (an error file in place of the prices), so UMBF and VLY are not in hand.
- For each file, only these were computed: the row count, the first and
  last dates, the trading days in 2005-2014 and those missing against
  JPM's file, the sha256, and every daily log change of Close above 0.25
  in absolute value in 2005-2014 with its date. No volatility was computed.
- Coverage. FHN runs from 26 March 1990 with every trading day of
  2005-2014. Seventeen files (BOKF, CAC, CBU, CHCO, FFBC, FFIN, FNB, FRME,
  NBTB, ONB, OSBC, PEBO, PRK, SFNC, TCBI, THFF, TRMK) start on 25 February
  2005 and have 5,449 or fewer rows, so Stooq now caps a download at
  about that many rows; they lack the first 38 trading days of 2005, and
  OSBC lacks one more. ASB runs from 26 December 2014 only, so it does
  not cover 2010-2014 and is not covered under rule 4 of
  PREREG_comparison_12.md.
- Changes above 0.25: FHN 14 July 2008 (-0.28), 29 September 2008
  (-0.44), 30 September 2008 (+0.26); FNB 14 January 2009 (-0.26); THFF
  1 December 2008 (-0.30); OSBC 10 March 2009 (+0.26), 23 March 2009
  (+0.29), 20 September 2010 (+0.45), 25 April 2013 (+0.26). None in the
  other files. Whether OSBC's two changes outside the crisis are price
  moves or data faults has not been checked.

## Amendments

1. The decision of PREREG_comparison_12.md is taken on the 18 new banks
   whose files are in hand, listed with their sha256 in the results. UMBF
   and VLY, if their files arrive later, are added to the report under a
   dated note and decide nothing. ASB is not covered.
2. Files are pinned in ~/.cache/wealth_garch/stooq_20261024/. sigma_E is
   computed over the dates present in 2005-2014, as rule 4 provides.
3. The report without the changes above 0.25 outside September 2008 to
   June 2009 (PREREG_comparison_12.md) sets to zero: FHN 14 July 2008,
   OSBC 20 September 2010 and 25 April 2013. It decides nothing.

Everything else in PREREG_comparison_12.md and PREREG_comparison_13.md
stands.
