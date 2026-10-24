# Pre-registration: the testing thread's comparison, twelfth file

Written and committed 24 October 2026, before any ticker is matched for
the banks below and before any price is fetched for them. It extends
measure (B), equity volatility, from the 17 banks of the stress-tested
firms to as many of sample B's 61 banks as have a listed parent, and
scores the extension on the test window against the baseline. The author
agreed (24 October 2026).

## What was seen before this file

Everything listed in PREREG_comparison_11.md. The top holding company
(RSSDHCR at 2014Q4) and its NIC legal name were listed for all 61 banks of
sample B on 20 October 2026 (for the stress-test matching). No ticker
other than the eleven of PREREG_comparison_9.md has been looked up, and no
other price has been fetched.

## Banks covered

A bank of sample B is covered if all of these hold.

1. Its parent is its top holding company at 2014Q4 (RSSDHCR), or the bank
   itself if it has none (matched by ID_FDIC_CERT in the NIC attributes
   files).
2. The parent's country of incorporation in the NIC attributes files
   (CNTRY_INC_NM) is the United States. Banks with foreign parents are not
   covered: their parents' shares are priced abroad.
3. One of the legal names the parent's RSSD has carried in the NIC
   attributes files (active and closed, as pinned in
   replication/gordon_hirtle_2026) matches the security name of a common
   stock in the Nasdaq Trader files of PREREG_comparison_9.md, up to case,
   spacing, punctuation, a leading "The", and the words "Common Stock" or
   "Common Shares". Securities named as warrants, units, rights, notes,
   preferred or depositary shares are not candidates. A name with no
   candidate, or with two or more, is not covered. Every match is listed
   and checked by hand in the results.
4. The author downloads its daily prices from stooq.com by browser, as
   before; the file covers at least 2010-2014. sigma_E is computed over
   the part of 2005-2014 the file covers.

The eleven firms of PREREG_comparison_9.md keep their files and tickers.
Firms that failed or were absorbed before October 2026 (so that their
RSSD carries no listed name) are not covered; they are listed by name in
the results, since leaving them out favours survivors.

## Price checks

For each new file: first and last dates, trading days and missing days in
2005-2014, and every daily log change of Close above 0.25 in absolute
value, listed. The central computation uses the series as served; sigma_E
is also reported without the changes above 0.25 that fall outside
September 2008 to June 2009. That decides nothing.

## Model, parameters, observed facts

As in PREREG_comparison_10.md, unchanged: sigma_B,i = sigma_E,i X_i from
2005-2014; the non-issuer barrier model from the 2014Q4 buffer, 40
quarters, 200 paths, minimum 8% central and R reported; the baseline as
in results_pk.md; facts by facts.py; F1 and F2 scored, F3 and F4 reported
(PREREG_comparison_11.md).

## Rule

- The decision is on the newly covered banks, those outside the 17 already
  scored: (B) beats the baseline if its F1 + F2 miss on the test window,
  minimum 8%, is smaller. If fewer than 10 new banks are covered, the
  decision is on all covered banks.
- Each model is also judged by the 10-point rule on F1 and F2.
- Reported, deciding nothing: all covered banks together; the training
  window for the new banks; sample A's covered banks.
- Nothing from 2025 or later enters a fact or a parameter. The Nasdaq
  Trader files (October 2026) serve only as a directory of names.
