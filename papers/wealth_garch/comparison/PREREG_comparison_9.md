# Pre-registration: the testing thread's comparison, ninth file

Written and committed 24 October 2026, after the share prices for measure
(B) were obtained and checked for completeness, and before any judged
volatility is computed or any model is run. It amends
PREREG_comparison_7.md and PREREG_comparison_8.md in the items below.

## What was seen before this file

- Stooq serves its pages only to a browser that passes a JavaScript check,
  and that check was not worked around. The author downloaded the eleven
  files below by browser from stooq.com (22 and 23 October 2026) and put
  them in the project's Drive folder (id 1GbF_rIXOWLbGOQP422Lyp6rS8-6eyTJo);
  they were fetched from there. Each has the columns Date, Open, High,
  Low, Close, Volume.
- For each file, only these were computed: the first and last dates; the
  trading days in 2005-2014; the largest absolute daily log change of
  Close in 2005-2014 and its date; every daily log change above 0.25 in
  absolute value; the dates missing against JPM's file. For JPM alone, the
  annualised standard deviation of daily log changes of Close, 2005-2014,
  was also computed: 0.432. No other volatility was computed.
- Every file covers 3 January 2005 to 31 December 2014 (2,517 trading
  days), except that Capital One's lacks 19 November 2010. Every daily
  change above 0.25 in absolute value falls between 12 June 2008 and 8 May
  2009. Citigroup's Close moves by about -2% on 9 May 2011, so the file is
  continuous across the share consolidation of that time. BNY Mellon's
  Close rises from 29.15 to 31.13 (log change 0.066) on 2 July 2007, its
  first trading day as a merged company, on lower volume than the day
  before; whether that step is a price move or an unadjusted share
  exchange has not been checked.
- The stress-test rows already recorded (lit/dfast_2013, lit/dfast_2014):
  Huntington and Zions are in the 2014 report only.
- The eleven tickers are in the Nasdaq Trader files pinned below with these
  security names: JPM "JP Morgan Chase & Co. Common Stock", BAC "Bank of
  America Corporation Common Stock", C "Citigroup, Inc. Common Stock", WFC
  "Wells Fargo & Company Common Stock", USB "U.S. Bancorp Common Stock",
  PNC "PNC Financial Services Group, Inc. (The) Common Stock", COF "Capital
  One Financial Corporation Common Stock", KEY "KeyCorp Common Stock", HBAN
  "Huntington Bancshares Incorporated - Common Stock", ZION "Zions
  Bancorporation N.A. - Common Stock", BNY "The Bank of New York Mellon
  Corporation Common Stock".

## Amendments

1. Prices. sigma_E,i is computed from the Close column of the files below,
   as served, pinned by sha256 in ~/.cache/wealth_garch/stooq_20261023/.
   A daily change runs between consecutive dates present in the file, so
   Capital One's change over 18-22 November 2010 is one change.

   | Holding company (NIC) | RSSD | File | sha256 |
   |---|---|---|---|
   | JPMORGAN CHASE & CO. | 1039502 | jpm_us_d.csv | 6836318dc26f4abb8df89bed430875940d85b05ecc6fdaceeb1cf5ed1ab10a39 |
   | BANK OF AMERICA CORPORATION | 1073757 | bac_us_d.csv | 4a3bae9500c923c87c50ee3518c6f578fcc2c0c2a785814015bd54266ee18fca |
   | CITIGROUP INC. | 1951350 | c_us_d.csv | 4cda892b1209869180b3aba09f38435db9d7b5b572483963ba78ad3c5506a21c |
   | WELLS FARGO & COMPANY | 1120754 | wfc_us_d.csv | 0129de4f0f11108f8bca172bda74f96e5a4ae9d37002e9d4cf352c124d01c5be |
   | U.S. BANCORP | 1119794 | usb_us_d.csv | dd5596935e7a31f249b66dcb284b09c177adcfcc46f17bcd0ab6ecae53346868 |
   | PNC FINANCIAL SERVICES GROUP, INC., THE | 1069778 | pnc_us_d.csv | 2814e9ad72e6f91cf075cff1018a8da02c320e02147dc6b2aa119835c2afd36b |
   | CAPITAL ONE FINANCIAL CORPORATION | 2277860 | cof_us_d.csv | 717d908fd3829063fb7236da8a0ca6f780448154fd2675ab5944dbee6786c7ab |
   | KEYCORP | 1068025 | key_us_d.csv | 4cfcb32565b4b7c030862d52ce64de5c7cb213a84ce1af9e60ab5252464c5872 |
   | HUNTINGTON BANCSHARES INCORPORATED | 1068191 | hban_us_d.csv | a5b1f3ad1136d7b42062bd40d585d55e7832578a1449b778ad20e0344389d9a5 |
   | ZIONS BANCORPORATION | 1027004 | zion_us_d.csv | 5f56697173bfa0ea69066ca39f608062eb1851719967c829dbaa05aa52262a83 |
   | BANK OF NEW YORK MELLON CORPORATION, THE | 3587146 | bny_us_d.csv | ab1061bb66d51340e15a4b4439080b6116c1db0398fb9961d13aae4b90423fd6 |

2. Tickers. The name match of PREREG_comparison_8.md is the table above,
   against nasdaqlisted.txt (sha256 df221177204f1c56add0c52adcd8f19c7371bde662eb34c8b32c5bba8bf4f5b0)
   and otherlisted.txt (sha256 cab8d0e5cb077acb38ebe201113a909b7ee3784c32249281d83399c0b0d5732f),
   pinned in ~/.cache/wealth_garch/nasdaqtrader_20261020/. The names agree
   up to case, spacing and punctuation for ten; for Zions the listed name adds
   "N.A.", and the match is kept, the listing being the same ticker. The
   results file repeats the table, and judged_risk.py checks each sha256
   and each ticker's security name before computing.
3. BNY Mellon's step of 2 July 2007. The central computation uses the
   series as served. The results also report sigma_E for BNY Mellon and
   both measures' F1 and F2 with that one change set to zero; this
   decides nothing.
4. Stress-test years. D_i is the mean decline over the reports a firm is
   in: both years for nine firms, 2014 alone for Huntington and Zions, as
   PREREG_comparison_7.md already provides.

Everything else in PREREG_comparison_7.md and PREREG_comparison_8.md
stands: banks, sigma_A and sigma_B, the check, the rule, and the window.
