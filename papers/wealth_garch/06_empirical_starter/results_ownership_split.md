# Results: ownership split of E2 and E3, run 1

Run 12 October 2026 under PREREG_ownership_split.md (commit 1c6a13a2). The
ownership fields were fetched by fetch_ownership.py after that commit.

Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
Pinned: fdic_ownership_2015_2024.csv.gz sha256 2862f658594b19484163ab6db5d7630c4cb1913ede21bdb85e5e904a7081abea

Reproduce: python3 ownership_split.py fdic_distress_panel_2015_2024.csv.gz fdic_ownership_2015_2024.csv.gz

```
panel sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
ownership sha256 2862f658594b19484163ab6db5d7630c4cb1913ede21bdb85e5e904a7081abea
bank-quarters in the panel by group: S 0, M 24360, unclassified 189411
banks whose group changes within 2015-2024: 0 of 6581

E3  dividend paid in the quarter, by x = ASSET/LIAB, banks with assets >= $100m; y* = 1.571518; holds if the share below y* is at most 5%
--- S stand-alone
  x in [1.0000, 1.0300):       0 bank-quarters, paid     -
  x in [1.0300, 1.1000):       0 bank-quarters, paid     -
  x in [1.1000, 1.2000):       0 bank-quarters, paid     -
  x in [1.2000, 1.3900):       0 bank-quarters, paid     -
  x in [1.3900, 1.5715):       0 bank-quarters, paid     -
  x in [1.5715, 2.0000):       0 bank-quarters, paid     -
--- M multibank subsidiary
  x in [1.0000, 1.0300):      36 bank-quarters, paid  55.6%
  x in [1.0300, 1.1000):    4513 bank-quarters, paid  59.2%
  x in [1.1000, 1.2000):   12650 bank-quarters, paid  59.2%
  x in [1.2000, 1.3900):     951 bank-quarters, paid  34.0%
  x in [1.3900, 1.5715):      49 bank-quarters, paid  32.7%
  x in [1.5715, 2.0000):      45 bank-quarters, paid  53.3%
  below y*: 18199 bank-quarters, paid 57.8%  ->  fails
--- U unclassified
  x in [1.0000, 1.0300):     617 bank-quarters, paid  49.1%
  x in [1.0300, 1.1000):   38904 bank-quarters, paid  60.3%
  x in [1.1000, 1.2000):   99614 bank-quarters, paid  56.1%
  x in [1.2000, 1.3900):    6782 bank-quarters, paid  31.7%
  x in [1.3900, 1.5715):     362 bank-quarters, paid  15.5%
  x in [1.5715, 2.0000):     249 bank-quarters, paid  31.7%
  below y*: 146279 bank-quarters, paid 56.0%  ->  reported
--- S0 no holding company (within S, reported only)
  x in [1.0000, 1.0300):       0 bank-quarters, paid     -
  x in [1.0300, 1.1000):       0 bank-quarters, paid     -
  x in [1.1000, 1.2000):       0 bank-quarters, paid     -
  x in [1.2000, 1.3900):       0 bank-quarters, paid     -
  x in [1.3900, 1.5715):       0 bank-quarters, paid     -
  x in [1.5715, 2.0000):       0 bank-quarters, paid     -

E2  within-bank regression of distress_targeting.py, proxies P1 and P2, by group of the pre-issue quarter
--- S stand-alone: 0 episodes
--- M multibank subsidiary: 279 episodes
  recapitalisation from stock sales 0.432, from transactions with the parent 0.568
  proxy           valid?   eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
  P1     z +2.85 p 0.002    14     3     1  too few banks
  P2     z +2.76 p 0.003    90    45    16  +0.2212  0.4612  +0.7339  0.4745  1.086  not measured
  group verdict: not measured
--- U unclassified: 2901 episodes
  recapitalisation from stock sales 0.300, from transactions with the parent 0.700
  proxy           valid?   eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
  P1    z +15.93 p 0.000   303   202    68  +0.4787  0.3675  +1.3819  0.1925  1.138  fails
  P2     z +3.75 p 0.000   845   392   159  +0.0512  0.1343  +1.3962  0.2003  1.110  fails

VERDICTS  E3: S None, M fails   E2: S None, M not measured
```

## Outcome under the pre-registered rule

- No bank-quarter has `HCTMULT` = 0. The field is 1 (24,360 bank-quarters)
  or empty (189,411). Group S is empty, so the stand-alone half of the test
  is not measured, for E2 and for E3.
- M: E3 fails (57.8% of bank-quarters below y* pay a dividend). E2 is not
  measured (P1 has too few banks; P2's interval contains both 0 and
  x̄/2).
- No bank's `HCTMULT` changes within 2015-2024, while `RSSDHCR` changes
  for 854 of 6,581 banks. The pre-registration's known limit applies:
  `HCTMULT` appears to carry the current structure, not the structure at
  each quarter.

## What follows

The pre-registration allows no change to this run. A second
pre-registration (PREREG_ownership_split_2.md) classifies each bank-quarter
from `RSSDHCR`, which varies by quarter. Both runs are reported.
