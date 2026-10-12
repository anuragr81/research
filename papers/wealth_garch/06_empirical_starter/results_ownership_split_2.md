# Results: ownership split of E2 and E3, run 2

Run 12 October 2026 under PREREG_ownership_split_2.md (commit a58e781f),
which replaced only the classification of run 1
(results_ownership_split.md). No new data were fetched.

Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
Pinned: fdic_ownership_2015_2024.csv.gz sha256 2862f658594b19484163ab6db5d7630c4cb1913ede21bdb85e5e904a7081abea

Reproduce: python3 ownership_split_2.py fdic_distress_panel_2015_2024.csv.gz fdic_ownership_2015_2024.csv.gz

```
HCTMULT = 1 bank-quarters classified M by RSSDHCR counts: 23612 of 24360
panel sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
ownership sha256 2862f658594b19484163ab6db5d7630c4cb1913ede21bdb85e5e904a7081abea
bank-quarters in the panel by group: S 190159, M 23612, unclassified 0
banks whose group changes within 2015-2024: 635 of 6581

E3  dividend paid in the quarter, by x = ASSET/LIAB, banks with assets >= $100m; y* = 1.571518; holds if the share below y* is at most 5%
--- S stand-alone
  x in [1.0000, 1.0300):     623 bank-quarters, paid  49.4%
  x in [1.0300, 1.1000):   39171 bank-quarters, paid  60.3%
  x in [1.1000, 1.2000):  100086 bank-quarters, paid  56.2%
  x in [1.2000, 1.3900):    6784 bank-quarters, paid  31.7%
  x in [1.3900, 1.5715):     362 bank-quarters, paid  15.5%
  x in [1.5715, 2.0000):     249 bank-quarters, paid  31.7%
  below y*: 147026 bank-quarters, paid 56.0%  ->  fails
--- M multibank subsidiary
  x in [1.0000, 1.0300):      30 bank-quarters, paid  50.0%
  x in [1.0300, 1.1000):    4246 bank-quarters, paid  58.8%
  x in [1.1000, 1.2000):   12178 bank-quarters, paid  58.5%
  x in [1.2000, 1.3900):     949 bank-quarters, paid  33.9%
  x in [1.3900, 1.5715):      49 bank-quarters, paid  32.7%
  x in [1.5715, 2.0000):      45 bank-quarters, paid  53.3%
  below y*: 17452 bank-quarters, paid 57.1%  ->  fails
--- U unclassified
  x in [1.0000, 1.0300):       0 bank-quarters, paid     -
  x in [1.0300, 1.1000):       0 bank-quarters, paid     -
  x in [1.1000, 1.2000):       0 bank-quarters, paid     -
  x in [1.2000, 1.3900):       0 bank-quarters, paid     -
  x in [1.3900, 1.5715):       0 bank-quarters, paid     -
  x in [1.5715, 2.0000):       0 bank-quarters, paid     -
--- S0 no holding company (within S, reported only)
  x in [1.0000, 1.0300):      62 bank-quarters, paid  21.0%
  x in [1.0300, 1.1000):    4636 bank-quarters, paid  28.9%
  x in [1.1000, 1.2000):   15776 bank-quarters, paid  28.2%
  x in [1.2000, 1.3900):    3093 bank-quarters, paid  17.9%
  x in [1.3900, 1.5715):     264 bank-quarters, paid  11.7%
  x in [1.5715, 2.0000):     159 bank-quarters, paid  38.4%
  below y*: 23831 bank-quarters, paid 26.8%  ->  reported

E2  within-bank regression of distress_targeting.py, proxies P1 and P2, by group of the pre-issue quarter
--- S stand-alone: 2918 episodes
  recapitalisation from stock sales 0.295, from transactions with the parent 0.705
  proxy           valid?   eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
  P1    z +15.88 p 0.000   304   202    68  +0.4787  0.3675  +1.3819  0.1925  1.138  fails
  P2     z +3.91 p 0.000   853   400   160  +0.0499  0.1334  +1.3924  0.1993  1.109  fails
  group verdict: fails
--- M multibank subsidiary: 262 episodes
  recapitalisation from stock sales 0.440, from transactions with the parent 0.560
  proxy           valid?   eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
  P1     z +2.84 p 0.002    13     3     1  too few banks
  P2     z +2.41 p 0.008    82    38    15  +0.0072  0.4225  +0.5824  0.4854  1.091  not measured
  group verdict: not measured
--- U unclassified: 0 episodes

VERDICTS  E3: S fails, M fails   E2: S fails, M not measured
```

## Outcome under the pre-registered rule

- E3: fails for stand-alone banks (56.0% of 147,026 bank-quarters below y*
  pay a dividend) and for multibank subsidiaries (57.1% of 17,452).
- E2: fails for stand-alone banks (P1 and P2 both exclude b_post = 0:
  +1.38 and +1.39). Not measured for multibank subsidiaries (P1 has one
  bank; P2's interval contains 0 and x̄/2).
- The reading fixed in advance: S fails E2 and E3, so the failures are not
  an artefact of subsidiaries' internal transfers. E2 and E3 stand, and
  hold for stand-alone banks as well as all banks.

## Reported, not used for a verdict

- Banks with no holding company (S0) pay a dividend in 26.8% of 23,831
  bank-quarters below y*: fewer than one-bank holding companies, still far
  above the model's 0%.
- 70.5% of the stand-alone episodes' recapitalisation comes from
  transactions with the parent: one-bank holding companies inject capital
  into their bank. The classification follows Holod and Peek (HP-Q11),
  who count these banks as stand-alone.
- 23,612 of the 24,360 bank-quarters with `HCTMULT` = 1 are classified M
  here; 635 banks change group within 2015-2024.
