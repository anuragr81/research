# Results: distress-targeting test

Run 11 October 2026 under PREREG_distress_targeting.md (commit d92b237d),
on fdic_distress_panel_2015_2024.csv.gz, fetched by fetch_distress_panel.py.

Pinned: fdic_distress_panel_2015_2024.csv.gz sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b

Reproduce: python3 distress_targeting.py fdic_distress_panel_2015_2024.csv.gz

```
panel sha256 3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b
banks 6581; P3 cutoff Tier1/RWA 0.1099

VALIDITY  no dividend in the issuing and preceding quarter, classified vs not
P1: classified 317 share 0.953; others 2621 share 0.480; z +15.94 p 0.0000 -> valid; inputs missing 242
P2: classified 935 share 0.589; others 1937 share 0.500; z +4.48 p 0.0000 -> valid; inputs missing 308
P3: classified 769 share 0.525; others 1989 share 0.537; z -0.57 p 0.7161 -> not valid; inputs missing 422
P4: classified 221 share 0.566; others 2651 share 0.526; z +1.13 p 0.1298 -> not valid; inputs missing 308

--- 2015-2024: 3180 episodes
group    eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
P1       317   207    70  +0.4748  0.3649  +1.3724  0.1917  1.137  fails
P2       935   444   176  +0.0079  0.1197  +1.3580  0.1788  1.108  fails
P3       769   420   158  -0.0638  0.0998  +1.0178  0.1662  1.095  fails
P4       221    61    25  -0.2382  0.1008  +0.8555  0.2121  1.105  fails
none    1440   860   312  +0.0481  0.1890  +0.7759  0.1588  1.122  fails
all     3180  2358   732  -0.0055  0.1036  +0.9514  0.1281  1.114  fails

--- 2015-2019 (reported, not used for the verdict): 1482 episodes
group    eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
P1       130    65    24  +0.4947  0.7755  +1.0993  0.3369  1.138  fails
P2       248    84    38  +0.0870  0.3069  +1.3418  0.2940  1.113  fails
P3       478   220    87  +0.0565  0.0542  +1.2209  0.1366  1.098  fails
P4       102    22    10  +0.0382  0.0551  +1.1008  0.0956  1.096  not enough data
none     740   400   155  +0.2360  0.3293  +0.3368  0.1412  1.123  fails
all     1482   941   335  +0.0424  0.2402  +0.5719  0.2293  1.117  fails

--- 2020-2024 (reported, not used for the verdict): 1698 episodes
group    eps w-eps banks    b_pre      se   b_post      se   xbar  verdict
P1       187   120    38  +0.5388  0.5143  +1.5009  0.2657  1.137  fails
P2       687   275   111  -0.1132  0.1249  +1.2210  0.2586  1.107  fails
P3       291   116    49  -0.5227  0.3044  +0.6230  0.3025  1.091  fails
P4       119    25     9  -0.1479  0.0935  +1.1539  0.0207  1.125  not enough data
none     700   295   118  -0.3903  0.2748  +1.0067  0.2155  1.124  fails
all     1698  1028   360  -0.2511  0.1074  +0.9642  0.1722  1.113  fails

proxies valid with enough data: ['P1', 'P2']
OVERALL VERDICT (exact targeting, b_post = 0): fails
```

## Choices the pre-registration did not fix

- An episode whose proxy inputs are missing is not classified by that
  proxy, and it is left out of that proxy's validity test. The counts are
  printed as "inputs missing".
- A quarter whose stock or parent-contribution flow is missing is not an
  episode. A missing flow is never read as zero.
- Clustered standard errors use the small-sample factor G/(G-1), with G the
  number of banks.

## Reading, beyond the verdict rule

- The verdict is "fails" for both proxies that are valid with enough data
  (P1, P2), and for every group in both halves of the period that has
  enough data.
- b_pre is within two standard errors of 0 for P1, P2, P3 and all
  episodes over 2015-2024. That is the fixed-size benchmark, the opposite
  pole to exact targeting: issue size does not depend on how low the bank
  was, and the post-issue state rises with the issue.
- Classical measurement error in f biases both slopes toward 0, so toward
  exact targeting. It cannot produce this rejection.
