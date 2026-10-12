# Replication of Peura and Keppo (2006), stage 2, run 1: their data analysis on Call Reports

Run 12 October 2026 under PREREG_stage2.md (commit 5a4fec61). Data fetched
by fetch_stage2.py after that commit.

Pinned: fdic_dec_1984_2002.csv.gz sha256 4a9662a11eada82ae174481782479554742eaff33e05240cfc4199b73ef34463
Pinned: fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

Reproduce: python3 stage2.py fdic_dec_1984_2002.csv.gz fedfunds.csv

```
fdic_dec_1984_2002.csv.gz sha256 4a9662a11eada82ae174481782479554742eaff33e05240cfc4199b73ef34463
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
institutions 22455; national banks passing the filters (sample A) 1638

===== SAMPLE A (all national banks): 1638 banks
above-average provisions 576 [29], other 1062 [32]
--- Table 1, all: 1638 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         1.76 [ 2.14]    0.00 [ 0.31]    0.18 [ 0.22]
  p25         5.17 [ 3.69]    0.65 [ 0.73]    0.52 [ 0.48]
  median      7.82 [ 4.70]    0.97 [ 0.91]    0.76 [ 0.60]
  mean       10.70 [ 5.01]    1.26 [ 0.92]    1.85 [ 0.76]
  p75        12.78 [ 5.95]    1.34 [ 1.11]    1.24 [ 1.05]
  max       295.62 [10.43]  292.53 [ 1.88] 1168.15 [ 1.89]
  SD         13.00 [ 1.67]    7.26 [ 0.32]   28.91 [ 0.40]
  corr(buf,mu) +0.04 (p 0.086)   [+0.15]
  corr(buf,sigma) +0.01 (p 0.578)   [+0.15]
  corr(mu,sigma) +1.00 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 576 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.91]    0.00 [ 0.32]    0.21 [ 0.41]
  p25         4.40 [ 3.64]    0.51 [ 0.63]    0.76 [ 0.65]
  median      6.56 [ 4.09]    0.76 [ 0.81]    1.21 [ 0.78]
  mean        9.14 [ 4.61]    1.47 [ 0.81]    3.68 [ 0.95]
  p75        10.13 [ 5.27]    1.12 [ 0.97]    1.82 [ 1.19]
  max       295.62 [ 8.11]  292.53 [ 1.35] 1168.15 [ 1.89]
  SD         14.65 [ 1.42]   12.21 [ 0.29]   48.69 [ 0.42]
  corr(buf,mu) +0.04 (p 0.310)   [-0.21]
  corr(buf,sigma) +0.02 (p 0.652)   [+0.58]
  corr(mu,sigma) +1.00 (p 0.000)   [-0.33]
--- Table 1, below-average provisions: 1062 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         1.76 [ 2.14]    0.00 [ 0.31]    0.18 [ 0.22]
  p25         5.68 [ 3.85]    0.76 [ 0.85]    0.48 [ 0.43]
  median      8.69 [ 5.31]    1.06 [ 0.97]    0.64 [ 0.49]
  mean       11.55 [ 5.37]    1.15 [ 1.02]    0.85 [ 0.58]
  p75        14.27 [ 6.36]    1.41 [ 1.18]    0.93 [ 0.60]
  max       275.15 [10.43]   10.37 [ 1.88]   37.90 [ 1.33]
  SD         11.94 [ 1.82]    0.69 [ 0.31]    1.39 [ 0.27]
  corr(buf,mu) +0.28 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.19 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.54 (p 0.000)   [-0.17]
barriers: unique admissible solution for 1285 of 1638 banks
--- Table 3, all: 1285 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    11.04 [ 5.01]    1.68 [ 0.80]    3.95 [ 2.40]    4.78 [ 2.72]
  sd     14.08 [ 1.67]    4.69 [ 0.78]    6.31 [ 1.63]   10.13 [ 2.09]
  mu      +0.40 [+0.15]   +0.77 [-0.47]   +0.75 [-0.48]   +0.76 [-0.47]
  sigma   +0.28 [+0.15]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  -0.06 [-0.03]   +0.56 [+0.07]   +0.55 [+0.07]   +0.55 [+0.06]
  size    -0.05 [-0.39]   +0.02 [+0.11]   +0.02 [+0.10]   +0.02 [+0.10]
  llp     -0.05 [-0.34]   +0.17 [+0.49]   +0.21 [+0.48]   +0.18 [+0.47]
--- Table 3, above-average provisions: 489 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     9.43 [ 4.61]    2.51 [ 1.20]    5.47 [ 3.24]    6.87 [ 3.78]
  sd     15.43 [ 1.42]    6.33 [ 0.81]    8.34 [ 1.69]   13.63 [ 2.24]
  mu      +0.46 [-0.21]   +0.86 [-0.38]   +0.83 [-0.44]   +0.85 [-0.42]
  sigma   +0.39 [+0.58]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  -0.05 [+0.19]   +0.45 [-0.02]   +0.45 [-0.01]   +0.43 [-0.01]
  size    -0.05 [-0.43]   -0.00 [-0.11]   -0.01 [-0.12]   -0.01 [-0.11]
  llp     +0.06 [-0.16]   +0.13 [+0.20]   +0.16 [+0.20]   +0.14 [+0.20]
--- Fig. 5, below-average provisions: 796 banks: corr(buffer, u2) +0.19 (p 0.000); OLS buffer = 10.29% (0.55) + 0.57 (0.10) u2, R2 0.04; corr(max buffer, u2) +0.21
--- Fig. 5, above-average provisions: 489 banks: corr(buffer, u2) +0.40 (p 0.000); OLS buffer = 5.41% (0.77) + 0.74 (0.08) u2, R2 0.16; corr(max buffer, u2) +0.61
means: buffer 10.70% [5.0], u2 3.95% [2.4], u0 7.01% [2.7]; min buffer 1.76%; corr(u2, sigma) +1.00 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: not reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: not reproduced

===== SAMPLE B (61 largest): 61 banks
above-average provisions 11 [29], other 50 [32]
--- Table 1, all: 61 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.14]    0.26 [ 0.31]    0.28 [ 0.22]
  p25         3.09 [ 3.69]    0.78 [ 0.73]    0.65 [ 0.48]
  median      3.80 [ 4.70]    1.06 [ 0.91]    1.00 [ 0.60]
  mean        4.40 [ 5.01]    6.32 [ 0.92]   21.65 [ 0.76]
  p75         5.22 [ 5.95]    1.52 [ 1.11]    1.49 [ 1.05]
  max        10.29 [10.43]  292.53 [ 1.88] 1168.15 [ 1.89]
  SD          1.84 [ 1.67]   37.30 [ 0.32]  149.39 [ 0.40]
  corr(buf,mu) +0.27 (p 0.033)   [+0.15]
  corr(buf,sigma) +0.27 (p 0.034)   [+0.15]
  corr(mu,sigma) +1.00 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 11 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.72 [ 2.91]    0.40 [ 0.32]    0.52 [ 0.41]
  p25         3.08 [ 3.64]    0.63 [ 0.63]    0.85 [ 0.65]
  median      3.25 [ 4.09]    1.02 [ 0.81]    1.15 [ 0.78]
  mean        4.33 [ 4.61]    1.81 [ 0.81]    1.61 [ 0.95]
  p75         4.92 [ 5.27]    2.81 [ 0.97]    2.48 [ 1.19]
  max         8.76 [ 8.11]    5.19 [ 1.35]    3.68 [ 1.89]
  SD          1.94 [ 1.42]    1.59 [ 0.29]    1.05 [ 0.42]
  corr(buf,mu) +0.72 (p 0.013)   [-0.21]
  corr(buf,sigma) +0.76 (p 0.006)   [+0.58]
  corr(mu,sigma) +0.75 (p 0.008)   [-0.33]
--- Table 1, below-average provisions: 50 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.14]    0.26 [ 0.31]    0.28 [ 0.22]
  p25         3.09 [ 3.85]    0.84 [ 0.85]    0.60 [ 0.43]
  median      3.81 [ 5.31]    1.08 [ 0.97]    0.99 [ 0.49]
  mean        4.41 [ 5.37]    7.31 [ 1.02]   26.06 [ 0.58]
  p75         5.10 [ 6.36]    1.37 [ 1.18]    1.44 [ 0.60]
  max        10.29 [10.43]  292.53 [ 1.88] 1168.15 [ 1.33]
  SD          1.84 [ 1.82]   41.20 [ 0.31]  164.97 [ 0.27]
  corr(buf,mu) +0.30 (p 0.036)   [+0.26]
  corr(buf,sigma) +0.30 (p 0.035)   [+0.04]
  corr(mu,sigma) +1.00 (p 0.000)   [-0.17]
barriers: unique admissible solution for 54 of 61 banks
--- Table 3, all: 54 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     4.35 [ 5.01]    4.83 [ 0.80]    8.19 [ 2.40]   11.37 [ 2.72]
  sd      1.76 [ 1.67]   14.58 [ 0.78]   18.76 [ 1.63]   30.25 [ 2.09]
  mu      +0.37 [+0.15]   +0.90 [-0.47]   +0.90 [-0.48]   +0.90 [-0.47]
  sigma   +0.34 [+0.15]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  +0.15 [-0.03]   +0.82 [+0.07]   +0.82 [+0.07]   +0.82 [+0.06]
  size    -0.14 [-0.39]   -0.08 [+0.11]   -0.08 [+0.10]   -0.08 [+0.10]
  llp     +0.07 [-0.34]   -0.08 [+0.49]   -0.07 [+0.48]   -0.08 [+0.47]
--- Table 3, above-average provisions: 11 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     4.33 [ 4.61]    2.18 [ 1.20]    5.06 [ 3.24]    5.87 [ 3.78]
  sd      1.94 [ 1.42]    1.73 [ 0.81]    3.15 [ 1.69]    3.79 [ 2.24]
  mu      +0.72 [-0.21]   +0.61 [-0.38]   +0.61 [-0.44]   +0.51 [-0.42]
  sigma   +0.76 [+0.58]   +0.98 [+0.98]   +0.98 [+0.99]   +0.95 [+0.99]
  growth  -0.14 [+0.19]   +0.34 [-0.02]   +0.35 [-0.01]   +0.30 [-0.01]
  size    -0.05 [-0.43]   -0.23 [-0.11]   -0.23 [-0.12]   -0.20 [-0.11]
  llp     +0.36 [-0.16]   +0.33 [+0.20]   +0.35 [+0.20]   +0.27 [+0.20]
--- Fig. 5, below-average provisions: 43 banks: corr(buffer, u2) +0.37 (p 0.015); OLS buffer = 4.08% (0.27) + 0.03 (0.01) u2, R2 0.14; corr(max buffer, u2) +0.52
--- Fig. 5, above-average provisions: 11 banks: corr(buffer, u2) +0.71 (p 0.015); OLS buffer = 2.13% (0.85) + 0.43 (0.14) u2, R2 0.50; corr(max buffer, u2) +0.74
means: buffer 4.40% [5.0], u2 8.19% [2.4], u0 91.04% [2.7]; min buffer 2.19%; corr(u2, sigma) +1.00 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: reproduced
  F6 mean u2 and mean u0 below mean buffer: not reproduced
  F7 no bank with mean buffer below 2%: reproduced
```

## Verdicts under the pre-registered rule

| Finding | Sample A (1,638 national banks) | Sample B (61 largest) |
|---|---|---|
| F1 μ-σ correlation negative, significant | not reproduced | not reproduced |
| F2 high-provision group: lower μ, higher σ | not reproduced | not reproduced |
| F3 high-provision group: buffer-σ positive, significant | not reproduced | reproduced |
| F4 u2-σ correlation at least 0.9 | reproduced | reproduced |
| F5 high-provision group: buffer-u2 positive, slope < 1, intercept > 0 | reproduced | reproduced |
| F6 mean u2 and u0 below mean buffer | reproduced | not reproduced |
| F7 no bank with mean buffer below 2% | not reproduced | reproduced |

## What the run shows about the data

- Charter consolidations dominate the moments. A small national bank that
  absorbs a large one keeps its charter: CERT 5134 had assets of 8-58
  million to 1996 and 7.6 billion in 1997, CERT 22488 8 million in 1986
  and .3 billion in 1987, CERT 3832 6 million in 1998 and .6 billion
  in 1999. The year's net income over the previous year's risk-weighted
  assets is then a ratio of the merged bank's income to the small bank's
  assets: μ up to 292.5%, σ up to 1,168%. 21 of 1,638 banks have μ or σ
  above 5%, five of them among the 61 largest. The μ-σ correlation of
  +1.00 in both samples is these banks. Compustat holding-company data,
  which Peura and Keppo used, aggregate across mergers and have no such
  breaks.
- Reported, not used for a verdict: the Spearman μ-σ correlation, which
  the outliers cannot dominate, is -0.10 (p < 0.001) in sample A and +0.19
  (p 0.14) in sample B.
- The pre-registration had no merger rule. PREREG_stage2_2.md adds one,
  written after this run and saying so; this run stays reported.

## Choices the pre-registration did not fix

- σ is the sample standard deviation (n - 1).
- The barrier solver searches 0 < u1 < u2 < 1.5 u0. A first attempt with
  the bound u2 < 0.5 stopped on an overflow before any statistic was
  computed; admissible solutions have u2 < u0, so the bound excludes none.
