# Replication of Peura and Keppo (2006), stage 2, run 2: with the merger screen

Run 12 October 2026 under PREREG_stage2_2.md (commit 98c65377), which adds
one rule to PREREG_stage2.md: a year's ROA enters only if total assets
stayed between half and double the previous year's. Run 1 is
results_stage2.md.

Pinned: fdic_dec_1984_2002.csv.gz sha256 4a9662a11eada82ae174481782479554742eaff33e05240cfc4199b73ef34463
Pinned: fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

Reproduce: python3 stage2_2.py fdic_dec_1984_2002.csv.gz fedfunds.csv

```
fdic_dec_1984_2002.csv.gz sha256 4a9662a11eada82ae174481782479554742eaff33e05240cfc4199b73ef34463
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
institutions 22455; run 1 sample A 1638 banks; ROA years removed by the screen among them 224; banks below 15 ROA values because of the screen 32
national banks passing the filters with the screen (sample A) 1612

===== SAMPLE A (all national banks): 1612 banks
above-average provisions 592 [29], other 1020 [32]
--- Table 1, all: 1612 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         1.76 [ 2.14]    0.00 [ 0.31]    0.18 [ 0.22]
  p25         5.27 [ 3.69]    0.64 [ 0.73]    0.51 [ 0.48]
  median      7.86 [ 4.70]    0.95 [ 0.91]    0.74 [ 0.60]
  mean       10.71 [ 5.01]    1.05 [ 0.92]    1.00 [ 0.76]
  p75        12.82 [ 5.95]    1.32 [ 1.11]    1.18 [ 1.05]
  max       295.62 [10.43]   22.01 [ 1.88]   49.54 [ 1.89]
  SD         12.91 [ 1.67]    0.83 [ 0.32]    1.44 [ 0.40]
  corr(buf,mu) +0.40 (p 0.000)   [+0.15]
  corr(buf,sigma) +0.39 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.61 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 592 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.91]    0.00 [ 0.32]    0.21 [ 0.41]
  p25         4.46 [ 3.64]    0.50 [ 0.63]    0.73 [ 0.65]
  median      6.50 [ 4.09]    0.76 [ 0.81]    1.11 [ 0.78]
  mean        8.90 [ 4.61]    0.91 [ 0.81]    1.42 [ 0.95]
  p75         9.90 [ 5.27]    1.10 [ 0.97]    1.70 [ 1.19]
  max       295.62 [ 8.11]   22.01 [ 1.35]   49.54 [ 1.89]
  SD         14.03 [ 1.42]    1.10 [ 0.29]    2.19 [ 0.42]
  corr(buf,mu) +0.48 (p 0.000)   [-0.21]
  corr(buf,sigma) +0.47 (p 0.000)   [+0.58]
  corr(mu,sigma) +0.77 (p 0.000)   [-0.33]
--- Table 1, below-average provisions: 1020 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         1.76 [ 2.14]    0.00 [ 0.31]    0.18 [ 0.22]
  p25         5.83 [ 3.85]    0.76 [ 0.85]    0.47 [ 0.43]
  median      9.05 [ 5.31]    1.06 [ 0.97]    0.62 [ 0.49]
  mean       11.77 [ 5.37]    1.13 [ 1.02]    0.75 [ 0.58]
  p75        14.50 [ 6.36]    1.41 [ 1.18]    0.88 [ 0.60]
  max       275.15 [10.43]    6.14 [ 1.88]   10.33 [ 1.33]
  SD         12.10 [ 1.82]    0.60 [ 0.31]    0.56 [ 0.27]
  corr(buf,mu) +0.32 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.55 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.34 (p 0.000)   [-0.17]
barriers: unique admissible solution for 1253 of 1612 banks
--- Table 3, all: 1253 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    11.16 [ 5.01]    1.38 [ 0.80]    3.51 [ 2.40]    4.11 [ 2.72]
  sd     14.15 [ 1.67]    3.30 [ 0.78]    4.50 [ 1.63]    7.29 [ 2.09]
  mu      +0.44 [+0.15]   +0.73 [-0.47]   +0.69 [-0.48]   +0.71 [-0.47]
  sigma   +0.40 [+0.15]   +0.99 [+0.98]   +0.99 [+0.99]   +0.99 [+0.99]
  growth  -0.01 [-0.03]   +0.01 [+0.07]   +0.01 [+0.07]   +0.01 [+0.06]
  size    -0.04 [-0.39]   -0.00 [+0.11]   -0.00 [+0.10]   -0.00 [+0.10]
  llp     -0.08 [-0.34]   +0.13 [+0.49]   +0.19 [+0.48]   +0.15 [+0.47]
--- Table 3, above-average provisions: 500 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     9.37 [ 4.61]    2.04 [ 1.20]    4.76 [ 3.24]    5.80 [ 3.78]
  sd     15.15 [ 1.42]    4.97 [ 0.81]    6.51 [ 1.69]   10.86 [ 2.24]
  mu      +0.49 [-0.21]   +0.85 [-0.38]   +0.81 [-0.44]   +0.83 [-0.42]
  sigma   +0.48 [+0.58]   +0.99 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  -0.01 [+0.19]   -0.00 [-0.02]   +0.00 [-0.01]   +0.00 [-0.01]
  size    -0.04 [-0.43]   -0.02 [-0.11]   -0.03 [-0.12]   -0.02 [-0.11]
  llp     +0.03 [-0.16]   +0.06 [+0.20]   +0.08 [+0.20]   +0.06 [+0.20]
--- Fig. 5, below-average provisions: 753 banks: corr(buffer, u2) +0.53 (p 0.000); OLS buffer = 2.70% (0.70) + 3.61 (0.21) u2, R2 0.28; corr(max buffer, u2) +0.54
--- Fig. 5, above-average provisions: 500 banks: corr(buffer, u2) +0.48 (p 0.000); OLS buffer = 4.04% (0.74) + 1.12 (0.09) u2, R2 0.23; corr(max buffer, u2) +0.67
means: buffer 10.71% [5.0], u2 3.51% [2.4], u0 3.45% [2.7]; min buffer 1.76%; corr(u2, sigma) +0.99 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: not reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: not reproduced

===== SAMPLE B (61 largest): 61 banks
above-average provisions 13 [29], other 48 [32]
--- Table 1, all: 61 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.14]    0.18 [ 0.31]    0.28 [ 0.22]
  p25         3.09 [ 3.69]    0.74 [ 0.73]    0.54 [ 0.48]
  median      3.66 [ 4.70]    1.00 [ 0.91]    0.83 [ 0.60]
  mean        4.39 [ 5.01]    1.17 [ 0.92]    0.99 [ 0.76]
  p75         5.22 [ 5.95]    1.36 [ 1.11]    1.15 [ 1.05]
  max        10.29 [10.43]    5.19 [ 1.88]    3.68 [ 1.89]
  SD          1.85 [ 1.67]    0.84 [ 0.32]    0.64 [ 0.40]
  corr(buf,mu) +0.16 (p 0.224)   [+0.15]
  corr(buf,sigma) +0.44 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.42 (p 0.001)   [-0.37]
--- Table 1, above-average provisions: 13 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.72 [ 2.91]    0.18 [ 0.32]    0.52 [ 0.41]
  p25         3.11 [ 3.64]    0.40 [ 0.63]    0.78 [ 0.65]
  median      3.38 [ 4.09]    0.78 [ 0.81]    1.15 [ 0.78]
  mean        4.20 [ 4.61]    1.56 [ 0.81]    1.45 [ 0.95]
  p75         4.41 [ 5.27]    2.71 [ 0.97]    2.06 [ 1.19]
  max         8.76 [ 8.11]    5.19 [ 1.35]    3.68 [ 1.89]
  SD          1.80 [ 1.42]    1.60 [ 0.29]    0.95 [ 0.42]
  corr(buf,mu) +0.72 (p 0.005)   [-0.21]
  corr(buf,sigma) +0.81 (p 0.001)   [+0.58]
  corr(mu,sigma) +0.79 (p 0.001)   [-0.33]
--- Table 1, below-average provisions: 48 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.19 [ 2.14]    0.26 [ 0.31]    0.28 [ 0.22]
  p25         3.09 [ 3.85]    0.83 [ 0.85]    0.53 [ 0.43]
  median      3.81 [ 5.31]    1.02 [ 0.97]    0.76 [ 0.49]
  mean        4.44 [ 5.37]    1.07 [ 1.02]    0.87 [ 0.58]
  p75         5.24 [ 6.36]    1.31 [ 1.18]    1.05 [ 0.60]
  max        10.29 [10.43]    2.53 [ 1.88]    2.14 [ 1.33]
  SD          1.87 [ 1.82]    0.43 [ 0.31]    0.46 [ 0.27]
  corr(buf,mu) -0.24 (p 0.095)   [+0.26]
  corr(buf,sigma) +0.38 (p 0.007)   [+0.04]
  corr(mu,sigma) -0.42 (p 0.003)   [-0.17]
barriers: unique admissible solution for 50 of 61 banks
--- Table 3, all: 50 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     4.54 [ 5.01]    1.34 [ 0.80]    3.56 [ 2.40]    4.02 [ 2.72]
  sd      1.85 [ 1.67]    1.04 [ 0.78]    2.06 [ 1.63]    2.48 [ 2.09]
  mu      +0.17 [+0.15]   +0.41 [-0.47]   +0.35 [-0.48]   +0.29 [-0.47]
  sigma   +0.44 [+0.15]   +0.95 [+0.98]   +0.97 [+0.99]   +0.94 [+0.99]
  growth  +0.28 [-0.03]   +0.10 [+0.07]   +0.11 [+0.07]   +0.14 [+0.06]
  size    -0.14 [-0.39]   -0.05 [+0.11]   -0.06 [+0.10]   -0.04 [+0.10]
  llp     +0.04 [-0.34]   +0.44 [+0.49]   +0.42 [+0.48]   +0.37 [+0.47]
--- Table 3, above-average provisions: 12 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     4.25 [ 4.61]    1.84 [ 1.20]    4.54 [ 3.24]    5.07 [ 3.78]
  sd      1.87 [ 1.42]    1.55 [ 0.81]    2.86 [ 1.69]    3.23 [ 2.24]
  mu      +0.73 [-0.21]   +0.74 [-0.38]   +0.70 [-0.44]   +0.67 [-0.42]
  sigma   +0.81 [+0.58]   +0.97 [+0.98]   +0.98 [+0.99]   +0.97 [+0.99]
  growth  -0.12 [+0.19]   +0.32 [-0.02]   +0.34 [-0.01]   +0.30 [-0.01]
  size    -0.13 [-0.43]   -0.26 [-0.11]   -0.28 [-0.12]   -0.24 [-0.11]
  llp     +0.38 [-0.16]   +0.43 [+0.20]   +0.43 [+0.20]   +0.39 [+0.20]
--- Fig. 5, below-average provisions: 38 banks: corr(buffer, u2) +0.36 (p 0.026); OLS buffer = 3.33% (0.63) + 0.40 (0.17) u2, R2 0.13; corr(max buffer, u2) +0.48
--- Fig. 5, above-average provisions: 12 banks: corr(buffer, u2) +0.79 (p 0.002); OLS buffer = 1.93% (0.67) + 0.51 (0.13) u2, R2 0.62; corr(max buffer, u2) +0.81
means: buffer 4.39% [5.0], u2 3.56% [2.4], u0 3.48% [2.7]; min buffer 2.19%; corr(u2, sigma) +0.97 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: reproduced

Spearman, sample A (reported, not used): mu-sigma -0.13 (p 0.000); high-provision buffer-sigma +0.29 (p 0.000)
Spearman, sample B (reported, not used): mu-sigma -0.18 (p 0.168); high-provision buffer-sigma +0.62 (p 0.025)
```

## Verdicts under the pre-registered rule

| Finding | Run 1, A | Run 1, B | Run 2, A (1,612) | Run 2, B (61 largest) |
|---|---|---|---|---|
| F1 μ-σ correlation negative, significant | no | no | no | no |
| F2 high-provision group: lower μ, higher σ | no | no | yes | no |
| F3 high-provision group: buffer-σ positive, significant | no | yes | yes | yes |
| F4 u2-σ correlation at least 0.9 | yes | yes | yes | yes |
| F5 high-provision group: buffer-u2 positive, slope < 1, intercept > 0 | yes | yes | no | yes |
| F6 mean u2 and u0 below mean buffer | yes | no | yes | yes |
| F7 no bank with mean buffer below 2% | no | yes | no | yes |

## Reading

- Sample B, the size match to their listed banks, reproduces F3 to F7:
  the model barrier is driven by volatility (corr(u2, σ) +0.97, theirs
  0.99); among high-provision banks, buffers rise with volatility (+0.81,
  theirs +0.58) and with the model barrier (+0.79, theirs +0.57), with
  slope 0.51 below one and intercept 1.93% above zero, as theirs; and the
  model underpredicts the level of capital (mean u2 3.56% and u0 3.48%
  against a mean buffer of 4.39%; theirs 2.4%, 2.7% and 5.0%).
- F1 does not reproduce in any run. Their negative μ-σ correlation (-0.37),
  which they read as a peso problem, is positive in our Pearson
  correlations (+0.42 in B) and weakly negative in rank correlations
  (Spearman -0.18, p 0.17, in B; -0.13, p < 0.001, in A; reported only).
- F2 fails in B on mean μ only: the high-provision group's mean μ (1.56%)
  exceeds the other group's (1.07%) while its median (0.78%) is below
  (1.02%); its mean σ is higher, as theirs.
- Sample A, every national bank, differs from their sample in size and
  composition; F5 fails there on the slope (1.12, not below one) and F7
  on small banks with buffers under 2%.
- The model-side findings of Peura and Keppo (F4 to F6) reproduce on Call
  Reports for the banks most like theirs. The return-side finding they
  used to explain the low-provision group (F1) does not.
