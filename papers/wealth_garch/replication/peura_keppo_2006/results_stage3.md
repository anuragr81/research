# Replication of Peura and Keppo (2006), stage 3: their method on recent data

Run 12 October 2026 under PREREG_stage3.md (commit cbe6181e). Data fetched
after that commit by fetch_stage2.py (December reports 2004-2024); the
FEDFUNDS file fetched with them is byte-identical to stage 2's pinned file,
which is used.

Pinned: fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
Pinned: fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

Reproduce: python3 stage3.py fdic_dec_2004_2024.csv.gz fedfunds.csv

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

######## WINDOW primary, ROA 2015-2024: institutions 9891; national banks passing the filters 453

===== SAMPLE A (all national banks), primary, ROA 2015-2024: 453 banks
above-average provisions 133 [29], other 320 [32]
--- Table 1, all: 453 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.18 [ 2.14]    0.02 [ 0.31]    0.09 [ 0.22]
  p25         5.81 [ 3.69]    0.98 [ 0.73]    0.42 [ 0.48]
  median      8.18 [ 4.70]    1.31 [ 0.91]    0.56 [ 0.60]
  mean       13.41 [ 5.01]    2.34 [ 0.92]    1.10 [ 0.76]
  p75        12.03 [ 5.95]    1.80 [ 1.11]    0.81 [ 1.05]
  max       484.40 [10.43]  127.71 [ 1.88]   53.34 [ 1.89]
  SD         33.88 [ 1.67]    9.27 [ 0.32]    3.67 [ 0.40]
  corr(buf,mu) +0.96 (p 0.000)   [+0.15]
  corr(buf,sigma) +0.93 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.93 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 133 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.18 [ 2.91]    0.07 [ 0.32]    0.13 [ 0.41]
  p25         4.99 [ 3.64]    1.07 [ 0.63]    0.44 [ 0.65]
  median      6.70 [ 4.09]    1.31 [ 0.81]    0.63 [ 0.78]
  mean        8.15 [ 4.61]    1.49 [ 0.81]    0.80 [ 0.95]
  p75         9.70 [ 5.27]    1.80 [ 0.97]    0.86 [ 1.19]
  max        31.54 [ 8.11]    5.14 [ 1.35]    6.66 [ 1.89]
  SD          4.55 [ 1.42]    0.78 [ 0.29]    0.70 [ 0.42]
  corr(buf,mu) +0.21 (p 0.015)   [-0.21]
  corr(buf,sigma) +0.43 (p 0.000)   [+0.58]
  corr(mu,sigma) +0.17 (p 0.047)   [-0.33]
--- Table 1, below-average provisions: 320 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.48 [ 2.14]    0.02 [ 0.31]    0.09 [ 0.22]
  p25         6.36 [ 3.85]    0.95 [ 0.85]    0.40 [ 0.43]
  median      8.83 [ 5.31]    1.31 [ 0.97]    0.54 [ 0.49]
  mean       15.59 [ 5.37]    2.70 [ 1.02]    1.23 [ 0.58]
  p75        13.06 [ 6.36]    1.80 [ 1.18]    0.74 [ 0.60]
  max       484.40 [10.43]  127.71 [ 1.88]   53.34 [ 1.33]
  SD         40.03 [ 1.82]   11.00 [ 0.31]    4.34 [ 0.27]
  corr(buf,mu) +0.96 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.94 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.94 (p 0.000)   [-0.17]
barriers: unique admissible solution for 256 of 453 banks
--- Table 3, all: 256 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    15.46 [ 5.01]    1.61 [ 0.80]    3.82 [ 2.40]    4.37 [ 2.72]
  sd     33.55 [ 1.67]    4.92 [ 0.78]    8.19 [ 1.63]   10.18 [ 2.09]
  mu      +0.94 [+0.15]   +0.64 [-0.47]   +0.79 [-0.48]   +0.67 [-0.47]
  sigma   +0.89 [+0.15]   +0.91 [+0.98]   +0.98 [+0.99]   +0.93 [+0.99]
  growth  +0.12 [-0.03]   +0.28 [+0.07]   +0.25 [+0.07]   +0.25 [+0.06]
  size    -0.03 [-0.39]   -0.02 [+0.11]   -0.02 [+0.10]   -0.02 [+0.10]
  llp     -0.06 [-0.34]   -0.04 [+0.49]   -0.04 [+0.48]   -0.04 [+0.47]
--- Table 3, above-average provisions: 79 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     9.34 [ 4.61]    1.12 [ 1.20]    3.06 [ 3.24]    3.46 [ 3.78]
  sd      5.19 [ 1.42]    1.54 [ 0.81]    2.67 [ 1.69]    3.75 [ 2.24]
  mu      +0.28 [-0.21]   +0.09 [-0.38]   +0.14 [-0.44]   +0.10 [-0.42]
  sigma   +0.36 [+0.58]   +0.97 [+0.98]   +0.98 [+0.99]   +0.98 [+0.99]
  growth  -0.13 [+0.19]   +0.16 [-0.02]   +0.18 [-0.01]   +0.17 [-0.01]
  size    -0.09 [-0.43]   -0.06 [-0.11]   -0.07 [-0.12]   -0.06 [-0.11]
  llp     +0.26 [-0.16]   +0.10 [+0.20]   +0.11 [+0.20]   +0.10 [+0.20]
--- Fig. 5, below-average provisions: 177 banks: corr(buffer, u2) +0.82 (p 0.000); OLS buffer = 4.15% (1.89) + 3.37 (0.18) u2, R2 0.67; corr(max buffer, u2) +0.89
--- Fig. 5, above-average provisions: 79 banks: corr(buffer, u2) +0.33 (p 0.003); OLS buffer = 7.39% (0.85) + 0.64 (0.21) u2, R2 0.11; corr(max buffer, u2) +0.19
means: buffer 13.41% [5.0], u2 3.82% [2.4], u0 3.09% [2.7]; min buffer 3.18%; corr(u2, sigma) +0.98 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: not reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: reproduced

===== SAMPLE B (61 largest), primary, ROA 2015-2024: 61 banks
above-average provisions 14 [29], other 47 [32]
--- Table 1, all: 61 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.61 [ 2.14]    0.05 [ 0.31]    0.23 [ 0.22]
  p25         4.80 [ 3.69]    1.10 [ 0.73]    0.40 [ 0.48]
  median      5.59 [ 4.70]    1.35 [ 0.91]    0.54 [ 0.60]
  mean        9.27 [ 5.01]    1.60 [ 0.92]    1.10 [ 0.76]
  p75         7.87 [ 5.95]    1.66 [ 1.11]    0.72 [ 1.05]
  max       109.80 [10.43]   10.74 [ 1.88]   22.91 [ 1.89]
  SD         14.20 [ 1.67]    1.36 [ 0.32]    2.98 [ 0.40]
  corr(buf,mu) +0.92 (p 0.000)   [+0.15]
  corr(buf,sigma) +0.95 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.88 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 14 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         4.05 [ 2.91]    0.48 [ 0.32]    0.31 [ 0.41]
  p25         4.89 [ 3.64]    1.03 [ 0.63]    0.44 [ 0.65]
  median      5.07 [ 4.09]    1.28 [ 0.81]    0.61 [ 0.78]
  mean        7.11 [ 4.61]    1.35 [ 0.81]    1.03 [ 0.95]
  p75         5.56 [ 5.27]    1.69 [ 0.97]    0.73 [ 1.19]
  max        20.21 [ 8.11]    2.43 [ 1.35]    6.66 [ 1.89]
  SD          5.16 [ 1.42]    0.48 [ 0.29]    1.63 [ 0.42]
  corr(buf,mu) +0.35 (p 0.226)   [-0.21]
  corr(buf,sigma) +0.79 (p 0.001)   [+0.58]
  corr(mu,sigma) +0.32 (p 0.259)   [-0.33]
--- Table 1, below-average provisions: 47 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.61 [ 2.14]    0.05 [ 0.31]    0.23 [ 0.22]
  p25         4.75 [ 3.85]    1.10 [ 0.85]    0.40 [ 0.43]
  median      5.80 [ 5.31]    1.37 [ 0.97]    0.51 [ 0.49]
  mean        9.92 [ 5.37]    1.67 [ 1.02]    1.12 [ 0.58]
  p75         8.09 [ 6.36]    1.65 [ 1.18]    0.71 [ 0.60]
  max       109.80 [10.43]   10.74 [ 1.88]   22.91 [ 1.33]
  SD         15.93 [ 1.82]    1.53 [ 0.31]    3.29 [ 0.27]
  corr(buf,mu) +0.93 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.97 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.91 (p 0.000)   [-0.17]
barriers: unique admissible solution for 29 of 61 banks
--- Table 3, all: 29 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    12.51 [ 5.01]    2.88 [ 0.80]    5.39 [ 2.40]    7.34 [ 2.72]
  sd     19.90 [ 1.67]    8.91 [ 0.78]   12.19 [ 1.63]   19.93 [ 2.09]
  mu      +0.97 [+0.15]   +0.93 [-0.47]   +0.92 [-0.48]   +0.92 [-0.47]
  sigma   +0.96 [+0.15]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  -0.01 [-0.03]   +0.04 [+0.07]   +0.02 [+0.07]   +0.04 [+0.06]
  size    -0.10 [-0.39]   -0.10 [+0.11]   -0.11 [+0.10]   -0.10 [+0.10]
  llp     -0.13 [-0.34]   -0.11 [+0.49]   -0.10 [+0.48]   -0.11 [+0.47]
--- Table 3, above-average provisions: 8 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     8.86 [ 4.61]    2.10 [ 1.20]    4.48 [ 3.24]    5.64 [ 3.78]
  sd      6.41 [ 1.42]    3.97 [ 0.81]    6.60 [ 1.69]    9.55 [ 2.24]
  mu      +0.86 [-0.21]   +0.63 [-0.38]   +0.64 [-0.44]   +0.62 [-0.42]
  sigma   +0.77 [+0.58]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  +0.44 [+0.19]   +0.22 [-0.02]   +0.23 [-0.01]   +0.22 [-0.01]
  size    -0.20 [-0.43]   -0.24 [-0.11]   -0.24 [-0.12]   -0.24 [-0.11]
  llp     +0.42 [-0.16]   -0.02 [+0.20]   -0.00 [+0.20]   -0.02 [+0.20]
--- Fig. 5, below-average provisions: 21 banks: corr(buffer, u2) +0.98 (p 0.000); OLS buffer = 4.58% (1.21) + 1.62 (0.08) u2, R2 0.95; corr(max buffer, u2) +0.99
--- Fig. 5, above-average provisions: 8 banks: corr(buffer, u2) +0.76 (p 0.028); OLS buffer = 5.54% (1.95) + 0.74 (0.26) u2, R2 0.58; corr(max buffer, u2) +0.89
means: buffer 9.27% [5.0], u2 5.39% [2.4], u0 3.92% [2.7]; min buffer 3.61%; corr(u2, sigma) +1.00 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: not reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: reproduced

Spearman, sample A (reported, not used): mu-sigma +0.24 (p 0.000); high-provision buffer-sigma +0.53 (p 0.000)
Spearman, sample B (reported, not used): mu-sigma +0.15 (p 0.265); high-provision buffer-sigma +0.36 (p 0.203)

######## WINDOW secondary, ROA 2005-2024: institutions 9891; national banks passing the filters 655

===== SAMPLE A (all national banks), secondary, ROA 2005-2024: 655 banks
above-average provisions 219 [29], other 436 [32]
--- Table 1, all: 655 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.88 [ 2.14]    0.01 [ 0.31]    0.18 [ 0.22]
  p25         6.44 [ 3.69]    0.94 [ 0.73]    0.45 [ 0.48]
  median      9.43 [ 4.70]    1.34 [ 0.91]    0.63 [ 0.60]
  mean       14.93 [ 5.01]    3.46 [ 0.92]    4.78 [ 0.76]
  p75        14.30 [ 5.95]    1.87 [ 1.11]    0.94 [ 1.05]
  max       484.40 [10.43]  950.87 [ 1.88] 2386.06 [ 1.89]
  SD         34.33 [ 1.67]   37.72 [ 0.32]   93.25 [ 0.40]
  corr(buf,mu) +0.57 (p 0.000)   [+0.15]
  corr(buf,sigma) +0.50 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.99 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 219 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.18 [ 2.91]    0.01 [ 0.32]    0.24 [ 0.41]
  p25         5.34 [ 3.64]    0.75 [ 0.63]    0.51 [ 0.65]
  median      7.61 [ 4.09]    1.13 [ 0.81]    0.77 [ 0.78]
  mean        9.63 [ 4.61]    1.71 [ 0.81]    1.31 [ 0.95]
  p75        11.58 [ 5.27]    1.59 [ 0.97]    1.23 [ 1.19]
  max       103.03 [ 8.11]   94.85 [ 1.35]   58.54 [ 1.89]
  SD          8.37 [ 1.42]    6.38 [ 0.29]    3.99 [ 0.42]
  corr(buf,mu) +0.78 (p 0.000)   [-0.21]
  corr(buf,sigma) +0.80 (p 0.000)   [+0.58]
  corr(mu,sigma) +0.97 (p 0.000)   [-0.33]
--- Table 1, below-average provisions: 436 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         2.88 [ 2.14]    0.07 [ 0.31]    0.18 [ 0.22]
  p25         7.37 [ 3.85]    1.04 [ 0.85]    0.44 [ 0.43]
  median     10.22 [ 5.31]    1.44 [ 0.97]    0.60 [ 0.49]
  mean       17.60 [ 5.37]    4.34 [ 1.02]    6.52 [ 0.58]
  p75        15.99 [ 6.36]    1.96 [ 1.18]    0.82 [ 0.60]
  max       484.40 [10.43]  950.87 [ 1.88] 2386.06 [ 1.33]
  SD         41.42 [ 1.82]   46.00 [ 0.31]  114.27 [ 0.27]
  corr(buf,mu) +0.57 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.50 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.99 (p 0.000)   [-0.17]
barriers: unique admissible solution for 389 of 655 banks
--- Table 3, all: 389 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    16.75 [ 5.01]    1.94 [ 0.80]    4.30 [ 2.40]    4.97 [ 2.72]
  sd     38.65 [ 1.67]    7.48 [ 0.78]   11.07 [ 1.63]   13.40 [ 2.09]
  mu      +0.66 [+0.15]   +0.82 [-0.47]   +0.87 [-0.48]   +0.79 [-0.47]
  sigma   +0.74 [+0.15]   +0.98 [+0.98]   +0.99 [+0.99]   +0.97 [+0.99]
  growth  +0.02 [-0.03]   +0.02 [+0.07]   +0.02 [+0.07]   +0.02 [+0.06]
  size    -0.02 [-0.39]   -0.02 [+0.11]   -0.02 [+0.10]   -0.02 [+0.10]
  llp     -0.07 [-0.34]   +0.28 [+0.49]   +0.27 [+0.48]   +0.27 [+0.47]
--- Table 3, above-average provisions: 145 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    10.12 [ 4.61]    2.18 [ 1.20]    4.73 [ 3.24]    5.52 [ 3.78]
  sd      9.61 [ 1.42]    8.73 [ 0.81]   12.29 [ 1.69]   14.56 [ 2.24]
  mu      +0.84 [-0.21]   +0.99 [-0.38]   +0.98 [-0.44]   +0.97 [-0.42]
  sigma   +0.83 [+0.58]   +1.00 [+0.98]   +1.00 [+0.99]   +0.99 [+0.99]
  growth  +0.16 [+0.19]   +0.02 [-0.02]   +0.02 [-0.01]   +0.02 [-0.01]
  size    -0.04 [-0.43]   -0.03 [-0.11]   -0.03 [-0.12]   -0.03 [-0.11]
  llp     +0.43 [-0.16]   +0.49 [+0.20]   +0.50 [+0.20]   +0.51 [+0.20]
--- Fig. 5, below-average provisions: 244 banks: corr(buffer, u2) +0.90 (p 0.000); OLS buffer = 3.76% (1.44) + 4.18 (0.13) u2, R2 0.81; corr(max buffer, u2) +0.93
--- Fig. 5, above-average provisions: 145 banks: corr(buffer, u2) +0.83 (p 0.000); OLS buffer = 7.06% (0.48) + 0.65 (0.04) u2, R2 0.68; corr(max buffer, u2) +0.63
means: buffer 14.93% [5.0], u2 4.30% [2.4], u0 20.43% [2.7]; min buffer 2.88%; corr(u2, sigma) +0.99 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: not reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: reproduced
  F6 mean u2 and mean u0 below mean buffer: not reproduced
  F7 no bank with mean buffer below 2%: reproduced

===== SAMPLE B (61 largest), secondary, ROA 2005-2024: 61 banks
above-average provisions 21 [29], other 40 [32]
--- Table 1, all: 61 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.61 [ 2.14]    0.01 [ 0.31]    0.25 [ 0.22]
  p25         4.84 [ 3.69]    0.96 [ 0.73]    0.44 [ 0.48]
  median      5.55 [ 4.70]    1.26 [ 0.91]    0.64 [ 0.60]
  mean        9.11 [ 5.01]    1.44 [ 0.92]    1.25 [ 0.76]
  p75         7.48 [ 5.95]    1.73 [ 1.11]    0.95 [ 1.05]
  max       109.80 [10.43]    5.59 [ 1.88]   16.61 [ 1.89]
  SD         14.17 [ 1.67]    0.92 [ 0.32]    2.27 [ 0.40]
  corr(buf,mu) +0.72 (p 0.000)   [+0.15]
  corr(buf,sigma) +0.90 (p 0.000)   [+0.15]
  corr(mu,sigma) +0.59 (p 0.000)   [-0.37]
--- Table 1, above-average provisions: 21 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         4.05 [ 2.91]    0.01 [ 0.32]    0.44 [ 0.41]
  p25         4.99 [ 3.64]    0.72 [ 0.63]    0.73 [ 0.65]
  median      5.38 [ 4.09]    0.96 [ 0.81]    0.89 [ 0.78]
  mean        7.81 [ 4.61]    1.26 [ 0.81]    1.48 [ 0.95]
  p75         6.66 [ 5.27]    1.60 [ 0.97]    1.84 [ 1.19]
  max        22.07 [ 8.11]    3.18 [ 1.35]    5.44 [ 1.89]
  SD          5.45 [ 1.42]    0.96 [ 0.29]    1.37 [ 0.42]
  corr(buf,mu) +0.54 (p 0.011)   [-0.21]
  corr(buf,sigma) +0.65 (p 0.001)   [+0.58]
  corr(mu,sigma) +0.41 (p 0.068)   [-0.33]
--- Table 1, below-average provisions: 40 banks   (theirs in brackets)
                     buffer               mu            sigma
  min         3.61 [ 2.14]    0.33 [ 0.31]    0.25 [ 0.22]
  p25         4.60 [ 3.85]    1.15 [ 0.85]    0.38 [ 0.43]
  median      5.63 [ 5.31]    1.34 [ 0.97]    0.55 [ 0.49]
  mean        9.79 [ 5.37]    1.54 [ 1.02]    1.13 [ 0.58]
  p75         7.90 [ 6.36]    1.75 [ 1.18]    0.72 [ 0.60]
  max       109.80 [10.43]    5.59 [ 1.88]   16.61 [ 1.33]
  SD         17.09 [ 1.82]    0.90 [ 0.31]    2.64 [ 0.27]
  corr(buf,mu) +0.83 (p 0.000)   [+0.26]
  corr(buf,sigma) +0.94 (p 0.000)   [+0.04]
  corr(mu,sigma) +0.70 (p 0.000)   [-0.17]
barriers: unique admissible solution for 32 of 61 banks
--- Table 3, all: 32 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg    11.87 [ 5.01]    2.69 [ 0.80]    5.48 [ 2.40]    7.13 [ 2.72]
  sd     19.14 [ 1.67]    6.00 [ 0.78]    8.63 [ 1.63]   13.71 [ 2.09]
  mu      +0.87 [+0.15]   +0.79 [-0.47]   +0.79 [-0.48]   +0.78 [-0.47]
  sigma   +0.94 [+0.15]   +1.00 [+0.98]   +1.00 [+0.99]   +1.00 [+0.99]
  growth  +0.17 [-0.03]   +0.04 [+0.07]   +0.05 [+0.07]   +0.03 [+0.06]
  size    -0.08 [-0.39]   -0.12 [+0.11]   -0.13 [+0.10]   -0.12 [+0.10]
  llp     -0.07 [-0.34]   +0.09 [+0.49]   +0.14 [+0.48]   +0.11 [+0.47]
--- Table 3, above-average provisions: 15 banks with u1, u2   (theirs in brackets)
              buf              u1              u2              u0
  avg     8.38 [ 4.61]    2.50 [ 1.20]    5.57 [ 3.24]    6.96 [ 3.78]
  sd      6.16 [ 1.42]    2.89 [ 0.81]    4.81 [ 1.69]    7.07 [ 2.24]
  mu      +0.93 [-0.21]   +0.76 [-0.38]   +0.70 [-0.44]   +0.72 [-0.42]
  sigma   +0.71 [+0.58]   +0.97 [+0.98]   +1.00 [+0.99]   +0.99 [+0.99]
  growth  +0.53 [+0.19]   -0.08 [-0.02]   -0.06 [-0.01]   -0.08 [-0.01]
  size    -0.12 [-0.43]   -0.19 [-0.11]   -0.22 [-0.12]   -0.21 [-0.11]
  llp     +0.61 [-0.16]   +0.68 [+0.20]   +0.66 [+0.20]   +0.66 [+0.20]
--- Fig. 5, below-average provisions: 17 banks: corr(buffer, u2) +0.98 (p 0.000); OLS buffer = 2.80% (1.49) + 2.25 (0.12) u2, R2 0.96; corr(max buffer, u2) +0.99
--- Fig. 5, above-average provisions: 15 banks: corr(buffer, u2) +0.68 (p 0.005); OLS buffer = 3.52% (1.89) + 0.87 (0.26) u2, R2 0.46; corr(max buffer, u2) +0.74
means: buffer 9.11% [5.0], u2 5.48% [2.4], u0 4.25% [2.7]; min buffer 3.61%; corr(u2, sigma) +1.00 [0.99]
  F1 mu-sigma correlation negative and significant: not reproduced
  F2 high-provision group: lower mean mu, higher mean sigma: reproduced
  F3 high-provision group: buffer-sigma correlation positive and significant: reproduced
  F4 u2-sigma correlation at least 0.9: reproduced
  F5 high-provision group: buffer-u2 positive significant, slope < 1, intercept > 0: not reproduced
  F6 mean u2 and mean u0 below mean buffer: reproduced
  F7 no bank with mean buffer below 2%: reproduced

Spearman, sample A (reported, not used): mu-sigma +0.13 (p 0.001); high-provision buffer-sigma +0.25 (p 0.000)
Spearman, sample B (reported, not used): mu-sigma -0.13 (p 0.327); high-provision buffer-sigma +0.37 (p 0.095)
```

## Verdicts under the pre-registered rule

| Finding | Stage 2 run 2, B (1985-2002) | Primary A (453) | Primary B (61) | Secondary A (655) | Secondary B (61) |
|---|---|---|---|---|---|
| F1 μ-σ negative, significant | no | no | no | no | no |
| F2 high provisions: lower μ, higher σ | no | no | no | no | yes |
| F3 high provisions: buffer-σ positive, significant | yes | yes | yes | yes | yes |
| F4 u2-σ correlation at least 0.9 | yes | yes | yes | yes | yes |
| F5 high provisions: buffer-u2 positive, slope < 1, intercept > 0 | yes | no | no | yes | no |
| F6 mean u2 and u0 below mean buffer | yes | yes | yes | no | yes |
| F7 no mean buffer below 2% | yes | yes | yes | yes | yes |

## Reading

- F3, F4 and F7 hold in every sample and window: among high-provision
  banks the buffer rises with return volatility, and the model barrier is
  volatility. F1 holds in none: on no data, period or sample does the mean
  return fall with its volatility. Rank correlations (reported only) are
  +0.24 and +0.15 in the primary window and +0.13 and -0.13 in the
  secondary; the crisis years of 2008-2010 do not produce Peura and
  Keppo's negative relation.
- F5 fails in four of five cases on its conjunction of conditions: the
  buffer-u2 correlation is positive and significant everywhere among
  high-provision banks (+0.33, +0.76, +0.83, +0.68), but the slope is not
  significantly below one, or the intercept not significantly above zero,
  with eight to fifteen banks in sample B.
- F6 fails only in secondary A, where a few banks with tiny risk-weighted
  assets give u0 values of hundreds of percent (mean 20.4%); in every other
  case the model underpredicts the level of capital, now by more than in
  their sample (mean buffers of 9-15% against model barriers of 3-5%).
- Outliers again dominate Pearson moments in sample A and in B's
  below-average group (buffer up to 110%, σ up to 23% of risk-weighted
  assets): banks with very small risk-weighted assets relative to income.
  The merger screen does not remove them; they are not mergers.

## Banks without a barrier solution (diagnostic, after the run)

Only 29 of 61 banks in primary B and 32 of 61 in secondary B have a unique
admissible (u1, u2). Every unsolved bank in primary B has μ/σ of 2.6 or more
and u0 of at most 1.4%: safe, profitable banks for which, at K = 0.25%, the
model has no issue barrier, so its optimum is the no-issuance policy with
dividend barrier u0. The pre-registered rule leaves them out of the u1 and
u2 statistics, so those statistics rest on the less safe half of the large
banks. F6 also uses u0 over all banks.
