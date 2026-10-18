# Testing thread: can a shortfall penalty close the gap? (training window)

Run 18 October 2026,
under PREREG_comparison_3.md (commit 7931b53a), part 2,
and under PREREG_comparison_2.md (commit d8cc561a) for sigma and the samples.
Only 2004-2014 data enter; nothing from the test window.

## Findings

Facts on 2005Q1-2014Q4, buffers over R = 10.5%, simulated from each
bank's 2004Q4 buffer.

| fact | observed B | baseline B | proxy B | observed A | baseline A | proxy A |
|---|---|---|---|---|---|---|
| F1 | 17.1% | 90.3% | 4.0% | 6.7% | 94.2% | 4.7% |
| F2 | 48.0% | 0.0% | 0.2% | 75.6% | 0.0% | 0.0% |
| F3 | 34.4% | 80.3% | 31.7% | 29.8% | 82.0% | 26.7% |
| F4 | 37.6% | 81.7% | 98.6% | 32.4% | 72.3% | 99.1% |

- Rule (PREREG_comparison_3.md): in sample B the proxy's F2 (0.2%) is
  47.8 points below the observed 48.0%, more than 10 points, so the
  shortfall-penalty enhancement alone is not expected to close F2.
- The proxy (the baseline with its minimum at R) moves F1 and F3 towards
  the data: F3 within 3 points in B (31.7% against 34.4%) and in A (26.7%
  against 29.8%), F1 within 10 points in A (4.7% against 6.7%) but 13.1
  below in B (4.0% against 17.1%). It moves F4 further away: issues land
  between 1 and 4 sigma in 98.6% of the proxy's episodes against 37.6%
  observed.
- In the proxy, capital almost never exceeds its minimum (R) by 4 sigma
  (F2 0.2% in B, 0.0% in A), so its dividend barrier lies within 4 sigma
  of the minimum. Real banks hold more.
- The samples are those of PREREG_comparison_2.md, whose membership
  needs a 2014Q4 buffer; sample.py reads the 2014-2024 quarterly files to
  find it, but no 2015-2024 value enters a fact or a simulation here.
- 6 banks of B and 27 used banks of A start at or below the proxy's
  minimum and start failed; 132 banks of A had no 2004Q4 buffer.

Pinned: ../replication/peura_keppo_2006/fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
Pinned: ../replication/peura_keppo_2006/fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
Pinned: sample.py sha256 857918747fc9ec718cec3efad39fe5ee5dff430528b6b77e83ba760d14a33755
Pinned: window.py sha256 69af27363f12132b2074ec1514fa68f6aa801590411278137fce168362b51340
Pinned: bound_check.py sha256 9465f6203187e68aac1daf661d05b176918c44fa39e5cb478a9d00fa88ef486a
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783
Pinned: ../common_data/fdic_q_2004.csv.gz sha256 a5f8b03c9cfe953766976da4805c609a8371f55d9cbaff6e0ef17b0f626fef58
Pinned: ../common_data/fdic_q_2005.csv.gz sha256 fd355c5d482151b976100bd8b7f537f2e68c576691acd521cf7c56c5bcbf3ec5
Pinned: ../common_data/fdic_q_2006.csv.gz sha256 3df0436ac0fd7c71c45cc5c3df488a612fb67dd08d67db91724cf486e1f13c3a
Pinned: ../common_data/fdic_q_2007.csv.gz sha256 fd9ee85fb95d919bc3331431d60500a44ee20fd4215e48b2337da3469fe5253a
Pinned: ../common_data/fdic_q_2008.csv.gz sha256 9502dcbc48edabe733836e806b87d4dde98358e05c9c60ebf78a21fe133e88f5
Pinned: ../common_data/fdic_q_2009.csv.gz sha256 b9c21601bfe3921168452c139568c10f19d0aa8c202219f7ad8ab186c8af42fd
Pinned: ../common_data/fdic_q_2010.csv.gz sha256 429946c81ed7f5f6962c552b403843f367ef6e7d0ebab72b8cbe433c12a50f1f
Pinned: ../common_data/fdic_q_2011.csv.gz sha256 8780a536cf9fa0b408cb61886ebf214913e04a48627ee1e7d6f7cd25753a8aba
Pinned: ../common_data/fdic_q_2012.csv.gz sha256 0ed0edc2bceda753b0c60d0efcc9f39d0d97ddf085bc05c53734e540407a779c
Pinned: ../common_data/fdic_q_2013.csv.gz sha256 64b685a0e7917085895f9b44ae5f86ee05602fd7171205315e914a8ffe54ead1
Pinned: ../common_data/fdic_q_2014.csv.gz sha256 7cf4299881b363f08cca394dd2d53ad5959bdd43f39e53a89b738e21a4a4d89b
Pinned: ../common_data/fdic_q_2015.csv.gz sha256 a1927a37f42ed95c7074dc196f5c06cbf29c238ef445ab2587add77df5f55373
Pinned: ../common_data/fdic_q_2016.csv.gz sha256 6111d9aebd8e12b351d5c3ccdda9f310b13c871c742d184d706062a346ef4609
Pinned: ../common_data/fdic_q_2017.csv.gz sha256 8546b1ab38b294adae748f0f5ff4fd91d9c548416d99998ad7d41bc29f049178
Pinned: ../common_data/fdic_q_2018.csv.gz sha256 d57d152847f0511a0bf76f33b2bae5c95b5bd1e0d1984ce28b5423b1ab715bb2
Pinned: ../common_data/fdic_q_2019.csv.gz sha256 363c0b516cbfb22ed7a68fb30ed19527d45a892a19fa7e1b90f696c90d253214
Pinned: ../common_data/fdic_q_2020.csv.gz sha256 26094e27170aa52b1b6c7dea8cd18de7a914423a33f542d5273b0b9fc8beac5c
Pinned: ../common_data/fdic_q_2021.csv.gz sha256 d8fc9f81ee1bbaa1a599bc950c6d152e03405ec0a68c0cfe45625e9d45168d75
Pinned: ../common_data/fdic_q_2022.csv.gz sha256 010f2271081f5b4afa27e2fa4fa3c961534a646760099935a280beb9085afa70
Pinned: ../common_data/fdic_q_2023.csv.gz sha256 7b346b82a1d1e29ced59eb58f4abad038c119f0ac48ca589052e0ce009234b05
Pinned: ../common_data/fdic_q_2024.csv.gz sha256 74af80f572fc73be46cf5cdc260bfef2e284e2c8449e470cb1037589e3d38160

Reproduce: python3 bound_check.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

===== sample B: 61 banks, 0 without a 2004Q4 buffer, 61 used
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
  F3: 32/93 = 34.4%
  F4: 35/93 = 37.6%
(a) baseline, minimum 8%: 0 banks start at or below the minimum
  F1: 331285/367033 = 90.3%
  F2: 0/456376 = 0.0%
  F3: 2126/2646 = 80.3%
  F4: 2161/2646 = 81.7%
(b) proxy, minimum at R = 10.5%: 6 banks start at or below the minimum
  F1: 12848/321389 = 4.0%
  F2: 672/387985 = 0.2%
  F3: 813/2564 = 31.7%
  F4: 2527/2564 = 98.6%

===== sample A: 668 banks, 132 without a 2004Q4 buffer, 536 used
observed, 2005-2014
  F1: 953/14185 = 6.7%
  F2: 16099/21299 = 75.6%
  F3: 122/410 = 29.8%
  F4: 133/410 = 32.4%
(a) baseline, minimum 8%: 0 banks start at or below the minimum
  F1: 3339823/3545013 = 94.2%
  F2: 0/4194290 = 0.0%
  F3: 11087/13527 = 82.0%
  F4: 9784/13527 = 72.3%
(b) proxy, minimum at R = 10.5%: 27 banks start at or below the minimum
  F1: 155946/3307548 = 4.7%
  F2: 1803/3885839 = 0.0%
  F3: 3760/14093 = 26.7%
  F4: 13969/14093 = 99.1%
```
