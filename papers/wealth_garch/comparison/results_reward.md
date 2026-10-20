# Testing thread: can a reward for capital close the gap? (training window)

Run 20 October 2026,
under PREREG_comparison_4.md (commit daa0eae7),
with sigma, the samples and the facts as under PREREG_comparison_2.md.
Only 2004-2014 data enter the facts and the simulations.

## Findings

Sample B (61 banks), 2005-2014; observed F2 48.0%, F1 17.1%. The reward
adds c (x - xbar_i) to the drift of the buffer; c_ins = 0.01264 is the
insurer's channel (ASMT-D2).

| c | F2, minimum 8% | F2, minimum at R | F1, minimum at R | median barrier, minimum at R |
|---|---|---|---|---|
| 0 | 0.5% | 5.2% | 4.1% | 0.0124 |
| c_ins (central) | 1.1% | 8.3% | 3.6% | 0.0127 |
| 2 c_ins | 2.0% | 9.9% | 1.8% | 0.0135 |
| 3 c_ins | 5.9% | 17.4% | 0.3% | 0.0157 |

- Rule (PREREG_comparison_4.md): at the central point with the minimum at R
  the predicted F2 is 8.3% against the observed 48.0%, 39.7 points short;
  at 3 c_ins it is 17.4%, 30.6 points short. F2 fails at every grid point,
  so the reward channel, linear, uncapped and up to three times the
  insurer's, cannot close F2.
- The reward moves the dividend barrier little: its median over the
  model's minimum rises from 0.0124 to 0.0157 at three times the insurer's
  channel. Holding a unit of capital still costs the shareholders' rate
  rho = 4% less the reward, and the reward stays below rho on the whole
  grid.
- Banks with no barrier below 2.0 are left out of the predictions: with
  the minimum at R, 1 to 3 of the 61 depending on the point. Counting
  every quarter of such a bank as far above R would add at most about
  their share of the banks, 3 of 61 or about 5 points, to F2; 3 c_ins
  would still be about 26 points short.
- The barrier solver matches the closed form at c = 0 for every bank of
  sample A (largest difference 2.62e-10).
- Sample A (reported, decides nothing): F2 observed 75.6%; with the
  minimum at R, 6.4% at c_ins and 20.7% at 3 c_ins.
- The c = 0 row with the minimum at R (F2 5.2%) differs from
  results_bound.md's proxy (0.2%) because this check has no issuance:
  every bank pays at u0, above the issuer banks' u2.

## Limits

- One channel, in a model with no issuance; F3 and F4 are not scored.
- The assessment base is taken as total assets (ASMT-D2), and the creditors'
  channel is only bounded by the grid, not measured.

Pinned: ../replication/peura_keppo_2006/fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
Pinned: ../replication/peura_keppo_2006/fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
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
Pinned: sample.py sha256 857918747fc9ec718cec3efad39fe5ee5dff430528b6b77e83ba760d14a33755
Pinned: window.py sha256 69af27363f12132b2074ec1514fa68f6aa801590411278137fce168362b51340
Pinned: reward_check.py sha256 47c4d9b8fb4a0d48a61a152853e055c273220e7d68a764408a437044c24fb224
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783

Reproduce: python3 reward_check.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
c_ins = 0.01264; grid 0.00000, 0.01264, 0.02528, 0.03792; rho = 0.04
validation: largest |barrier at c = 0 - closed-form u0| over sample A = 2.62e-10 -> passes

===== sample B: 61 banks with a 2004Q4 buffer
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
  F3: 32/93 = 34.4%
  F4: 35/93 = 37.6%
minimum 8% (reward alone), c = 0.00000: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0124
  F1: 315173/352387 = 89.4%
  F2: 2469/450710 = 0.5%
minimum 8% (reward alone), c = 0.01264: 2 banks with no barrier below 2.0; 0 start failed; median barrier 0.0126
  F1: 303092/345168 = 87.8%
  F2: 4854/445074 = 1.1%
minimum 8% (reward alone), c = 0.02528: 4 banks with no barrier below 2.0; 0 start failed; median barrier 0.0135
  F1: 285624/337545 = 84.6%
  F2: 8887/443291 = 2.0%
minimum 8% (reward alone), c = 0.03792: 5 banks with no barrier below 2.0; 0 start failed; median barrier 0.0143
  F1: 261401/322447 = 81.1%
  F2: 25071/428009 = 5.9%
minimum at R (reward and penalty proxy), c = 0.00000: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0124
  F1: 12597/305053 = 4.1%
  F2: 19694/378488 = 5.2%
minimum at R (reward and penalty proxy), c = 0.01264: 1 banks with no barrier below 2.0; 5 start failed; median barrier 0.0127
  F1: 10875/301450 = 3.6%
  F2: 30904/374122 = 8.3%
minimum at R (reward and penalty proxy), c = 0.02528: 3 banks with no barrier below 2.0; 5 start failed; median barrier 0.0135
  F1: 5396/295341 = 1.8%
  F2: 36472/369765 = 9.9%
minimum at R (reward and penalty proxy), c = 0.03792: 3 banks with no barrier below 2.0; 5 start failed; median barrier 0.0157
  F1: 912/286718 = 0.3%
  F2: 64021/368560 = 17.4%

===== sample A: 536 banks with a 2004Q4 buffer
observed, 2005-2014
  F1: 953/14185 = 6.7%
  F2: 16099/21299 = 75.6%
  F3: 122/410 = 29.8%
  F4: 133/410 = 32.4%
minimum 8% (reward alone), c = 0.00000: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0117
  F1: 3244152/3477666 = 93.3%
  F2: 2886/4170293 = 0.1%
minimum 8% (reward alone), c = 0.01264: 8 banks with no barrier below 2.0; 0 start failed; median barrier 0.0125
  F1: 3124107/3389846 = 92.2%
  F2: 7297/4133354 = 0.2%
minimum 8% (reward alone), c = 0.02528: 14 banks with no barrier below 2.0; 0 start failed; median barrier 0.0136
  F1: 2979819/3288802 = 90.6%
  F2: 23827/4083806 = 0.6%
minimum 8% (reward alone), c = 0.03792: 30 banks with no barrier below 2.0; 0 start failed; median barrier 0.0163
  F1: 2690781/3157363 = 85.2%
  F2: 169764/3997107 = 4.2%
minimum at R (reward and penalty proxy), c = 0.00000: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0117
  F1: 154098/3226421 = 4.8%
  F2: 137313/3841784 = 3.6%
minimum at R (reward and penalty proxy), c = 0.01264: 3 banks with no barrier below 2.0; 26 start failed; median barrier 0.0123
  F1: 117151/3168728 = 3.7%
  F2: 245613/3818033 = 6.4%
minimum at R (reward and penalty proxy), c = 0.02528: 11 banks with no barrier below 2.0; 26 start failed; median barrier 0.0135
  F1: 85006/3103342 = 2.7%
  F2: 395823/3803374 = 10.4%
minimum at R (reward and penalty proxy), c = 0.03792: 19 banks with no barrier below 2.0; 26 start failed; median barrier 0.0158
  F1: 37935/3008796 = 1.3%
  F2: 774632/3745709 = 20.7%
```
