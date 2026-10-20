# Testing thread: four mechanisms for large voluntary buffers (training window)

Run 20 October 2026,
under PREREG_comparison_5.md (commit f2d1301e),
with sigma, the samples and the facts as under PREREG_comparison_2.md.
Only 2004-2014 data enter the facts and the simulations.

## Findings

Sample B (61 banks), 2005-2014; observed F2 48.0%, F1 17.1%. No
issuance; dividends at the barrier.

| mechanism | point | F2, minimum 8% | F2, minimum at R | banks with no barrier |
|---|---|---|---|---|
| 1. rare large losses | J = 0.04, lambda = 1/25 (central) | 0.7% | 4.0% | 0 |
| 1. rare large losses | largest F2 on the grid (J = 0.02, lambda = 1/10) | 0.8% | 12.7% | 0 |
| 2. no issuance in a crisis | never issue (results_reward.md, c = 0) | 0.5% | 5.2% | 0 |
| 3. growth | g_i (central) | 0.1% | 23.7% | 54 of 61 |
| 4. supervisors expect more | minimum at R + 0.01 | | 59.1% | 0 (26 start failed) |
| 4. supervisors expect more | minimum at R + 0.02 | | 82.3% | 0 (39 start failed) |

- 1, rare large losses: fails by the rule. Losses of 2 to 6 points of
  risk-weighted assets once in 10 or 25 years lower the dividend barrier
  (median 0.0115 to 0.0087 over the minimum, against 0.0124 with no
  jumps) instead of raising it. Our reading: with mean earnings held at
  mu_i, the risk of sudden failure shortens the bank's expected life, and
  it pays out sooner.
- 2, no issuance in a crisis: fails by the rule on the rows of
  results_reward.md, as fixed in advance.
- 3, growth: by the rule's letter it fails (23.7% against 48.0% with the
  minimum at R), but the rule cannot judge it. 52 of the 61 banks grew
  their assets by 4% a year or more over 2005-2014 (median g_i 0.0799),
  at or above rho, so for them the model has no finite value and no
  barrier: growing for ever at that rate, the bank never pays out. The 7
  banks left are the slow growers, and the bound of results_reward.md
  (counting every excluded bank's quarters as far above R) allows up to
  54 of 61 banks, which says nothing. Growth is strong enough to break
  the model, as the convex equity term did for MMP.
- 4, supervisors expecting more: no grid point is within 10 points (5.2%
  at d = 0 in results_reward.md, 59.1% at d = 0.01), and the figures for
  d > 0 are biased upwards: 26 of the 61 banks (39 at d = 0.02) start
  2005 below the raised minimum and are dropped as failed, so F2 runs
  over the better-capitalized banks. A hard minimum one point above R is
  also contradicted by the record: those 26 banks held less than 11.5% of
  risk-weighted assets at the end of 2004 and were not closed: the sample
  requires a buffer reported at the end of 2014. F1 falls
  to 0.1% or less, against the observed 17.1%.
- The barrier solver matches the closed form with no jumps and no growth
  for every bank of sample A (largest difference 1.65e-07).
- Sample A (reported, decides nothing): observed F2 75.6%; at the central
  points with the minimum at R, jumps 2.8%, growth 18.7% with 366 of 536
  banks without a barrier, R + 0.01 61.0% with 101 starting failed.

## Reading

None of the four closes F2 within this model family as fixed in advance.
Two are informative: growth, which retains capital so strongly that the
model breaks for most banks, so that a version with growth that slows
would be needed to judge it; and supervisors' expectations, which reach
the observed level with a minimum between R and R + 1 point, but not as
a hard floor, since banks below it were not closed. These two are the
candidates for the evidence phase.

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
Pinned: mechanisms.py sha256 1d154eb182db187d2505b61c3179ba32ccdca2b8890565db6da6a169e1a27349
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783

Reproduce: python3 mechanisms.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
validation: largest |numerical barrier - closed-form u0| over sample A = 1.65e-07 -> passes

===== sample B: 61 banks with a 2004Q4 buffer; median g_i 0.0799, 52 with g_i >= rho
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
  F3: 32/93 = 34.4%
  F4: 35/93 = 37.6%
1. jumps J = 0.02, lambda = 0.040, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0114
  F1: 233135/303326 = 76.9%
  F2: 2611/395301 = 0.7%
1. jumps J = 0.02, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0100
  F1: 185683/245474 = 75.6%
  F2: 2744/327559 = 0.8%
1. jumps J = 0.04, lambda = 0.040 (central), minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0115
  F1: 286759/304037 = 94.3%
  F2: 2711/376234 = 0.7%
1. jumps J = 0.04, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0093
  F1: 229335/244536 = 93.8%
  F2: 7428/295791 = 2.5%
1. jumps J = 0.06, lambda = 0.040, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0111
  F1: 294020/309420 = 95.0%
  F2: 7394/379496 = 1.9%
1. jumps J = 0.06, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0087
  F1: 241599/250913 = 96.3%
  F2: 3574/289139 = 1.2%
3. growth 0.5 g_i, minimum 8%: 33 banks with no barrier below 2.0; 0 start failed; median barrier 0.0168
  F1: 127627/157304 = 81.1%
  F2: 3338/218955 = 1.5%
3. growth 1.0 g_i (central), minimum 8%: 54 banks with no barrier below 2.0; 0 start failed; median barrier 0.0429
  F1: 18636/30197 = 61.7%
  F2: 57/54963 = 0.1%
3. growth 1.5 g_i, minimum 8%: 58 banks with no barrier below 2.0; 0 start failed; median barrier 0.0487
  F1: 1169/11385 = 10.3%
  F2: 0/23591 = 0.0%
1. jumps J = 0.02, lambda = 0.040, minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0114
  F1: 26162/258887 = 10.1%
  F2: 33451/325682 = 10.3%
1. jumps J = 0.02, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0100
  F1: 41840/207754 = 20.1%
  F2: 33668/265058 = 12.7%
1. jumps J = 0.04, lambda = 0.040 (central), minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0115
  F1: 31994/265696 = 12.0%
  F2: 12858/319998 = 4.0%
1. jumps J = 0.04, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0093
  F1: 58740/214496 = 27.4%
  F2: 16464/252091 = 6.5%
1. jumps J = 0.06, lambda = 0.040, minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0111
  F1: 39292/268998 = 14.6%
  F2: 10047/321052 = 3.1%
1. jumps J = 0.06, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 6 start failed; median barrier 0.0087
  F1: 74657/220710 = 33.8%
  F2: 4775/249620 = 1.9%
3. growth 0.5 g_i, minimum at R: 33 banks with no barrier below 2.0; 1 start failed; median barrier 0.0173
  F1: 305/138679 = 0.2%
  F2: 12239/185463 = 6.6%
3. growth 1.0 g_i (central), minimum at R: 54 banks with no barrier below 2.0; 0 start failed; median barrier 0.0430
  F1: 0/24485 = 0.0%
  F2: 11179/47226 = 23.7%
3. growth 1.5 g_i, minimum at R: 58 banks with no barrier below 2.0; 0 start failed; median barrier 0.0484
  F1: 1/9384 = 0.0%
  F2: 2876/18597 = 15.5%
4. minimum at R + 0.01: 0 banks with no barrier below 2.0; 26 start failed; median barrier 0.0124
  F1: 138/191186 = 0.1%
  F2: 140881/238491 = 59.1%
4. minimum at R + 0.02: 0 banks with no barrier below 2.0; 39 start failed; median barrier 0.0124
  F1: 40/121223 = 0.0%
  F2: 120433/146304 = 82.3%
4. minimum at R + 0.03: 0 banks with no barrier below 2.0; 45 start failed; median barrier 0.0124
  F1: 4/94519 = 0.0%
  F2: 105203/113800 = 92.4%

===== sample A: 536 banks with a 2004Q4 buffer; median g_i 0.0524, 356 with g_i >= rho
observed, 2005-2014
  F1: 953/14185 = 6.7%
  F2: 16099/21299 = 75.6%
  F3: 122/410 = 29.8%
  F4: 133/410 = 32.4%
1. jumps J = 0.02, lambda = 0.040, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0100
  F1: 2480143/2968862 = 83.5%
  F2: 3060/3584021 = 0.1%
1. jumps J = 0.02, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0085
  F1: 2028566/2350708 = 86.3%
  F2: 3213/2830562 = 0.1%
1. jumps J = 0.04, lambda = 0.040 (central), minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0097
  F1: 2843437/2965107 = 95.9%
  F2: 3535/3467331 = 0.1%
1. jumps J = 0.04, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0080
  F1: 2298561/2364224 = 97.2%
  F2: 23862/2665913 = 0.9%
1. jumps J = 0.06, lambda = 0.040, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0094
  F1: 2912171/3019407 = 96.4%
  F2: 15572/3459646 = 0.5%
1. jumps J = 0.06, lambda = 0.100, minimum 8%: 0 banks with no barrier below 2.0; 0 start failed; median barrier 0.0075
  F1: 2370204/2414394 = 98.2%
  F2: 6294/2641713 = 0.2%
3. growth 0.5 g_i, minimum 8%: 140 banks with no barrier below 2.0; 0 start failed; median barrier 0.0147
  F1: 2222647/2426792 = 91.6%
  F2: 13125/3091728 = 0.4%
3. growth 1.0 g_i (central), minimum 8%: 364 banks with no barrier below 2.0; 0 start failed; median barrier 0.0191
  F1: 813214/937819 = 86.7%
  F2: 10754/1331768 = 0.8%
3. growth 1.5 g_i, minimum 8%: 441 banks with no barrier below 2.0; 0 start failed; median barrier 0.0232
  F1: 402189/482496 = 83.4%
  F2: 9944/736108 = 1.4%
1. jumps J = 0.02, lambda = 0.040, minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0100
  F1: 233635/2745898 = 8.5%
  F2: 340740/3283883 = 10.4%
1. jumps J = 0.02, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0085
  F1: 360019/2174660 = 16.6%
  F2: 228419/2578171 = 8.9%
1. jumps J = 0.04, lambda = 0.040 (central), minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0097
  F1: 261319/2768887 = 9.4%
  F2: 91272/3206774 = 2.8%
1. jumps J = 0.04, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0080
  F1: 500202/2206576 = 22.7%
  F2: 39522/2462802 = 1.6%
1. jumps J = 0.06, lambda = 0.040, minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0094
  F1: 302470/2806957 = 10.8%
  F2: 20476/3196456 = 0.6%
1. jumps J = 0.06, lambda = 0.100, minimum at R: 0 banks with no barrier below 2.0; 27 start failed; median barrier 0.0075
  F1: 637952/2255623 = 28.3%
  F2: 9672/2452626 = 0.4%
3. growth 0.5 g_i, minimum at R: 143 banks with no barrier below 2.0; 14 start failed; median barrier 0.0150
  F1: 39940/2218392 = 1.8%
  F2: 308361/2880647 = 10.7%
3. growth 1.0 g_i (central), minimum at R: 366 banks with no barrier below 2.0; 2 start failed; median barrier 0.0203
  F1: 1401/869572 = 0.2%
  F2: 237126/1270524 = 18.7%
3. growth 1.5 g_i, minimum at R: 441 banks with no barrier below 2.0; 1 start failed; median barrier 0.0221
  F1: 1103/442832 = 0.2%
  F2: 150846/685358 = 22.0%
4. minimum at R + 0.01: 0 banks with no barrier below 2.0; 101 start failed; median barrier 0.0117
  F1: 274/2797185 = 0.0%
  F2: 2017714/3305761 = 61.0%
4. minimum at R + 0.02: 0 banks with no barrier below 2.0; 167 start failed; median barrier 0.0117
  F1: 40/2383122 = 0.0%
  F2: 2567099/2796540 = 91.8%
4. minimum at R + 0.03: 0 banks with no barrier below 2.0; 221 start failed; median barrier 0.0117
  F1: 4/2056229 = 0.0%
  F2: 2311683/2397139 = 96.4%
```
