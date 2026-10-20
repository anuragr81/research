# Testing thread: how much larger risk, or how much cheaper equity? (training window)

Run 20 October 2026,
under PREREG_comparison_6.md (commit c2bb2ab0),
with sigma, the samples and the facts as under PREREG_comparison_2.md.
Only 2004-2014 data enter the facts and the simulations.

## Findings

Sample B (61 banks), 2005-2014; observed F2 48.0%, F1 17.1%. The buffer's
volatility is k times the bank's measured sigma_i; the facts are always
measured in units of the measured sigma_i.

| k (rho = 4%) | median barrier over the minimum | F2, minimum 8% | F1, minimum 8% | F2, minimum at R |
|---|---|---|---|---|
| 1 | 2.24 sigma_i | 0.6% | 89.4% | 5.3% |
| 1.5 | 4.14 sigma_i | 5.8% | 81.2% | 30.3% |
| 2 | 6.34 sigma_i | 16.8% | 68.8% | 73.3% |
| 3 | 10.80 sigma_i | 42.8% | 34.0% | 93.3% |
| 4 | 15.55 sigma_i | 67.6% | 5.7% | 94.8% |

| rho (k = 1) | F2, minimum 8% | F2, minimum at R |
|---|---|---|
| 2% | 2.7% | 11.4% |
| 1% | 6.5% | 15.3% |
| 0.5% | 8.4% | 18.0% |

- Risk scale: with the minimum at 8% the smallest grid value within 10
  points of the observed F2 is k = 3 (42.8%), where F1 is 34.0% against
  17.1%. With the minimum at R no grid value is within 10 points: F2
  passes from 30.3% at k = 1.5 to 73.3% at k = 2.
- Cost of equity: no grid value comes within 10 points; at 0.5% F2 is
  8.4% (minimum 8%) or 18.0% (minimum at R). A cheaper equity alone does
  not produce the buffers.
- So, within the non-issuer barrier model, the observed buffers need the
  risk the bank guards against to be about two to three times the
  volatility of its own yearly earnings over 2005-2014, a period that
  includes the 2008 to 2010 crisis. This is a size for the evidence to
  judge; nothing is chosen from it.
- Sample A (reported, decides nothing): observed F2 75.6%; with the minimum
  at 8%, 49.4% at k = 3 and 75.1% at k = 4.

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
Pinned: scale_check.py sha256 2178888542b2bd4580768ab4d249aec55a4b4e11f1e9a32fb7ef41feeca9a83f
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783

Reproduce: python3 scale_check.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

===== sample B: 61 banks with a 2004Q4 buffer
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
  F3: 32/93 = 34.4%
  F4: 35/93 = 37.6%
risk scale k = 1.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 2.24 sigma_i above the minimum
  F1: 315471/352857 = 89.4%
  F2: 2534/450028 = 0.6%
risk scale k = 1.5, rho = 0.04, minimum 8%: 0 start failed; median barrier 4.14 sigma_i above the minimum
  F1: 246880/303968 = 81.2%
  F2: 25443/434966 = 5.8%
risk scale k = 2.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 6.34 sigma_i above the minimum
  F1: 181274/263414 = 68.8%
  F2: 70288/418027 = 16.8%
risk scale k = 3.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 10.80 sigma_i above the minimum
  F1: 68292/200808 = 34.0%
  F2: 164111/383579 = 42.8%
risk scale k = 4.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 15.55 sigma_i above the minimum
  F1: 8846/155940 = 5.7%
  F2: 236328/349717 = 67.6%
risk scale k = 6.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 23.93 sigma_i above the minimum
  F1: 336/98719 = 0.3%
  F2: 261490/297444 = 87.9%
risk scale k = 1, rho = 0.02, minimum 8%: 0 start failed; median barrier 2.51 sigma_i above the minimum
  F1: 289583/345790 = 83.7%
  F2: 12313/455857 = 2.7%
risk scale k = 1, rho = 0.01, minimum 8%: 0 start failed; median barrier 2.79 sigma_i above the minimum
  F1: 280950/337579 = 83.2%
  F2: 30026/458940 = 6.5%
risk scale k = 1, rho = 0.005, minimum 8%: 0 start failed; median barrier 3.10 sigma_i above the minimum
  F1: 275104/329731 = 83.4%
  F2: 38540/460950 = 8.4%
risk scale k = 1.0, rho = 0.04, minimum at R: 6 start failed; median barrier 2.24 sigma_i above the minimum
  F1: 12592/304617 = 4.1%
  F2: 19928/378865 = 5.3%
risk scale k = 1.5, rho = 0.04, minimum at R: 6 start failed; median barrier 4.14 sigma_i above the minimum
  F1: 198/257918 = 0.1%
  F2: 106862/352783 = 30.3%
risk scale k = 2.0, rho = 0.04, minimum at R: 6 start failed; median barrier 6.34 sigma_i above the minimum
  F1: 47/216733 = 0.0%
  F2: 242525/330911 = 73.3%
risk scale k = 3.0, rho = 0.04, minimum at R: 6 start failed; median barrier 10.80 sigma_i above the minimum
  F1: 25/151227 = 0.0%
  F2: 263494/282473 = 93.3%
risk scale k = 4.0, rho = 0.04, minimum at R: 6 start failed; median barrier 15.55 sigma_i above the minimum
  F1: 41/107260 = 0.0%
  F2: 228812/241334 = 94.8%
risk scale k = 6.0, rho = 0.04, minimum at R: 6 start failed; median barrier 23.93 sigma_i above the minimum
  F1: 38/60415 = 0.1%
  F2: 175605/182523 = 96.2%
risk scale k = 1, rho = 0.02, minimum at R: 6 start failed; median barrier 2.51 sigma_i above the minimum
  F1: 9835/298219 = 3.3%
  F2: 42891/377883 = 11.4%
risk scale k = 1, rho = 0.01, minimum at R: 6 start failed; median barrier 2.79 sigma_i above the minimum
  F1: 1702/292579 = 0.6%
  F2: 58258/381511 = 15.3%
risk scale k = 1, rho = 0.005, minimum at R: 6 start failed; median barrier 3.10 sigma_i above the minimum
  F1: 598/287373 = 0.2%
  F2: 68727/382479 = 18.0%

===== sample A: 536 banks with a 2004Q4 buffer
observed, 2005-2014
  F1: 953/14185 = 6.7%
  F2: 16099/21299 = 75.6%
  F3: 122/410 = 29.8%
  F4: 133/410 = 32.4%
risk scale k = 1.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 2.15 sigma_i above the minimum
  F1: 3244311/3477214 = 93.3%
  F2: 3045/4169336 = 0.1%
risk scale k = 1.5, rho = 0.04, minimum 8%: 0 start failed; median barrier 4.15 sigma_i above the minimum
  F1: 2469765/3060744 = 80.7%
  F2: 121235/4095786 = 3.0%
risk scale k = 2.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 6.43 sigma_i above the minimum
  F1: 1633769/2696620 = 60.6%
  F2: 579917/4013898 = 14.4%
risk scale k = 3.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 11.36 sigma_i above the minimum
  F1: 611028/2130502 = 28.7%
  F2: 1898004/3838896 = 49.4%
risk scale k = 4.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 16.39 sigma_i above the minimum
  F1: 166827/1705190 = 9.8%
  F2: 2735302/3643502 = 75.1%
risk scale k = 6.0, rho = 0.04, minimum 8%: 0 start failed; median barrier 26.01 sigma_i above the minimum
  F1: 19390/1147146 = 1.7%
  F2: 2950673/3260233 = 90.5%
risk scale k = 1, rho = 0.02, minimum 8%: 0 start failed; median barrier 2.46 sigma_i above the minimum
  F1: 3127826/3452575 = 90.6%
  F2: 58497/4201114 = 1.4%
risk scale k = 1, rho = 0.01, minimum 8%: 0 start failed; median barrier 2.75 sigma_i above the minimum
  F1: 3029636/3416273 = 88.7%
  F2: 203324/4214269 = 4.8%
risk scale k = 1, rho = 0.005, minimum 8%: 0 start failed; median barrier 3.03 sigma_i above the minimum
  F1: 2959071/3385012 = 87.4%
  F2: 295744/4224763 = 7.0%
risk scale k = 1.0, rho = 0.04, minimum at R: 27 start failed; median barrier 2.15 sigma_i above the minimum
  F1: 153971/3228322 = 4.8%
  F2: 138483/3844424 = 3.6%
risk scale k = 1.5, rho = 0.04, minimum at R: 27 start failed; median barrier 4.15 sigma_i above the minimum
  F1: 938/2798843 = 0.0%
  F2: 1370839/3713843 = 36.9%
risk scale k = 2.0, rho = 0.04, minimum at R: 27 start failed; median barrier 6.43 sigma_i above the minimum
  F1: 367/2423644 = 0.0%
  F2: 2926520/3589250 = 81.5%
risk scale k = 3.0, rho = 0.04, minimum at R: 27 start failed; median barrier 11.36 sigma_i above the minimum
  F1: 195/1836656 = 0.0%
  F2: 3209813/3334226 = 96.3%
risk scale k = 4.0, rho = 0.04, minimum at R: 27 start failed; median barrier 16.39 sigma_i above the minimum
  F1: 155/1412157 = 0.0%
  F2: 2994191/3082791 = 97.1%
risk scale k = 6.0, rho = 0.04, minimum at R: 27 start failed; median barrier 26.01 sigma_i above the minimum
  F1: 85/889480 = 0.0%
  F2: 2578203/2636878 = 97.8%
risk scale k = 1, rho = 0.02, minimum at R: 27 start failed; median barrier 2.46 sigma_i above the minimum
  F1: 95420/3196607 = 3.0%
  F2: 402568/3858781 = 10.4%
risk scale k = 1, rho = 0.01, minimum at R: 27 start failed; median barrier 2.75 sigma_i above the minimum
  F1: 55379/3162459 = 1.8%
  F2: 597673/3871601 = 15.4%
risk scale k = 1, rho = 0.005, minimum at R: 27 start failed; median barrier 3.03 sigma_i above the minimum
  F1: 37613/3137974 = 1.2%
  F2: 781089/3879532 = 20.1%
```
