# Testing thread: equity volatility against the baseline on the test window

Run 24 October 2026,
under PREREG_comparison_10.md (commit 3f805a69),
with measure (B) as fixed in results_judged.md under PREREG_comparison_7.md,
under PREREG_comparison_8.md and under PREREG_comparison_9.md.
Every parameter is from 2004-2014 data; 2015-2024 enters only as the
observed facts and the window simulated. Nothing from 2025 or later enters.

## Findings

Sample B, the 17 banks whose top holding company is one of the 11 firms;
start 2014Q4, 40 quarters; facts in units of sigma_i over R = 10.5%.

| | F1 | F2 | Miss, F1 + F2 (points) |
|---|---|---|---|
| observed, 2015-2024 | 7.5% | 64.2% | |
| baseline (Peura and Keppo), minimum 8% | 83.1% | 0.0% | 75.6 + 64.2 = 139.8 |
| (B) equity volatility, minimum 8% | 1.3% | 88.0% | 6.2 + 23.8 = 30.0 |
| (B) equity volatility, minimum at R (reported) | 0.6% | 94.3% | 6.9 + 30.1 = 37.0 |

- Rule: (B) beats the baseline, 30.0 points against 139.8.
- By the 10-point rule, (B) agrees on F1 (1.3% against 7.5%) and not on
  F2 (88.0% against 64.2%, 23.8 points over). The baseline agrees on
  neither.
- (B) overshoots F2 in both windows: on the training window it predicted
  83.1% against 32.9% (results_judged.md), here 88.0% against 64.2%. The
  smaller miss here comes from the observed share rising, not from the
  prediction moving.
- The comparison covers 17 of the 61 banks of sample B, all owned by the
  11 stress-tested holding companies matched in results_judged.md. It says
  nothing yet about the other 44.
- The baseline's F3 and F4 are reported and not scored: 93.4% and 72.2%
  against 33.3% and 55.6% observed, from 9 observed recapitalisations.
- (B)'s random draws use judged_risk.py's seed: 20261024, the bank's
  certificate number, the minimum and the judged volatility. The
  pre-registration names the first two only.
- Sample A (reported, decides nothing): 20 banks, one more than in
  results_judged.md (cert 58180, Citigroup, which has a 2014Q4 buffer but
  no 2004Q4 buffer); observed F1 6.8%, F2 68.0%; baseline 75.1% and 0.0%
  (miss 136.3); (B) 0.9% and 89.8% (miss 27.7).

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
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783
Pinned: judged_risk.py sha256 b7a37555e77041b1e5c7117cdcf1a03900ef2399435e44db8165dd9e156d1d78
Pinned: ../lit/dfast_2013/CLAIMS.md sha256 5a5a3a9f1f169e66dcba49f40372773495d12266acab987a0d586aba9b60cde5
Pinned: ../lit/dfast_2014/CLAIMS.md sha256 4ddded8205e7a3468913e5b94931421f04bac6f5079a993f5185c203e5c84021
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/bac_us_d.csv sha256 4a3bae9500c923c87c50ee3518c6f578fcc2c0c2a785814015bd54266ee18fca
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/bny_us_d.csv sha256 ab1061bb66d51340e15a4b4439080b6116c1db0398fb9961d13aae4b90423fd6
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/cof_us_d.csv sha256 717d908fd3829063fb7236da8a0ca6f780448154fd2675ab5944dbee6786c7ab
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/c_us_d.csv sha256 4cda892b1209869180b3aba09f38435db9d7b5b572483963ba78ad3c5506a21c
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/hban_us_d.csv sha256 a5b1f3ad1136d7b42062bd40d585d55e7832578a1449b778ad20e0344389d9a5
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/jpm_us_d.csv sha256 6836318dc26f4abb8df89bed430875940d85b05ecc6fdaceeb1cf5ed1ab10a39
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/key_us_d.csv sha256 4cfcb32565b4b7c030862d52ce64de5c7cb213a84ce1af9e60ab5252464c5872
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/pnc_us_d.csv sha256 2814e9ad72e6f91cf075cff1018a8da02c320e02147dc6b2aa119835c2afd36b
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/usb_us_d.csv sha256 dd5596935e7a31f249b66dcb284b09c177adcfcc46f17bcd0ab6ecae53346868
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/wfc_us_d.csv sha256 0129de4f0f11108f8bca172bda74f96e5a4ae9d37002e9d4cf352c124d01c5be
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261023/zion_us_d.csv sha256 5f56697173bfa0ea69066ca39f608062eb1851719967c829dbaa05aa52262a83
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/nasdaqlisted.txt sha256 df221177204f1c56add0c52adcd8f19c7371bde662eb34c8b32c5bba8bf4f5b0
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/otherlisted.txt sha256 cab8d0e5cb077acb38ebe201113a909b7ee3784c32249281d83399c0b0d5732f
Pinned: test_judged.py sha256 f545aafca13f9442d2488cb0931129d0eab493dd9d92071c4d87fb25d95c29e5
Pinned: facts.py sha256 44e6723a38b55d920e92cf6ac08c5cc8f7e3b7ef01cd953ac489e5c6d2749eca
Pinned: predict_pk.py sha256 0c957c45b9cb6a501a516d4168f37983c5b380c102c760657c181b98ffe4d962

Reproduce: python3 test_judged.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
fdic_q_2014.csv.gz sha256 7cf4299881b363f08cca394dd2d53ad5959bdd43f39e53a89b738e21a4a4d89b
fdic_q_2015.csv.gz sha256 a1927a37f42ed95c7074dc196f5c06cbf29c238ef445ab2587add77df5f55373
fdic_q_2016.csv.gz sha256 6111d9aebd8e12b351d5c3ccdda9f310b13c871c742d184d706062a346ef4609
fdic_q_2017.csv.gz sha256 8546b1ab38b294adae748f0f5ff4fd91d9c548416d99998ad7d41bc29f049178
fdic_q_2018.csv.gz sha256 d57d152847f0511a0bf76f33b2bae5c95b5bd1e0d1984ce28b5423b1ab715bb2
fdic_q_2019.csv.gz sha256 363c0b516cbfb22ed7a68fb30ed19527d45a892a19fa7e1b90f696c90d253214
fdic_q_2020.csv.gz sha256 26094e27170aa52b1b6c7dea8cd18de7a914423a33f542d5273b0b9fc8beac5c
fdic_q_2021.csv.gz sha256 d8fc9f81ee1bbaa1a599bc950c6d152e03405ec0a68c0cfe45625e9d45168d75
fdic_q_2022.csv.gz sha256 010f2271081f5b4afa27e2fa4fa3c961534a646760099935a280beb9085afa70
fdic_q_2023.csv.gz sha256 7b346b82a1d1e29ced59eb58f4abad038c119f0ac48ca589052e0ce009234b05
fdic_q_2024.csv.gz sha256 74af80f572fc73be46cf5cdc260bfef2e284e2c8449e470cb1037589e3d38160

===== sample B: 17 banks whose top holding company is one of the 11 firms; start 2014Q4, 2015-2024
     628 JPM   x0 0.1250 sigma_i 0.00338 sigma_B 0.05463
    3510 BAC   x0 0.1463 sigma_i 0.00601 sigma_B 0.07533
    7213 C     x0 0.1473 sigma_i 0.00944 sigma_B 0.09005
    3511 WFC   x0 0.1261 sigma_i 0.00418 sigma_B 0.05632
    6548 USB   x0 0.1277 sigma_i 0.00368 sigma_B 0.04411
    6384 PNC   x0 0.1374 sigma_i 0.00289 sigma_B 0.05782
    4297 COF   x0 0.1357 sigma_i 0.01005 sigma_B 0.06426
   23702 JPM   x0 0.1219 sigma_i 0.01176 sigma_B 0.06103
   17534 KEY   x0 0.1351 sigma_i 0.00956 sigma_B 0.07408
    6560 HBAN  x0 0.1279 sigma_i 0.02478 sigma_B 0.07867
    7946 BNY   x0 0.1106 sigma_i 0.01865 sigma_B 0.05602
   25178 BAC   x0 0.2794 sigma_i 0.01412 sigma_B 0.12879
    2270 ZION  x0 0.1532 sigma_i 0.00904 sigma_B 0.07062
   13718 WFC   x0 0.3353 sigma_i 0.02268 sigma_B 0.07926
   24107 ZION  x0 0.1409 sigma_i 0.02461 sigma_B 0.07341
   20626 ZION  x0 0.1522 sigma_i 0.02177 sigma_B 0.08203
   27389 WFC   x0 0.1660 sigma_i 0.01736 sigma_B 0.06895
observed, 2015-2024
  F1: 32/427 = 7.5%
  F2: 374/583 = 64.2%
  F3: 3/9 = 33.3%
  F4: 5/9 = 55.6%
baseline (Peura and Keppo), minimum 8%
  F1: 72679/87435 = 83.1%
  F2: 0/116442 = 0.0%
  F3: 670/717 = 93.4%
  F4: 518/717 = 72.2%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 158/11942 = 1.3%
  F2: 50020/56860 = 88.0%
(B) equity volatility, minimum at R: 0 start failed
  F1: 53/8282 = 0.6%
  F2: 38513/40861 = 94.3%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 75.6 + |dF2| 64.2 = 139.8 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 6.2 + |dF2| 23.8 = 30.0 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.9 + |dF2| 30.1 = 37.0 points; F1 within 10: True; F2 within 10: False
  rule (sample B, 17 banks, minimum 8%): (B) beats the baseline (30.0 against 139.8 points)

===== sample A: 20 banks whose top holding company is one of the 11 firms; start 2014Q4, 2015-2024
     628 JPM   x0 0.1250 sigma_i 0.00338 sigma_B 0.05463
    3510 BAC   x0 0.1463 sigma_i 0.00601 sigma_B 0.07533
    7213 C     x0 0.1473 sigma_i 0.00944 sigma_B 0.09005
    3511 WFC   x0 0.1261 sigma_i 0.00418 sigma_B 0.05632
    6548 USB   x0 0.1277 sigma_i 0.00368 sigma_B 0.04411
    6384 PNC   x0 0.1374 sigma_i 0.00289 sigma_B 0.05782
    4297 COF   x0 0.1357 sigma_i 0.01005 sigma_B 0.06426
   23702 JPM   x0 0.1219 sigma_i 0.01176 sigma_B 0.06103
   17534 KEY   x0 0.1351 sigma_i 0.00956 sigma_B 0.07408
    6560 HBAN  x0 0.1279 sigma_i 0.02478 sigma_B 0.07867
    7946 BNY   x0 0.1106 sigma_i 0.01865 sigma_B 0.05602
   25178 BAC   x0 0.2794 sigma_i 0.01412 sigma_B 0.12879
    2270 ZION  x0 0.1532 sigma_i 0.00904 sigma_B 0.07062
   13718 WFC   x0 0.3353 sigma_i 0.02268 sigma_B 0.07926
   24107 ZION  x0 0.1409 sigma_i 0.02461 sigma_B 0.07341
   20626 ZION  x0 0.1522 sigma_i 0.02177 sigma_B 0.08203
   27389 WFC   x0 0.1660 sigma_i 0.01736 sigma_B 0.06895
   23472 BNY   x0 6.9330 sigma_i 0.38854 sigma_B 1.26774
   27298 ZION  x0 0.1304 sigma_i 0.00899 sigma_B 0.07599
   58180 C     x0 0.3974 sigma_i 0.01531 sigma_B 0.18071
observed, 2015-2024
  F1: 32/470 = 6.8%
  F2: 446/656 = 68.0%
  F3: 3/10 = 30.0%
  F4: 5/10 = 50.0%
baseline (Peura and Keppo), minimum 8%
  F1: 79804/106251 = 75.1%
  F2: 0/140373 = 0.0%
  F3: 737/917 = 80.4%
  F4: 678/917 = 73.9%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 158/17365 = 0.9%
  F2: 65307/72753 = 89.8%
(B) equity volatility, minimum at R: 0 start failed
  F1: 53/13402 = 0.4%
  F2: 52547/55112 = 95.3%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 68.3 + |dF2| 68.0 = 136.3 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 5.9 + |dF2| 21.8 = 27.7 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.4 + |dF2| 27.4 = 33.8 points; F1 within 10: True; F2 within 10: False
```
