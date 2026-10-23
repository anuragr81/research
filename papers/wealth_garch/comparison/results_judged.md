# Testing thread: the risk a bank is judged on, the regulator's or the market's? (training window)

Run 24 October 2026,
under PREREG_comparison_7.md (commit 5a10ee1f),
under PREREG_comparison_8.md (commit 6e4b474a),
under PREREG_comparison_9.md (commit 7a7058e8),
with sigma, the samples and the facts as under PREREG_comparison_2.md.
Only 2004-2014 data enter the facts and the simulations.

## Findings

Sample B, the 17 banks with a 2004Q4 buffer whose top holding company is
one of the 11 firms both measures cover; 2005-2014; observed F2 32.9%, F1
26.2%. The model is the non-issuer barrier model of results_scale.md with
the buffer's volatility replaced by the judged volatility; the facts are
measured in units of the measured sigma_i.

| Measure | judged volatility over sigma_i, median of the 17 | F2, minimum 8% | F1, minimum 8% | F2, minimum at R | F1, minimum at R |
|---|---|---|---|---|---|
| observed | | 32.9% | 26.2% | 32.9% | 26.2% |
| (A) stress tests, z = 1.645 | | 18.6% | 60.7% | 29.5% | 30.4% |
| (A) stress tests, z = 2.326 (central) | 0.99 | 12.6% | 83.7% | 21.7% | 48.7% |
| (A) stress tests, z = 3.090 | | 1.9% | 80.7% | 14.5% | 45.6% |
| (B) equity volatility | 7.75 | 83.1% | 2.4% | 90.3% | 0.4% |

- Rule (central point, minimum at 8%, sample B): (A) misses by 57.6 + 20.3
  = 77.9 points, (B) by 23.8 + 50.2 = 74.0 points. By the rule fixed in
  PREREG_comparison_7.md, (B) is the better measure, and it is the measure
  fixed for the test window.
- Neither measure passes the 10-point rule on either fact, at either
  minimum. The two err in opposite directions. The stress-test measure
  gives a judged volatility about equal to the bank's measured sigma_i
  (median ratio 0.99), and the predicted buffers are too thin: F2 too low,
  F1 too high. The equity measure gives about eight times sigma_i (median
  7.75), and the predicted buffers are too thick: F2 too high, F1 too low.
  The observed facts lie between the two.
- Reported, decides nothing: with the minimum at R, the central (A) misses
  by 22.6 + 11.3 = 33.8 points and (B) by 25.8 + 57.3 = 83.1. With BNY
  Mellon's change of 2 July 2007 set to zero, sigma_E for BNY Mellon is
  0.4199 against 0.4205, and (B) gives F2 83.6% and F1 2.3% (minimum 8%);
  (A) does not use prices and is unchanged.
- Sample A (reported, decides nothing): 19 banks; observed F2 35.3%, F1
  23.1%; central (A) 16.0% and 82.5%, (B) 84.5% and 1.8% (minimum 8%). One
  of the two added banks (cert 23472, BNY Mellon) has a mean total capital
  ratio of 3.0 and a sigma_i of 0.389.
- F3 and F4 are not scored (no issuance), as fixed.

## Correction

PREREG_comparison_8.md says the 11 firms are top holding companies of 16
banks of sample B. They are top holding companies of 17: three banks each
for Wells Fargo and Zions, two each for JPMorgan Chase and Bank of America,
one for each of the other seven firms. All 17 have a 2004Q4 buffer.

## Firms

D is the mean decline in the tier 1 common ratio over the reports a firm
is in (lit/dfast_2013, lit/dfast_2014). sigma_E is the annualised standard
deviation of daily log changes of Close, 2005-2014. The listed names are
from the Nasdaq Trader files and match the NIC legal names as recorded in
PREREG_comparison_9.md; the script checks each.

| RSSD | Ticker | Declines (points) | D | sigma_E |
|---|---|---|---|---|
| 1039502 | JPM | DF13-Q1 4.1, DF14-Q1 4.2 | 4.15 | 0.4321 |
| 1073757 | BAC | DF13-Q2 4.6, DF14-Q2 5.2 | 4.90 | 0.5751 |
| 1951350 | C | DF13-Q3 4.4, DF14-Q3 5.5 | 4.95 | 0.6065 |
| 1120754 | WFC | DF13-Q4 2.9, DF14-Q4 2.4 | 2.65 | 0.4682 |
| 1119794 | USB | DF13-Q5 0.7, DF14-Q5 1.1 | 0.90 | 0.3787 |
| 1069778 | PNC | DF13-Q6 0.8, DF14-Q6 1.3 | 1.05 | 0.4469 |
| 2277860 | COF | DF13-Q7 3.3, DF14-Q7 4.9 | 4.10 | 0.5102 |
| 1068025 | KEY | DF13-Q8 3.3, DF14-Q8 2.0 | 2.65 | 0.5613 |
| 1068191 | HBAN | DF14-Q10 3.5 | 3.50 | 0.6715 |
| 1027004 | ZION | DF14-Q11 6.9 | 6.90 | 0.5523 |
| 3587146 | BNY | DF13-Q9 0.1, DF14-Q9 1.0 | 0.55 | 0.4205 |

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

Reproduce: python3 judged_risk.py

```
fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25

firms: RSSD, ticker, listed name, stress-test declines (claim, points), D, sigma_E, changes
  1039502 JPM   JP Morgan Chase & Co. Common Stock | DF13-Q1 4.1, DF14-Q1 4.2 | D 4.15 | sigma_E 0.4321 | 2516
  1073757 BAC   Bank of America Corporation Common Stock | DF13-Q2 4.6, DF14-Q2 5.2 | D 4.90 | sigma_E 0.5751 | 2516
  1951350 C     Citigroup, Inc. Common Stock | DF13-Q3 4.4, DF14-Q3 5.5 | D 4.95 | sigma_E 0.6065 | 2516
  1120754 WFC   Wells Fargo & Company Common Stock | DF13-Q4 2.9, DF14-Q4 2.4 | D 2.65 | sigma_E 0.4682 | 2516
  1119794 USB   U.S. Bancorp Common Stock | DF13-Q5 0.7, DF14-Q5 1.1 | D 0.90 | sigma_E 0.3787 | 2516
  1069778 PNC   PNC Financial Services Group, Inc. (The) Common Stock | DF13-Q6 0.8, DF14-Q6 1.3 | D 1.05 | sigma_E 0.4469 | 2516
  2277860 COF   Capital One Financial Corporation Common Stock | DF13-Q7 3.3, DF14-Q7 4.9 | D 4.10 | sigma_E 0.5102 | 2515
  1068025 KEY   KeyCorp Common Stock | DF13-Q8 3.3, DF14-Q8 2.0 | D 2.65 | sigma_E 0.5613 | 2516
  1068191 HBAN  Huntington Bancshares Incorporated - Common Stock | DF14-Q10 3.5 | D 3.50 | sigma_E 0.6715 | 2516
  1027004 ZION  Zions Bancorporation N.A. - Common Stock | DF14-Q11 6.9 | D 6.90 | sigma_E 0.5523 | 2516
  3587146 BNY   The Bank of New York Mellon Corporation Common Stock | DF13-Q9 0.1, DF14-Q9 1.0 | D 0.55 | sigma_E 0.4205 | 2516
  BNY with the 2007-07-02 change set to zero: sigma_E 0.4199 | 2516

===== sample B: 17 banks with a 2004Q4 buffer whose top holding company both measures cover
banks: cert, ticker, X_i, sigma_i, sigma_A at z = 2.326, sigma_B, ratios to sigma_i
     628 JPM   X 0.1264 sigma_i 0.00338 sigma_A 0.01189 sigma_B 0.05463 | 3.52 16.16
    3510 BAC   X 0.1310 sigma_i 0.00601 sigma_A 0.01404 sigma_B 0.07533 | 2.34 12.53
    7213 C     X 0.1485 sigma_i 0.00944 sigma_A 0.01419 sigma_B 0.09005 | 1.50 9.54
    3511 WFC   X 0.1203 sigma_i 0.00418 sigma_A 0.00760 sigma_B 0.05632 | 1.82 13.47
    6548 USB   X 0.1165 sigma_i 0.00368 sigma_A 0.00258 sigma_B 0.04411 | 0.70 11.98
    6384 PNC   X 0.1294 sigma_i 0.00289 sigma_A 0.00301 sigma_B 0.05782 | 1.04 20.03
    4297 COF   X 0.1260 sigma_i 0.01005 sigma_A 0.01175 sigma_B 0.06426 | 1.17 6.39
   23702 JPM   X 0.1413 sigma_i 0.01176 sigma_A 0.01189 sigma_B 0.06103 | 1.01 5.19
   17534 KEY   X 0.1320 sigma_i 0.00956 sigma_A 0.00760 sigma_B 0.07408 | 0.79 7.75
    6560 HBAN  X 0.1171 sigma_i 0.02478 sigma_A 0.01003 sigma_B 0.07867 | 0.40 3.17
    7946 BNY   X 0.1332 sigma_i 0.01865 sigma_A 0.00158 sigma_B 0.05602 | 0.08 3.00
   25178 BAC   X 0.2239 sigma_i 0.01412 sigma_A 0.01404 sigma_B 0.12879 | 0.99 9.12
    2270 ZION  X 0.1279 sigma_i 0.00904 sigma_A 0.01978 sigma_B 0.07062 | 2.19 7.81
   13718 WFC   X 0.1693 sigma_i 0.02268 sigma_A 0.00760 sigma_B 0.07926 | 0.33 3.49
   24107 ZION  X 0.1329 sigma_i 0.02461 sigma_A 0.01978 sigma_B 0.07341 | 0.80 2.98
   20626 ZION  X 0.1485 sigma_i 0.02177 sigma_A 0.01978 sigma_B 0.08203 | 0.91 3.77
   27389 WFC   X 0.1472 sigma_i 0.01736 sigma_A 0.00760 sigma_B 0.06895 | 0.44 3.97
observed, 2005-2014
  F1: 94/359 = 26.2%
  F2: 224/680 = 32.9%
  F3: 14/31 = 45.2%
  F4: 16/31 = 51.6%
(A) stress tests, z = 1.645, minimum 8%: 0 start failed; median barrier 3.58 sigma_i above the minimum
  F1: 41219/67937 = 60.7%
  F2: 20341/109074 = 18.6%
(A) stress tests, z = 2.326 (central), minimum 8%: 0 start failed; median barrier 2.16 sigma_i above the minimum
  F1: 65940/78748 = 83.7%
  F2: 14784/117394 = 12.6%
(A) stress tests, z = 3.09, minimum 8%: 0 start failed; median barrier 1.82 sigma_i above the minimum
  F1: 70800/87768 = 80.7%
  F2: 2370/121824 = 1.9%
(B) equity volatility, minimum 8%: 0 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 195/8136 = 2.4%
  F2: 37319/44888 = 83.1%
(B) with BNY's 2007-07-02 change set to zero, minimum 8%: 0 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 195/8299 = 2.3%
  F2: 38083/45563 = 83.6%
(A) stress tests, z = 1.645, minimum at R: 3 start failed; median barrier 3.58 sigma_i above the minimum
  F1: 17473/57394 = 30.4%
  F2: 23173/78465 = 29.5%
(A) stress tests, z = 2.326 (central), minimum at R: 3 start failed; median barrier 2.16 sigma_i above the minimum
  F1: 31920/65494 = 48.7%
  F2: 18529/85524 = 21.7%
(A) stress tests, z = 3.09, minimum at R: 3 start failed; median barrier 1.82 sigma_i above the minimum
  F1: 33128/72643 = 45.6%
  F2: 13343/92060 = 14.5%
(B) equity volatility, minimum at R: 3 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 15/4192 = 0.4%
  F2: 22535/24967 = 90.3%
(B) with BNY's 2007-07-02 change set to zero, minimum at R: 3 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 15/4179 = 0.4%
  F2: 22932/25426 = 90.2%
scores against observed F1 and F2 (points)
  (A) central, minimum 8%: |dF1| 57.6 + |dF2| 20.3 = 77.9 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 23.8 + |dF2| 50.2 = 74.0 points; F1 within 10: False; F2 within 10: False
  rule (sample B, minimum 8%, central): better measure (B) equity volatility
  (A) central, minimum at R: |dF1| 22.6 + |dF2| 11.3 = 33.8 points; F1 within 10: False; F2 within 10: False
  (B), minimum at R: |dF1| 25.8 + |dF2| 57.3 = 83.1 points; F1 within 10: False; F2 within 10: False

===== sample A: 19 banks with a 2004Q4 buffer whose top holding company both measures cover
banks: cert, ticker, X_i, sigma_i, sigma_A at z = 2.326, sigma_B, ratios to sigma_i
     628 JPM   X 0.1264 sigma_i 0.00338 sigma_A 0.01189 sigma_B 0.05463 | 3.52 16.16
    3510 BAC   X 0.1310 sigma_i 0.00601 sigma_A 0.01404 sigma_B 0.07533 | 2.34 12.53
    7213 C     X 0.1485 sigma_i 0.00944 sigma_A 0.01419 sigma_B 0.09005 | 1.50 9.54
    3511 WFC   X 0.1203 sigma_i 0.00418 sigma_A 0.00760 sigma_B 0.05632 | 1.82 13.47
    6548 USB   X 0.1165 sigma_i 0.00368 sigma_A 0.00258 sigma_B 0.04411 | 0.70 11.98
    6384 PNC   X 0.1294 sigma_i 0.00289 sigma_A 0.00301 sigma_B 0.05782 | 1.04 20.03
    4297 COF   X 0.1260 sigma_i 0.01005 sigma_A 0.01175 sigma_B 0.06426 | 1.17 6.39
   23702 JPM   X 0.1413 sigma_i 0.01176 sigma_A 0.01189 sigma_B 0.06103 | 1.01 5.19
   17534 KEY   X 0.1320 sigma_i 0.00956 sigma_A 0.00760 sigma_B 0.07408 | 0.79 7.75
    6560 HBAN  X 0.1171 sigma_i 0.02478 sigma_A 0.01003 sigma_B 0.07867 | 0.40 3.17
    7946 BNY   X 0.1332 sigma_i 0.01865 sigma_A 0.00158 sigma_B 0.05602 | 0.08 3.00
   25178 BAC   X 0.2239 sigma_i 0.01412 sigma_A 0.01404 sigma_B 0.12879 | 0.99 9.12
    2270 ZION  X 0.1279 sigma_i 0.00904 sigma_A 0.01978 sigma_B 0.07062 | 2.19 7.81
   13718 WFC   X 0.1693 sigma_i 0.02268 sigma_A 0.00760 sigma_B 0.07926 | 0.33 3.49
   24107 ZION  X 0.1329 sigma_i 0.02461 sigma_A 0.01978 sigma_B 0.07341 | 0.80 2.98
   20626 ZION  X 0.1485 sigma_i 0.02177 sigma_A 0.01978 sigma_B 0.08203 | 0.91 3.77
   27389 WFC   X 0.1472 sigma_i 0.01736 sigma_A 0.00760 sigma_B 0.06895 | 0.44 3.97
   23472 BNY   X 3.0151 sigma_i 0.38854 sigma_A 0.00158 sigma_B 1.26774 | 0.00 3.26
   27298 ZION  X 0.1376 sigma_i 0.00899 sigma_A 0.01978 sigma_B 0.07599 | 2.20 8.45
observed, 2005-2014
  F1: 94/407 = 23.1%
  F2: 268/760 = 35.3%
  F3: 14/32 = 43.8%
  F4: 17/32 = 53.1%
(A) stress tests, z = 1.645, minimum 8%: 0 start failed; median barrier 3.58 sigma_i above the minimum
  F1: 49219/77544 = 63.5%
  F2: 26585/124447 = 21.4%
(A) stress tests, z = 2.326 (central), minimum 8%: 0 start failed; median barrier 2.16 sigma_i above the minimum
  F1: 73940/89572 = 82.5%
  F2: 21344/133240 = 16.0%
(A) stress tests, z = 3.09, minimum 8%: 0 start failed; median barrier 1.82 sigma_i above the minimum
  F1: 78800/99667 = 79.1%
  F2: 4691/137769 = 3.4%
(B) equity volatility, minimum 8%: 0 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 195/10657 = 1.8%
  F2: 46013/54464 = 84.5%
(B) with BNY's 2007-07-02 change set to zero, minimum 8%: 0 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 195/10821 = 1.8%
  F2: 47083/55447 = 84.9%
(A) stress tests, z = 1.645, minimum at R: 3 start failed; median barrier 3.58 sigma_i above the minimum
  F1: 25473/66396 = 38.4%
  F2: 28772/92700 = 31.0%
(A) stress tests, z = 2.326 (central), minimum at R: 3 start failed; median barrier 2.16 sigma_i above the minimum
  F1: 39920/75631 = 52.8%
  F2: 25535/100988 = 25.3%
(A) stress tests, z = 3.09, minimum at R: 3 start failed; median barrier 1.82 sigma_i above the minimum
  F1: 41128/83844 = 49.1%
  F2: 20702/107994 = 19.2%
(B) equity volatility, minimum at R: 3 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 15/6667 = 0.2%
  F2: 30884/33902 = 91.1%
(B) with BNY's 2007-07-02 change set to zero, minimum at R: 3 start failed; median barrier 19.15 sigma_i above the minimum
  F1: 15/6554 = 0.2%
  F2: 31444/34547 = 91.0%
scores against observed F1 and F2 (points)
  (A) central, minimum 8%: |dF1| 59.5 + |dF2| 19.2 = 78.7 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 21.3 + |dF2| 49.2 = 70.5 points; F1 within 10: False; F2 within 10: False
  (A) central, minimum at R: |dF1| 29.7 + |dF2| 10.0 = 39.7 points; F1 within 10: False; F2 within 10: True
  (B), minimum at R: |dF1| 22.9 + |dF2| 55.8 = 78.7 points; F1 within 10: False; F2 within 10: False
```
