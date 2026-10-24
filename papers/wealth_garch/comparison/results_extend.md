# Testing thread: equity volatility against the baseline on 18 more banks (test window)

Run 24 October 2026,
under PREREG_comparison_12.md (commit 70691b75),
under PREREG_comparison_13.md (commit 1ea9bfbc),
under PREREG_comparison_14.md (commit 059c33ef),
with the banks of results_match.md and the model, parameters and facts of
results_test_judged.md (PREREG_comparison_10.md), F1 and F2 scored
(PREREG_comparison_11.md). Every parameter is from 2004-2014 data;
2015-2024 enters only as the observed facts and the window simulated.
Nothing from 2025 or later enters.

## Findings

The 18 new banks of sample B, one for each of 18 parents listed in
PREREG_comparison_14.md; none of their results had been seen before this
run. Start 2014Q4, 40 quarters; facts in units of sigma_i over R = 10.5%.

| | F1 | F2 | Miss, F1 + F2 (points) |
|---|---|---|---|
| observed, 2015-2024 | 6.7% | 63.4% | |
| baseline (Peura and Keppo), minimum 8% | 92.6% | 0.0% | 85.9 + 63.4 = 149.3 |
| (B) equity volatility, minimum 8% | 0.2% | 89.8% | 6.5 + 26.4 = 32.8 |
| (B) equity volatility, minimum at R (reported) | 0.0% | 94.8% | 6.7 + 31.4 = 38.1 |
| (B) with three out-of-crisis changes set to zero (reported) | 0.2% | 89.5% | 6.5 + 26.1 = 32.5 |

- Rule: (B) beats the baseline on the 18 new banks, 32.8 points against
  149.3.
- By the 10-point rule, (B) agrees on F1 (0.2% against 6.7%) and not on
  F2 (89.8% against 63.4%, 26.4 points over). The baseline agrees on
  neither.
- The pattern of results_test_judged.md repeats on banks whose results
  had not been seen: there, on the 17 banks of the stress-tested firms,
  (B) gave F1 1.3% against 7.5% and F2 88.0% against 64.2%, a miss of
  30.0 against the baseline's 139.8.
- (B)'s predicted F2 is close to 90% whatever the banks: 89.8% here, 88.0%
  on the 17, 89.2% on these 18 in the training window (where the observed
  share was 51.4%). The judged volatility sigma_B is 2.6 to 24 times the
  measured sigma_i (median about 11), so the dividend barrier u0 sits above
  4 sigma_i for every one of the 18 (4.7 to 98 sigma_i, median 47), and
  nearly every surviving path stays above 4 sigma_i. The model with (B) says little more about F2 than "far
  above the minimum"; it does not place the buffers.
- All 35 covered banks of sample B together (reported, decides nothing):
  observed F1 7.0%, F2 63.7%; baseline 88.8% and 0.0% (miss 145.5); (B)
  0.7% and 89.0% (miss 31.6).
- Training window, the 18 new banks (reported): observed F1 11.5%, F2
  51.4%; (B) 0.4% and 89.2% with the minimum at 8%.
- Setting to zero the three daily changes above 0.25 outside September
  2008 to June 2009 (FHN 14 July 2008; OSBC 20 September 2010 and 25 April
  2013) lowers sigma_E for FHN from 0.5071 to 0.4990 and for OSBC from
  0.6581 to 0.6365, and moves the miss by 0.3 points.
- Seventeen of the 18 files begin on 25 February 2005 (Stooq's download cap,
  PREREG_comparison_14.md), so their sigma_E omits the first 38 trading
  days of 2005; FHN's file is complete; OSBC's lacks one more day.
- F3 and F4, reported and not scored: observed 26.7% and 20.0% from 15
  recapitalisations; baseline 84.7% and 91.9%.
- Sample A (reported, decides nothing): the same 18 new banks give the
  same numbers; all 38 covered banks of sample A give baseline miss 143.5
  and (B) miss 30.3.
- Not covered, as PREREG_comparison_14.md fixed: ASB (Stooq history from
  26 December 2014 only); UMBF and VLY (files not in hand when this ran).
  If their files arrive they are added under a dated note and decide
  nothing.

## Coverage of sample B so far

35 of 61 banks scored with (B) on the test window, in two runs
(results_test_judged.md: 17; this file: 18), (B) beating the baseline in
each. The 26 others: 8 with foreign parents, 14 with United States
parents matching no listed common stock, 1 whose parent has no NIC
record, ASB, UMBF and VLY.

## New parents

sigma_E is the annualised standard deviation of daily log changes of
Close over the dates present in 2005-2014. The listed names are from the
Nasdaq Trader files and the script checks each.

| RSSD | Ticker | First date in file | Changes | sigma_E |
|---|---|---|---|---|
| 1883693 | BOKF | 2005-02-25 | 2478 | 0.3083 |
| 1130249 | CAC | 2005-02-25 | 2478 | 0.4343 |
| 1048867 | CBU | 2005-02-25 | 2478 | 0.3480 |
| 1076262 | CHCO | 2005-02-25 | 2478 | 0.3514 |
| 1071276 | FFBC | 2005-02-25 | 2478 | 0.4140 |
| 1102312 | FFIN | 2005-02-25 | 2478 | 0.3187 |
| 1094640 | FHN | 2005-01-03 | 2516 | 0.5071 |
| 3005332 | FNB | 2005-02-25 | 2478 | 0.4377 |
| 1208559 | FRME | 2005-02-25 | 2478 | 0.4504 |
| 1139279 | NBTB | 2005-02-25 | 2478 | 0.3628 |
| 1098303 | ONB | 2005-02-25 | 2478 | 0.3872 |
| 1206911 | OSBC | 2005-02-25 | 2477 | 0.6581 |
| 1070578 | PEBO | 2005-02-25 | 2478 | 0.5169 |
| 1142336 | PRK | 2005-02-25 | 2478 | 0.4320 |
| 1094828 | SFNC | 2005-02-25 | 2478 | 0.4024 |
| 2706735 | TCBI | 2005-02-25 | 2478 | 0.3948 |
| 1208595 | THFF | 2005-02-25 | 2478 | 0.4235 |
| 1079562 | TRMK | 2005-02-25 | 2478 | 0.3932 |

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
Pinned: facts.py sha256 44e6723a38b55d920e92cf6ac08c5cc8f7e3b7ef01cd953ac489e5c6d2749eca
Pinned: predict_pk.py sha256 0c957c45b9cb6a501a516d4168f37983c5b380c102c760657c181b98ffe4d962
Pinned: test_extend.py sha256 31c6202b639a44534ba0e8a0c84983c922bc2d9c1c82535e84fbd3588a95719e
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/bokf_us_d.csv sha256 08245d8e7b738b77fd4f6e9c59660b28350273a67c338f2e13e30b4482524065
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/cac_us_d.csv sha256 c506541ca857b4c10a2cb25489e962caeb6fca48b2b5ef7ef6f3693e303249f8
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/cbu_us_d.csv sha256 db0784bc38d786fc17fbb36f54c988ad5e875da0262e94118f4d4c71714ed4f8
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/chco_us_d.csv sha256 626e6c294f2dd40aa668125f4f173c46cbd0dda1c03b9417f1ffa619ba3e19cd
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/ffbc_us_d.csv sha256 8c9e7e9cfcbb1dbdb46f9746b50009b123e85ba4a019091fb1f4294ae6534174
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/ffin_us_d.csv sha256 90a6f86134fd1d6fee012696a555e4cf7f53fcc1edcb3ef0905672659c0c2da8
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/fhn_us_d.csv sha256 6b5de821054df589cb95aa5fa6ca6d23ff9061c4920c88d7bd66516278806cd8
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/fnb_us_d.csv sha256 1db223eaaa9b7ab8be4b720e936de40d774bb248ed060d43f8f8bf82abdf6c17
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/frme_us_d.csv sha256 10d8504dea6c7e3560d678104a67443a132579dc5935958a8ba661ee1b0a057c
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/nbtb_us_d.csv sha256 34f6bd1e145dc5fa100a856fdac1700b4ccc498c3240d5a5916f15ed8ec54425
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/onb_us_d.csv sha256 3d37aa56fde1c7d37fb649ae3e64b1a7889625032a0a7d7657cf1153d6038183
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/osbc_us_d.csv sha256 08ced618c030268d1c798e6b7f3285a1ff28d9b039a828ad010a209521332c6b
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/pebo_us_d.csv sha256 1f535e6ab1503ecb57884da0def70e73f2d234114315303b7d137250d60e3613
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/prk_us_d.csv sha256 a7678bc8f69e051155aa2dd2093b24617216714292f9059fdc28f576f1498718
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/sfnc_us_d.csv sha256 99b2c0b93f8380b90a2cbd8663ec813c502aef7a748b106592f8a0bacbebf978
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/tcbi_us_d.csv sha256 a7efff7b01e925e2454fb460398ad28900067bfcd646cc8a6e330ae58ee624c2
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/thff_us_d.csv sha256 b52d29f5afd6699536ec51d3852a112fc8eea8c0f0c1a27b9727a0430f3f4bc0
Pinned: /home/anuragr/.cache/wealth_garch/stooq_20261024/trmk_us_d.csv sha256 41ecd1882b3e053b7aaa99a20bcc637dfec52a737897e66c35ac88e69f46dde6

Reproduce: python3 test_extend.py

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

new parents: RSSD, ticker, listed name, first date in 2005-2014, changes, sigma_E, sigma_E zeroed
  1883693 BOKF  BOK Financial Corporation - Common Stock | 2005-02-25 | 2478 | 0.3083 | 0.3083
  1130249 CAC   Camden National Corporation - Common Stock | 2005-02-25 | 2478 | 0.4343 | 0.4343
  1048867 CBU   Community Financial System, Inc. Common Stock | 2005-02-25 | 2478 | 0.3480 | 0.3480
  1076262 CHCO  City Holding Company - Common Stock | 2005-02-25 | 2478 | 0.3514 | 0.3514
  1071276 FFBC  First Financial Bancorp. - Common Stock | 2005-02-25 | 2478 | 0.4140 | 0.4140
  1102312 FFIN  First Financial Bankshares, Inc. - Common Stock | 2005-02-25 | 2478 | 0.3187 | 0.3187
  1094640 FHN   First Horizon Corporation Common Stock | 2005-01-03 | 2516 | 0.5071 | 0.4990
  3005332 FNB   F.N.B. Corporation Common Stock | 2005-02-25 | 2478 | 0.4377 | 0.4377
  1208559 FRME  First Merchants Corporation - Common Stock | 2005-02-25 | 2478 | 0.4504 | 0.4504
  1139279 NBTB  NBT Bancorp Inc. - Common Stock | 2005-02-25 | 2478 | 0.3628 | 0.3628
  1098303 ONB   Old National Bancorp - Common Stock | 2005-02-25 | 2478 | 0.3872 | 0.3872
  1206911 OSBC  Old Second Bancorp, Inc. - Common Stock | 2005-02-25 | 2477 | 0.6581 | 0.6365
  1070578 PEBO  Peoples Bancorp Inc. - Common Stock | 2005-02-25 | 2478 | 0.5169 | 0.5169
  1142336 PRK   Park National Corporation Common Stock | 2005-02-25 | 2478 | 0.4320 | 0.4320
  1094828 SFNC  Simmons First National Corporation - Common Stock | 2005-02-25 | 2478 | 0.4024 | 0.4024
  2706735 TCBI  Texas Capital Bancshares, Inc. Common Stock | 2005-02-25 | 2478 | 0.3948 | 0.3948
  1208595 THFF  First Financial Corporation  - Common Stock | 2005-02-25 | 2478 | 0.4235 | 0.4235
  1079562 TRMK  Trustmark Corporation - Common Stock | 2005-02-25 | 2478 | 0.3932 | 0.3932

===== sample B: 35 covered banks, 18 new
    4977 FHN   x0 0.1758 X 0.1673 sigma_i 0.00964 sigma_B 0.08481 | 8.80
    4214 BOKF  x0 0.1156 X 0.1218 sigma_i 0.00513 sigma_B 0.03754 | 7.32
    4988 TRMK  x0 0.1433 X 0.1380 sigma_i 0.00248 sigma_B 0.05426 | 21.86
    7888 FNB   x0 0.1152 X 0.1146 sigma_i 0.00253 sigma_B 0.05015 | 19.80
    3832 ONB   x0 0.1302 X 0.1339 sigma_i 0.00562 sigma_B 0.05185 | 9.23
   34383 TCBI  x0 0.1057 X 0.0998 sigma_i 0.00339 sigma_B 0.03940 | 11.63
    6989 CBU   x0 0.1578 X 0.1326 sigma_i 0.00474 sigma_B 0.04615 | 9.74
    7230 NBTB  x0 0.1272 X 0.1163 sigma_i 0.00194 sigma_B 0.04219 | 21.80
    6600 FFBC  x0 0.1252 X 0.1401 sigma_i 0.02228 sigma_B 0.05800 | 2.60
    6653 PRK   x0 0.1174 X 0.1113 sigma_i 0.00370 sigma_B 0.04807 | 13.00
    4365 FRME  x0 0.1464 X 0.1327 sigma_i 0.00546 sigma_B 0.05979 | 10.95
   17735 CHCO  x0 0.1273 X 0.1288 sigma_i 0.00407 sigma_B 0.04526 | 11.11
    4382 THFF  x0 0.1713 X 0.1665 sigma_i 0.00292 sigma_B 0.07051 | 24.12
    3066 FFIN  x0 0.1447 X 0.1423 sigma_i 0.00538 sigma_B 0.04535 | 8.43
    3603 OSBC  x0 0.1872 X 0.1328 sigma_i 0.02519 sigma_B 0.08742 | 3.47
    4255 CAC   x0 0.1385 X 0.1292 sigma_i 0.00273 sigma_B 0.05610 | 20.54
    3890 SFNC  x0 0.1464 X 0.1445 sigma_i 0.00526 sigma_B 0.05814 | 11.06
    6544 PEBO  x0 0.1326 X 0.1362 sigma_i 0.00568 sigma_B 0.07041 | 12.41
  barrier u0 over sigma_i, new banks: min 4.7, median 46.7, max 98.3

----- sample B, new banks: 18 banks; start 2014Q4, 2015-2024
observed, 2015-2024
  F1: 40/595 = 6.7%
  F2: 456/719 = 63.4%
  F3: 4/15 = 26.7%
  F4: 3/15 = 20.0%
baseline (Peura and Keppo), minimum 8%
  F1: 117324/126632 = 92.6%
  F2: 0/143427 = 0.0%
  F3: 483/570 = 84.7%
  F4: 524/570 = 91.9%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 35/14469 = 0.2%
  F2: 71268/79381 = 89.8%
(B) equity volatility, minimum at R: 0 start failed
  F1: 1/9074 = 0.0%
  F2: 48767/51447 = 94.8%
(B) with the three out-of-crisis changes set to zero, minimum 8%
  F1: 36/14576 = 0.2%
  F2: 71251/79629 = 89.5%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 85.9 + |dF2| 63.4 = 149.3 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 6.5 + |dF2| 26.4 = 32.8 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.7 + |dF2| 31.4 = 38.1 points; F1 within 10: True; F2 within 10: False
  (B) zeroed, minimum 8%: |dF1| 6.5 + |dF2| 26.1 = 32.5 points; F1 within 10: True; F2 within 10: False
  rule (the 18 new banks of sample B, minimum 8%): (B) beats the baseline (32.8 against 149.3 points)

----- sample B, all covered banks: 35 banks; start 2014Q4, 2015-2024
observed, 2015-2024
  F1: 72/1022 = 7.0%
  F2: 830/1302 = 63.7%
  F3: 7/24 = 29.2%
  F4: 8/24 = 33.3%
baseline (Peura and Keppo), minimum 8%
  F1: 190003/214067 = 88.8%
  F2: 0/259869 = 0.0%
  F3: 1153/1287 = 89.6%
  F4: 1042/1287 = 81.0%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 193/26411 = 0.7%
  F2: 121288/136241 = 89.0%
(B) equity volatility, minimum at R: 0 start failed
  F1: 54/17356 = 0.3%
  F2: 87280/92308 = 94.6%
(B) with the three out-of-crisis changes set to zero, minimum 8%
  F1: 194/26518 = 0.7%
  F2: 121271/136489 = 88.9%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 81.7 + |dF2| 63.7 = 145.5 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 6.3 + |dF2| 25.3 = 31.6 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.7 + |dF2| 30.8 = 37.5 points; F1 within 10: True; F2 within 10: False
  (B) zeroed, minimum 8%: |dF1| 6.3 + |dF2| 25.1 = 31.4 points; F1 within 10: True; F2 within 10: False

----- sample B, new banks, training window 2005-2014 (reported): 18 banks with a 2004Q4 buffer
observed, 2005-2014
  F1: 65/566 = 11.5%
  F2: 370/720 = 51.4%
  F3: 6/19 = 31.6%
  F4: 5/19 = 26.3%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 48/12030 = 0.4%
  F2: 59883/67156 = 89.2%
(B) equity volatility, minimum at R: 1 start failed
  F1: 4/6397 = 0.1%
  F2: 33546/34565 = 97.1%

===== sample A: 38 covered banks, 18 new
    4977 FHN   x0 0.1758 X 0.1673 sigma_i 0.00964 sigma_B 0.08481 | 8.80
    4214 BOKF  x0 0.1156 X 0.1218 sigma_i 0.00513 sigma_B 0.03754 | 7.32
    4988 TRMK  x0 0.1433 X 0.1380 sigma_i 0.00248 sigma_B 0.05426 | 21.86
    7888 FNB   x0 0.1152 X 0.1146 sigma_i 0.00253 sigma_B 0.05015 | 19.80
    3832 ONB   x0 0.1302 X 0.1339 sigma_i 0.00562 sigma_B 0.05185 | 9.23
   34383 TCBI  x0 0.1057 X 0.0998 sigma_i 0.00339 sigma_B 0.03940 | 11.63
    6989 CBU   x0 0.1578 X 0.1326 sigma_i 0.00474 sigma_B 0.04615 | 9.74
    7230 NBTB  x0 0.1272 X 0.1163 sigma_i 0.00194 sigma_B 0.04219 | 21.80
    6600 FFBC  x0 0.1252 X 0.1401 sigma_i 0.02228 sigma_B 0.05800 | 2.60
    6653 PRK   x0 0.1174 X 0.1113 sigma_i 0.00370 sigma_B 0.04807 | 13.00
    4365 FRME  x0 0.1464 X 0.1327 sigma_i 0.00546 sigma_B 0.05979 | 10.95
   17735 CHCO  x0 0.1273 X 0.1288 sigma_i 0.00407 sigma_B 0.04526 | 11.11
    4382 THFF  x0 0.1713 X 0.1665 sigma_i 0.00292 sigma_B 0.07051 | 24.12
    3066 FFIN  x0 0.1447 X 0.1423 sigma_i 0.00538 sigma_B 0.04535 | 8.43
    3603 OSBC  x0 0.1872 X 0.1328 sigma_i 0.02519 sigma_B 0.08742 | 3.47
    4255 CAC   x0 0.1385 X 0.1292 sigma_i 0.00273 sigma_B 0.05610 | 20.54
    3890 SFNC  x0 0.1464 X 0.1445 sigma_i 0.00526 sigma_B 0.05814 | 11.06
    6544 PEBO  x0 0.1326 X 0.1362 sigma_i 0.00568 sigma_B 0.07041 | 12.41
  barrier u0 over sigma_i, new banks: min 4.7, median 46.7, max 98.3

----- sample A, new banks: 18 banks; start 2014Q4, 2015-2024
observed, 2015-2024
  F1: 40/595 = 6.7%
  F2: 456/719 = 63.4%
  F3: 4/15 = 26.7%
  F4: 3/15 = 20.0%
baseline (Peura and Keppo), minimum 8%
  F1: 117324/126632 = 92.6%
  F2: 0/143427 = 0.0%
  F3: 483/570 = 84.7%
  F4: 524/570 = 91.9%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 35/14469 = 0.2%
  F2: 71268/79381 = 89.8%
(B) equity volatility, minimum at R: 0 start failed
  F1: 1/9074 = 0.0%
  F2: 48767/51447 = 94.8%
(B) with the three out-of-crisis changes set to zero, minimum 8%
  F1: 36/14576 = 0.2%
  F2: 71251/79629 = 89.5%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 85.9 + |dF2| 63.4 = 149.3 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 6.5 + |dF2| 26.4 = 32.8 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.7 + |dF2| 31.4 = 38.1 points; F1 within 10: True; F2 within 10: False
  (B) zeroed, minimum 8%: |dF1| 6.5 + |dF2| 26.1 = 32.5 points; F1 within 10: True; F2 within 10: False

----- sample A, all covered banks: 38 banks; start 2014Q4, 2015-2024
observed, 2015-2024
  F1: 72/1065 = 6.8%
  F2: 902/1375 = 65.6%
  F3: 7/25 = 28.0%
  F4: 8/25 = 32.0%
baseline (Peura and Keppo), minimum 8%
  F1: 197128/232883 = 84.6%
  F2: 0/283800 = 0.0%
  F3: 1220/1487 = 82.0%
  F4: 1202/1487 = 80.8%
(B) equity volatility, minimum 8%: 0 start failed
  F1: 193/31834 = 0.6%
  F2: 136575/152134 = 89.8%
(B) equity volatility, minimum at R: 0 start failed
  F1: 54/22476 = 0.2%
  F2: 101314/106559 = 95.1%
(B) with the three out-of-crisis changes set to zero, minimum 8%
  F1: 194/31941 = 0.6%
  F2: 136558/152382 = 89.6%
misses against observed F1 and F2 (points)
  baseline, minimum 8%: |dF1| 77.9 + |dF2| 65.6 = 143.5 points; F1 within 10: False; F2 within 10: False
  (B), minimum 8%: |dF1| 6.2 + |dF2| 24.2 = 30.3 points; F1 within 10: True; F2 within 10: False
  (B), minimum at R: |dF1| 6.5 + |dF2| 29.5 = 36.0 points; F1 within 10: True; F2 within 10: False
  (B) zeroed, minimum 8%: |dF1| 6.2 + |dF2| 24.0 = 30.2 points; F1 within 10: True; F2 within 10: False
```
