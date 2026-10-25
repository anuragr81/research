# Testing thread: the frozen enhanced model on the training and test windows

Run 25 October 2026,
under PREREG_comparison_20.md (commit 6aecfb5e).
Reported; decides nothing. The decision for the frozen model (P) is the
2025 hold-out, not yet opened. Nothing from 2025 or later enters.

## Findings

(P): the barrier model with book-capital volatility sigma_D and the floor
the sources set, 10% to 2009Q4 and 10.5% from 2010Q1 (OR-Q1 to Q6, OR-Q13,
LM-Q5). Baseline: Peura and Keppo with the 8% minimum.

| | observed F1 / F2 | (P) F1 / F2 | (P) miss | baseline miss |
|---|---|---|---|---|
| training 2005-2014, sample B (61) | 17.1% / 48.0% | 4.2% / 62.4% | 12.9 + 14.4 = 27.4 | 121.1 |
| training, sample A (530) | 6.7% / 75.8% | 3.2% / 66.4% | 3.5 + 9.3 = 12.8 | 163.5 |
| test 2015-2024, sample B (61) | 4.8% / 61.6% | 1.0% / 70.7% | 3.9 + 9.1 = 12.9 | 146.8 |
| test, sample A (558) | 1.6% / 82.5% | 0.8% / 72.0% | 0.8 + 10.5 = 11.3 | 174.9 |

- On the test window (P) agrees with both facts in sample B, F1 within 3.9
  points and F2 within 9.1; in sample A it agrees on F1 and misses F2 by
  10.5 points. These test-window figures had in effect been seen
  (results_book_vol.md, minimum at R), as PREREG_comparison_20.md records.
- On the training window, where the switch of floor in 2010 is new, (P)
  agrees on neither fact in sample B (F1 12.9 points low, F2 14.4 points
  high) and on both in sample A. With the floor at 10% throughout the
  training F2 was 54.0% (results_book_vol.md); the rise to 10.5% for
  2010-2014 adds about 8 points.
- (P) beats the baseline in every window and sample, by 94 to 164 points.
- Standing of the claim: the enhancement thread's pre-registered result
  is still the earlier one on unseen banks (results_test_judged.md,
  results_extend.md, results_book_vol.md); this frozen version is
  sourced rather than chosen, and its own test is the 2025 hold-out.

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
Pinned: diag_f2.py sha256 7faa2b444897ba01130bfe34a2c861d5121365e0344dd5488aa23d7b6bea068e
Pinned: book_vol.py sha256 93b2aaae484367775b055b4273339d90a10cb69f9d0f78029e9c21aa85d0406d
Pinned: frozen.py sha256 abe6bdb9e78235576b0f3eb687f5ecdb59d861e00bfa811d581b021412d8d5a2

Reproduce: python3 frozen.py

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

===== training 2005-2014, floor 10% to 2009Q4 and 10.5% from 2010Q1

--- sample B: 61 banks; 0 without sigma_D
observed
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
(P) frozen model: 1 start failed
  F1: 8684/207875 = 4.2%
  F2: 232193/372052 = 62.4%
baseline (Peura and Keppo), minimum 8%
  F1: 331285/367033 = 90.3%
  F2: 0/456376 = 0.0%
  (P): |dF1| 12.9 + |dF2| 14.4 = 27.4 points; F1 within 10: False; F2 within 10: False
  baseline: |dF1| 73.1 + |dF2| 48.0 = 121.1 points; F1 within 10: False; F2 within 10: False
  (P) beats the baseline (27.4 against 121.1 points); reported, decides nothing

--- sample A: 530 banks; 110 without sigma_D
observed
  F1: 951/14151 = 6.7%
  F2: 16040/21165 = 75.8%
(P) frozen model: 5 start failed
  F1: 73280/2256430 = 3.2%
  F2: 2527347/3803643 = 66.4%
baseline (Peura and Keppo), minimum 8%
  F1: 3324946/3521863 = 94.4%
  F2: 0/4155006 = 0.0%
  (P): |dF1| 3.5 + |dF2| 9.3 = 12.8 points; F1 within 10: True; F2 within 10: True
  baseline: |dF1| 87.7 + |dF2| 75.8 = 163.5 points; F1 within 10: False; F2 within 10: False
  (P) beats the baseline (12.8 against 163.5 points); reported, decides nothing

===== test 2015-2024, floor 10.5%

--- sample B: 61 banks; 0 without sigma_D
observed
  F1: 78/1612 = 4.8%
  F2: 1342/2177 = 61.6%
(P) frozen model: 0 start failed
  F1: 2420/245120 = 1.0%
  F2: 297538/420869 = 70.7%
baseline (Peura and Keppo), minimum 8%
  F1: 331652/368608 = 90.0%
  F2: 0/457526 = 0.0%
  (P): |dF1| 3.9 + |dF2| 9.1 = 12.9 points; F1 within 10: True; F2 within 10: True
  baseline: |dF1| 85.1 + |dF2| 61.6 = 146.8 points; F1 within 10: False; F2 within 10: False
  (P) beats the baseline (12.9 against 146.8 points); reported, decides nothing

--- sample A: 558 banks; 110 without sigma_D
observed
  F1: 193/12095 = 1.6%
  F2: 13865/16800 = 82.5%
(P) frozen model: 0 start failed
  F1: 21977/2615924 = 0.8%
  F2: 3040206/4221892 = 72.0%
baseline (Peura and Keppo), minimum 8%
  F1: 3482871/3706874 = 94.0%
  F2: 0/4382542 = 0.0%
  (P): |dF1| 0.8 + |dF2| 10.5 = 11.3 points; F1 within 10: True; F2 within 10: False
  baseline: |dF1| 92.4 + |dF2| 82.5 = 174.9 points; F1 within 10: False; F2 within 10: False
  (P) beats the baseline (11.3 against 174.9 points); reported, decides nothing
```
