# Testing thread: the do-nothing volatility, and the level bank by bank

Run 25 October 2026,
under PREREG_comparison_19.md (commit 21e8f283).
Every model parameter is from 2004-2014 data. Nothing from 2025 or later
enters.

## Findings

1. Measure (N) is larger than (D). The volatility of the do-nothing
   change (Berger et al.'s DNK less last year's ratio; BDF-Q30) has median
   1.36 points in sample B, 2.37 times sigma_i, and 1.19 times sigma_D
   (sample A: 1.05). Capital before the bank's own capital actions moves
   more than capital after them: on the median bank of sample B, issues,
   repurchases and dividend changes damp the yearly movement of the ratio
   by about a sixth.
2. Training gate: (N) with the 10% floor predicts F2 71.2% against 48.0%
   in sample B, 23.2 points over; F1 4.5% against 17.1%. (N) does not
   pass, and (D) stays as the enhanced model, with the circularity stated:
   its risk input contains the damping by the bank's own actions, and
   removing it overshoots. In sample A (reported) (N) is within 10 points
   on both facts (F2 66.8% against 75.8%, F1 4.8% against 6.7%).
3. Test window (reported): on the 26 banks of PREREG_comparison_17.md,
   (N) misses F1 + F2 by 22.7 points, (D) by 15.2, the baseline by 149.2;
   on all 61, (N) 11.8, (D) 7.8, baseline 146.8. (N) agrees on F1 and
   overshoots F2 (72.5% against 61.6% on the 61).
4. Bank by bank. Spearman's rank correlation of each bank's observed
   median buffer over R with the model's:

   | | training, sample B (60) | test, sample B (60) | training, sample A (525-529) | test, sample A (551) |
   |---|---|---|---|---|
   | baseline | +0.238 (p 0.067) | +0.188 (p 0.15) | +0.179 (p 4e-5) | +0.188 (p 9e-6) |
   | (D) minimum 10% | +0.538 (p 9e-6) | +0.247 (p 0.057) | +0.413 (p 5e-23) | +0.250 (p 3e-9) |
   | (N) minimum 10% | +0.277 (p 0.032) | +0.137 (p 0.30) | +0.392 (p 1e-20) | +0.239 (p 1e-8) |

   - By the rule fixed (positive, p below 0.05, both windows), no model
     places buffers among the 61 largest banks: (D) does in training and
     misses in the test window (p 0.057). In sample A all three do,
     (D) most strongly; the baseline's correlations are the weakest.
   - The correlations are modest. All three models order banks mainly by
     their volatility, and the observed buffer rises with risk at about
     the same strength (results_book_vol.md: +0.23 to +0.26), so part of
     every model's ranking is the risk gradient.
   - Least squares (reported) is dominated by a few banks with very large
     buffers (results_diag_f2.md item 6), so its R2 is not a guide to fit.
     In sample A's test window, observed = 4.09 points + 0.906 (se 0.034)
     times (D)'s prediction: the slope is near one and the model misses a
     roughly constant 4 points across banks, the extra capital smaller
     banks hold (results_book_vol.md part 2).

## Reading

The robustness checks weaken the level result rather than confirm it.
Removing the banks' own decisions from the risk input makes the model
overshoot, so (D)'s match rests partly on capital that banks have already
smoothed. Bank by bank, the enhanced model orders banks better than the
baseline in the full sample but not significantly among the 61 largest in
the test window, and in the full sample it misses a near-constant margin
of about 4 points. Two findings are firm: active capital management damps
the movement of capital ratios (finding 1), and the gap between models
and data is largely a constant extra margin that grows as banks get
smaller.

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
Pinned: do_nothing.py sha256 7963c569f3c0d9ec4c4760157b0f877e7529b16063e1560030798943fcb265f6

Reproduce: python3 do_nothing.py

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

===== 1. training gate: sample B, 2005-2014, from the 2004Q4 buffer

--- sample B: 61 banks with sigma_N, sigma_D and a 2004Q4 buffer; 0 not covered
  sigma_N in points: median 1.36; over sigma_i: lq 1.52, median 2.37, uq 3.62; over sigma_D: median 1.19
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
(N) do-nothing volatility, minimum 10%: 1 start failed
  F1: 8257/184598 = 4.5%
  F2: 250873/352148 = 71.2%
  (N), minimum 10%: |dF1| 12.6 + |dF2| 23.2 = 35.9 points; F1 within 10: False; F2 within 10: False
  gate (sample B, minimum 10%): F2 71.2% against 48.0%; (N) does not pass; (D) stays
(N) do-nothing volatility, minimum 8%: 0 start failed
  F1: 89800/226461 = 39.7%
  F2: 134870/417182 = 32.3%
  (N), minimum 8%: |dF1| 22.5 + |dF2| 15.7 = 38.2 points; F1 within 10: False; F2 within 10: False
(N) do-nothing volatility, minimum at R: 6 start failed
  F1: 352/155278 = 0.2%
  F2: 238633/295308 = 80.8%
  (N), minimum at R: |dF1| 16.9 + |dF2| 32.8 = 49.7 points; F1 within 10: False; F2 within 10: False

--- sample A: 530 banks with sigma_N, sigma_D and a 2004Q4 buffer; 110 not covered
  sigma_N in points: median 1.22; over sigma_i: lq 1.54, median 2.27, uq 3.47; over sigma_D: median 1.05
observed, 2005-2014
  F1: 951/14151 = 6.7%
  F2: 16040/21165 = 75.8%
(N) do-nothing volatility, minimum 10%: 5 start failed
  F1: 107904/2263399 = 4.8%
  F2: 2525380/3780849 = 66.8%
  (N), minimum 10%: |dF1| 2.0 + |dF2| 9.0 = 10.9 points; F1 within 10: True; F2 within 10: True
(N) do-nothing volatility, minimum 8%: 0 start failed
  F1: 1125783/2451976 = 45.9%
  F2: 1268916/4013131 = 31.6%
  (N), minimum 8%: |dF1| 39.2 + |dF2| 44.2 = 83.4 points; F1 within 10: False; F2 within 10: False
(N) do-nothing volatility, minimum at R: 26 start failed
  F1: 1655/2110662 = 0.1%
  F2: 2760912/3544857 = 77.9%
  (N), minimum at R: |dF1| 6.6 + |dF2| 2.1 = 8.7 points; F1 within 10: True; F2 within 10: True

===== 2. test window, reported: sample B, 2015-2024, from the 2014Q4 buffer

  the 26 of PREREG_comparison_17.md: 26 banks
observed, 2015-2024
  F1: 6/590 = 1.0%
  F2: 512/875 = 58.5%
baseline (Peura and Keppo), minimum 8%
  F1: 141649/154541 = 91.7%
  F2: 0/197657 = 0.0%
  baseline: |dF1| 90.6 + |dF2| 58.5 = 149.2 points; F1 within 10: False; F2 within 10: False
(N), minimum 10%: 0 start failed
  F1: 8197/95806 = 8.6%
  F2: 126079/171085 = 73.7%
  (N), minimum 10%: |dF1| 7.5 + |dF2| 15.2 = 22.7 points; F1 within 10: True; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 380/93472 = 0.4%
  F2: 127557/174381 = 73.1%
  (D), minimum 10%: |dF1| 0.6 + |dF2| 14.6 = 15.2 points; F1 within 10: True; F2 within 10: False

  all: 61 banks
observed, 2015-2024
  F1: 78/1612 = 4.8%
  F2: 1342/2177 = 61.6%
baseline (Peura and Keppo), minimum 8%
  F1: 331652/368608 = 90.0%
  F2: 0/457526 = 0.0%
  baseline: |dF1| 85.1 + |dF2| 61.6 = 146.8 points; F1 within 10: False; F2 within 10: False
(N), minimum 10%: 0 start failed
  F1: 8700/221586 = 3.9%
  F2: 299013/412445 = 72.5%
  (N), minimum 10%: |dF1| 0.9 + |dF2| 10.9 = 11.8 points; F1 within 10: True; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 18274/254407 = 7.2%
  F2: 243546/433242 = 56.2%
  (D), minimum 10%: |dF1| 2.3 + |dF2| 5.4 = 7.8 points; F1 within 10: True; F2 within 10: True

===== 3. bank by bank: observed median buffer over R against each model's

--- training 2005-2014, sample B: 61 banks
  baseline: 60 banks; Spearman +0.238 (p 0.067); observed = +5.25 points + 2.387 (se 0.761) x predicted; R2 0.625
  (D) minimum 10%: 60 banks; Spearman +0.538 (p 9.2e-06); observed = -0.16 points + 0.995 (se 0.063) x predicted; R2 0.898
  (N) minimum 10%: 60 banks; Spearman +0.277 (p 0.032); observed = -0.45 points + 0.900 (se 0.168) x predicted; R2 0.790

--- training 2005-2014, sample A: 530 banks
  baseline: 529 banks; Spearman +0.179 (p 3.5e-05); observed = +8.09 points + 2.225 (se 0.293) x predicted; R2 0.739
  (D) minimum 10%: 525 banks; Spearman +0.413 (p 4.8e-23); observed = +3.98 points + 0.533 (se 0.013) x predicted; R2 0.854
  (N) minimum 10%: 525 banks; Spearman +0.392 (p 1e-20); observed = +4.15 points + 0.460 (se 0.033) x predicted; R2 0.835

--- test 2015-2024, sample B: 61 banks
  baseline: 60 banks; Spearman +0.188 (p 0.15); observed = +11.20 points + 3.191 (se 3.180) x predicted; R2 0.044
  (D) minimum 10%: 60 banks; Spearman +0.247 (p 0.057); observed = +4.20 points + 1.254 (se 1.357) x predicted; R2 0.054
  (N) minimum 10%: 60 banks; Spearman +0.137 (p 0.3); observed = -5.30 points + 2.771 (se 2.623) x predicted; R2 0.239

--- test 2015-2024, sample A: 558 banks
  baseline: 551 banks; Spearman +0.188 (p 8.9e-06); observed = +11.49 points + 4.222 (se 0.837) x predicted; R2 0.536
  (D) minimum 10%: 551 banks; Spearman +0.250 (p 2.5e-09); observed = +4.09 points + 0.906 (se 0.034) x predicted; R2 0.625
  (N) minimum 10%: 551 banks; Spearman +0.239 (p 1.4e-08); observed = +4.33 points + 0.790 (se 0.104) x predicted; R2 0.623

  places buffers (Spearman positive, p < 0.05, in both windows):
    sample B, baseline: no (no, no)
    sample B, (D) minimum 10%: no (yes, no)
    sample B, (N) minimum 10%: no (yes, no)
    sample A, baseline: yes (yes, yes)
    sample A, (D) minimum 10%: yes (yes, yes)
    sample A, (N) minimum 10%: yes (yes, yes)
```
