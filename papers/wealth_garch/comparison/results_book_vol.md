# Testing thread: book-capital volatility with the well-capitalised floor, and the risk and size directions

Run 24 October 2026,
under PREREG_comparison_17.md (commit 41ecb406).
Every model parameter is from 2004-2014 data; 2015-2024 enters only as the
observed facts and levels and the window simulated. Nothing from 2025 or
later enters.

## Findings, part 1: the level (measure (D), minimum at the well-capitalised 10%)

sigma_D,i is the standard deviation of the yearly change in the bank's
total capital ratio, 2004-2014. Sample B: median 1.18 points, 2.08 times
sigma_i (quartiles 1.30 and 3.10). The buffer copy of the baseline used
in part 2 reproduces predict_pk.py's per-bank counts on sample B exactly.

| Sample B | F1 | F2 | Miss, F1 + F2 (points) |
|---|---|---|---|
| training 2005-2014, observed | 17.1% | 48.0% | |
| training, (D) minimum 10% | 7.6% | 54.0% | 9.5 + 6.0 = 15.5 |
| test 2015-2024, the 26 unscored banks, observed | 1.0% | 58.5% | |
| test, 26 banks, baseline (Peura and Keppo) | 91.7% | 0.0% | 149.2 |
| test, 26 banks, (D) minimum 10% | 0.4% | 73.1% | 0.6 + 14.6 = 15.2 |
| test, all 61 banks, observed | 4.8% | 61.6% | |
| test, 61 banks, baseline | 90.0% | 0.0% | 146.8 |
| test, 61 banks, (D) minimum 10% | 7.2% | 56.2% | 2.3 + 5.4 = 7.8 |

- Stage 1: (D) predicts training F2 54.0% against 48.0%, within 10 points,
  and F1 7.6% against 17.1%, also within 10. (D) goes forward.
- Stage 2: on the 26 banks of sample B whose test-window facts had not
  been computed, (D) beats the baseline, 15.2 points against 149.2. It
  agrees on F1 (0.4% against 1.0%) and not on F2 (73.1% against 58.5%,
  14.6 points over).
- Over all 61 banks (reported, decides nothing) (D) is within 10 points
  on both facts: F1 7.2% against 4.8%, F2 56.2% against 61.6%. It is the
  first model in this thread to agree on F2.
- The floor carries the result. With the same sigma_D and the minimum at
  8%, F2 is 24.0% in training and 25.3% on the 61 test banks; with the
  minimum at R, 71.2% and 70.7%. As PREREG_comparison_17.md records, the
  10% floor (OCC-Q3) was chosen after a minimum near R had been seen to
  work, which is why the decision rests on the 26 unscored banks.
- sigma_D is the observed movement of book capital, which includes the
  bank's own dividends and issues; the model treats it as the shock
  before those decisions. That is a reading, not a derivation.
- Sample A (reported): on its 520 banks outside the 38 scored with (B),
  (D) gives F2 62.4% against 84.0% and F1 5.5% against 1.1% (miss 26.1;
  baseline 177.5).

## Findings, part 2: the directions of BDF-Q21

Spearman's rank correlation across banks of the median buffer over R with
the log of mean total assets (size) and with sigma_i (risk); a direction
holds if it has the predicted sign with p below 0.05.

| Sample A | size, data | size, baseline | size, (D) | risk, data | risk, baseline | risk, (D) |
|---|---|---|---|---|---|---|
| training (530 banks) | -0.341 (p 7e-16) | -0.059 (p 0.18) | -0.032 (p 0.46) | +0.229 (p 1e-7) | +0.935 | +0.332 |
| test (552 banks) | -0.387 (p 4e-21) | -0.058 (p 0.17) | -0.004 (p 0.93) | +0.262 (p 4e-10) | +0.937 | +0.361 |

- In the data both directions of BDF-Q21 hold in both windows over the
  banks of sample A. By tercile of size, the median buffer over R falls
  from 6.42 to 4.66 to 3.10 points (training) and from 7.53 to 5.96 to
  3.49 (test). By tercile of risk it rises from 3.66 to 4.65 to 5.36
  (training) and from 4.48 to 5.34 to 7.24 (test).
- Both models reproduce the risk direction. The baseline ties the buffer
  to sigma_i almost one for one (correlation 0.94 against the data's
  0.23 to 0.26); (D) less tightly (0.33 to 0.36).
- Neither model reproduces the size direction: their correlations with
  size are near zero and not significant. In both, size reaches the
  buffer only through the bank's volatility (sigma_i, sigma_D), and that
  channel carries none of the size gradient. Smaller banks hold larger
  buffers for some reason neither model contains.
- Sample B, the 61 largest banks, spans too narrow a range of size: the
  size direction does not hold in the data there (training -0.097, p
  0.46; test -0.060, p 0.64), and the risk direction holds in training
  (+0.277, p 0.031) but not in the test window (+0.226, p 0.08). There,
  too, both models reproduce the risk sign and neither a size gradient;
  the baseline's correlation with size is positive (+0.28, p 0.03).

## Reading

The level of buffers is matched, at the bank-quarter shares F1 and F2,
by a barrier model whose floor is the well-capitalised line where the
supervisor's restrictions begin and whose risk is the observed movement
of book capital. The match rests on that floor, chosen with the answer
partly in view and then confirmed on 26 unseen banks, with F2 still 14.6
points high there. The size gradient that Berger et al. report and the
data show is not produced by either model, which is a finding in its own
right: whatever makes smaller banks hold more capital is not their
volatility.

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

Reproduce: python3 book_vol.py

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

validation: the buffer-recording copy reproduces predict_pk.py's per-bank F1 and F2 counts on sample B: True

===== part 1, stage 1: training window 2005-2014, from the 2004Q4 buffer

--- sample B: 61 banks with sigma_D and a 2004Q4 buffer; 0 with fewer than 8 changes
  sigma_D in points: median 1.18; sigma_D over sigma_i: lq 1.30, median 2.08, uq 3.10
observed, 2005-2014
  F1: 264/1542 = 17.1%
  F2: 1171/2440 = 48.0%
(D) book-capital volatility, minimum 10%: 1 start failed
  F1: 16594/217992 = 7.6%
  F2: 203326/376705 = 54.0%
  (D), minimum 10%: |dF1| 9.5 + |dF2| 6.0 = 15.5 points; F1 within 10: True; F2 within 10: True
  stage 1 rule (sample B, minimum 10%): F2 54.0% against 48.0%; (D) goes forward
(D) book-capital volatility, minimum 8%: 0 start failed
  F1: 144003/257511 = 55.9%
  F2: 105216/437935 = 24.0%
  (D), minimum 8%: |dF1| 38.8 + |dF2| 24.0 = 62.8 points; F1 within 10: False; F2 within 10: False
(D) book-capital volatility, minimum at R: 6 start failed
  F1: 500/190148 = 0.3%
  F2: 229564/322257 = 71.2%
  (D), minimum at R: |dF1| 16.9 + |dF2| 23.2 = 40.1 points; F1 within 10: False; F2 within 10: False

--- sample A: 530 banks with sigma_D and a 2004Q4 buffer; 110 with fewer than 8 changes
  sigma_D in points: median 1.16; sigma_D over sigma_i: lq 1.46, median 2.22, uq 3.28
observed, 2005-2014
  F1: 951/14151 = 6.7%
  F2: 16040/21165 = 75.8%
(D) book-capital volatility, minimum 10%: 5 start failed
  F1: 132970/2343160 = 5.7%
  F2: 2342258/3827849 = 61.2%
  (D), minimum 10%: |dF1| 1.0 + |dF2| 14.6 = 15.6 points; F1 within 10: True; F2 within 10: False
(D) book-capital volatility, minimum 8%: 0 start failed
  F1: 1336865/2524638 = 53.0%
  F2: 1214151/4052807 = 30.0%
  (D), minimum 8%: |dF1| 46.2 + |dF2| 45.8 = 92.1 points; F1 within 10: False; F2 within 10: False
(D) book-capital volatility, minimum at R: 26 start failed
  F1: 11630/2190386 = 0.5%
  F2: 2614955/3586550 = 72.9%
  (D), minimum at R: |dF1| 6.2 + |dF2| 2.9 = 9.1 points; F1 within 10: True; F2 within 10: True

===== part 1, stage 2: test window 2015-2024, from the 2014Q4 buffer

--- sample B: 61 banks with sigma_D; 0 with fewer than 8 changes

  unscored: 26 banks
observed, 2015-2024
  F1: 6/590 = 1.0%
  F2: 512/875 = 58.5%
baseline (Peura and Keppo), minimum 8%
  F1: 141649/154541 = 91.7%
  F2: 0/197657 = 0.0%
  baseline, minimum 8%: |dF1| 90.6 + |dF2| 58.5 = 149.2 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 380/93472 = 0.4%
  F2: 127557/174381 = 73.1%
  (D), minimum 10%: |dF1| 0.6 + |dF2| 14.6 = 15.2 points; F1 within 10: True; F2 within 10: False
  stage 2 rule (the 26 unscored banks of sample B): (D) beats the baseline (15.2 against 149.2 points)
(D), minimum 8%: 0 start failed
  F1: 35282/100655 = 35.1%
  F2: 53707/180747 = 29.7%
  (D), minimum 8%: |dF1| 34.0 + |dF2| 28.8 = 62.8 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 416/90868 = 0.5%
  F2: 142604/170105 = 83.8%
  (D), minimum at R: |dF1| 0.6 + |dF2| 25.3 = 25.9 points; F1 within 10: True; F2 within 10: False

  already scored with (B): 35 banks
observed, 2015-2024
  F1: 72/1022 = 7.0%
  F2: 830/1302 = 63.7%
baseline (Peura and Keppo), minimum 8%
  F1: 190003/214067 = 88.8%
  F2: 0/259869 = 0.0%
  baseline, minimum 8%: |dF1| 81.7 + |dF2| 63.7 = 145.5 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 17894/160935 = 11.1%
  F2: 115989/258861 = 44.8%
  (D), minimum 10%: |dF1| 4.1 + |dF2| 18.9 = 23.0 points; F1 within 10: True; F2 within 10: False
(D), minimum 8%: 0 start failed
  F1: 109682/169166 = 64.8%
  F2: 58911/264319 = 22.3%
  (D), minimum 8%: |dF1| 57.8 + |dF2| 41.5 = 99.3 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 2106/154160 = 1.4%
  F2: 154454/250125 = 61.8%
  (D), minimum at R: |dF1| 5.7 + |dF2| 2.0 = 7.7 points; F1 within 10: True; F2 within 10: True

  all: 61 banks
observed, 2015-2024
  F1: 78/1612 = 4.8%
  F2: 1342/2177 = 61.6%
baseline (Peura and Keppo), minimum 8%
  F1: 331652/368608 = 90.0%
  F2: 0/457526 = 0.0%
  baseline, minimum 8%: |dF1| 85.1 + |dF2| 61.6 = 146.8 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 18274/254407 = 7.2%
  F2: 243546/433242 = 56.2%
  (D), minimum 10%: |dF1| 2.3 + |dF2| 5.4 = 7.8 points; F1 within 10: True; F2 within 10: True
(D), minimum 8%: 0 start failed
  F1: 144964/269821 = 53.7%
  F2: 112618/445066 = 25.3%
  (D), minimum 8%: |dF1| 48.9 + |dF2| 36.3 = 85.2 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 2522/245028 = 1.0%
  F2: 297058/420230 = 70.7%
  (D), minimum at R: |dF1| 3.8 + |dF2| 9.0 = 12.9 points; F1 within 10: True; F2 within 10: True

--- sample A: 558 banks with sigma_D; 110 with fewer than 8 changes

  unscored: 520 banks
observed, 2015-2024
  F1: 121/11030 = 1.1%
  F2: 12963/15425 = 84.0%
baseline (Peura and Keppo), minimum 8%
  F1: 3285743/3473991 = 94.6%
  F2: 0/4098742 = 0.0%
  baseline, minimum 8%: |dF1| 93.5 + |dF2| 84.0 = 177.5 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 136317/2477081 = 5.5%
  F2: 2477950/3973602 = 62.4%
  (D), minimum 10%: |dF1| 4.4 + |dF2| 21.7 = 26.1 points; F1 within 10: True; F2 within 10: False
(D), minimum 8%: 0 start failed
  F1: 1306038/2536606 = 51.5%
  F2: 1217136/4009899 = 30.4%
  (D), minimum 8%: |dF1| 50.4 + |dF2| 53.7 = 104.1 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 19941/2450649 = 0.8%
  F2: 2867209/3945938 = 72.7%
  (D), minimum at R: |dF1| 0.3 + |dF2| 11.4 = 11.7 points; F1 within 10: True; F2 within 10: False

  already scored with (B): 38 banks
observed, 2015-2024
  F1: 72/1065 = 6.8%
  F2: 902/1375 = 65.6%
baseline (Peura and Keppo), minimum 8%
  F1: 197128/232883 = 84.6%
  F2: 0/283800 = 0.0%
  baseline, minimum 8%: |dF1| 77.9 + |dF2| 65.6 = 143.5 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 17894/175026 = 10.2%
  F2: 130802/282772 = 46.3%
  (D), minimum 10%: |dF1| 3.5 + |dF2| 19.3 = 22.8 points; F1 within 10: True; F2 within 10: False
(D), minimum 8%: 0 start failed
  F1: 115071/183259 = 62.8%
  F2: 68053/288194 = 23.6%
  (D), minimum 8%: |dF1| 56.0 + |dF2| 42.0 = 98.0 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 2106/167873 = 1.3%
  F2: 169616/274017 = 61.9%
  (D), minimum at R: |dF1| 5.5 + |dF2| 3.7 = 9.2 points; F1 within 10: True; F2 within 10: True

  all: 558 banks
observed, 2015-2024
  F1: 193/12095 = 1.6%
  F2: 13865/16800 = 82.5%
baseline (Peura and Keppo), minimum 8%
  F1: 3482871/3706874 = 94.0%
  F2: 0/4382542 = 0.0%
  baseline, minimum 8%: |dF1| 92.4 + |dF2| 82.5 = 174.9 points; F1 within 10: False; F2 within 10: False
(D), minimum 10%: 0 start failed
  F1: 154211/2652107 = 5.8%
  F2: 2608752/4256374 = 61.3%
  (D), minimum 10%: |dF1| 4.2 + |dF2| 21.2 = 25.5 points; F1 within 10: True; F2 within 10: False
(D), minimum 8%: 0 start failed
  F1: 1421109/2719865 = 52.2%
  F2: 1285189/4298093 = 29.9%
  (D), minimum 8%: |dF1| 50.7 + |dF2| 52.6 = 103.3 points; F1 within 10: False; F2 within 10: False
(D), minimum at R: 0 start failed
  F1: 22047/2618522 = 0.8%
  F2: 3036825/4219955 = 72.0%
  (D), minimum at R: |dF1| 0.8 + |dF2| 10.6 = 11.3 points; F1 within 10: True; F2 within 10: False

===== part 2: directions of BDF-Q21 (level falls with size, rises with risk)

--- training 2005-2014, sample A: 530 banks
  observed level: 530 banks; with size rho -0.341 (p 6.9e-16); with risk rho +0.229 (p 9.8e-08)
  observed, by size: median level by tercile (low, middle, high): 6.42, 4.66, 3.10 points
  observed, by risk: median level by tercile (low, middle, high): 3.66, 4.65, 5.36 points
  baseline: 1 banks with no surviving path
  baseline level: 529 banks; with size rho -0.059 (p 0.18); with risk rho +0.935 (p 1.1e-238)
  (D), minimum 10%: 5 banks with no surviving path
  (D) level, minimum 10%: 525 banks; with size rho -0.032 (p 0.46); with risk rho +0.332 (p 5.1e-15)
  baseline, size: data direction holds (predicted negative); model does not reproduce the data's sign
  baseline, risk: data direction holds (predicted positive); model reproduces the data's sign
  (D), size: data direction holds (predicted negative); model does not reproduce the data's sign
  (D), risk: data direction holds (predicted positive); model reproduces the data's sign

--- training 2005-2014, sample B: 61 banks
  observed level: 61 banks; with size rho -0.097 (p 0.46); with risk rho +0.277 (p 0.031)
  observed, by size: median level by tercile (low, middle, high): 3.17, 2.21, 2.95 points
  observed, by risk: median level by tercile (low, middle, high): 2.03, 2.81, 3.09 points
  baseline: 1 banks with no surviving path
  baseline level: 60 banks; with size rho +0.281 (p 0.03); with risk rho +0.934 (p 1.2e-27)
  (D), minimum 10%: 1 banks with no surviving path
  (D) level, minimum 10%: 60 banks; with size rho -0.002 (p 0.99); with risk rho +0.302 (p 0.019)
  baseline, size: data direction does not hold (predicted negative); model does not reproduce the data's sign
  baseline, risk: data direction holds (predicted positive); model reproduces the data's sign
  (D), size: data direction does not hold (predicted negative); model does not reproduce the data's sign
  (D), risk: data direction holds (predicted positive); model reproduces the data's sign

--- test 2015-2024, sample A: 558 banks
  observed level: 552 banks; with size rho -0.387 (p 3.9e-21); with risk rho +0.262 (p 4.1e-10)
  observed, by size: median level by tercile (low, middle, high): 7.53, 5.96, 3.49 points
  observed, by risk: median level by tercile (low, middle, high): 4.48, 5.34, 7.24 points
  baseline: 1 banks with no surviving path
  baseline level: 557 banks; with size rho -0.058 (p 0.17); with risk rho +0.937 (p 1.8e-256)
  (D), minimum 10%: 1 banks with no surviving path
  (D) level, minimum 10%: 557 banks; with size rho -0.004 (p 0.93); with risk rho +0.361 (p 1.5e-18)
  baseline, size: data direction holds (predicted negative); model does not reproduce the data's sign
  baseline, risk: data direction holds (predicted positive); model reproduces the data's sign
  (D), size: data direction holds (predicted negative); model does not reproduce the data's sign
  (D), risk: data direction holds (predicted positive); model reproduces the data's sign

--- test 2015-2024, sample B: 61 banks
  observed level: 61 banks; with size rho -0.060 (p 0.64); with risk rho +0.226 (p 0.08)
  observed, by size: median level by tercile (low, middle, high): 3.85, 2.12, 2.66 points
  observed, by risk: median level by tercile (low, middle, high): 2.49, 3.00, 3.16 points
  baseline: 1 banks with no surviving path
  baseline level: 60 banks; with size rho +0.282 (p 0.029); with risk rho +0.934 (p 1.1e-27)
  (D), minimum 10%: 1 banks with no surviving path
  (D) level, minimum 10%: 60 banks; with size rho +0.034 (p 0.8); with risk rho +0.326 (p 0.011)
  baseline, size: data direction does not hold (predicted negative); model does not reproduce the data's sign
  baseline, risk: data direction does not hold (predicted positive); model reproduces the data's sign
  (D), size: data direction does not hold (predicted negative); model does not reproduce the data's sign
  (D), risk: data direction does not hold (predicted positive); model reproduces the data's sign
```
