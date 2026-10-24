# Testing thread: why measure (B) overshoots F2 (diagnostic, decides nothing)

Run 24 October 2026,
under PREREG_comparison_15.md (commit 8d603f67),
on the 35 covered banks of sample B of results_test_judged.md and
results_extend.md, with sigma_B,i as there. A description of data already
scored; no model is scored or chosen here.

## Findings

Buffers are in points of risk-weighted assets over R = 10.5%. The model
is the non-issuer barrier model with sigma_B,i in the dynamics and in the
barrier u0, minimum 8%, the same paths as the scored runs.

1. Level. The model's barrier u0 is far above the buffers banks hold.
   Over the 35 banks, the median of u0 over the bank's observed median
   buffer is 6.6 (test window) and 7.8 (training window). Typical values:
   u0 about 20 points, observed median buffer about 2 to 4 points, model
   median buffer about 10 to 13 points. The model's median sits well
   below its own barrier because the paths are so noisy (item 2).
2. Dynamics. Observed buffers move far less than sigma_B says. The median
   over banks of the standard deviation of year-on-year (Q4 to Q4) changes
   of the buffer is 0.94 points (test) and 1.07 points (training): 1.8
   times sigma_i, and 0.15 to 0.21 times sigma_B. sigma_B,i = sigma_E,i X_i
   assumes book capital moves in proportion to the market value of
   equity; book capital ratios move at about a fifth of that.
3. Where the gap is. The 25.3-point gap in F2 (model 89.0% against
   observed 63.7%) comes mostly from the banks with the largest sigma_i.
   In the top tercile of sigma_i (13 banks, sigma_i 0.96 to 2.52 points,
   so 4 sigma_i is 4 to 10 points) the observed F2 is 29.8% and the
   model's 79.0%, contributing 16.0 of the points; the low tercile
   (sigma_i 0.19 to 0.41 points, 4 sigma_i under 2 points) contributes
   3.1, with observed 84.5% and model 93.8%. For low-sigma_i banks almost
   any buffer counts as "far above", so model and data agree there
   whatever the level; the level error shows only where 4 sigma_i is
   large.
4. Implied scale. The scale k_i at which u0(mu_i, k_i sigma_i, rho)
   equals the bank's observed median training buffer has median 1.54
   (lower quartile 0.74, upper 2.06, 33 banks; TCBI's median buffer is
   negative and one Zions bank's u0 is below its buffer at every k).
   sigma_B,i exceeds k_i sigma_i by a median factor of 7.1 (3.1 to 30.6).
   Caveat: as pre-registered, u0 (over the 8% minimum) is set equal to
   the buffer over R, which is 2.5 points lower than the buffer over the
   minimum; a like-for-like version would give larger k_i. It is not
   computed here, and would belong to a follow-up pre-registration.
5. Two banks do not fit the barrier model at all with (B): Zions cert
   20626 has mu_i of 0.012 points, so u0 is 0.31 points and every path
   dies within the first quarter; Huntington's u0 is 2.24 points, below
   R, so its model buffer over R is negative. u0 falls with mu_i as well
   as rising with sigma, and for these banks mu_i is small.
6. Oddities in the observed data, left as they are: Wells Fargo cert
   13718 has an observed median buffer of 366 points in 2015-2024 (its
   risk-weighted assets are small against its capital), and Bank of
   America cert 25178 one of 24 points; both count as "far above" in
   every quarter and both are in the scored facts as recorded.

## Reading

Measure (B) has the right sign and the wrong size. A judged risk larger
than earnings volatility does raise the barrier, which is why (B) beats
the baseline; but equity volatility scaled by the capital ratio is about
five to seven times the risk that banks' buffers and buffer movements
imply, in both the barrier and the dynamics. The overshoot in F2 is the
visible part of that; it is small in the pooled share only because for
most banks 4 sigma_i is under 2 points. Whatever replaces (B) has to
place the buffers, not just exceed 4 sigma_i. Candidates are new
mechanisms and are not chosen here; any one of them is tested under its
own pre-registration, which states that these 35 banks' test-window facts
have been seen.

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

Reproduce: python3 diag_f2.py

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

===== 35 covered banks of sample B; buffers in points of risk-weighted assets over R = 10.5%

1. per bank, sorted by sigma_i
  cert   tick  sigma_i  sigma_B  sB/si   u0    | test: obs median  obs yr sd  obs F2 | model median  model F2 | train: obs median  obs yr sd  obs F2 | k_i  (sB/si)/k_i
    7230 NBTB    0.19    4.22  21.80  19.02 |     2.20     0.91  100.0% |     9.55   94.9% |     0.76     0.91   47.5% |  2.06 10.60
    4988 TRMK    0.25    5.43  21.86  23.48 |     2.46     0.90   90.0% |    12.41   96.8% |     3.64     1.81   65.0% |  4.42  4.95
    7888 FNB     0.25    5.02  19.80  20.94 |     1.07     0.50   50.0% |     9.49   91.9% |     1.03     0.61   52.5% |  1.83 10.82
    4255 CAC     0.27    5.61  20.54  25.16 |     2.81     0.63  100.0% |    12.78   95.3% |     2.09     0.78   70.0% |  2.98  6.89
    6384 PNC     0.29    5.78  20.03  20.99 |     2.07     0.80   90.0% |    10.37   93.8% |     3.37     1.54   60.0% |  3.25  6.15
    4382 THFF    0.29    7.05  24.12  25.09 |     6.40     1.63  100.0% |    13.47   96.4% |     6.60     1.54  100.0% |  5.42  4.45
     628 JPM     0.34    5.46  16.16  19.46 |     5.26     0.63  100.0% |     8.62   89.9% |     1.97     0.68   65.0% |  1.90  8.53
   34383 TCBI    0.34    3.94  11.63  16.29 |     0.81     1.33   42.5% |     7.21   87.3% |    -0.21     0.81    0.0% |     -     -
    6548 USB     0.37    4.41  11.98  20.64 |     2.14     0.99  100.0% |    10.96   94.0% |     1.00     0.48   42.5% |  1.44  8.33
    6653 PRK     0.37    4.81  13.00  22.52 |     1.67     0.43   60.0% |    10.76   91.8% |     0.89     0.28   10.0% |  1.44  9.06
   17735 CHCO    0.41    4.53  11.11  21.09 |     3.90     0.45   97.4% |    13.09   95.5% |     2.65     0.97   90.0% |  2.55  4.35
    3511 WFC     0.42    5.63  13.47  24.79 |     3.75     0.94  100.0% |    13.53   92.8% |     1.62     0.83   47.5% |  1.63  8.28
    6989 CBU     0.47    4.61   9.74  21.51 |     4.90     1.07  100.0% |    12.39   95.7% |     2.68     1.01   67.5% |  2.03  4.80
    4214 BOKF    0.51    3.75   7.32  17.57 |     1.61     0.73   17.5% |     8.19   86.7% |     1.69     0.89   40.0% |  1.33  5.51
    3890 SFNC    0.53    5.81  11.06  25.42 |     3.04     1.42   75.0% |    13.01   94.5% |     2.20     2.43   50.0% |  1.57  7.06
    3066 FFIN    0.54    4.53   8.43  20.20 |     7.38     1.03  100.0% |    13.69   96.8% |     3.87     0.97   92.5% |  2.71  3.11
    4365 FRME    0.55    5.98  10.95  24.43 |     2.71     0.57   85.0% |    12.71   94.2% |     2.23     1.41   50.0% |  1.44  7.60
    3832 ONB     0.56    5.18   9.23  21.08 |     2.02     1.03   40.0% |     9.96   88.0% |     2.93     0.68   65.0% |  1.58  5.85
    6544 PEBO    0.57    7.04  12.41  23.92 |     2.70     0.62   80.0% |    11.84   90.4% |     3.47     1.48   60.0% |  1.74  7.14
    3510 BAC     0.60    7.53  12.53  24.54 |     3.51     0.59  100.0% |    12.52   92.6% |     3.14     0.83   55.0% |  1.54  8.14
    2270 ZION    0.90    7.06   7.81  17.35 |     3.16     0.73   42.5% |     9.16   84.2% |     1.45     0.79   35.0% |  0.53 14.67
    7213 C       0.94    9.01   9.54  18.08 |     4.52     0.62   92.5% |     8.59   79.6% |     4.81     1.13   62.5% |  1.17  8.15
   17534 KEY     0.96    7.41   7.75  11.50 |     2.21     0.74   10.0% |     4.94   61.9% |     2.95     1.44   27.5% |  0.71 10.89
    4977 FHN     0.96    8.48   8.80   7.78 |     1.83     0.94    5.0% |     2.98   32.3% |     7.07     2.56   65.0% |  2.78  3.17
    4297 COF     1.01    6.43   6.39  12.15 |     3.31     1.21   20.0% |     5.52   64.9% |     1.72     1.04    7.5% |  0.47 13.65
   23702 JPM     1.18    6.10   5.19  28.09 |     2.07     0.94   11.8% |    14.38   83.6% |     3.64     2.78   30.0% |  1.06  4.90
   25178 BAC     1.41   12.88   9.12  58.78 |    23.69     9.30  100.0% |    36.63   97.5% |    11.31     5.10   90.0% |  2.35  3.88
   27389 WFC     1.74    6.89   3.97  31.40 |    16.64    11.19   75.0% |    21.04   90.5% |     4.13     2.38   15.0% |  0.99  4.02
    7946 BNY     1.86    5.60   3.00  19.48 |     2.01     1.62    0.0% |     9.10   56.7% |     3.38     1.14    0.0% |  0.49  6.14
   20626 ZION    2.18    8.20   3.77   0.31 |     4.52        -    0.0% |        -      -% |     4.87     3.28    0.0% |     -     -
    6600 FFBC    2.23    5.80   2.60  26.89 |     2.54     0.68    0.0% |    13.93   68.8% |     3.09     1.34    0.0% |  0.50  5.17
   13718 WFC     2.27    7.93   3.49  37.09 |   366.29    97.31  100.0% |    28.48   97.6% |     4.60     3.77   22.5% |  0.74  4.70
   24107 ZION    2.46    7.34   2.98   5.88 |     3.47        -    0.0% |     1.46    0.0% |     3.33     1.67    0.0% |  0.30 10.06
    6560 HBAN    2.48    7.87   3.17   2.24 |     2.56     0.96    0.0% |    -1.19    0.0% |     1.19     0.63    0.0% |  0.10 30.59
    3603 OSBC    2.52    8.74   3.47  11.85 |     3.79     0.99    0.0% |     5.73    0.0% |     1.09     1.07    0.0% |  0.14 25.62

2. terciles of sigma_i (test window)
  low    11 banks, sigma_i 0.19 to 0.41 points: observed F2  84.5%, model F2  93.8%, weight 34% of observed bank-quarters, contribution +3.1 points
  middle 11 banks, sigma_i 0.42 to 0.94 points: observed F2  75.7%, model F2  91.8%, weight 34% of observed bank-quarters, contribution +5.4 points
  high   13 banks, sigma_i 0.96 to 2.52 points: observed F2  29.8%, model F2  79.0%, weight 32% of observed bank-quarters, contribution +16.0 points
  overall gap, model less observed: +25.3 points (contributions sum to +24.6)

3. over banks (medians)
  u0 over observed median buffer: test 6.62 (35 banks with a positive median), training 7.77 (34)
  observed yearly sd of the buffer, test window: median 0.94 points; over sigma_i 1.84; over sigma_B 0.15
  observed yearly sd of the buffer, training window: median 1.07 points; over sigma_i 1.81; over sigma_B 0.21

4. implied scale k_i (training median buffer = u0(mu_i, k_i sigma_i, rho))
  33 banks with a k_i in [0.1, 50]: min 0.10, lower quartile 0.74, median 1.54, upper quartile 2.06, max 5.42
  sigma_B over k_i sigma_i: min 3.11, median 7.06, max 30.59
  banks with no k_i: ['TCBI', 'ZION']
```
