# Testing thread: does access to capital carry the size gradient of buffers?

Run 24 October 2026,
under PREREG_comparison_18.md (commit 4e62f829).
Observed buffers only; no model is scored. Nothing from 2025 or later
enters.

## Findings

Sample A, classification at each window's start. Level: the bank's median
buffer over R, winsorised at the 1st and 99th percentiles. Regression with
HC1 standard errors; listed affiliated banks are the base.

| | training 2005-2014 (519 banks) | test 2015-2024 (638 banks) |
|---|---|---|
| log size | -0.0097 (p 2e-10) | -0.0137 (p 2e-12) |
| sigma_i | +0.605 (p 8e-9) | +0.744 (p 6e-11) |
| stand-alone (predicted +) | +0.0007 (p 0.89) | -0.0169 (p 0.033) |
| unlisted (predicted +) | +0.0051 (p 0.35) | -0.0070 (p 0.50) |
| foreign parent | +0.107 (p 0.0002) | +0.046 (p 0.005) |
| log size without the access terms | -0.0082 | -0.0100 |
| share of the size coefficient carried by access | -0.18 | -0.36 |

- Stand-alone direction: does not hold. No effect in training; in the test
  window the sign is the opposite of the prediction (stand-alone banks
  hold 1.7 points less, p 0.033).
- Unlisted direction: does not hold in the test window, which decides
  (+, -0.0070, p 0.50); not in training either.
- Access does not carry the size gradient. Adding the access terms makes
  the size coefficient larger in magnitude, not smaller (by 18% and 36%).
- The gradient is there within each group. Spearman of level with log
  size: stand-alone banks -0.381 (training) and -0.410 (test), affiliated
  banks -0.222 and -0.269, all with p below 0.02. By size tercile, median
  levels fall from 7.96 to 6.82 to 4.00 points among stand-alone banks
  (test) and from 6.82 to 5.74 to 3.31 among affiliated ones.
- Reported, deciding nothing: on ranks, unlisted is positive and
  significant in both windows (rank coefficient +51.7, p 0.0099; +57.7, p
  0.021), stand-alone is not. Listed parents' banks hold a median of 2.65
  and 2.74 points against unlisted 4.78 and 6.33, but they are far larger
  (median log size 14.7 and 14.6 against 12.6 and 12.4), so most of that
  raw difference is size. Read with the main regression: there is at most
  a weak listing effect, sensitive to how outliers are treated, and no
  stand-alone effect.
- Unpredicted: the few banks with foreign parents (8 and 11) hold much
  more, +10.7 and +4.6 points after size and risk. No source read
  predicts this, and the groups are small; it is recorded, not claimed.
- Counts: training 403 stand-alone and 116 affiliated; 55 listed, 456
  unlisted, 8 foreign; 17 banks whose parent has no NIC record left out.
  Test: 532 and 106; 65, 562, 11; 22 left out, and 8 with no level.
- Caveat as fixed: listing is read from the October 2026 directory, so
  parents absorbed or failed since count as unlisted.

## Reading

The size gradient in buffers is not a matter of access to capital as the
data can measure it: stand-alone banks and banks without a listed parent
do not hold more capital than affiliated or listed banks of the same
size, and the gradient persists within each group (steeper among
stand-alone banks than among affiliated ones). Of the explanations
Berger et al. list (BDF-Q24), the one that would have been testable here,
a lower cost of raising equity for larger or better-connected banks, gets
no support from these two proxies. Diversification as earnings volatility
was ruled out in results_book_vol.md. What remains is scale economies in
risk management and expectations of support for the largest banks, which
this data does not measure, or something none of the sources names.

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
Pinned: ../replication/gordon_hirtle_2026/CSV_ATTRIBUTES_ACTIVE.zip sha256 a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4
Pinned: ../replication/gordon_hirtle_2026/CSV_ATTRIBUTES_CLOSED.zip sha256 adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c
Pinned: sample.py sha256 857918747fc9ec718cec3efad39fe5ee5dff430528b6b77e83ba760d14a33755
Pinned: window.py sha256 69af27363f12132b2074ec1514fa68f6aa801590411278137fce168362b51340
Pinned: judged_risk.py sha256 b7a37555e77041b1e5c7117cdcf1a03900ef2399435e44db8165dd9e156d1d78
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/nasdaqlisted.txt sha256 df221177204f1c56add0c52adcd8f19c7371bde662eb34c8b32c5bba8bf4f5b0
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/otherlisted.txt sha256 cab8d0e5cb077acb38ebe201113a909b7ee3784c32249281d83399c0b0d5732f
Pinned: match_tickers.py sha256 025c132c26c76070aa461894f5b3744976c5bf94d15c87155be9e6ca553da1ee
Pinned: access.py sha256 c934ef66e0589516574a616c28aa02ea7451fec81d6176edbe39df29f4e2b5a7

Reproduce: python3 access.py

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

===== training 2005-2014: 519 banks of sample A with a buffer at 20041231; 17 whose parent has no NIC record, 0 with no level
  counts (stand-alone, listing): affiliated/foreign 5, affiliated/listed 35, affiliated/unlisted 76, stand-alone/foreign 3, stand-alone/listed 20, stand-alone/unlisted 380
  level winsorised at 0.56 and 28.63 points; median 4.55
  1. level on log size, sigma_i, stand-alone, unlisted, foreign (HC1)
    const      +0.1761 (se 0.0238, p 1.5e-13)
    logsize    -0.0097 (se 0.0015, p 2.3e-10)
    sigma      +0.6050 (se 0.1050, p 8.3e-09)
    standalone +0.0007 (se 0.0052, p 0.89)
    unlisted   +0.0051 (se 0.0055, p 0.35)
    foreign    +0.1066 (se 0.0290, p 0.00024)
    n 519, R2 0.235
  2. level on log size and sigma_i only (HC1)
    const      +0.1630 (se 0.0164, p 3e-23)
    logsize    -0.0082 (se 0.0012, p 1.1e-11)
    sigma      +0.6190 (se 0.1122, p 3.5e-08)
    n 519, R2 0.178
  share of the size coefficient carried by access: 1 - -0.0097 / -0.0082 = -0.18
  stand-alone direction (b3 > 0, p < 0.05): does not hold in this window
  unlisted direction (b4 > 0, p < 0.05): does not hold in this window (reported)
  3. the same on ranks (HC1)
    const      +242.8514 (se 31.1172, p 6e-15)
    logsize    -0.3182 (se 0.0471, p 1.5e-11)
    sigma      +0.1866 (se 0.0415, p 6.9e-06)
    standalone +3.5566 (se 16.8225, p 0.83)
    unlisted   +51.7381 (se 20.0705, p 0.0099)
    foreign    +204.0064 (se 47.6508, p 1.9e-05)
    n 519, R2 0.196
  stand-alone: 403 banks; Spearman of level with log size -0.381 (p 2.2e-15); median level by size tercile 6.57, 5.09, 3.10 points
  affiliated: 116 banks; Spearman of level with log size -0.222 (p 0.017); median level by size tercile 4.65, 4.98, 2.68 points
  listed: 55 banks; median level 2.65 points; median log size 14.72
  unlisted: 456 banks; median level 4.78 points; median log size 12.56
  foreign: 8 banks; median level 10.05 points; median log size 16.07

===== test 2015-2024: 638 banks of sample A with a buffer at 20141231; 22 whose parent has no NIC record, 8 with no level
  counts (stand-alone, listing): affiliated/foreign 6, affiliated/listed 34, affiliated/unlisted 66, stand-alone/foreign 5, stand-alone/listed 31, stand-alone/unlisted 496
  level winsorised at 0.97 and 35.70 points; median 5.99
  1. level on log size, sigma_i, stand-alone, unlisted, foreign (HC1)
    const      +0.2653 (se 0.0348, p 2.6e-14)
    logsize    -0.0137 (se 0.0019, p 2e-12)
    sigma      +0.7439 (se 0.1137, p 6e-11)
    standalone -0.0169 (se 0.0079, p 0.033)
    unlisted   -0.0070 (se 0.0105, p 0.5)
    foreign    +0.0464 (se 0.0166, p 0.0053)
    n 638, R2 0.184
  2. level on log size and sigma_i only (HC1)
    const      +0.1987 (se 0.0205, p 3.1e-22)
    logsize    -0.0100 (se 0.0016, p 1.8e-10)
    sigma      +0.7944 (se 0.1427, p 2.6e-08)
    n 638, R2 0.165
  share of the size coefficient carried by access: 1 - -0.0137 / -0.0100 = -0.36
  stand-alone direction (b3 > 0, p < 0.05): does not hold in this window
  unlisted direction (b4 > 0, p < 0.05): does not hold in this window (decides)
  3. the same on ranks (HC1)
    const      +310.8221 (se 33.8830, p 4.6e-20)
    logsize    -0.3452 (se 0.0384, p 2.3e-19)
    sigma      +0.2150 (se 0.0365, p 3.9e-09)
    standalone -4.5126 (se 20.2085, p 0.82)
    unlisted   +57.6764 (se 24.8989, p 0.021)
    foreign    +186.4505 (se 52.6914, p 0.0004)
    n 638, R2 0.215
  stand-alone: 532 banks; Spearman of level with log size -0.410 (p 5.2e-23); median level by size tercile 7.96, 6.82, 4.00 points
  affiliated: 106 banks; Spearman of level with log size -0.269 (p 0.0052); median level by size tercile 6.82, 5.74, 3.31 points
  listed: 65 banks; median level 2.74 points; median log size 14.58
  unlisted: 562 banks; median level 6.33 points; median log size 12.37
  foreign: 11 banks; median level 9.62 points; median log size 15.63
```
