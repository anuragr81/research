# Testing thread: Mankart, Michaelides and Pagratis on the four facts

Run 18 October 2026,
under PREREG_comparison.md (commit 26f44a15),
under PREREG_comparison_2.md (commit d8cc561a)
and under PREREG_comparison_3.md (commit 7931b53a).

## Findings

MMP's small-bank model at the published parameters, against the small
bank-quarters of sample A (below the 95th percentile of commercial banks'
assets each quarter), 2015-2024. Sample B is not scoreable for MMP.

| fact | MMP | observed (small, A) | verdict |
|---|---|---|---|
| F1, dividends paid from a buffer below 1 sigma | 30.3% | 1.1% | disagrees |
| F2, buffer above 4 sigma | 45.0% | 85.9% | disagrees |
| F3, recapitalisations starting below 1 sigma | no episodes | 256 episodes | fails |
| F4, recapitalisations landing between 1 and 4 sigma | no episodes | 256 episodes | fails |

- MMP agrees on none of the four facts. Its banks hold more capital far
  above R than the baseline's (45.0% of bank-quarters against 0.0% in
  results_pk.md, though on different samples), and fewer than real small
  banks (85.9%).
- MMP's banks pay a dividend in almost every quarter (4,988,046 dividend
  quarters with the previous buffer observed, out of 4,999,023 live
  bank-quarters), so its F1 runs over nearly all of them.
- sigma of a simulated bank is its own annual return on risk-weighted
  assets; every simulated bank had at least 8 complete years.

Pinned: ../replication/peura_keppo_2006/fdic_dec_2004_2024.csv.gz sha256 0f3af028bb3471f2693e34869663c050909adc6cdeae89f294f1d2b3d4f1801d
Pinned: ../replication/peura_keppo_2006/fedfunds.csv sha256 74680b2d09bb28f90fb30c1c09e93ac2d443de1b7345ba5a9112b3eca9be0a25
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
Pinned: predict_mmp.py sha256 2e6299c9ad04ad8b380c3d4b8eb14bdb94bf048010f000b768e6f229b39a6ca2
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783
Pinned: ../replication/mankart_michaelides_pagratis_2020/stage2.py sha256 b6be175625a9ce996d838975d872a1f4d01840725f6fa8c02028e299fab4aac6
Pinned: ../replication/mankart_michaelides_pagratis_2020/mmp_config.py sha256 06cc133c320e1bb8c5156be77776c6e320ee960372e7ead0f1160a4a2af60c29
Pinned: ../replication/mankart_michaelides_pagratis_2020/mmp_model.py sha256 aa51c30d5bc933ff2491cfec74db19884a5c60a312c6d66fc91611d049112752
Pinned: ../replication/mankart_michaelides_pagratis_2020/solved_fast_small_41.npz sha256 63393d88102e995e880005d8324ffe4da16ad30cac0297a8a146a5d49d5d4bf1

Reproduce: python3 predict_mmp.py

```
solved_fast_small_41.npz sha256 63393d88102e995e880005d8324ffe4da16ad30cac0297a8a146a5d49d5d4bf1

observed, sample A small bank-quarters, 2015-2024
  F1: 139/12466 = 1.1%
  F2: 15220/17726 = 85.9%
  F3: 27/256 = 10.5%
  F4: 57/256 = 22.3%

MMP small banks, simulated (10000 banks, last 500 quarters; 0 with fewer than 8 complete years)
  F1: 1510037/4988046 = 30.3%
  F2: 2252016/4999023 = 45.0%
  F3: 0/0 = n/a
  F4: 0/0 = n/a

verdicts (agree within 10 points; F3, F4: no episodes predicted)
  F1: 30.3% vs 1.1% -> disagrees
  F2: 45.0% vs 85.9% -> disagrees
  F3: no episodes vs 256 observed -> fails
  F4: no episodes vs 256 observed -> fails
```
