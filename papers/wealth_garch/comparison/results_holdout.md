# Enhancement thread: the 2025 hold-out

Run 25 October 2026,
under PREREG_comparison_20.md (commit 6aecfb5e),
with the 2025 file fetched under PREREG_holdout_2025.md (commits f348b46e
and 8f723a9a) and the test script committed before the fetch (ffe74ea8)
and run unchanged. The first and only look at 2025.

## Findings

Sample B banks with a 2024Q4 buffer and 2015-2024 inputs: 42 of 61 (9
without 2015-2024 parameters, 9 without a 2024Q4 buffer, 1 with fewer
than 8 yearly changes; the count was known from a dry run on 2014-2024
data before the fetch). Parameters from 2015-2024 only; 2025Q1 to 2025Q4
from the 2024Q4 buffer; the frozen model (P) with the floor at 10.5%.

| 2025, sample B (42 banks) | F1 | F2 | Miss, F1 + F2 (points) |
|---|---|---|---|
| observed | 1.6% (2 of 128) | 89.1% (147 of 165) | |
| standard error of the observed share | 1.1 | 2.4 | |
| frozen model (P) | 0.2% | 67.1% | 1.3 + 22.0 = 23.3 |
| baseline (Peura and Keppo) | 94.8% | 0.0% | 93.3 + 89.1 = 182.4 |

- Rule: (P) beats the baseline on the hold-out, 23.3 points against
  182.4. The enhancement thread's claim holds on data that played no part
  in any choice.
- By the 10-point rule (P) agrees on F1 (0.2% against 1.6%) and not on F2:
  it predicts 67.1% of bank-quarters far above R against 89.1% observed,
  22.0 points low, about nine standard errors. The baseline agrees on
  neither.
- The sign of the F2 miss has turned: on 2015-2024 (P) was 9.1 points
  high in sample B (results_frozen.md); on 2025 it is 22.0 points low.
  The 2025 banks are better capitalised relative to their recent risk:
  the median 2024Q4 buffer over R is 3.44 points against a median sigma_i
  of 0.49 points (2015-2024), so 4 sigma_i is about 2 points and most
  observed bank-quarters clear it.
- Sample A (reported): 291 banks; observed F1 1.4%, F2 86.9%; (P) 1.7% and
  68.8% (miss 18.4); baseline 179.8.

## Reading

On a year it never saw, the frozen enhanced model again beats the
standard model by a wide margin and gets the dividend fact right; it
under-predicts how many bank-quarters sit far above R by about a quarter
(67.1% against 89.1%). The
hold-out confirms the comparison with the baseline, not the model's fit
to the level of buffers.

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
Pinned: ../common_data/holdout/fdic_q_2025.csv.gz sha256 fa762fef47f02d325f283271386f3c862b41629b6afb1a8d9cd3d5c92640bb77
Pinned: holdout.py sha256 a13918f96840b310018b928b744bf5755ee3b004451bbb987aae5c120f2c0b49
Pinned: sample.py sha256 857918747fc9ec718cec3efad39fe5ee5dff430528b6b77e83ba760d14a33755
Pinned: window.py sha256 69af27363f12132b2074ec1514fa68f6aa801590411278137fce168362b51340
Pinned: judged_risk.py sha256 b7a37555e77041b1e5c7117cdcf1a03900ef2399435e44db8165dd9e156d1d78
Pinned: predict_pk.py sha256 0c957c45b9cb6a501a516d4168f37983c5b380c102c760657c181b98ffe4d962
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783

Reproduce: python3 holdout.py

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
fdic_q_2025.csv.gz sha256 fa762fef47f02d325f283271386f3c862b41629b6afb1a8d9cd3d5c92640bb77

===== sample B: 42 of 61 banks; left out: {'no 2015-2024 parameters': 9, 'no 2024Q4 buffer': 9, 'fewer than 8 yearly changes': 1}
  medians, 2015-2024: sigma_i 0.49 points, sigma_D 0.99 points, 2024Q4 buffer over R 3.44 points
observed, 2025
  F1: 2/128 = 1.6%
  F2: 147/165 = 89.1%
  standard errors of the observed shares: F1 1.1, F2 2.4 points
(P) frozen model, floor 10.5%: 0 start at or below the floor
  F1: 51/21150 = 0.2%
  F2: 22192/33060 = 67.1%
baseline (Peura and Keppo), minimum 8%
  F1: 28831/30402 = 94.8%
  F2: 0/33595 = 0.0%
  (P): |dF1| 1.3 + |dF2| 22.0 = 23.3 points; F1 within 10: True; F2 within 10: False
  baseline: |dF1| 93.3 + |dF2| 89.1 = 182.4 points; F1 within 10: False; F2 within 10: False
  rule (sample B): (P) beats the baseline (23.3 against 182.4 points)

===== sample A: 291 of 668 banks; left out: {'no 2024Q4 buffer': 305, 'no 2015-2024 parameters': 59, 'fewer than 8 yearly changes': 13}
  medians, 2015-2024: sigma_i 0.53 points, sigma_D 1.08 points, 2024Q4 buffer over R 4.74 points
observed, 2025
  F1: 12/862 = 1.4%
  F2: 978/1126 = 86.9%
  standard errors of the observed shares: F1 0.4, F2 1.0 points
(P) frozen model, floor 10.5%: 0 start at or below the floor
  F1: 2479/146077 = 1.7%
  F2: 158563/230615 = 68.8%
baseline (Peura and Keppo), minimum 8%
  F1: 199549/211461 = 94.4%
  F2: 0/232787 = 0.0%
  (P): |dF1| 0.3 + |dF2| 18.1 = 18.4 points; F1 within 10: True; F2 within 10: False
  baseline: |dF1| 93.0 + |dF2| 86.9 = 179.8 points; F1 within 10: False; F2 within 10: False
```
