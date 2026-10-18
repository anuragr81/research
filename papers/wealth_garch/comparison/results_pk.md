# Testing thread: the baseline (Peura and Keppo) on the four facts

Run 18 October 2026,
under PREREG_comparison.md (commit 26f44a15)
and under PREREG_comparison_2.md (commit d8cc561a),
with sample.py of commit cba68cdd. Observed shares from results_facts.md.

## Findings

Predicted against observed share, difference in percentage points;
agrees if within 10 points.

| fact | sample B (primary) | sample A |
|---|---|---|
| F1 | 90.0% vs 4.8% (+85.2): disagrees | 93.9% vs 1.6% (+92.3): disagrees |
| F2 | 0.0% vs 61.6% (-61.6): disagrees | 0.0% vs 83.2% (-83.2): disagrees |
| F3 | 70.8% vs 15.2% (+55.6): disagrees | 81.1% vs 11.0% (+70.1): disagrees |
| F4 | 81.9% vs 30.4% (+51.5): disagrees | 68.1% vs 22.9% (+45.2): disagrees |

- The baseline agrees on none of the four facts in either sample.
- In the model banks pay dividends from a thin buffer
  (90.0% and 93.9% of dividend quarters) and never hold capital above
  4 sigma_i over R, while real banks rarely pay from a thin buffer (4.8%,
  1.6%) and mostly hold capital far above R (61.6%, 83.2%).
- In the model most recapitalisations start thin and land between 1 and 4
  sigma_i; in the data most do neither.
- No bank in either sample starts at or below the model's 8% minimum.
- These numbers differ from Peura and Keppo stage 4, which measured
  relative to each bank's own barriers and used 2015-2024 parameters.

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
Pinned: predict_pk.py sha256 0c957c45b9cb6a501a516d4168f37983c5b380c102c760657c181b98ffe4d962
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783

Reproduce: python3 predict_pk.py

```

sample A: 668 banks, 320 issuer, 0 starting at or below 8%; 200 paths each
  F1: 4162483/4431876 = 93.9%
  F2: 0/5253099 = 0.0%
  F3: 11886/14648 = 81.1%
  F4: 9978/14648 = 68.1%

sample B: 61 banks, 28 issuer, 0 starting at or below 8%; 200 paths each
  F1: 331652/368608 = 90.0%
  F2: 0/457526 = 0.0%
  F3: 1834/2592 = 70.8%
  F4: 2123/2592 = 81.9%
```
