# Replication of Mankart, Michaelides and Pagratis, stage 1: the data side

Run 14 October 2026 under PREREG_stage1.md (commit 384d3963). The
supplement, failures list and recession indicator were fetched by
fetch_stage1.py after that commit. Testing the failures endpoint's date
filter before the fetch showed 1,292 events for 1990-2010. Before the run,
two code fixes: the state-conditional AR(1) pairs consecutive quarters
only, and wholesale funding is written as total liabilities less deposits
(the same quantity).

## Findings

- Sample (S): the final sample has 721,945 bank-quarters (-9.7% against
  799,437) of 15,253 banks (-7.9% against 16,564), and the failures list
  has 1,292 events (exactly theirs): all three hold. Our initial count of
  commercial banks is 15.6% below theirs (750,974 against 890,252), and
  692 events match a bank report against their 810.
- Table 5, the moments their estimation targets: 20 of 22 hold. For both
  size classes the means and standard deviations of loans, deposits and
  equity over assets, profit over equity and dividends over equity
  reproduce. Differ: the small banks' failure rate (0.090% a quarter
  against 0.050%) and the large banks' standard deviation of deposits over
  assets (0.077 against 0.086).
- Table 1 (2010Q4): 14 of 16 hold. Differ: large banks' deposits (72.2%
  against 68%) and wholesale funding (18.0% against 22%).
- Table 2: 29 of 60 hold. The write-off and deposit-growth processes
  mostly reproduce for small banks (12 of 18), less for large banks, where
  only 20 banks have 35 quarters in each state; failure rates hold in all
  six cells. Every rate cell differs (18 of 18): the deposit rate (ours
  +0.14% a quarter for small banks, theirs -0.46%), the loan spread (1.41
  against 1.83) and the liquid-asset spread (-0.69 against +0.81). The
  liquid-asset return follows their data appendix's words (Fed funds
  income plus securities gains, p. 52-53), which leave out interest on
  securities; a negative spread shows their number must include it. Their
  real deposit rate implies a deflator other than ours.

Pinned: ../../common_data/fdic_q_1989.csv.gz sha256 ecb3f2bc74667751a04b2c891e73fe3e8480bf8656c694ef937cdcaa064d1f08
Pinned: ../../common_data/fdic_q_1990.csv.gz sha256 19d6120719c0d039851ddbdc26277302d0f2a134ca9d898586069698a5c606cc
Pinned: ../../common_data/fdic_q_1991.csv.gz sha256 1363b994af7a03ad75cf0715a0167b3dedf1777e947066f77bfa995cfee383d0
Pinned: ../../common_data/fdic_q_1992.csv.gz sha256 04c8233d8338024e4fbe156fa2b0fffaa1e9a62b74e555eac16924fd629d6fcc
Pinned: ../../common_data/fdic_q_1993.csv.gz sha256 83a93f8072b7c0a6473fa4d3e329291494e15219636da3938bcafb1d765e7afc
Pinned: ../../common_data/fdic_q_1994.csv.gz sha256 6b1835c13e104ce7b0d35cd44643c348bd564dced615a62372acf065f3038f70
Pinned: ../../common_data/fdic_q_1995.csv.gz sha256 c48ddb22620558e0b879a89093187d77cadb76180c24a6978dd22b0a2a9aa2e2
Pinned: ../../common_data/fdic_q_1996.csv.gz sha256 4f5e50f12920a3ae3cf60df2969527d875d7a95463e588c25c912cc3f518f234
Pinned: ../../common_data/fdic_q_1997.csv.gz sha256 01a8a43f755756e252d8313b2478f1848a7ac18f0dbedf60d01190754149eb2b
Pinned: ../../common_data/fdic_q_1998.csv.gz sha256 3cf00a0ae7a4262d228a4804046410b17599307d3ddd96d635b615c46fef9507
Pinned: ../../common_data/fdic_q_1999.csv.gz sha256 6929dd3cdb481a2e87807d5bfb374773096712482293e7edb7fab0cf99c988ed
Pinned: ../../common_data/fdic_q_2000.csv.gz sha256 70f46723779cb67615d5e8b234bcf712d776e5971e6254d07c3b33bc354f9ecd
Pinned: ../../common_data/fdic_q_2001.csv.gz sha256 72a74fd3d99ca300ac547abeb6a916670cfccbe944c35b71ca9b2e391c1ab06e
Pinned: ../../common_data/fdic_q_2002.csv.gz sha256 76c906794399256486879a8d4e02a8aa7aa1ffa9543b358468a02c9de2b9565b
Pinned: ../../common_data/fdic_q_2003.csv.gz sha256 5c849d20bf0b97acb1556b861426c025e3baace3052ae3d568bdf3c9af056c17
Pinned: ../../common_data/fdic_q_2004.csv.gz sha256 a5f8b03c9cfe953766976da4805c609a8371f55d9cbaff6e0ef17b0f626fef58
Pinned: ../../common_data/fdic_q_2005.csv.gz sha256 fd355c5d482151b976100bd8b7f537f2e68c576691acd521cf7c56c5bcbf3ec5
Pinned: ../../common_data/fdic_q_2006.csv.gz sha256 3df0436ac0fd7c71c45cc5c3df488a612fb67dd08d67db91724cf486e1f13c3a
Pinned: ../../common_data/fdic_q_2007.csv.gz sha256 fd9ee85fb95d919bc3331431d60500a44ee20fd4215e48b2337da3469fe5253a
Pinned: ../../common_data/fdic_q_2008.csv.gz sha256 9502dcbc48edabe733836e806b87d4dde98358e05c9c60ebf78a21fe133e88f5
Pinned: ../../common_data/fdic_q_2009.csv.gz sha256 b9c21601bfe3921168452c139568c10f19d0aa8c202219f7ad8ab186c8af42fd
Pinned: ../../common_data/fdic_q_2010.csv.gz sha256 429946c81ed7f5f6962c552b403843f367ef6e7d0ebab72b8cbe433c12a50f1f
Pinned: fdic_mmp_supplement_1989.csv.gz sha256 10beb557144102a96b41f0e21e7b48cc534304a6118304dead130246bbf70626
Pinned: fdic_mmp_supplement_1990.csv.gz sha256 94cf65e9e68ca5a57de080d883f155f86a2abdcfd501620f2873ff96c9991675
Pinned: fdic_mmp_supplement_1991.csv.gz sha256 598503ac7798e6a3ea30e71f80fc715331ec924138f474b7b3e2def74768030c
Pinned: fdic_mmp_supplement_1992.csv.gz sha256 03f5a47b87ca4e514f48e74749e7bd30cb6bbfb65c810eb9a3cef74c69d655fb
Pinned: fdic_mmp_supplement_1993.csv.gz sha256 874459774e3967d07c3742a8e2649d6308569723fb76d81cf71b78cab101ee43
Pinned: fdic_mmp_supplement_1994.csv.gz sha256 e54f7f163c514c6a21442174944b6bf1ee71cbad91b6e76c2bcdfbbf6c151157
Pinned: fdic_mmp_supplement_1995.csv.gz sha256 a4ef9dd962eb69b0f5287e432165b38597b8e9bd6657f67c527fb6b7c06c5875
Pinned: fdic_mmp_supplement_1996.csv.gz sha256 263e20466ee24dcc025ac4a3dc5d15ab42da99e22431c8dd0f43e38e6e4042b4
Pinned: fdic_mmp_supplement_1997.csv.gz sha256 b21bdbec83aecf33fb1add0ae6d8a3cd7d3c9799beeed0dd7db623db08aa7f4c
Pinned: fdic_mmp_supplement_1998.csv.gz sha256 a75115a8ba2173f35179175fae63cc6a03b8e1f28913365c0c9aef43d876495a
Pinned: fdic_mmp_supplement_1999.csv.gz sha256 fceb7649f5374f3673baa0d4dd09e82a7571cfc2dde42561017ea52d46cba1a2
Pinned: fdic_mmp_supplement_2000.csv.gz sha256 5c6de2977494ee8dc759b56694a5f6d993553d1038532f36f03331659df892e5
Pinned: fdic_mmp_supplement_2001.csv.gz sha256 58ea02543bb2837b3057c50a0eff7d6755081f17c966e5884a20d0fa2d85ff36
Pinned: fdic_mmp_supplement_2002.csv.gz sha256 3f512e23d13dc4284da9a9ed51f9c127fddff96846a444aea0b48a580a671487
Pinned: fdic_mmp_supplement_2003.csv.gz sha256 f27439c1e60e1f2534f546947a518ed3328072113b66cba82734947d33e7af8c
Pinned: fdic_mmp_supplement_2004.csv.gz sha256 05ee960dbf930b49284a93a6e9c7892a2d5c7102eacc738b6430d3303a9d2ccb
Pinned: fdic_mmp_supplement_2005.csv.gz sha256 89bd1f46520da29262905529f5809b3d598e472c7bca2d24a148cc2be005c489
Pinned: fdic_mmp_supplement_2006.csv.gz sha256 1e77efcfa30b4f78af02d81d8d5e90e3f8c3c7550b6e554b2c30bb3aaaaa4590
Pinned: fdic_mmp_supplement_2007.csv.gz sha256 b45173ad64647f8774568f4c804b73718a9d5413a94bfa1ebaa160fd02e7744c
Pinned: fdic_mmp_supplement_2008.csv.gz sha256 e46c712bfd4f21991b3c4889a179e5627288af3852363ab59178f84568e850b3
Pinned: fdic_mmp_supplement_2009.csv.gz sha256 948c0f9e75b80d342aa831dccdbc4ad55981091464b093b73abb39b3ecf25553
Pinned: fdic_mmp_supplement_2010.csv.gz sha256 6e5a7dec436d3e4e549e8be4b1078f3ae9d974808dc5e4cf83f8911e31bd7aa0
Pinned: fdic_failures_1990_2010.csv.gz sha256 00b2045e03f05571255f34292de479ffcdfe70e635e0eea1b70d51acec321c5a
Pinned: usrecq_1989_2011.csv sha256 12b670a6e5c290d669ff179d90ea732446dd737f7d662274efcbcf6295c2c64c
Pinned: ../gordon_hirtle_2026/cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2

Reproduce: python3 stage1.py ../../common_data/fdic_q_1989.csv.gz ../../common_data/fdic_q_1990.csv.gz ../../common_data/fdic_q_1991.csv.gz ../../common_data/fdic_q_1992.csv.gz ../../common_data/fdic_q_1993.csv.gz ../../common_data/fdic_q_1994.csv.gz ../../common_data/fdic_q_1995.csv.gz ../../common_data/fdic_q_1996.csv.gz ../../common_data/fdic_q_1997.csv.gz ../../common_data/fdic_q_1998.csv.gz ../../common_data/fdic_q_1999.csv.gz ../../common_data/fdic_q_2000.csv.gz ../../common_data/fdic_q_2001.csv.gz ../../common_data/fdic_q_2002.csv.gz ../../common_data/fdic_q_2003.csv.gz ../../common_data/fdic_q_2004.csv.gz ../../common_data/fdic_q_2005.csv.gz ../../common_data/fdic_q_2006.csv.gz ../../common_data/fdic_q_2007.csv.gz ../../common_data/fdic_q_2008.csv.gz ../../common_data/fdic_q_2009.csv.gz ../../common_data/fdic_q_2010.csv.gz fdic_mmp_supplement_1989.csv.gz fdic_mmp_supplement_1990.csv.gz fdic_mmp_supplement_1991.csv.gz fdic_mmp_supplement_1992.csv.gz fdic_mmp_supplement_1993.csv.gz fdic_mmp_supplement_1994.csv.gz fdic_mmp_supplement_1995.csv.gz fdic_mmp_supplement_1996.csv.gz fdic_mmp_supplement_1997.csv.gz fdic_mmp_supplement_1998.csv.gz fdic_mmp_supplement_1999.csv.gz fdic_mmp_supplement_2000.csv.gz fdic_mmp_supplement_2001.csv.gz fdic_mmp_supplement_2002.csv.gz fdic_mmp_supplement_2003.csv.gz fdic_mmp_supplement_2004.csv.gz fdic_mmp_supplement_2005.csv.gz fdic_mmp_supplement_2006.csv.gz fdic_mmp_supplement_2007.csv.gz fdic_mmp_supplement_2008.csv.gz fdic_mmp_supplement_2009.csv.gz fdic_mmp_supplement_2010.csv.gz fdic_failures_1990_2010.csv.gz usrecq_1989_2011.csv ../gordon_hirtle_2026/cpiaucsl_1989_2024.csv.gz

```
fdic_q_1989.csv.gz sha256 ecb3f2bc74667751a04b2c891e73fe3e8480bf8656c694ef937cdcaa064d1f08
fdic_q_1990.csv.gz sha256 19d6120719c0d039851ddbdc26277302d0f2a134ca9d898586069698a5c606cc
fdic_q_1991.csv.gz sha256 1363b994af7a03ad75cf0715a0167b3dedf1777e947066f77bfa995cfee383d0
fdic_q_1992.csv.gz sha256 04c8233d8338024e4fbe156fa2b0fffaa1e9a62b74e555eac16924fd629d6fcc
fdic_q_1993.csv.gz sha256 83a93f8072b7c0a6473fa4d3e329291494e15219636da3938bcafb1d765e7afc
fdic_q_1994.csv.gz sha256 6b1835c13e104ce7b0d35cd44643c348bd564dced615a62372acf065f3038f70
fdic_q_1995.csv.gz sha256 c48ddb22620558e0b879a89093187d77cadb76180c24a6978dd22b0a2a9aa2e2
fdic_q_1996.csv.gz sha256 4f5e50f12920a3ae3cf60df2969527d875d7a95463e588c25c912cc3f518f234
fdic_q_1997.csv.gz sha256 01a8a43f755756e252d8313b2478f1848a7ac18f0dbedf60d01190754149eb2b
fdic_q_1998.csv.gz sha256 3cf00a0ae7a4262d228a4804046410b17599307d3ddd96d635b615c46fef9507
fdic_q_1999.csv.gz sha256 6929dd3cdb481a2e87807d5bfb374773096712482293e7edb7fab0cf99c988ed
fdic_q_2000.csv.gz sha256 70f46723779cb67615d5e8b234bcf712d776e5971e6254d07c3b33bc354f9ecd
fdic_q_2001.csv.gz sha256 72a74fd3d99ca300ac547abeb6a916670cfccbe944c35b71ca9b2e391c1ab06e
fdic_q_2002.csv.gz sha256 76c906794399256486879a8d4e02a8aa7aa1ffa9543b358468a02c9de2b9565b
fdic_q_2003.csv.gz sha256 5c849d20bf0b97acb1556b861426c025e3baace3052ae3d568bdf3c9af056c17
fdic_q_2004.csv.gz sha256 a5f8b03c9cfe953766976da4805c609a8371f55d9cbaff6e0ef17b0f626fef58
fdic_q_2005.csv.gz sha256 fd355c5d482151b976100bd8b7f537f2e68c576691acd521cf7c56c5bcbf3ec5
fdic_q_2006.csv.gz sha256 3df0436ac0fd7c71c45cc5c3df488a612fb67dd08d67db91724cf486e1f13c3a
fdic_q_2007.csv.gz sha256 fd9ee85fb95d919bc3331431d60500a44ee20fd4215e48b2337da3469fe5253a
fdic_q_2008.csv.gz sha256 9502dcbc48edabe733836e806b87d4dde98358e05c9c60ebf78a21fe133e88f5
fdic_q_2009.csv.gz sha256 b9c21601bfe3921168452c139568c10f19d0aa8c202219f7ad8ab186c8af42fd
fdic_q_2010.csv.gz sha256 429946c81ed7f5f6962c552b403843f367ef6e7d0ebab72b8cbe433c12a50f1f
fdic_mmp_supplement_1989.csv.gz sha256 10beb557144102a96b41f0e21e7b48cc534304a6118304dead130246bbf70626
fdic_mmp_supplement_1990.csv.gz sha256 94cf65e9e68ca5a57de080d883f155f86a2abdcfd501620f2873ff96c9991675
fdic_mmp_supplement_1991.csv.gz sha256 598503ac7798e6a3ea30e71f80fc715331ec924138f474b7b3e2def74768030c
fdic_mmp_supplement_1992.csv.gz sha256 03f5a47b87ca4e514f48e74749e7bd30cb6bbfb65c810eb9a3cef74c69d655fb
fdic_mmp_supplement_1993.csv.gz sha256 874459774e3967d07c3742a8e2649d6308569723fb76d81cf71b78cab101ee43
fdic_mmp_supplement_1994.csv.gz sha256 e54f7f163c514c6a21442174944b6bf1ee71cbad91b6e76c2bcdfbbf6c151157
fdic_mmp_supplement_1995.csv.gz sha256 a4ef9dd962eb69b0f5287e432165b38597b8e9bd6657f67c527fb6b7c06c5875
fdic_mmp_supplement_1996.csv.gz sha256 263e20466ee24dcc025ac4a3dc5d15ab42da99e22431c8dd0f43e38e6e4042b4
fdic_mmp_supplement_1997.csv.gz sha256 b21bdbec83aecf33fb1add0ae6d8a3cd7d3c9799beeed0dd7db623db08aa7f4c
fdic_mmp_supplement_1998.csv.gz sha256 a75115a8ba2173f35179175fae63cc6a03b8e1f28913365c0c9aef43d876495a
fdic_mmp_supplement_1999.csv.gz sha256 fceb7649f5374f3673baa0d4dd09e82a7571cfc2dde42561017ea52d46cba1a2
fdic_mmp_supplement_2000.csv.gz sha256 5c6de2977494ee8dc759b56694a5f6d993553d1038532f36f03331659df892e5
fdic_mmp_supplement_2001.csv.gz sha256 58ea02543bb2837b3057c50a0eff7d6755081f17c966e5884a20d0fa2d85ff36
fdic_mmp_supplement_2002.csv.gz sha256 3f512e23d13dc4284da9a9ed51f9c127fddff96846a444aea0b48a580a671487
fdic_mmp_supplement_2003.csv.gz sha256 f27439c1e60e1f2534f546947a518ed3328072113b66cba82734947d33e7af8c
fdic_mmp_supplement_2004.csv.gz sha256 05ee960dbf930b49284a93a6e9c7892a2d5c7102eacc738b6430d3303a9d2ccb
fdic_mmp_supplement_2005.csv.gz sha256 89bd1f46520da29262905529f5809b3d598e472c7bca2d24a148cc2be005c489
fdic_mmp_supplement_2006.csv.gz sha256 1e77efcfa30b4f78af02d81d8d5e90e3f8c3c7550b6e554b2c30bb3aaaaa4590
fdic_mmp_supplement_2007.csv.gz sha256 b45173ad64647f8774568f4c804b73718a9d5413a94bfa1ebaa160fd02e7744c
fdic_mmp_supplement_2008.csv.gz sha256 e46c712bfd4f21991b3c4889a179e5627288af3852363ab59178f84568e850b3
fdic_mmp_supplement_2009.csv.gz sha256 948c0f9e75b80d342aa831dccdbc4ad55981091464b093b73abb39b3ecf25553
fdic_mmp_supplement_2010.csv.gz sha256 6e5a7dec436d3e4e549e8be4b1078f3ae9d974808dc5e4cf83f8911e31bd7aa0
fdic_failures_1990_2010.csv.gz sha256 00b2045e03f05571255f34292de479ffcdfe70e635e0eea1b70d51acec321c5a
usrecq_1989_2011.csv sha256 12b670a6e5c290d669ff179d90ea732446dd737f7d662274efcbcf6295c2c64c
cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2

initial: bank-quarters 750974, banks 15425 [890,252; 17,226]
final: bank-quarters 721945, banks 15253 [799,437; 16,564]
failure events 1292 [1,292], matched to a report 692 [810]

===== small: bank-quarters 686333, banks 14613, banks with 35+35 quarters 3328
Table 2 (ours / theirs; uncond, recession, expansion)
  w mean            0.11/ 0.10 holds     0.14/ 0.13 holds     0.09/ 0.08 holds
  w AR(1)           0.15/ 0.21 differs     0.12/ 0.20 differs     0.07/ 0.14 differs
  w s.d.            0.18/ 0.17 holds     0.21/ 0.20 holds     0.13/ 0.10 holds
  g mean            0.71/ 0.81 holds     0.82/ 0.65 differs     0.63/ 0.90 differs
  g AR(1)          -0.04/-0.01 holds    -0.03/-0.01 holds    -0.08/-0.01 differs
  g s.d.            3.57/ 3.71 holds     3.64/ 3.50 holds     3.43/ 3.48 holds
  deposit rate      0.14/-0.46 differs     0.21/-0.34 differs     0.10/-0.51 differs
  loan spread       1.41/ 1.83 differs     1.37/ 1.72 differs     1.45/ 1.87 differs
  liquid spread    -0.69/ 0.81 differs    -0.70/ 0.42 differs    -0.67/ 0.92 differs
  failure rate      0.09/ 0.05 holds     0.18/ 0.17 holds     0.02/ 0.01 holds
Table 5 data moments (banks with 20+20 quarters: 6393)
  failure rate           0.090 [0.05] -> differs
  loans/assets           0.587 [0.622] -> holds
  deposits/assets        0.857 [0.857] -> holds
  equity/assets          0.100 [0.099] -> holds
  profit/equity          0.035 [0.037] -> holds
  dividends/equity       0.014 [0.013] -> holds
  sd loans/assets        0.084 [0.082] -> holds
  sd deposits/assets     0.035 [0.035] -> holds
  sd equity/assets       0.015 [0.014] -> holds
  sd profit/equity       0.027 [0.024] -> holds
  sd dividends/equity    0.019 [0.016] -> holds

===== large: bank-quarters 14005, banks 523, banks with 35+35 quarters 20
Table 2 (ours / theirs; uncond, recession, expansion)
  w mean            0.18/ 0.31 differs     0.27/ 0.37 differs     0.12/ 0.22 differs
  w AR(1)           0.66/ 0.72 holds     0.60/ 0.70 holds     0.38/ 0.51 differs
  w s.d.            0.19/ 0.24 differs     0.23/ 0.28 holds     0.08/ 0.14 differs
  g mean            1.32/ 1.63 holds     1.02/ 1.60 differs     1.56/ 1.64 holds
  g AR(1)           0.06/ 0.03 holds     0.07/ 0.03 holds    -0.01/ 0.03 holds
  g s.d.            4.79/ 5.64 holds     4.80/ 5.31 holds     4.63/ 5.41 holds
  deposit rate      0.01/-0.55 differs     0.04/-0.48 differs    -0.02/-0.58 differs
  loan spread       1.23/ 1.84 differs     1.16/ 1.68 differs     1.27/ 1.89 differs
  liquid spread    -0.58/ 0.96 differs    -0.58/ 0.56 differs    -0.58/ 1.07 differs
  failure rate      0.09/ 0.08 holds     0.19/ 0.18 holds     0.01/ 0.01 holds
Table 5 data moments (banks with 20+20 quarters: 87)
  failure rate           0.086 [0.084] -> holds
  loans/assets           0.620 [0.665] -> holds
  deposits/assets        0.685 [0.633] -> holds
  equity/assets          0.073 [0.072] -> holds
  profit/equity          0.064 [0.063] -> holds
  dividends/equity       0.029 [0.028] -> holds
  sd loans/assets        0.073 [0.076] -> holds
  sd deposits/assets     0.077 [0.086] -> differs
  sd equity/assets       0.014 [0.013] -> holds
  sd profit/equity       0.047 [0.048] -> holds
  sd dividends/equity    0.036 [0.034] -> holds

===== Table 1, 2010Q4 (mean of bank shares of tangible assets, %; theirs in brackets)
  small: banks 6204 [6528] -> holds
    cash                      9.1 [9] -> holds
    securities               21.4 [21] -> holds
    loans                    61.4 [62] -> holds
    deposits                 84.6 [85] -> holds
    transaction deposits     22.3 [22] -> holds
    wholesale funding         4.9 [5] -> holds
    tangible equity          10.4 [10] -> holds
  large: banks 131 [137] -> holds
    cash                      7.2 [7] -> holds
    securities               21.1 [20] -> holds
    loans                    62.2 [61] -> holds
    deposits                 72.2 [68] -> differs
    transaction deposits      7.4 [7] -> holds
    wholesale funding        18.0 [22] -> differs
    tangible equity           9.8 [10] -> holds

SUMMARY: Table 1 14/16, Table 2 29/60, Table 5 20/22 hold
```
