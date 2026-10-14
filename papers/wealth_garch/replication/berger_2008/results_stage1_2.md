# Berger et al. replication, stage 1: sensitivity

Run 14 October 2026 under PREREG_stage1_2.md (commits eec93522 and
83f73797, the second correcting a citation before any run). gmm.py's
collapse option was validated against pydynpd before this run
(results_validation.md). stage1.py gained parameters with stage 1 as the
default; results_stage1.md still reproduces.

## Findings

- None of the eight runs has the Step 1 speed within 0.10 of theirs for
  two or more measures (0 of 8; the pre-registered threshold was 4). By
  the pre-registered reading, the speed gap is due to the data, not to our
  instrument set or variable definitions.
- Under system GMM (I1 to I3, both data versions) the Step 1 speed lies
  between 0.145 and 0.261, against their 0.486 to 0.602.
  Difference GMM (I4) gives speeds near zero or negative and Step 2 speeds
  near zero; it identifies little here.
- D2's definitions bring the variable means close to theirs: RETAILDEP
  0.592 (theirs 0.605), BUSINESSLOAN 0.329 (0.281). STDROA annualised is
  0.0028 (0.0034). NMERGER counting bank predecessors only is 0.007,
  below their 0.066, where counting all predecessors (D1) was 0.454 above
  it.
- With D2 under system GMM the Step 2 mean speeds lie between
  0.244 and 0.353, against their 0.279 to 0.411.
- Step 3 mean targets range from -13.822 to 0.998 across
  the system-GMM runs: the target level is not pinned down in any variant,
  because the Step 1 targets divide by a small speed.
- Stage 1's verdicts stand (results_stage1.md). What reproduces in every
  variant is the mean capital ratios; what does not, in any, is the
  constant adjustment speed of Step 1.

Pinned: ../gordon_hirtle_2026/cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
Pinned: ../gordon_hirtle_2026/CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
Pinned: crsp_20240930v2.csv.gz sha256 eba7ab822c86d311cb87afaa23de49e5978dc9767e1e32d4e986d25e3b14cb06
Pinned: CSV_TRANSFORMATIONS.zip sha256 41fd50d3ebc1f6e8163bd671d4e67259f9ab0f448c979a714091f4f1a4263003
Pinned: ../gordon_hirtle_2026/bhcf199412.csv.gz sha256 2a4a5dcbc1e393e2a64ae4d14371390648066ed4edebbc8433224b7e9e97c001
Pinned: ../gordon_hirtle_2026/bhcf199512.csv.gz sha256 4d15116261e2c44508b42724e3aa2279f31fc924352ad33a5e8dd578c05ae725
Pinned: ../gordon_hirtle_2026/bhcf199612.csv.gz sha256 471096310c58aa25cd5542e1bffbe57f40917550ab3d2e90ed790c7faebb5e66
Pinned: ../gordon_hirtle_2026/bhcf199712.csv.gz sha256 32f02d770629744734caf6218e34855f2c360fa41b1689ba4288851733dbf947
Pinned: ../gordon_hirtle_2026/bhcf199812.csv.gz sha256 ee7597b443174ae463f7e14572690fb03835427dbaaf972a376db5ca14ae6ab6
Pinned: ../gordon_hirtle_2026/bhcf199912.csv.gz sha256 8016aee5af3e1568b500b58f94a7da7add972673229d3a020d7450bb463d471b
Pinned: ../gordon_hirtle_2026/bhcf200012.csv.gz sha256 fa4910ec7110dcd0750a0bfa162044b33be0f020ae81d5335e177887153fd739
Pinned: ../gordon_hirtle_2026/bhcf200112.csv.gz sha256 9bdf1286d7633fc667ff05a189995afe29b0547d9189d8dfb184cd74e29d0b56
Pinned: ../gordon_hirtle_2026/bhcf200212.csv.gz sha256 cf366d780af44bb95493cb4ddef6a9892b07f1131bc061215e614d9c5d902af5
Pinned: ../gordon_hirtle_2026/bhcf200312.csv.gz sha256 71c709ebdf7011e9faa846243d188e6ffb1da1efa95e81d1b01614ddfc66ce93
Pinned: ../gordon_hirtle_2026/bhcf200412.csv.gz sha256 69fa855ca01cfc152bdbebc3f331e21e84ca377215fc3fb04a9de6d940de172f
Pinned: ../gordon_hirtle_2026/bhcf200512.csv.gz sha256 2092029e1833908b54c7993adbf974784e3fd74a8206c211177749ef998eb723
Pinned: ../gordon_hirtle_2026/bhcf200612.csv.gz sha256 ae04ae5ed0eea08f7126d7918c12fd8028c2e6eec501765d031572953a60299c
Pinned: bhcf199403.csv.gz sha256 0f3992668ff15814dc90f3629e42bae7e8f147c41a93c4e0fcca87e3b875342c
Pinned: bhcf199406.csv.gz sha256 67055ad8aa6ba3f341901f61896a923c857d518ba9ecce0064767fb4db8fef79
Pinned: bhcf199409.csv.gz sha256 574bb5a449a8205944675f8932757583cc46243ce1e8584cc4440d6b30bb4704
Pinned: bhcf199503.csv.gz sha256 4c09365d6ed5609a9aa5e0a452dd7818ba01af0705c56c87e79643e11da74d76
Pinned: bhcf199506.csv.gz sha256 4a4c51c2d2f3b0197527473e2590164c9072663c42e48b008305ebc35c205205
Pinned: bhcf199509.csv.gz sha256 a7fe266d9a25f8701dbd1d65d0eca7097b56670f0f5b5dca8679d8f5e35605a3
Pinned: bhcf199603.csv.gz sha256 6b8180589aeb4846855e3978060e57d3db0b1c226e71428ae0ab41335777ece5
Pinned: bhcf199606.csv.gz sha256 df3e811adfc5b3b1a819ee6c915509fc987c5fe5f947d186471a2c7323cc52bc
Pinned: bhcf199609.csv.gz sha256 e0d4618f3085fde63ec6e5ed0bccf844ed3168dce0be4052f174200d4b80c1ac
Pinned: bhcf199703.csv.gz sha256 c5d636a84daed8aa05bc22a5f67b5c1088c31ae29e57ebff7e61f6a4413f3f5a
Pinned: bhcf199706.csv.gz sha256 de65ce80fc67a732eedde080905eb11c53b2da683be544d9b1f3b390a0c49582
Pinned: bhcf199709.csv.gz sha256 899d7f410fce0bae7aed02dd366aa51bba3305401210e94918bbb63c8ef7810a
Pinned: bhcf199803.csv.gz sha256 25ac127d6e363b9ddeb4a72623d6b96bb6187dbba3268418acb47812766e7e2f
Pinned: bhcf199806.csv.gz sha256 bc637d78f2c724734a01585eb8fdc85afcb826263c6d7b6f279a04e92118b7a0
Pinned: bhcf199809.csv.gz sha256 1fa384b17ade01fc33805ff052f128b832bc962eeba31744adb4d4852bfca312
Pinned: bhcf199903.csv.gz sha256 84a1c168459994936219c9eca7ffd641bad7c9abb18e72d83d55fd4c3eebcb51
Pinned: bhcf199906.csv.gz sha256 508c80c2de5d25f1597e0936a99154c163299138f023117cf78bd8b356d09e56
Pinned: bhcf199909.csv.gz sha256 522e60540b24a113a5ed79490d3cec66b6a8108ef3a4b2229328f40c9da12e20
Pinned: bhcf200003.csv.gz sha256 0b9b0d83ffdcfd2563fc8985e525ae562c09455ce7ab8432ef89ace0ef610e82
Pinned: bhcf200006.csv.gz sha256 efe49909941f00ac9678d8e07a2c4503f4fadf4691dfb767536a1d267aa9b6cc
Pinned: bhcf200009.csv.gz sha256 5251789d4c5103d27d62bc517fee13b807811bb048611cf37185015b1e0bf1fa
Pinned: bhcf200103.csv.gz sha256 3dbc5afcf3ec47e066a594644b46fef8581a2a2eea6bda5a293e2df9ffb727cb
Pinned: bhcf200106.csv.gz sha256 3c73f862404fb69607be4278a2f38a9c3d2223b9e42c207573a894ed1a95060c
Pinned: bhcf200109.csv.gz sha256 5dbc6cede269528800e257da1ba9910b638372719b2d88387609e85d31f3d1c7
Pinned: bhcf200203.csv.gz sha256 624ef2deecfe37ccd042e3eb6c43cc7cc1bdc788e6f0dc4398e11279ea9b7a2a
Pinned: bhcf200206.csv.gz sha256 8616eff70ca589cdd8fbaf9fbc82e40d30313272a48735c6fc7af9ad26d1cd6b
Pinned: bhcf200209.csv.gz sha256 8fe5c87583c130b2091130a6a2cacff4178f5a6141509a2cc27e108dd5c39ad8
Pinned: bhcf200303.csv.gz sha256 350ed227045a16255f4c8ab133a42a86915ab8bac0c50326ce5e8b71198e7745
Pinned: bhcf200306.csv.gz sha256 232283656d7037945d5a4843c76e467f92b16a0e16c8a0aa8a6c5a28c51f4ce7
Pinned: bhcf200309.csv.gz sha256 483228548858cfc2fed4f3a07f20a014fe65cf206efef9fbc2110f7195e72c26
Pinned: bhcf200403.csv.gz sha256 1dcb8bdbdfedcc9a93fef37c91aa7fb848484ea6fbc8b2e4f74e3ab9b42cd434
Pinned: bhcf200406.csv.gz sha256 e06cf5fd5d3ccd0e243ca6675022064217f37f1a21e12a8968f76b041a80f510
Pinned: bhcf200409.csv.gz sha256 5fde8d29b2288bba17961c4793f3c901d4d9f067ba40e2a2439a2ca958c6e960
Pinned: bhcf200503.csv.gz sha256 b67014164c80e14212b027190bbf86c20b76e474fd9b6e0dba269cb072d1b863
Pinned: bhcf200506.csv.gz sha256 fb7b191b43b0d337eb31bacd72baf84af67bb6d4c2b1f38839cb81a9ee5fd252
Pinned: bhcf200509.csv.gz sha256 2053c62cb9265bf4cf3cad90ab6a7a770f732f9aa7b9011283671c432eacf32f
Pinned: bhcf200603.csv.gz sha256 2a9b77a4a19e7786db28cebd15d19e7ac55ce3f2da2058eaf4fa4dfd655301b6
Pinned: bhcf200606.csv.gz sha256 d8495711a48af90dc5aaaead840cb30a241d18be868c6b79fc473a8fea059f32
Pinned: bhcf200609.csv.gz sha256 d556fafa30539bec2dd874a0ee093ae1b0e57646762d2e5b03762c8916e870b8
Pinned: ../gordon_hirtle_2026/CSV_ATTRIBUTES_ACTIVE.zip sha256 a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4
Pinned: ../gordon_hirtle_2026/CSV_ATTRIBUTES_CLOSED.zip sha256 adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c

Reproduce: python3 stage1_2.py ../gordon_hirtle_2026/cpiaucsl_1989_2024.csv.gz ../gordon_hirtle_2026/CSV_RELATIONSHIPS.zip crsp_20240930v2.csv.gz CSV_TRANSFORMATIONS.zip ../gordon_hirtle_2026/bhcf199412.csv.gz ../gordon_hirtle_2026/bhcf199512.csv.gz ../gordon_hirtle_2026/bhcf199612.csv.gz ../gordon_hirtle_2026/bhcf199712.csv.gz ../gordon_hirtle_2026/bhcf199812.csv.gz ../gordon_hirtle_2026/bhcf199912.csv.gz ../gordon_hirtle_2026/bhcf200012.csv.gz ../gordon_hirtle_2026/bhcf200112.csv.gz ../gordon_hirtle_2026/bhcf200212.csv.gz ../gordon_hirtle_2026/bhcf200312.csv.gz ../gordon_hirtle_2026/bhcf200412.csv.gz ../gordon_hirtle_2026/bhcf200512.csv.gz ../gordon_hirtle_2026/bhcf200612.csv.gz bhcf199403.csv.gz bhcf199406.csv.gz bhcf199409.csv.gz bhcf199503.csv.gz bhcf199506.csv.gz bhcf199509.csv.gz bhcf199603.csv.gz bhcf199606.csv.gz bhcf199609.csv.gz bhcf199703.csv.gz bhcf199706.csv.gz bhcf199709.csv.gz bhcf199803.csv.gz bhcf199806.csv.gz bhcf199809.csv.gz bhcf199903.csv.gz bhcf199906.csv.gz bhcf199909.csv.gz bhcf200003.csv.gz bhcf200006.csv.gz bhcf200009.csv.gz bhcf200103.csv.gz bhcf200106.csv.gz bhcf200109.csv.gz bhcf200203.csv.gz bhcf200206.csv.gz bhcf200209.csv.gz bhcf200303.csv.gz bhcf200306.csv.gz bhcf200309.csv.gz bhcf200403.csv.gz bhcf200406.csv.gz bhcf200409.csv.gz bhcf200503.csv.gz bhcf200506.csv.gz bhcf200509.csv.gz bhcf200603.csv.gz bhcf200606.csv.gz bhcf200609.csv.gz ../gordon_hirtle_2026/CSV_ATTRIBUTES_ACTIVE.zip ../gordon_hirtle_2026/CSV_ATTRIBUTES_CLOSED.zip

```
cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
crsp_20240930v2.csv.gz sha256 eba7ab822c86d311cb87afaa23de49e5978dc9767e1e32d4e986d25e3b14cb06
CSV_TRANSFORMATIONS.zip sha256 41fd50d3ebc1f6e8163bd671d4e67259f9ab0f448c979a714091f4f1a4263003
bhcf199412.csv.gz sha256 2a4a5dcbc1e393e2a64ae4d14371390648066ed4edebbc8433224b7e9e97c001
bhcf199512.csv.gz sha256 4d15116261e2c44508b42724e3aa2279f31fc924352ad33a5e8dd578c05ae725
bhcf199612.csv.gz sha256 471096310c58aa25cd5542e1bffbe57f40917550ab3d2e90ed790c7faebb5e66
bhcf199712.csv.gz sha256 32f02d770629744734caf6218e34855f2c360fa41b1689ba4288851733dbf947
bhcf199812.csv.gz sha256 ee7597b443174ae463f7e14572690fb03835427dbaaf972a376db5ca14ae6ab6
bhcf199912.csv.gz sha256 8016aee5af3e1568b500b58f94a7da7add972673229d3a020d7450bb463d471b
bhcf200012.csv.gz sha256 fa4910ec7110dcd0750a0bfa162044b33be0f020ae81d5335e177887153fd739
bhcf200112.csv.gz sha256 9bdf1286d7633fc667ff05a189995afe29b0547d9189d8dfb184cd74e29d0b56
bhcf200212.csv.gz sha256 cf366d780af44bb95493cb4ddef6a9892b07f1131bc061215e614d9c5d902af5
bhcf200312.csv.gz sha256 71c709ebdf7011e9faa846243d188e6ffb1da1efa95e81d1b01614ddfc66ce93
bhcf200412.csv.gz sha256 69fa855ca01cfc152bdbebc3f331e21e84ca377215fc3fb04a9de6d940de172f
bhcf200512.csv.gz sha256 2092029e1833908b54c7993adbf974784e3fd74a8206c211177749ef998eb723
bhcf200612.csv.gz sha256 ae04ae5ed0eea08f7126d7918c12fd8028c2e6eec501765d031572953a60299c
bhcf199403.csv.gz sha256 0f3992668ff15814dc90f3629e42bae7e8f147c41a93c4e0fcca87e3b875342c
bhcf199406.csv.gz sha256 67055ad8aa6ba3f341901f61896a923c857d518ba9ecce0064767fb4db8fef79
bhcf199409.csv.gz sha256 574bb5a449a8205944675f8932757583cc46243ce1e8584cc4440d6b30bb4704
bhcf199503.csv.gz sha256 4c09365d6ed5609a9aa5e0a452dd7818ba01af0705c56c87e79643e11da74d76
bhcf199506.csv.gz sha256 4a4c51c2d2f3b0197527473e2590164c9072663c42e48b008305ebc35c205205
bhcf199509.csv.gz sha256 a7fe266d9a25f8701dbd1d65d0eca7097b56670f0f5b5dca8679d8f5e35605a3
bhcf199603.csv.gz sha256 6b8180589aeb4846855e3978060e57d3db0b1c226e71428ae0ab41335777ece5
bhcf199606.csv.gz sha256 df3e811adfc5b3b1a819ee6c915509fc987c5fe5f947d186471a2c7323cc52bc
bhcf199609.csv.gz sha256 e0d4618f3085fde63ec6e5ed0bccf844ed3168dce0be4052f174200d4b80c1ac
bhcf199703.csv.gz sha256 c5d636a84daed8aa05bc22a5f67b5c1088c31ae29e57ebff7e61f6a4413f3f5a
bhcf199706.csv.gz sha256 de65ce80fc67a732eedde080905eb11c53b2da683be544d9b1f3b390a0c49582
bhcf199709.csv.gz sha256 899d7f410fce0bae7aed02dd366aa51bba3305401210e94918bbb63c8ef7810a
bhcf199803.csv.gz sha256 25ac127d6e363b9ddeb4a72623d6b96bb6187dbba3268418acb47812766e7e2f
bhcf199806.csv.gz sha256 bc637d78f2c724734a01585eb8fdc85afcb826263c6d7b6f279a04e92118b7a0
bhcf199809.csv.gz sha256 1fa384b17ade01fc33805ff052f128b832bc962eeba31744adb4d4852bfca312
bhcf199903.csv.gz sha256 84a1c168459994936219c9eca7ffd641bad7c9abb18e72d83d55fd4c3eebcb51
bhcf199906.csv.gz sha256 508c80c2de5d25f1597e0936a99154c163299138f023117cf78bd8b356d09e56
bhcf199909.csv.gz sha256 522e60540b24a113a5ed79490d3cec66b6a8108ef3a4b2229328f40c9da12e20
bhcf200003.csv.gz sha256 0b9b0d83ffdcfd2563fc8985e525ae562c09455ce7ab8432ef89ace0ef610e82
bhcf200006.csv.gz sha256 efe49909941f00ac9678d8e07a2c4503f4fadf4691dfb767536a1d267aa9b6cc
bhcf200009.csv.gz sha256 5251789d4c5103d27d62bc517fee13b807811bb048611cf37185015b1e0bf1fa
bhcf200103.csv.gz sha256 3dbc5afcf3ec47e066a594644b46fef8581a2a2eea6bda5a293e2df9ffb727cb
bhcf200106.csv.gz sha256 3c73f862404fb69607be4278a2f38a9c3d2223b9e42c207573a894ed1a95060c
bhcf200109.csv.gz sha256 5dbc6cede269528800e257da1ba9910b638372719b2d88387609e85d31f3d1c7
bhcf200203.csv.gz sha256 624ef2deecfe37ccd042e3eb6c43cc7cc1bdc788e6f0dc4398e11279ea9b7a2a
bhcf200206.csv.gz sha256 8616eff70ca589cdd8fbaf9fbc82e40d30313272a48735c6fc7af9ad26d1cd6b
bhcf200209.csv.gz sha256 8fe5c87583c130b2091130a6a2cacff4178f5a6141509a2cc27e108dd5c39ad8
bhcf200303.csv.gz sha256 350ed227045a16255f4c8ab133a42a86915ab8bac0c50326ce5e8b71198e7745
bhcf200306.csv.gz sha256 232283656d7037945d5a4843c76e467f92b16a0e16c8a0aa8a6c5a28c51f4ce7
bhcf200309.csv.gz sha256 483228548858cfc2fed4f3a07f20a014fe65cf206efef9fbc2110f7195e72c26
bhcf200403.csv.gz sha256 1dcb8bdbdfedcc9a93fef37c91aa7fb848484ea6fbc8b2e4f74e3ab9b42cd434
bhcf200406.csv.gz sha256 e06cf5fd5d3ccd0e243ca6675022064217f37f1a21e12a8968f76b041a80f510
bhcf200409.csv.gz sha256 5fde8d29b2288bba17961c4793f3c901d4d9f067ba40e2a2439a2ca958c6e960
bhcf200503.csv.gz sha256 b67014164c80e14212b027190bbf86c20b76e474fd9b6e0dba269cb072d1b863
bhcf200506.csv.gz sha256 fb7b191b43b0d337eb31bacd72baf84af67bb6d4c2b1f38839cb81a9ee5fd252
bhcf200509.csv.gz sha256 2053c62cb9265bf4cf3cad90ab6a7a770f732f9aa7b9011283671c432eacf32f
bhcf200603.csv.gz sha256 2a9b77a4a19e7786db28cebd15d19e7ac55ce3f2da2058eaf4fa4dfd655301b6
bhcf200606.csv.gz sha256 d8495711a48af90dc5aaaead840cb30a241d18be868c6b79fc473a8fea059f32
bhcf200609.csv.gz sha256 d556fafa30539bec2dd874a0ee093ae1b0e57646762d2e5b03762c8916e870b8
CSV_ATTRIBUTES_ACTIVE.zip sha256 a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4
CSV_ATTRIBUTES_CLOSED.zip sha256 adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c

D1: bank-years 3517; means STDROA 0.0007 (annualised x4 0.0028), NMERGER 0.454, RETAILDEP 0.300, BUSINESSLOAN 0.501  [theirs .0034, .066, .605, .281]

D2: bank-years 3517; means STDROA 0.0007 (annualised x4 0.0028), NMERGER 0.007, RETAILDEP 0.592, BUSINESSLOAN 0.329  [theirs .0034, .066, .605, .281]

######## I1 x D1

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1513 [0.6024] (DNK coefficient 0.8487, se nan); 2585 differenced obs, 41 instruments, Hansen 27.7 on 22 df; mean target -1.9726 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2067 [0.4113] -> T3 differs
    dev                     0.2055 (t  15.73, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0007 (t  0.93, p 0.3499, n 219)
    DEV_WELLCAP_400TO600    0.0014 (t  1.88, p 0.0601, n 831)
    DEV_WELLCAP_200TO400    0.0012 (t  1.61, p 0.1084, n 1723)
    DEV_WELLCAP_0TO200      0.0009 (t  1.09, p 0.2744, n 644)
    DEV_ADEQCAP             0.0006 (t  0.39, p 0.6958, n 25)
    DEV_UNDERCAP           -0.0097 (t -11.24, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0115 (t -1.21, p 0.2267, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -2.6379 [0.0864] -> T5 differs
    lam_STDROA         -2.6155 (se nan)
    lam_NMERGER         0.0071 (se nan)
    lam_LNASSETS       -0.0082 (se nan)
    lam_D50B           -0.0290 (se nan)
    lam_D50B_LNASSETS   0.0021 (se nan)
    lam_RETAILDEP       0.0101 (se nan)
    lam_BUSINESSLOAN    0.0302 (se nan)
    lam_OFFBSHEET      -0.0005 (se nan)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.1812 [0.4864] (DNK coefficient 0.8188, se 0.1622); 2585 differenced obs, 39 instruments, Hansen 27.5 on 22 df; mean target 0.2191 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2111 [0.2785] -> T3 holds
    dev                     0.2309 (t  11.97, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0304 (t -2.98, p 0.0029, n 773)
    DEV_WELLCAP_400TO600   -0.0266 (t -2.54, p 0.0110, n 1117)
    DEV_WELLCAP_200TO400   -0.0211 (t -1.94, p 0.0520, n 652)
    DEV_WELLCAP_0TO200     -0.0209 (t -1.24, p 0.2140, n 164)
    DEV_ADEQCAP             0.0363 (t  0.67, p 0.5009, n 8)
    DEV_UNDERCAP            0.3414 (t  3.23, p 0.0012, n 2)
    DEV_SIG_UNDERCAP       -0.1474 (t -10.42, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.1892 [0.144]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -0.6710 (se 8.7618)
    lam_NMERGER         0.0098 (se 0.0031)
    lam_LNASSETS       -0.0138 (se 0.0092)
    lam_D50B           -0.2316 (se 2.2242)
    lam_D50B_LNASSETS   0.0126 (se 0.1173)
    lam_RETAILDEP      -0.0017 (se 0.0481)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.1926 [0.5632] (DNK coefficient 0.8074, se nan); 2585 differenced obs, 39 instruments, Hansen 23.3 on 22 df; mean target 0.3753 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.2486 [0.3604] -> T3 differs
    dev                     0.2726 (t  13.27, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0142 (t -1.94, p 0.0527, n 300)
    DEV_WELLCAP_400TO600   -0.0238 (t -3.31, p 0.0009, n 671)
    DEV_WELLCAP_200TO400   -0.0322 (t -4.26, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0268 (t -3.40, p 0.0007, n 1006)
    DEV_ADEQCAP            -0.0042 (t -0.37, p 0.7146, n 58)
    DEV_UNDERCAP            0.0467 (t  1.06, p 0.2904, n 5)
    DEV_SIG_UNDERCAP        0.0312 (t  0.36, p 0.7156, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.2731 [0.1694]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -2.2997 (se 1.4046)
    lam_NMERGER         0.0097 (se 0.0011)
    lam_LNASSETS       -0.0091 (se 0.0012)
    lam_D50B           -0.0650 (se nan)
    lam_D50B_LNASSETS   0.0042 (se nan)
    lam_RETAILDEP      -0.0015 (se 0.0063)
  T6 LNASSETS negative, STDROA positive -> differs

######## I2 x D1

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1670 [0.6024] (DNK coefficient 0.8330, se 0.0713); 2585 differenced obs, 47 instruments, Hansen 33.6 on 28 df; mean target 0.0885 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2279 [0.4113] -> T3 differs
    dev                     0.1519 (t  3.55, p 0.0004, n 3517)
    DEV_WELLCAP_600TO800    0.0578 (t  1.12, p 0.2633, n 219)
    DEV_WELLCAP_400TO600    0.0594 (t  1.26, p 0.2065, n 831)
    DEV_WELLCAP_200TO400    0.0914 (t  2.00, p 0.0460, n 1723)
    DEV_WELLCAP_0TO200      0.0682 (t  1.39, p 0.1632, n 644)
    DEV_ADEQCAP             0.0817 (t  1.01, p 0.3131, n 25)
    DEV_UNDERCAP            0.4928 (t  10.86, p 0.0000, n 2)
    DEV_SIG_UNDERCAP        0.2900 (t  0.91, p 0.3649, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.1089 [0.0864] -> T5 differs
    lam_STDROA         -1.4861 (se nan)
    lam_NMERGER         0.0063 (se nan)
    lam_LNASSETS       -0.0076 (se 0.0048)
    lam_D50B           -0.0341 (se nan)
    lam_D50B_LNASSETS   0.0024 (se nan)
    lam_RETAILDEP       0.0077 (se nan)
    lam_BUSINESSLOAN    0.0246 (se nan)
    lam_OFFBSHEET      -0.0004 (se nan)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.1831 [0.4864] (DNK coefficient 0.8169, se 0.0315); 2585 differenced obs, 45 instruments, Hansen 33.6 on 28 df; mean target -0.0361 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2330 [0.2785] -> T3 holds
    dev                     0.2147 (t  17.24, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0244 (t  4.85, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0254 (t  4.80, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0241 (t  4.44, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0181 (t  2.38, p 0.0172, n 164)
    DEV_ADEQCAP            -0.0898 (t -1.10, p 0.2710, n 8)
    DEV_UNDERCAP           -0.8614 (t -4.74, p 0.0000, n 2)
    DEV_SIG_UNDERCAP        1.0960 (t  17.19, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.0043 [0.144]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -4.4524 (se 37.8928)
    lam_NMERGER         0.0081 (se 0.0030)
    lam_LNASSETS       -0.0119 (se 0.0189)
    lam_D50B           -0.2441 (se nan)
    lam_D50B_LNASSETS   0.0131 (se nan)
    lam_RETAILDEP      -0.0032 (se 0.1825)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.1784 [0.5632] (DNK coefficient 0.8216, se nan); 2585 differenced obs, 45 instruments, Hansen 28.8 on 28 df; mean target 1.3293 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.2433 [0.3604] -> T3 differs
    dev                     0.2474 (t  15.44, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0024 (t -1.94, p 0.0522, n 300)
    DEV_WELLCAP_400TO600   -0.0041 (t -3.54, p 0.0004, n 671)
    DEV_WELLCAP_200TO400   -0.0058 (t -4.63, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0046 (t -3.53, p 0.0004, n 1006)
    DEV_ADEQCAP             0.0006 (t  0.28, p 0.7825, n 58)
    DEV_UNDERCAP            0.0139 (t  1.31, p 0.1918, n 5)
    DEV_SIG_UNDERCAP        0.0114 (t  0.58, p 0.5595, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.9976 [0.1694]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -2.8373 (se nan)
    lam_NMERGER         0.0098 (se 0.0023)
    lam_LNASSETS       -0.0087 (se 0.0072)
    lam_D50B           -0.0616 (se 2.8877)
    lam_D50B_LNASSETS   0.0040 (se 0.1506)
    lam_RETAILDEP      -0.0016 (se 0.1289)
  T6 LNASSETS negative, STDROA positive -> differs

######## I3 x D1

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1588 [0.6024] (DNK coefficient 0.8412, se nan); 2585 differenced obs, 27 instruments, Hansen 4.4 on 8 df; mean target -10.5676 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2122 [0.4113] -> T3 differs
    dev                     0.2120 (t  16.34, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0002 (t  1.04, p 0.2988, n 219)
    DEV_WELLCAP_400TO600    0.0003 (t  2.04, p 0.0414, n 831)
    DEV_WELLCAP_200TO400    0.0003 (t  1.79, p 0.0732, n 1723)
    DEV_WELLCAP_0TO200      0.0002 (t  1.27, p 0.2053, n 644)
    DEV_ADEQCAP             0.0001 (t  0.45, p 0.6520, n 25)
    DEV_UNDERCAP           -0.0018 (t -11.20, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0021 (t -1.17, p 0.2403, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -13.8222 [0.0864] -> T5 differs
    lam_STDROA         -2.9084 (se 13.2514)
    lam_NMERGER         0.0068 (se 0.0041)
    lam_LNASSETS       -0.0081 (se 0.0070)
    lam_D50B           -0.0992 (se 5.1605)
    lam_D50B_LNASSETS   0.0057 (se 0.2722)
    lam_RETAILDEP       0.0117 (se 0.1129)
    lam_BUSINESSLOAN    0.0303 (se 0.0209)
    lam_OFFBSHEET      -0.0030 (se 0.2696)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.2102 [0.4864] (DNK coefficient 0.7898, se nan); 2585 differenced obs, 25 instruments, Hansen 4.7 on 8 df; mean target -19.3209 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2442 [0.2785] -> T3 holds
    dev                     0.2440 (t  16.54, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0002 (t  5.59, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0003 (t  5.56, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0002 (t  5.10, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0002 (t  3.20, p 0.0014, n 164)
    DEV_ADEQCAP            -0.0002 (t -0.50, p 0.6178, n 8)
    DEV_UNDERCAP           -0.0030 (t -3.47, p 0.0005, n 2)
    DEV_SIG_UNDERCAP        0.0019 (t  15.80, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -11.4887 [0.144]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -1.6893 (se nan)
    lam_NMERGER         0.0078 (se nan)
    lam_LNASSETS       -0.0121 (se nan)
    lam_D50B           -0.2333 (se 0.1847)
    lam_D50B_LNASSETS   0.0125 (se 0.0098)
    lam_RETAILDEP      -0.0014 (se nan)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.2405 [0.5632] (DNK coefficient 0.7595, se nan); 2585 differenced obs, 25 instruments, Hansen 9.1 on 8 df; mean target 2.5722 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.3033 [0.3604] -> T3 holds
    dev                     0.3068 (t  17.82, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0021 (t -3.43, p 0.0006, n 300)
    DEV_WELLCAP_400TO600   -0.0033 (t -5.92, p 0.0000, n 671)
    DEV_WELLCAP_200TO400   -0.0045 (t -7.54, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0044 (t -6.75, p 0.0000, n 1006)
    DEV_ADEQCAP            -0.0023 (t -2.02, p 0.0433, n 58)
    DEV_UNDERCAP            0.0038 (t  0.75, p 0.4557, n 5)
    DEV_SIG_UNDERCAP        0.0017 (t  0.17, p 0.8653, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.9084 [0.1694]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -1.5327 (se 32.5007)
    lam_NMERGER         0.0069 (se 0.0040)
    lam_LNASSETS       -0.0077 (se 0.0061)
    lam_D50B           -0.0859 (se nan)
    lam_D50B_LNASSETS   0.0051 (se nan)
    lam_RETAILDEP      -0.0015 (se 0.0255)
  T6 LNASSETS negative, STDROA positive -> differs

######## I4 x D1

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1841 [0.6024] (DNK coefficient 0.8159, se 0.2086); 2869 differenced obs, 32 instruments, Hansen 14.9 on 14 df; mean target -1.4915 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.0157 [0.4113] -> T3 differs
    dev                     0.0205 (t  10.34, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0014 (t -1.22, p 0.2219, n 219)
    DEV_WELLCAP_400TO600   -0.0031 (t -2.80, p 0.0050, n 831)
    DEV_WELLCAP_200TO400   -0.0053 (t -4.68, p 0.0000, n 1723)
    DEV_WELLCAP_0TO200     -0.0074 (t -6.31, p 0.0000, n 644)
    DEV_ADEQCAP            -0.0100 (t -5.22, p 0.0000, n 25)
    DEV_UNDERCAP           -0.0252 (t -22.05, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0315 (t -2.73, p 0.0063, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -4.4014 [0.0864] -> T5 differs
    lam_STDROA         -32.3377 (se 23.7596)
    lam_NMERGER         0.1211 (se 0.0158)
    lam_LNASSETS       -1.1766 (se 0.1374)
    lam_D50B            6.5387 (se 8.8953)
    lam_D50B_LNASSETS  -0.3728 (se 0.5019)
    lam_RETAILDEP       0.1962 (se 0.3311)
    lam_BUSINESSLOAN    0.5702 (se 0.5460)
    lam_OFFBSHEET       0.4728 (se 0.7930)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda -0.0307 [0.4864] (DNK coefficient 1.0307, se 0.1788); 2869 differenced obs, 30 instruments, Hansen 9.3 on 14 df; mean target 18.0012 [0.1302] -> T2 differs
  Step 2: mean lambda_it -0.0017 [0.2785] -> T3 differs
    dev                    -0.0021 (t -10.28, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0003 (t  5.23, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0005 (t  9.68, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0006 (t  11.48, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0007 (t  8.70, p 0.0000, n 164)
    DEV_ADEQCAP             0.0016 (t  3.92, p 0.0001, n 8)
    DEV_UNDERCAP            0.0051 (t  5.37, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0000 (t -0.57, p 0.5686, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target 48.5552 [0.144]; mean target above mean ratio: True -> T5 differs
    lam_STDROA          353.9120 (se 446.3997)
    lam_NMERGER        -1.6122 (se 0.2136)
    lam_LNASSETS        14.9339 (se 1.8746)
    lam_D50B           -87.4338 (se 80.0225)
    lam_D50B_LNASSETS   4.9364 (se 4.6387)
    lam_RETAILDEP       0.2172 (se 5.1018)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.0253 [0.5632] (DNK coefficient 0.9747, se 0.2250); 2869 differenced obs, 30 instruments, Hansen 10.8 on 14 df; mean target -25.2781 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.0009 [0.3604] -> T3 differs
    dev                     0.0013 (t  9.25, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0003 (t -4.37, p 0.0000, n 300)
    DEV_WELLCAP_400TO600   -0.0004 (t -6.95, p 0.0000, n 671)
    DEV_WELLCAP_200TO400   -0.0005 (t -8.74, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0006 (t -11.92, p 0.0000, n 1006)
    DEV_ADEQCAP            -0.0010 (t -10.33, p 0.0000, n 58)
    DEV_UNDERCAP           -0.0018 (t -3.58, p 0.0003, n 5)
    DEV_SIG_UNDERCAP       -0.0019 (t -2.16, p 0.0309, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -60.5097 [0.1694]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -1603.9177 (se 813.1286)
    lam_NMERGER         4.0145 (se 0.4928)
    lam_LNASSETS       -16.8707 (se 2.5919)
    lam_D50B           -172.3707 (se 275.8122)
    lam_D50B_LNASSETS   9.3375 (se 15.4569)
    lam_RETAILDEP      -13.5665 (se 10.1353)
  T6 LNASSETS negative, STDROA positive -> differs

######## I1 x D2

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1452 [0.6024] (DNK coefficient 0.8548, se 0.0842); 2585 differenced obs, 41 instruments, Hansen 26.6 on 22 df; mean target 0.1077 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2443 [0.4113] -> T3 differs
    dev                     0.1169 (t  1.66, p 0.0966, n 3517)
    DEV_WELLCAP_600TO800    0.1337 (t  1.50, p 0.1342, n 219)
    DEV_WELLCAP_400TO600    0.1090 (t  1.52, p 0.1290, n 831)
    DEV_WELLCAP_200TO400    0.1287 (t  1.81, p 0.0703, n 1723)
    DEV_WELLCAP_0TO200      0.1580 (t  2.21, p 0.0268, n 644)
    DEV_ADEQCAP             0.1183 (t  1.39, p 0.1645, n 25)
    DEV_UNDERCAP            0.5132 (t  7.22, p 0.0000, n 2)
    DEV_SIG_UNDERCAP        0.3250 (t  1.20, p 0.2304, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.1377 [0.0864] -> T5 differs
    lam_STDROA         -2.3186 (se nan)
    lam_NMERGER         0.0026 (se 0.0213)
    lam_LNASSETS       -0.0066 (se 0.0008)
    lam_D50B           -0.0246 (se 0.1262)
    lam_D50B_LNASSETS   0.0016 (se 0.0066)
    lam_RETAILDEP      -0.0015 (se 0.0138)
    lam_BUSINESSLOAN    0.0427 (se 0.0117)
    lam_OFFBSHEET       0.0021 (se 0.0102)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.1676 [0.4864] (DNK coefficient 0.8324, se 0.0747); 2585 differenced obs, 39 instruments, Hansen 26.0 on 22 df; mean target -0.5705 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2621 [0.2785] -> T3 holds
    dev                     0.2564 (t  15.45, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0072 (t  5.88, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0079 (t  6.06, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0073 (t  5.17, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0061 (t  3.17, p 0.0015, n 164)
    DEV_ADEQCAP            -0.0099 (t -0.73, p 0.4629, n 8)
    DEV_UNDERCAP           -0.0963 (t -3.41, p 0.0006, n 2)
    DEV_SIG_UNDERCAP        0.0597 (t  15.00, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.3266 [0.144]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -2.5376 (se 0.8209)
    lam_NMERGER        -0.0052 (se 0.0094)
    lam_LNASSETS       -0.0096 (se 0.0009)
    lam_D50B           -0.1607 (se 0.0955)
    lam_D50B_LNASSETS   0.0083 (se 0.0050)
    lam_RETAILDEP      -0.0135 (se 0.0146)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.1861 [0.5632] (DNK coefficient 0.8139, se 0.0681); 2585 differenced obs, 39 instruments, Hansen 22.3 on 22 df; mean target 1.3745 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.3464 [0.3604] -> T3 holds
    dev                     0.3556 (t  16.24, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0054 (t -4.36, p 0.0000, n 300)
    DEV_WELLCAP_400TO600   -0.0087 (t -7.57, p 0.0000, n 671)
    DEV_WELLCAP_200TO400   -0.0113 (t -9.10, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0113 (t -8.19, p 0.0000, n 1006)
    DEV_ADEQCAP            -0.0087 (t -3.70, p 0.0002, n 58)
    DEV_UNDERCAP            0.0057 (t  0.53, p 0.5973, n 5)
    DEV_SIG_UNDERCAP        0.0021 (t  0.11, p 0.9126, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.7658 [0.1694]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -1.7695 (se nan)
    lam_NMERGER        -0.0020 (se nan)
    lam_LNASSETS       -0.0052 (se nan)
    lam_D50B           -0.0167 (se nan)
    lam_D50B_LNASSETS   0.0012 (se nan)
    lam_RETAILDEP      -0.0058 (se nan)
  T6 LNASSETS negative, STDROA positive -> differs

######## I2 x D2

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1750 [0.6024] (DNK coefficient 0.8250, se 0.0718); 2585 differenced obs, 47 instruments, Hansen 31.9 on 28 df; mean target -0.0587 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2981 [0.4113] -> T3 differs
    dev                     0.2622 (t  16.07, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0156 (t  1.77, p 0.0769, n 219)
    DEV_WELLCAP_400TO600    0.0332 (t  3.72, p 0.0002, n 831)
    DEV_WELLCAP_200TO400    0.0407 (t  4.49, p 0.0000, n 1723)
    DEV_WELLCAP_0TO200      0.0387 (t  3.99, p 0.0001, n 644)
    DEV_ADEQCAP             0.0558 (t  2.26, p 0.0238, n 25)
    DEV_UNDERCAP           -0.1533 (t -12.38, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.2731 (t -1.53, p 0.1258, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.0021 [0.0864] -> T5 differs
    lam_STDROA         -1.7521 (se 91.7294)
    lam_NMERGER         0.0021 (se 0.7539)
    lam_LNASSETS       -0.0049 (se nan)
    lam_D50B           -0.0151 (se 8.6233)
    lam_D50B_LNASSETS   0.0010 (se 0.4478)
    lam_RETAILDEP      -0.0035 (se 0.4529)
    lam_BUSINESSLOAN    0.0334 (se 0.4496)
    lam_OFFBSHEET      -0.0013 (se 0.5308)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.1782 [0.4864] (DNK coefficient 0.8218, se 0.0683); 2585 differenced obs, 45 instruments, Hansen 32.0 on 28 df; mean target -0.4257 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2730 [0.2785] -> T3 holds
    dev                     0.2650 (t  15.55, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0097 (t  6.30, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0110 (t  6.66, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0107 (t  5.88, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0095 (t  3.85, p 0.0001, n 164)
    DEV_ADEQCAP            -0.0094 (t -0.54, p 0.5873, n 8)
    DEV_UNDERCAP           -0.1219 (t -3.33, p 0.0009, n 2)
    DEV_SIG_UNDERCAP        0.0887 (t  15.33, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.2583 [0.144]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -2.3700 (se nan)
    lam_NMERGER         0.0009 (se nan)
    lam_LNASSETS       -0.0089 (se nan)
    lam_D50B           -0.1553 (se 1.9683)
    lam_D50B_LNASSETS   0.0080 (se 0.1297)
    lam_RETAILDEP      -0.0129 (se nan)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.1780 [0.5632] (DNK coefficient 0.8220, se nan); 2585 differenced obs, 45 instruments, Hansen 27.4 on 28 df; mean target -0.3153 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.3533 [0.3604] -> T3 holds
    dev                     0.3299 (t  17.72, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0129 (t  4.23, p 0.0000, n 300)
    DEV_WELLCAP_400TO600    0.0215 (t  7.62, p 0.0000, n 671)
    DEV_WELLCAP_200TO400    0.0290 (t  9.65, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200      0.0297 (t  8.75, p 0.0000, n 1006)
    DEV_ADEQCAP             0.0218 (t  3.24, p 0.0012, n 58)
    DEV_UNDERCAP           -0.0276 (t -0.80, p 0.4216, n 5)
    DEV_SIG_UNDERCAP       -0.0193 (t -0.33, p 0.7407, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.0993 [0.1694]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -1.8901 (se nan)
    lam_NMERGER         0.0024 (se nan)
    lam_LNASSETS       -0.0048 (se 0.0044)
    lam_D50B           -0.0142 (se nan)
    lam_D50B_LNASSETS   0.0010 (se nan)
    lam_RETAILDEP      -0.0080 (se 0.0635)
  T6 LNASSETS negative, STDROA positive -> differs

######## I3 x D2

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1899 [0.6024] (DNK coefficient 0.8101, se 0.1796); 2585 differenced obs, 27 instruments, Hansen 3.8 on 8 df; mean target -1.5526 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.2923 [0.4113] -> T3 differs
    dev                     0.2886 (t  18.51, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0018 (t  1.92, p 0.0545, n 219)
    DEV_WELLCAP_400TO600    0.0036 (t  3.76, p 0.0002, n 831)
    DEV_WELLCAP_200TO400    0.0041 (t  4.25, p 0.0000, n 1723)
    DEV_WELLCAP_0TO200      0.0038 (t  3.80, p 0.0001, n 644)
    DEV_ADEQCAP             0.0050 (t  2.55, p 0.0107, n 25)
    DEV_UNDERCAP           -0.0091 (t -8.22, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0093 (t -0.75, p 0.4530, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target -0.7132 [0.0864] -> T5 differs
    lam_STDROA         -1.4882 (se nan)
    lam_NMERGER         0.0021 (se nan)
    lam_LNASSETS       -0.0052 (se nan)
    lam_D50B           -0.0612 (se 1.1375)
    lam_D50B_LNASSETS   0.0033 (se 0.0610)
    lam_RETAILDEP      -0.0044 (se nan)
    lam_BUSINESSLOAN    0.0364 (se 0.0899)
    lam_OFFBSHEET      -0.0017 (se 0.0281)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda 0.2270 [0.4864] (DNK coefficient 0.7730, se 0.0934); 2585 differenced obs, 25 instruments, Hansen 4.3 on 8 df; mean target 0.4146 [0.1302] -> T2 differs
  Step 2: mean lambda_it 0.2719 [0.2785] -> T3 holds
    dev                     0.2875 (t  13.44, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0191 (t -6.14, p 0.0000, n 773)
    DEV_WELLCAP_400TO600   -0.0211 (t -6.02, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400   -0.0208 (t -5.42, p 0.0000, n 652)
    DEV_WELLCAP_0TO200     -0.0199 (t -3.98, p 0.0001, n 164)
    DEV_ADEQCAP             0.0036 (t  0.14, p 0.8882, n 8)
    DEV_UNDERCAP            0.1399 (t  2.90, p 0.0038, n 2)
    DEV_SIG_UNDERCAP       -0.1004 (t -12.93, p 0.0000, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.2552 [0.144]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -1.0181 (se 8.3609)
    lam_NMERGER        -0.0035 (se 0.0536)
    lam_LNASSETS       -0.0100 (se 0.0017)
    lam_D50B           -0.1751 (se 0.2952)
    lam_D50B_LNASSETS   0.0091 (se 0.0152)
    lam_RETAILDEP      -0.0129 (se 0.0360)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.2610 [0.5632] (DNK coefficient 0.7390, se 0.0915); 2585 differenced obs, 25 instruments, Hansen 8.4 on 8 df; mean target 0.3652 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.3526 [0.3604] -> T3 holds
    dev                     0.4099 (t  13.32, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0354 (t -4.35, p 0.0000, n 300)
    DEV_WELLCAP_400TO600   -0.0552 (t -6.98, p 0.0000, n 671)
    DEV_WELLCAP_200TO400   -0.0696 (t -8.01, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0701 (t -7.31, p 0.0000, n 1006)
    DEV_ADEQCAP            -0.0576 (t -4.27, p 0.0000, n 58)
    DEV_UNDERCAP           -0.0014 (t -0.03, p 0.9746, n 5)
    DEV_SIG_UNDERCAP       -0.0312 (t -0.35, p 0.7240, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target 0.2435 [0.1694]; mean target above mean ratio: True -> T5 differs
    lam_STDROA         -1.6074 (se 1.4636)
    lam_NMERGER        -0.0013 (se 0.0101)
    lam_LNASSETS       -0.0053 (se 0.0010)
    lam_D50B           -0.0462 (se 0.0322)
    lam_D50B_LNASSETS   0.0026 (se 0.0023)
    lam_RETAILDEP      -0.0069 (se 0.0219)
  T6 LNASSETS negative, STDROA positive -> differs

######## I4 x D2

===== CAPLEV: bank-years 3517, holding companies 643
  T1 mean ratio 0.0845 [0.0846] -> holds
  Step 1: lambda 0.1911 [0.6024] (DNK coefficient 0.8089, se 0.2238); 2869 differenced obs, 32 instruments, Hansen 16.4 on 14 df; mean target -1.7414 [0.083] -> T2 differs
  Step 2: mean lambda_it 0.0135 [0.4113] -> T3 differs
    dev                     0.0177 (t  10.13, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0013 (t -1.28, p 0.2017, n 219)
    DEV_WELLCAP_400TO600   -0.0028 (t -2.88, p 0.0039, n 831)
    DEV_WELLCAP_200TO400   -0.0046 (t -4.73, p 0.0000, n 1723)
    DEV_WELLCAP_0TO200     -0.0064 (t -6.30, p 0.0000, n 644)
    DEV_ADEQCAP            -0.0087 (t -5.16, p 0.0000, n 25)
    DEV_UNDERCAP           -0.0218 (t -22.05, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0271 (t -2.72, p 0.0066, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -5.2799 [0.0864] -> T5 differs
    lam_STDROA         -51.5032 (se 26.8041)
    lam_NMERGER         0.0870 (se 0.1551)
    lam_LNASSETS       -1.5673 (se 0.1664)
    lam_D50B            9.3350 (se 10.9254)
    lam_D50B_LNASSETS  -0.5314 (se 0.6174)
    lam_RETAILDEP      -0.6365 (se 0.3775)
    lam_BUSINESSLOAN    0.7370 (se 0.6604)
    lam_OFFBSHEET       0.5736 (se 0.9823)
  T6 LNASSETS negative -> holds

===== CAPTIER1: bank-years 3517, holding companies 643
  T1 mean ratio 0.1206 [0.1208] -> holds
  Step 1: lambda -0.0688 [0.4864] (DNK coefficient 1.0688, se 0.1938); 2869 differenced obs, 30 instruments, Hansen 10.0 on 14 df; mean target 9.9041 [0.1302] -> T2 differs
  Step 2: mean lambda_it -0.0030 [0.2785] -> T3 differs
    dev                    -0.0036 (t -9.75, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800    0.0005 (t  5.24, p 0.0000, n 773)
    DEV_WELLCAP_400TO600    0.0009 (t  9.71, p 0.0000, n 1117)
    DEV_WELLCAP_200TO400    0.0012 (t  11.28, p 0.0000, n 652)
    DEV_WELLCAP_0TO200      0.0013 (t  8.34, p 0.0000, n 164)
    DEV_ADEQCAP             0.0029 (t  3.94, p 0.0001, n 8)
    DEV_UNDERCAP            0.0093 (t  5.36, p 0.0000, n 2)
    DEV_SIG_UNDERCAP       -0.0000 (t -0.14, p 0.8910, n 1)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> holds
  Step 3: mean target 26.8461 [0.144]; mean target above mean ratio: True -> T5 differs
    lam_STDROA          305.8174 (se 252.2531)
    lam_NMERGER         0.1977 (se 1.0807)
    lam_LNASSETS        9.1128 (se 1.1284)
    lam_D50B           -74.1631 (se 43.9380)
    lam_D50B_LNASSETS   4.1463 (se 2.5763)
    lam_RETAILDEP       4.3116 (se 2.6160)
  T6 LNASSETS negative, STDROA positive -> differs

===== CAPTOTAL: bank-years 3517, holding companies 643
  T1 mean ratio 0.1364 [0.1381] -> holds
  Step 1: lambda 0.0496 [0.5632] (DNK coefficient 0.9504, se 0.2318); 2869 differenced obs, 30 instruments, Hansen 12.6 on 14 df; mean target -14.6512 [0.15] -> T2 differs
  Step 2: mean lambda_it 0.0014 [0.3604] -> T3 differs
    dev                     0.0022 (t  8.86, p 0.0000, n 3517)
    DEV_WELLCAP_600TO800   -0.0005 (t -4.31, p 0.0000, n 300)
    DEV_WELLCAP_400TO600   -0.0006 (t -6.84, p 0.0000, n 671)
    DEV_WELLCAP_200TO400   -0.0008 (t -8.57, p 0.0000, n 1151)
    DEV_WELLCAP_0TO200     -0.0011 (t -11.73, p 0.0000, n 1006)
    DEV_ADEQCAP            -0.0018 (t -10.24, p 0.0000, n 58)
    DEV_UNDERCAP           -0.0031 (t -3.55, p 0.0004, n 5)
    DEV_SIG_UNDERCAP       -0.0033 (t -2.16, p 0.0308, n 3)
  T4 400TO600, 200TO400, 0TO200 positive at 5% -> differs
  Step 3: mean target -34.3712 [0.1694]; mean target above mean ratio: False -> T5 differs
    lam_STDROA         -1211.2576 (se 501.8746)
    lam_NMERGER        -0.3908 (se 2.3229)
    lam_LNASSETS       -9.6834 (se 1.5822)
    lam_D50B           -78.8395 (se 162.1685)
    lam_D50B_LNASSETS   4.3036 (se 9.0780)
    lam_RETAILDEP      -3.8045 (se 6.3645)
  T6 LNASSETS negative, STDROA positive -> differs

######## SUMMARY: Step 1 speed, Step 2 mean speed, Step 3 mean target; targets holding of 6
I1 x D1: CAPLEV 0.151/0.207/-2.638 (2/6); CAPTIER1 0.181/0.211/0.189 (2/6); CAPTOTAL 0.193/0.249/0.273 (1/6); Step 1 speed within 0.10 for 0 of 3
I2 x D1: CAPLEV 0.167/0.228/0.109 (2/6); CAPTIER1 0.183/0.233/-0.004 (3/6); CAPTOTAL 0.178/0.243/0.998 (1/6); Step 1 speed within 0.10 for 0 of 3
I3 x D1: CAPLEV 0.159/0.212/-13.822 (2/6); CAPTIER1 0.210/0.244/-11.489 (3/6); CAPTOTAL 0.241/0.303/0.908 (2/6); Step 1 speed within 0.10 for 0 of 3
I4 x D1: CAPLEV 0.184/0.016/-4.401 (2/6); CAPTIER1 -0.031/-0.002/48.555 (2/6); CAPTOTAL 0.025/0.001/-60.510 (1/6); Step 1 speed within 0.10 for 0 of 3
I1 x D2: CAPLEV 0.145/0.244/0.138 (2/6); CAPTIER1 0.168/0.262/-0.327 (3/6); CAPTOTAL 0.186/0.346/0.766 (2/6); Step 1 speed within 0.10 for 0 of 3
I2 x D2: CAPLEV 0.175/0.298/-0.002 (3/6); CAPTIER1 0.178/0.273/-0.258 (3/6); CAPTOTAL 0.178/0.353/-0.099 (3/6); Step 1 speed within 0.10 for 0 of 3
I3 x D2: CAPLEV 0.190/0.292/-0.713 (3/6); CAPTIER1 0.227/0.272/0.255 (2/6); CAPTOTAL 0.261/0.353/0.244 (2/6); Step 1 speed within 0.10 for 0 of 3
I4 x D2: CAPLEV 0.191/0.013/-5.280 (2/6); CAPTIER1 -0.069/-0.003/26.846 (2/6); CAPTOTAL 0.050/0.001/-34.371 (1/6); Step 1 speed within 0.10 for 0 of 3
runs with the Step 1 speed within 0.10 of theirs for at least two measures: 0 of 8
reading: the speed gap is due to the data
```
