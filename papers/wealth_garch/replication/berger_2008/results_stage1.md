# Replication of Berger et al. (2008), stage 1

Run 14 October 2026 under PREREG_stage1.md (commit 4906480e), on files
fetched by fetch_stage1.py after that commit and files pinned earlier.

## Runs

- Run 1 counted acquisitions with NIC relationship end dates read as
  exclusive. They are inclusive: a predecessor's link to its parent ends
  the day before the merger that ends it, so no merger inside an
  organisation was excluded, and NMERGER averaged 1.68. Fixed (the same
  error in the Gordon and Hirtle scripts was fixed and rerun, see their
  results files) and rerun; this file is run 2. Run 1 had 8 of 18 targets
  holding; three verdicts changed with the fix (CAPTIER1 T4, CAPTOTAL T3
  and T5, all from holds to differs), a sign of how much the estimates
  move with one regressor's definition.

## Findings

- 5 of 18 targets hold. Sample: 3,517 bank-years of 643 publicly traded
  bank holding companies, 1997-2006, 316 to 388 a year (theirs about 304).
- T1 holds for all three measures: mean CAPLEV 0.0845, CAPTIER1 0.1206,
  CAPTOTAL 0.1364 (theirs 0.0846, 0.1208, 0.1381).
- T2 differs for all three: Step 1 adjustment speeds 0.151, 0.181 and
  0.193 against 0.602, 0.486 and 0.563. With speeds this low the Step 1
  targets, (constant + b'X)/lambda, are ill-determined (CAPLEV -1.97), and
  some two-step standard errors are undefined (the Windmeijer-corrected
  variance has negative diagonal entries).
- T3 holds for CAPTIER1 only (mean Step 2 speed 0.211 against 0.279);
  CAPLEV 0.207 and CAPTOTAL 0.249 against 0.411 and 0.360.
- T4 differs for all three: the capital-position interactions are small
  and mostly negative; the speed is carried by the base DEV coefficient
  (0.206, 0.231, 0.273, each t > 11).
- T5 differs for all three; T6 holds for CAPLEV (LNASSETS negative) and
  differs for CAPTIER1 and CAPTOTAL (STDROA negative).
- Variables built from public data differ from theirs in mean: STDROA
  0.0007 (theirs 0.0034; ours is quarterly, theirs possibly annualised),
  NMERGER 0.454 (0.066; ours counts every merger and purchase-and-
  assumption into the organisation, non-bank entities included), RETAILDEP
  0.300 (0.605), BUSINESSLOAN 0.501 (0.281). On all bank holding company
  filers 1996-2005, business loans average 0.488 of total loans (our Table
  2 definition) and 0.319 of total assets (their text's denominator), so
  their reported mean is closer to the text's definition. Diagnostic, run
  after the results; not used for a verdict.

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

Reproduce: python3 stage1.py ../gordon_hirtle_2026/cpiaucsl_1989_2024.csv.gz ../gordon_hirtle_2026/CSV_RELATIONSHIPS.zip crsp_20240930v2.csv.gz CSV_TRANSFORMATIONS.zip ../gordon_hirtle_2026/bhcf199412.csv.gz ../gordon_hirtle_2026/bhcf199512.csv.gz ../gordon_hirtle_2026/bhcf199612.csv.gz ../gordon_hirtle_2026/bhcf199712.csv.gz ../gordon_hirtle_2026/bhcf199812.csv.gz ../gordon_hirtle_2026/bhcf199912.csv.gz ../gordon_hirtle_2026/bhcf200012.csv.gz ../gordon_hirtle_2026/bhcf200112.csv.gz ../gordon_hirtle_2026/bhcf200212.csv.gz ../gordon_hirtle_2026/bhcf200312.csv.gz ../gordon_hirtle_2026/bhcf200412.csv.gz ../gordon_hirtle_2026/bhcf200512.csv.gz ../gordon_hirtle_2026/bhcf200612.csv.gz bhcf199403.csv.gz bhcf199406.csv.gz bhcf199409.csv.gz bhcf199503.csv.gz bhcf199506.csv.gz bhcf199509.csv.gz bhcf199603.csv.gz bhcf199606.csv.gz bhcf199609.csv.gz bhcf199703.csv.gz bhcf199706.csv.gz bhcf199709.csv.gz bhcf199803.csv.gz bhcf199806.csv.gz bhcf199809.csv.gz bhcf199903.csv.gz bhcf199906.csv.gz bhcf199909.csv.gz bhcf200003.csv.gz bhcf200006.csv.gz bhcf200009.csv.gz bhcf200103.csv.gz bhcf200106.csv.gz bhcf200109.csv.gz bhcf200203.csv.gz bhcf200206.csv.gz bhcf200209.csv.gz bhcf200303.csv.gz bhcf200306.csv.gz bhcf200309.csv.gz bhcf200403.csv.gz bhcf200406.csv.gz bhcf200409.csv.gz bhcf200503.csv.gz bhcf200506.csv.gz bhcf200509.csv.gz bhcf200603.csv.gz bhcf200606.csv.gz bhcf200609.csv.gz

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

sample: bank-years 3517, holding companies 643, 99th-percentile cuts CAPLEV 0.1657, CAPTIER1 0.2855, CAPTOTAL 0.2992
by year: 1997:335 1998:316 1999:328 2000:337 2001:342 2002:372 2003:384 2004:366 2005:388 2006:349
means: STDROA 0.0007, NMERGER 0.4541, RETAILDEP 0.2996, BUSINESSLOAN 0.5014, OFFBSHEET 0.0065, D50B 0.0469  [theirs .0034, .066, .605, .281, .009, .054]

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
```
