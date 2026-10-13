# Gordon and Hirtle stage 1: the sample-size difference

Run 13 October 2026 under PREREG_stage1_3.md (commit 51a5a4d3) and
under PREREG_stage1_4.md (commit 780d58ad). The GDP deflator and the
December 1989 Y-9C file were fetched by fetch_stage1_3.py after the first
file; the 1989 file is needed by C3's definition for 1990, though the
first file's data section does not list it. C3 reads "a December Y-9C
report in t-1" as a December file record with total assets reported.
stage1.py was split into a sample step and a report step with no change to
its output (results_stage1.md still reproduces).

## Findings

- C1 (bank holding companies only, entity type BHC or FHD) drops 335
  bank-years: 319 savings and loan holding companies and 16 intermediate
  holding companies of foreign banks, 1 in 2011 and 18 to 44 a year from
  2012. It accounts for the jump in our counts in 2012. Under the
  pre-registered rule C1 is adopted, because its ground is the paper's own
  description of the sample: the replication's primary sample is now 7,508
  bank-years.
- On the C1 sample the dividend-paying share is 85.4% and reproduces their
  "more than 85 percent"; every other target that reproduced in stage 1
  still reproduces; the total of 7,508 against 6,448 still differs.
- The 2020 control: the attributes files and `RSSD9346` agree on keep or
  drop for all 156 holding companies compared. For 2021-2024 the entity
  type came from the attributes files for 856 bank-years, from the 2020
  `RSSD9346` for 353, and was unknown for 7 (kept).
- C2 (GDP deflator) and C3 (continuity) are reported, not adopted. All
  three together leave 7,170 bank-years. Every combination keeps R4's
  means, the Tier 1 buffer and all of R5 reproducing; see the output.
- Their counts by year (results_chart2.md) run 11 to 48 below our C1
  counts in every year 1990-2023, 30 on average, and by period 28
  (1990-1995), 33 (1996-2011) and 27 (2012-2023). The remaining
  difference is spread evenly over time and is not identified by C1 to C3.

Pinned: cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
Pinned: gdpdef_1989_2024.csv.gz sha256 38a7b2e5f1a111f6d351f19b67d40b01448e591a17d8f49a5ba38d60af43b4a4
Pinned: CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
Pinned: CSV_ATTRIBUTES_ACTIVE.zip sha256 a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4
Pinned: CSV_ATTRIBUTES_CLOSED.zip sha256 adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c
Pinned: bhcf198912.csv.gz sha256 7d75ba9f688fc4150c323c49c02b76a5f1a3d70fe85e2fb66be49270b9ef9101
Pinned: bhcf199012.csv.gz sha256 ad9151a8921254a02e30992ec21ce885c91b0f2809b7dfd7098d2d0209d6a77f
Pinned: bhcf199112.csv.gz sha256 96c893131760377fa05cea0ebff59a9b319663f5a7e3ed7532d5cb62b61f1650
Pinned: bhcf199212.csv.gz sha256 a93d958a905d772060022785194bc4c057572c0e2a8d31120150be90d5693802
Pinned: bhcf199312.csv.gz sha256 b06d9f6cf3dccaf336e00d52b00e95aa58f9bd80a340f48317bf79628a50ff39
Pinned: bhcf199412.csv.gz sha256 2a4a5dcbc1e393e2a64ae4d14371390648066ed4edebbc8433224b7e9e97c001
Pinned: bhcf199512.csv.gz sha256 4d15116261e2c44508b42724e3aa2279f31fc924352ad33a5e8dd578c05ae725
Pinned: bhcf199612.csv.gz sha256 471096310c58aa25cd5542e1bffbe57f40917550ab3d2e90ed790c7faebb5e66
Pinned: bhcf199712.csv.gz sha256 32f02d770629744734caf6218e34855f2c360fa41b1689ba4288851733dbf947
Pinned: bhcf199812.csv.gz sha256 ee7597b443174ae463f7e14572690fb03835427dbaaf972a376db5ca14ae6ab6
Pinned: bhcf199912.csv.gz sha256 8016aee5af3e1568b500b58f94a7da7add972673229d3a020d7450bb463d471b
Pinned: bhcf200012.csv.gz sha256 fa4910ec7110dcd0750a0bfa162044b33be0f020ae81d5335e177887153fd739
Pinned: bhcf200112.csv.gz sha256 9bdf1286d7633fc667ff05a189995afe29b0547d9189d8dfb184cd74e29d0b56
Pinned: bhcf200212.csv.gz sha256 cf366d780af44bb95493cb4ddef6a9892b07f1131bc061215e614d9c5d902af5
Pinned: bhcf200312.csv.gz sha256 71c709ebdf7011e9faa846243d188e6ffb1da1efa95e81d1b01614ddfc66ce93
Pinned: bhcf200412.csv.gz sha256 69fa855ca01cfc152bdbebc3f331e21e84ca377215fc3fb04a9de6d940de172f
Pinned: bhcf200512.csv.gz sha256 2092029e1833908b54c7993adbf974784e3fd74a8206c211177749ef998eb723
Pinned: bhcf200612.csv.gz sha256 ae04ae5ed0eea08f7126d7918c12fd8028c2e6eec501765d031572953a60299c
Pinned: bhcf200712.csv.gz sha256 ac1f37e5bbd5e899c1b48d3c525a5b5782619a58e3af426cc022f6a547a12a97
Pinned: bhcf200812.csv.gz sha256 1a95f9abbc0f0e061f619a1c8e47c1e89a763487afff9e8c6bda01ed574889df
Pinned: bhcf200912.csv.gz sha256 050e5fbde56fbc61668eb674b7a15f8a530065ff8201fd1717f09ef04fa0d3d6
Pinned: bhcf201012.csv.gz sha256 563c2b2caaa052a81b9b37b93ca4eaa29804abc2736e91743a36e71f2a25b3a3
Pinned: bhcf201112.csv.gz sha256 bc9e4f017f971d72846fe609084065f78885d0f6319e8be3079aa81056690368
Pinned: bhcf201212.csv.gz sha256 ed4f361efa5efa54501135b366e4143e8ffb9d0c848af86bb0e93fcaee6d22b8
Pinned: bhcf201312.csv.gz sha256 406fd1f6e47db28a38154656fe26f7aa1cdc2d12e9195d97907d29909112fe3e
Pinned: bhcf201412.csv.gz sha256 d70ed62b2cc820fffdc2f86e8fcbd4d466e3b97dba3e7dd613edfe859de9db72
Pinned: bhcf201512.csv.gz sha256 793e03fe524d3880c4b56a1ec82ec0d602fd03e5ea930404e5fffddbdd703f76
Pinned: bhcf201612.csv.gz sha256 37fcb764153a509809dccbd68842271ab703f08cf2ed3e35933f0f5d44fc6f84
Pinned: bhcf201712.csv.gz sha256 de8ce89e601459dd93d85ac877f38b5089c0782703f0395b3941984ae2295e5b
Pinned: bhcf201812.csv.gz sha256 808a6de6c5ee48bcd6e47ec07af6117c4177fdf1f01edf66eff7d00d67e95ea0
Pinned: bhcf201912.csv.gz sha256 ed36e0bd28776f5b7e63cb866a4affeaf591c6abc61ec275c84d62905db1002f
Pinned: bhcf202012.csv.gz sha256 da7cf0758fde1b2a274261bd57f85888f77f9532a760d0767b8c1cb37af8b401
Pinned: BHCF20211231.ZIP sha256 157149cf9a2698fcf28f49c8a55f3fdc006b2ea9e6192e31121189eab08e871c
Pinned: BHCF20221231.ZIP sha256 d2f8b476de05276a93ed6e6926e14d261a6c28399e6c482815a32cd7b41154f5
Pinned: BHCF20231231.ZIP sha256 d7b60d0c97e76d4e4641bb0d8285e5636fc33f7525b5ecf744f67908b2e946d3
Pinned: BHCF20241231.ZIP sha256 a22b6c39d264ae1697c5e0a6a2d085f414aa059648b0059f83f8dc6545d336a3

Reproduce: python3 stage1_3.py cpiaucsl_1989_2024.csv.gz gdpdef_1989_2024.csv.gz CSV_RELATIONSHIPS.zip CSV_ATTRIBUTES_ACTIVE.zip CSV_ATTRIBUTES_CLOSED.zip bhcf198912.csv.gz bhcf199012.csv.gz bhcf199112.csv.gz bhcf199212.csv.gz bhcf199312.csv.gz bhcf199412.csv.gz bhcf199512.csv.gz bhcf199612.csv.gz bhcf199712.csv.gz bhcf199812.csv.gz bhcf199912.csv.gz bhcf200012.csv.gz bhcf200112.csv.gz bhcf200212.csv.gz bhcf200312.csv.gz bhcf200412.csv.gz bhcf200512.csv.gz bhcf200612.csv.gz bhcf200712.csv.gz bhcf200812.csv.gz bhcf200912.csv.gz bhcf201012.csv.gz bhcf201112.csv.gz bhcf201212.csv.gz bhcf201312.csv.gz bhcf201412.csv.gz bhcf201512.csv.gz bhcf201612.csv.gz bhcf201712.csv.gz bhcf201812.csv.gz bhcf201912.csv.gz bhcf202012.csv.gz BHCF20211231.ZIP BHCF20221231.ZIP BHCF20231231.ZIP BHCF20241231.ZIP

```
cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
gdpdef_1989_2024.csv.gz sha256 38a7b2e5f1a111f6d351f19b67d40b01448e591a17d8f49a5ba38d60af43b4a4
CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
CSV_ATTRIBUTES_ACTIVE.zip sha256 a5b2f2b7ec3a2282cf71a6c858838b4a100ce958752ddf118d280c33212aa0f4
CSV_ATTRIBUTES_CLOSED.zip sha256 adcedf003026cf001c606466ea420bf49ebfd086e39c01ab1381d21ca04d818c
bhcf198912.csv.gz sha256 7d75ba9f688fc4150c323c49c02b76a5f1a3d70fe85e2fb66be49270b9ef9101
bhcf199012.csv.gz sha256 ad9151a8921254a02e30992ec21ce885c91b0f2809b7dfd7098d2d0209d6a77f
bhcf199112.csv.gz sha256 96c893131760377fa05cea0ebff59a9b319663f5a7e3ed7532d5cb62b61f1650
bhcf199212.csv.gz sha256 a93d958a905d772060022785194bc4c057572c0e2a8d31120150be90d5693802
bhcf199312.csv.gz sha256 b06d9f6cf3dccaf336e00d52b00e95aa58f9bd80a340f48317bf79628a50ff39
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
bhcf200712.csv.gz sha256 ac1f37e5bbd5e899c1b48d3c525a5b5782619a58e3af426cc022f6a547a12a97
bhcf200812.csv.gz sha256 1a95f9abbc0f0e061f619a1c8e47c1e89a763487afff9e8c6bda01ed574889df
bhcf200912.csv.gz sha256 050e5fbde56fbc61668eb674b7a15f8a530065ff8201fd1717f09ef04fa0d3d6
bhcf201012.csv.gz sha256 563c2b2caaa052a81b9b37b93ca4eaa29804abc2736e91743a36e71f2a25b3a3
bhcf201112.csv.gz sha256 bc9e4f017f971d72846fe609084065f78885d0f6319e8be3079aa81056690368
bhcf201212.csv.gz sha256 ed4f361efa5efa54501135b366e4143e8ffb9d0c848af86bb0e93fcaee6d22b8
bhcf201312.csv.gz sha256 406fd1f6e47db28a38154656fe26f7aa1cdc2d12e9195d97907d29909112fe3e
bhcf201412.csv.gz sha256 d70ed62b2cc820fffdc2f86e8fcbd4d466e3b97dba3e7dd613edfe859de9db72
bhcf201512.csv.gz sha256 793e03fe524d3880c4b56a1ec82ec0d602fd03e5ea930404e5fffddbdd703f76
bhcf201612.csv.gz sha256 37fcb764153a509809dccbd68842271ab703f08cf2ed3e35933f0f5d44fc6f84
bhcf201712.csv.gz sha256 de8ce89e601459dd93d85ac877f38b5089c0782703f0395b3941984ae2295e5b
bhcf201812.csv.gz sha256 808a6de6c5ee48bcd6e47ec07af6117c4177fdf1f01edf66eff7d00d67e95ea0
bhcf201912.csv.gz sha256 ed36e0bd28776f5b7e63cb866a4affeaf591c6abc61ec275c84d62905db1002f
bhcf202012.csv.gz sha256 da7cf0758fde1b2a274261bd57f85888f77f9532a760d0767b8c1cb37af8b401
BHCF20211231.ZIP sha256 157149cf9a2698fcf28f49c8a55f3fdc006b2ea9e6192e31121189eab08e871c
BHCF20221231.ZIP sha256 d2f8b476de05276a93ed6e6926e14d261a6c28399e6c482815a32cd7b41154f5
BHCF20231231.ZIP sha256 d7b60d0c97e76d4e4641bb0d8285e5636fc33f7525b5ecf744f67908b2e946d3
BHCF20241231.ZIP sha256 a22b6c39d264ae1697c5e0a6a2d085f414aa059648b0059f83f8dc6545d336a3

control: 2020 keep-or-drop by attributes against RSSD9346: 156/156 agree
2021-2024 entity type read from: {'attributes': 856, 'carried RSSD9346': 353, 'unknown': 7}
C1 drops by entity type: {'IHC': 16, 'SLHC': 319}
C1 drops by year: 2011:1 2012:44 2013:38 2014:28 2015:27 2016:26 2017:25 2018:24 2019:23 2020:18 2021:20 2022:20 2023:21 2024:20

===== stage 1 sample: bank-years 7843, banks 760
by year: 1990:202 1991:196 1992:185 1993:177 1994:170 1995:170 1996:165 1997:173 1998:177 1999:186 2000:181 2001:188 2002:187 2003:198 2004:197 2005:208 2006:218 2007:220 2008:209 2009:212 2010:206 2011:202 2012:249 2013:250 2014:259 2015:277 2016:278 2017:285 2018:269 2019:262 2020:271 2021:286 2022:299 2023:314 2024:317

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.86 theirs  11.6 -> reproduces
  iss   ours   2.68 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.86 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 84.9% theirs >85% -> differs
R3 median issuance among issuers (% of prior equity): ours 0.86 theirs 0.9 -> reproduces (n 5959)

R4 Table 1
  N ours 7843 theirs 6448 -> differs
  equity/assets        ours   9.74 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.97 theirs 21.00 -> reproduces
  dividends/assets     ours   0.37 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.25 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   7.13 theirs  6.74 -> reproduces; N ours 6497 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.224 (0.0631) p 0.0004 N 5366; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.224 (0.0633) p 0.0004 N 5366; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.229 (0.0291) p 0.0000 N 5366; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.106 (0.0326) p 0.0011 N 5303; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C1: bank-years 7508, banks 718
by year: 1990:202 1991:196 1992:185 1993:177 1994:170 1995:170 1996:165 1997:173 1998:177 1999:186 2000:181 2001:188 2002:187 2003:198 2004:197 2005:208 2006:218 2007:220 2008:209 2009:212 2010:206 2011:201 2012:205 2013:212 2014:231 2015:250 2016:252 2017:260 2018:245 2019:239 2020:253 2021:266 2022:279 2023:293 2024:297

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.67 theirs  11.6 -> reproduces
  iss   ours   2.68 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.92 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.4% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.86 theirs 0.9 -> reproduces (n 5801)

R4 Table 1
  N ours 7508 theirs 6448 -> differs
  equity/assets        ours   9.59 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.58 theirs 21.00 -> reproduces
  dividends/assets     ours   0.35 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.25 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   7.00 theirs  6.74 -> reproduces; N ours 6327 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.222 (0.0637) p 0.0005 N 5218; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.222 (0.0639) p 0.0005 N 5218; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.225 (0.0289) p 0.0000 N 5218; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.109 (0.0327) p 0.0009 N 5155; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C2: bank-years 7652, banks 751
by year: 1990:186 1991:183 1992:174 1993:168 1994:165 1995:161 1996:158 1997:159 1998:167 1999:178 2000:178 2001:185 2002:183 2003:194 2004:190 2005:200 2006:213 2007:211 2008:205 2009:205 2010:200 2011:201 2012:246 2013:247 2014:256 2015:270 2016:274 2017:281 2018:269 2019:260 2020:269 2021:286 2022:299 2023:314 2024:317

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.86 theirs  11.6 -> reproduces
  iss   ours   2.68 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.87 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.0% theirs >85% -> differs
R3 median issuance among issuers (% of prior equity): ours 0.85 theirs 0.9 -> reproduces (n 5832)

R4 Table 1
  N ours 7652 theirs 6448 -> differs
  equity/assets        ours   9.77 theirs  9.10 -> reproduces
  negative EMP (%)     ours  21.17 theirs 21.00 -> reproduces
  dividends/assets     ours   0.37 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.22 theirs  0.22 -> reproduces
  issuance/assets      ours   0.25 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   7.12 theirs  6.74 -> reproduces; N ours 6370 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.224 (0.0638) p 0.0004 N 5263; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.224 (0.0640) p 0.0005 N 5263; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.230 (0.0293) p 0.0000 N 5263; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.107 (0.0332) p 0.0013 N 5197; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C3: bank-years 7638, banks 730
by year: 1990:199 1991:193 1992:184 1993:174 1994:166 1995:167 1996:162 1997:168 1998:171 1999:182 2000:180 2001:183 2002:187 2003:194 2004:193 2005:207 2006:217 2007:215 2008:209 2009:205 2010:200 2011:198 2012:202 2013:248 2014:259 2015:273 2016:276 2017:285 2018:267 2019:258 2020:268 2021:274 2022:275 2023:297 2024:302

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.87 theirs  11.6 -> reproduces
  iss   ours   2.57 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.88 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.4% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.85 theirs 0.9 -> reproduces (n 5832)

R4 Table 1
  N ours 7638 theirs 6448 -> differs
  equity/assets        ours   9.70 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.88 theirs 21.00 -> reproduces
  dividends/assets     ours   0.37 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.21 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   7.07 theirs  6.74 -> reproduces; N ours 6364 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.222 (0.0640) p 0.0005 N 5268; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.222 (0.0643) p 0.0005 N 5268; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.228 (0.0297) p 0.0000 N 5268; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.109 (0.0326) p 0.0009 N 5218; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C1+C2: bank-years 7319, banks 709
by year: 1990:186 1991:183 1992:174 1993:168 1994:165 1995:161 1996:158 1997:159 1998:167 1999:178 2000:178 2001:185 2002:183 2003:194 2004:190 2005:200 2006:213 2007:211 2008:205 2009:205 2010:200 2011:200 2012:202 2013:210 2014:228 2015:244 2016:248 2017:256 2018:245 2019:237 2020:251 2021:266 2022:279 2023:293 2024:297

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.67 theirs  11.6 -> reproduces
  iss   ours   2.68 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.92 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.5% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.86 theirs 0.9 -> reproduces (n 5675)

R4 Table 1
  N ours 7319 theirs 6448 -> differs
  equity/assets        ours   9.62 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.78 theirs 21.00 -> reproduces
  dividends/assets     ours   0.36 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.25 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   6.99 theirs  6.74 -> reproduces; N ours 6201 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.222 (0.0645) p 0.0006 N 5116; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.222 (0.0646) p 0.0006 N 5116; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.225 (0.0291) p 0.0000 N 5116; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.109 (0.0334) p 0.0011 N 5050; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C1+C3: bank-years 7355, banks 694
by year: 1990:199 1991:193 1992:184 1993:174 1994:166 1995:167 1996:162 1997:168 1998:171 1999:182 2000:180 2001:183 2002:187 2003:194 2004:193 2005:207 2006:217 2007:215 2008:209 2009:205 2010:200 2011:198 2012:201 2013:212 2014:231 2015:248 2016:251 2017:260 2018:244 2019:236 2020:250 2021:254 2022:255 2023:277 2024:282

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.67 theirs  11.6 -> reproduces
  iss   ours   2.57 theirs   2.5 -> reproduces
  div   ours   4.42 theirs   4.7 -> reproduces
  rep   ours   3.92 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.8% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.86 theirs 0.9 -> reproduces (n 5699)

R4 Table 1
  N ours 7355 theirs 6448 -> differs
  equity/assets        ours   9.58 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.52 theirs 21.00 -> reproduces
  dividends/assets     ours   0.36 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.22 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   6.94 theirs  6.74 -> reproduces; N ours 6199 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.219 (0.0646) p 0.0007 N 5126; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.219 (0.0647) p 0.0007 N 5126; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.223 (0.0294) p 0.0000 N 5126; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.110 (0.0328) p 0.0008 N 5076; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C2+C3: bank-years 7451, banks 720
by year: 1990:184 1991:180 1992:173 1993:165 1994:162 1995:158 1996:155 1997:154 1998:161 1999:174 2000:177 2001:180 2002:183 2003:191 2004:186 2005:199 2006:212 2007:206 2008:205 2009:198 2010:194 2011:197 2012:199 2013:245 2014:256 2015:266 2016:272 2017:281 2018:267 2019:257 2020:266 2021:274 2022:275 2023:297 2024:302

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.87 theirs  11.6 -> reproduces
  iss   ours   2.58 theirs   2.5 -> reproduces
  div   ours   4.43 theirs   4.7 -> reproduces
  rep   ours   3.88 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 85.5% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.85 theirs 0.9 -> reproduces (n 5707)

R4 Table 1
  N ours 7451 theirs 6448 -> differs
  equity/assets        ours   9.73 theirs  9.10 -> reproduces
  negative EMP (%)     ours  21.07 theirs 21.00 -> reproduces
  dividends/assets     ours   0.37 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.22 theirs  0.22 -> reproduces
  issuance/assets      ours   0.22 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   7.06 theirs  6.74 -> reproduces; N ours 6239 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.223 (0.0647) p 0.0006 N 5167; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.223 (0.0650) p 0.0006 N 5167; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.228 (0.0299) p 0.0000 N 5167; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.109 (0.0333) p 0.0011 N 5115; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces

===== C1+C2+C3: bank-years 7170, banks 684
by year: 1990:184 1991:180 1992:173 1993:165 1994:162 1995:158 1996:155 1997:154 1998:161 1999:174 2000:177 2001:180 2002:183 2003:191 2004:186 2005:199 2006:212 2007:206 2008:205 2009:198 2010:194 2011:197 2012:198 2013:210 2014:228 2015:242 2016:247 2017:256 2018:244 2019:235 2020:248 2021:254 2022:255 2023:277 2024:282

R1 medians over years of aggregate component / aggregate adjusted prior equity (%)
  earn  ours  11.67 theirs  11.6 -> reproduces
  iss   ours   2.58 theirs   2.5 -> reproduces
  div   ours   4.43 theirs   4.7 -> reproduces
  rep   ours   3.92 theirs   4.2 -> reproduces

R2 bank-years with positive dividends: ours 86.0% theirs >85% -> reproduces
R3 median issuance among issuers (% of prior equity): ours 0.85 theirs 0.9 -> reproduces (n 5575)

R4 Table 1
  N ours 7170 theirs 6448 -> differs
  equity/assets        ours   9.61 theirs  9.10 -> reproduces
  negative EMP (%)     ours  20.71 theirs 21.00 -> reproduces
  dividends/assets     ours   0.36 theirs  0.37 -> reproduces
  repurchases/assets   ours   0.21 theirs  0.22 -> reproduces
  issuance/assets      ours   0.22 theirs  0.20 -> reproduces
  Tier 1 buffer        ours   6.93 theirs  6.74 -> reproduces; N ours 6075 theirs 6394 -> reproduces

R5 Table 2, coefficient on negative EMP at t-1 (their SE in brackets)
  (1) ours 0.219 (0.0653) p 0.0008 N 5026; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (2) ours 0.219 (0.0655) p 0.0008 N 5026; theirs 0.201 (0.060) N 5148 -> reproduces; N reproduces
  (3) ours 0.223 (0.0297) p 0.0000 N 5026; theirs 0.208 (0.029) N 5148 -> reproduces; N reproduces
  (4) ours 0.111 (0.0335) p 0.0010 N 4974; theirs 0.097 (0.029) N 5091 -> reproduces; N reproduces
```
