# Replication of Gordon and Hirtle (2026), stage 1: their facts on their data

Run 13 October 2026 under PREREG_stage1.md (commit 799cfea6) and
under PREREG_stage1_2.md (commit 48a74626). Y-9C December files 1990-2020 and CPI
were fetched by fetch_stage1.py after the first file; the FFIEC files
2021-2024 and the NIC relationships file were downloaded by the author
after the second.

Seen before this run: a development run on 1990-2020 (TODO.md), and one run
on 1990-2024 that stopped with no output while parsing an FFIEC file,
because a stray quote character in a caret-delimited text field opened a
quoted field. The fix reads those files with quoting off; afterwards every
line of the four files parses with the header's field count.

## Findings

- 20 of 22 targets reproduce under the pre-registered rule; 2 differ.
- Reproduce: the four typical-year components (R1; earnings 11.86% against
  11.6%, issuance 2.68 against 2.5, dividends 4.42 against 4.7, repurchases
  3.86 against 4.2), the median issue among issuers (R3; 0.86% against
  0.9%), the Table 1 means of equity to assets, negative EMP, dividends,
  repurchases and issuance, the Tier 1 buffer and its count (R4), and all
  four columns of Table 2 with their counts (R5; the first-lag coefficient
  0.224, 0.224, 0.229 and 0.106 against 0.201, 0.201, 0.208 and 0.097).
- Differ: the share of bank-years paying dividends is 84.9% against their
  "more than 85 percent" (R2), and the sample has 7,843 bank-years against
  6,448 (R4), 21.6% more. The 1990-2020 part alone has 6,627. The source
  of the larger sample is not identified here; the authors do not state
  their deflator or any exclusion beyond top-tier, U.S.-owned, full-year
  and the size threshold.
- Control of the amended top-tier rule: on 2015-2020 it agrees with
  `RSSD9348` on 99.0% of bank-years passing the other rules, above the
  pre-registered 97%, so the 2021-2024 sample uses it alone.
- Known differences from the pre-registration hold: no OCI before 2001, no
  Tier 1 buffer before 1996, no bank-specific capital surcharges.

Pinned: cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
Pinned: CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
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

Reproduce: python3 stage1.py cpiaucsl_1989_2024.csv.gz CSV_RELATIONSHIPS.zip bhcf199012.csv.gz bhcf199112.csv.gz bhcf199212.csv.gz bhcf199312.csv.gz bhcf199412.csv.gz bhcf199512.csv.gz bhcf199612.csv.gz bhcf199712.csv.gz bhcf199812.csv.gz bhcf199912.csv.gz bhcf200012.csv.gz bhcf200112.csv.gz bhcf200212.csv.gz bhcf200312.csv.gz bhcf200412.csv.gz bhcf200512.csv.gz bhcf200612.csv.gz bhcf200712.csv.gz bhcf200812.csv.gz bhcf200912.csv.gz bhcf201012.csv.gz bhcf201112.csv.gz bhcf201212.csv.gz bhcf201312.csv.gz bhcf201412.csv.gz bhcf201512.csv.gz bhcf201612.csv.gz bhcf201712.csv.gz bhcf201812.csv.gz bhcf201912.csv.gz bhcf202012.csv.gz BHCF20211231.ZIP BHCF20221231.ZIP BHCF20231231.ZIP BHCF20241231.ZIP

```
cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
CSV_RELATIONSHIPS.zip sha256 a96a58838f5064b55407dd6d857b3f580998baa96906d9a6379a81ea5428cd8b
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

bank-years 7843, banks 760, years 1990-2024 (35)
reported: bank-years from files with holder fields also top-tier under RSSD9364 6621
control: RSSD9348 and the NIC rule agree on 1829/1848 = 99.0% of 2015-2020 bank-years passing the other rules -> at least 97%
reported: 2021-2024 bank-years 1216, of which top-tier in 2020 1008

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
```
