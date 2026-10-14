# Gordon and Hirtle stage 2: their findings on banks

Run 14 October 2026 under PREREG_stage2.md (commit 5e21d393), on the
pinned common bank dataset, December reports 1990-2024. Fixed effects are
absorbed by alternating demeaning, which reproduced dummy-variable
coefficients to six decimals on simulated data before this run.
Ownership groups are subsets of P and A: their winsorised values come from
the whole sample, and their lags from within the group.

## Findings, primary sample P (commercial banks above $3 billion in 2024 dollars)

- 10,437 bank-years of 1,112 banks: 5,044 in multibank holding companies,
  4,845 the only bank of their holding company, 548 with none.
- H4 holds: higher equity to assets raises the chance that payouts exceed
  earnings (0.544 with year effects, 1.353 with bank effects, both
  p < 0.01; theirs 0.910 and 2.331).
- H5 holds: a fall in net income raises it (-12.377, theirs -13.620).
- H1 fails: dividends are positive in 78.2% of bank-years, not over 85%.
- H2 fails narrowly: payouts exceed earnings in 19.8%, not over 20%.
- H3 fails: persistence is positive with year effects (0.141, p < 0.001)
  but vanishes with bank effects (0.015, p 0.50; theirs 0.097). It
  differs between banks rather than within a bank over time.
- Payouts are dividends: the typical year pays 7.40% of prior equity in
  common dividends (theirs 4.7%) and 0.05% in net repurchases (theirs
  4.2%). Net issuance is 0.59% (theirs 2.5%). Net income is 11.81%
  (theirs 11.6% for earnings).

## Reported, all commercial banks (A) and ownership groups

- A has 253,660 bank-years. H4 and H5 hold there too, and persistence
  holds with bank effects (0.125, p < 0.001); H1 (75.5%) and H2 (15.9%)
  do not.
- Banks with no holding company pay dividends in fewer years (64.6% in P,
  59.7% in A). Multibank subsidiaries are the group closest to their H2
  share (23.2% in P, 20.2% in A).

Pinned: cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
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
Pinned: ../../common_data/fdic_q_2011.csv.gz sha256 8780a536cf9fa0b408cb61886ebf214913e04a48627ee1e7d6f7cd25753a8aba
Pinned: ../../common_data/fdic_q_2012.csv.gz sha256 0ed0edc2bceda753b0c60d0efcc9f39d0d97ddf085bc05c53734e540407a779c
Pinned: ../../common_data/fdic_q_2013.csv.gz sha256 64b685a0e7917085895f9b44ae5f86ee05602fd7171205315e914a8ffe54ead1
Pinned: ../../common_data/fdic_q_2014.csv.gz sha256 7cf4299881b363f08cca394dd2d53ad5959bdd43f39e53a89b738e21a4a4d89b
Pinned: ../../common_data/fdic_q_2015.csv.gz sha256 a1927a37f42ed95c7074dc196f5c06cbf29c238ef445ab2587add77df5f55373
Pinned: ../../common_data/fdic_q_2016.csv.gz sha256 6111d9aebd8e12b351d5c3ccdda9f310b13c871c742d184d706062a346ef4609
Pinned: ../../common_data/fdic_q_2017.csv.gz sha256 8546b1ab38b294adae748f0f5ff4fd91d9c548416d99998ad7d41bc29f049178
Pinned: ../../common_data/fdic_q_2018.csv.gz sha256 d57d152847f0511a0bf76f33b2bae5c95b5bd1e0d1984ce28b5423b1ab715bb2
Pinned: ../../common_data/fdic_q_2019.csv.gz sha256 363c0b516cbfb22ed7a68fb30ed19527d45a892a19fa7e1b90f696c90d253214
Pinned: ../../common_data/fdic_q_2020.csv.gz sha256 26094e27170aa52b1b6c7dea8cd18de7a914423a33f542d5273b0b9fc8beac5c
Pinned: ../../common_data/fdic_q_2021.csv.gz sha256 d8fc9f81ee1bbaa1a599bc950c6d152e03405ec0a68c0cfe45625e9d45168d75
Pinned: ../../common_data/fdic_q_2022.csv.gz sha256 010f2271081f5b4afa27e2fa4fa3c961534a646760099935a280beb9085afa70
Pinned: ../../common_data/fdic_q_2023.csv.gz sha256 7b346b82a1d1e29ced59eb58f4abad038c119f0ac48ca589052e0ce009234b05
Pinned: ../../common_data/fdic_q_2024.csv.gz sha256 74af80f572fc73be46cf5cdc260bfef2e284e2c8449e470cb1037589e3d38160

Reproduce: python3 stage2.py cpiaucsl_1989_2024.csv.gz ../../common_data/fdic_q_1990.csv.gz ../../common_data/fdic_q_1991.csv.gz ../../common_data/fdic_q_1992.csv.gz ../../common_data/fdic_q_1993.csv.gz ../../common_data/fdic_q_1994.csv.gz ../../common_data/fdic_q_1995.csv.gz ../../common_data/fdic_q_1996.csv.gz ../../common_data/fdic_q_1997.csv.gz ../../common_data/fdic_q_1998.csv.gz ../../common_data/fdic_q_1999.csv.gz ../../common_data/fdic_q_2000.csv.gz ../../common_data/fdic_q_2001.csv.gz ../../common_data/fdic_q_2002.csv.gz ../../common_data/fdic_q_2003.csv.gz ../../common_data/fdic_q_2004.csv.gz ../../common_data/fdic_q_2005.csv.gz ../../common_data/fdic_q_2006.csv.gz ../../common_data/fdic_q_2007.csv.gz ../../common_data/fdic_q_2008.csv.gz ../../common_data/fdic_q_2009.csv.gz ../../common_data/fdic_q_2010.csv.gz ../../common_data/fdic_q_2011.csv.gz ../../common_data/fdic_q_2012.csv.gz ../../common_data/fdic_q_2013.csv.gz ../../common_data/fdic_q_2014.csv.gz ../../common_data/fdic_q_2015.csv.gz ../../common_data/fdic_q_2016.csv.gz ../../common_data/fdic_q_2017.csv.gz ../../common_data/fdic_q_2018.csv.gz ../../common_data/fdic_q_2019.csv.gz ../../common_data/fdic_q_2020.csv.gz ../../common_data/fdic_q_2021.csv.gz ../../common_data/fdic_q_2022.csv.gz ../../common_data/fdic_q_2023.csv.gz ../../common_data/fdic_q_2024.csv.gz

```
cpiaucsl_1989_2024.csv.gz sha256 85b0b55ff552e0b817679263134d0ecbf68a74d8e2553a10c6d508f419c9eae2
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
fdic_q_2011.csv.gz sha256 8780a536cf9fa0b408cb61886ebf214913e04a48627ee1e7d6f7cd25753a8aba
fdic_q_2012.csv.gz sha256 0ed0edc2bceda753b0c60d0efcc9f39d0d97ddf085bc05c53734e540407a779c
fdic_q_2013.csv.gz sha256 64b685a0e7917085895f9b44ae5f86ee05602fd7171205315e914a8ffe54ead1
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

===== sample P: bank-years 10437, banks 1112; multibank subsidiary 5044, no holding company 548, only bank of its holding company 4845
  typical-year components, % of prior equity: ni 11.81 [11.6], iss 0.59 [2.5], div 7.40 [4.7], rep 0.05 [4.2]
  median net issue among issuers 0.40% [0.9] (n 2375); dividends missing 0
  H1 dividends positive: 78.2% (theirs >85%) -> fails
  H2 payouts above earnings: 19.8% (theirs >20%; 21% mean) -> fails
  Table 2 col (3) N 8131: lag 0.141 (0.018) p 0.0000 [theirs 0.208]; equity/assets 0.544 (0.193) p 0.0048 [theirs 0.910]
  Table 2 col (4) N 8036: lag 0.015 (0.022) p 0.4982 [theirs 0.097]; equity/assets 1.353 (0.320) p 0.0000 [theirs 2.331]
  H3 persistence -> fails
  H4 capital management -> holds
  Table 3 col (3) N 9061: change in net income/assets -12.377 (1.117) p 0.0000 [theirs -13.620]
  H5 earnings shock -> holds

----- P, multibank subsidiary: bank-years 5044, banks 685
  H1 dividends positive: 78.9% (theirs >85%) -> reported
  H2 payouts above earnings: 23.2% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 3672: lag 0.099 (0.021) p 0.0000 [theirs 0.208]; equity/assets 0.654 (0.249) p 0.0088 [theirs 0.910]
  Table 2 col (4) N 3602: lag -0.054 (0.026) p 0.0354 [theirs 0.097]; equity/assets 1.607 (0.467) p 0.0006 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 4208: change in net income/assets -12.016 (1.242) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported

----- P, no holding company: bank-years 548, banks 81
  H1 dividends positive: 64.6% (theirs >85%) -> reported
  H2 payouts above earnings: 15.1% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 377: lag 0.204 (0.059) p 0.0005 [theirs 0.208]; equity/assets 0.616 (0.517) p 0.2334 [theirs 0.910]
  Table 2 col (4) N 370: lag -0.040 (0.067) p 0.5529 [theirs 0.097]; equity/assets 0.083 (1.457) p 0.9547 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 434: change in net income/assets -7.310 (1.956) p 0.0002 [theirs -13.620]
  H5 earnings shock -> reported

----- P, only bank of its holding company: bank-years 4845, banks 569
  H1 dividends positive: 79.1% (theirs >85%) -> reported
  H2 payouts above earnings: 16.7% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 3518: lag 0.175 (0.026) p 0.0000 [theirs 0.208]; equity/assets 0.369 (0.328) p 0.2614 [theirs 0.910]
  Table 2 col (4) N 3460: lag 0.045 (0.031) p 0.1489 [theirs 0.097]; equity/assets 0.679 (0.560) p 0.2249 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 4034: change in net income/assets -13.894 (1.601) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported

===== sample A: bank-years 253660, banks 15247; multibank subsidiary 59698, no holding company 50398, only bank of its holding company 143564
  typical-year components, % of prior equity: ni 11.87 [11.6], iss 0.77 [2.5], div 7.39 [4.7], rep 0.06 [4.2]
  median net issue among issuers 0.89% [0.9] (n 24952); dividends missing 0
  H1 dividends positive: 75.5% (theirs >85%) -> reported
  H2 payouts above earnings: 15.9% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 217823: lag 0.230 (0.015) p 0.0000 [theirs 0.208]; equity/assets 0.668 (0.137) p 0.0000 [theirs 0.910]
  Table 2 col (4) N 217131: lag 0.125 (0.015) p 0.0000 [theirs 0.097]; equity/assets 1.124 (0.162) p 0.0000 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 231758: change in net income/assets -15.334 (0.942) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported

----- A, multibank subsidiary: bank-years 59698, banks 7599
  H1 dividends positive: 76.3% (theirs >85%) -> reported
  H2 payouts above earnings: 20.2% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 44481: lag 0.172 (0.015) p 0.0000 [theirs 0.208]; equity/assets 1.011 (0.148) p 0.0000 [theirs 0.910]
  Table 2 col (4) N 43770: lag 0.007 (0.021) p 0.7349 [theirs 0.097]; equity/assets 1.777 (0.235) p 0.0000 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 50328: change in net income/assets -14.757 (0.686) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported

----- A, no holding company: bank-years 50398, banks 5239
  H1 dividends positive: 59.7% (theirs >85%) -> reported
  H2 payouts above earnings: 16.9% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 39339: lag 0.326 (0.014) p 0.0000 [theirs 0.208]; equity/assets 0.381 (0.125) p 0.0024 [theirs 0.910]
  Table 2 col (4) N 38867: lag 0.185 (0.013) p 0.0000 [theirs 0.097]; equity/assets 0.850 (0.205) p 0.0000 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 43754: change in net income/assets -12.211 (0.566) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported

----- A, only bank of its holding company: bank-years 143564, banks 8790
  H1 dividends positive: 80.8% (theirs >85%) -> reported
  H2 payouts above earnings: 13.7% (theirs >20%; 21% mean) -> reported
  Table 2 col (3) N 120128: lag 0.232 (0.019) p 0.0000 [theirs 0.208]; equity/assets 0.453 (0.142) p 0.0014 [theirs 0.910]
  Table 2 col (4) N 119630: lag 0.126 (0.014) p 0.0000 [theirs 0.097]; equity/assets 1.059 (0.187) p 0.0000 [theirs 2.331]
  H3 persistence -> reported
  H4 capital management -> reported
  Table 3 col (3) N 128876: change in net income/assets -17.123 (1.318) p 0.0000 [theirs -13.620]
  H5 earnings shock -> reported
```
