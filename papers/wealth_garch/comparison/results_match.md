# Testing thread: which banks of sample B have a listed parent

Run 24 October 2026,
under PREREG_comparison_12.md (commit 70691b75),
under PREREG_comparison_13.md (commit 1ea9bfbc).
No price is used here.

## Findings

- 38 of the 61 banks of sample B are covered, through 32 parents: the 17
  banks of the eleven firms of PREREG_comparison_9.md, and 21 new banks,
  one for each of 21 new parents. The decision of PREREG_comparison_12.md
  is therefore taken on the 21 new banks.
- Hand check of the 21 new matches: each NIC legal name and listed name
  agree word for word up to the allowed differences. Three distinct
  parents carry "First Financial" names (First Financial Bancorp, FFBC;
  First Financial Bankshares, FFIN; First Financial Corporation, THFF);
  each matched its own name only. The NIC names are those on the active
  records (October 2026), as PREREG_comparison_13.md provides; whether a
  parent carried another name in 2014 is not checked here.
- 23 banks are not covered, through 22 parents:
  - foreign parents (8 banks, 7 parents): HSBC Holdings (United Kingdom),
    Toronto-Dominion Bank (Canada, two banks), Bank of Montreal (Canada),
    Mitsubishi UFJ (Japan), Mercantil Servicios Financieros (Venezuela),
    SNBNY Holdings (Gibraltar), Banco de Sabadell (Spain);
  - United States parents whose NIC names match no listed common stock in
    the Nasdaq Trader files (14 banks, 14 parents): City National, Webster
    Financial, TCF Financial, FirstMerit, MB Financial, National Penn
    Bancshares, Lauritzen, Intrust Financial, Otto Bremer Trust, Amarillo
    National Bancorp, Broadway Bancshares, A.N.B. Holding, Lone Star
    National Bancshares--Texas, and First National Bank Alaska (no holding
    company; the bank itself). Why each is unlisted (absorbed, failed,
    private, or traded elsewhere) is not checked here; where it was
    absorbed or failed, leaving it out favours survivors, as
    PREREG_comparison_12.md says;
  - one parent (RSSD 4199229, bank cert 23220) with no record in either
    NIC attributes file.

## Stooq pages for the 21 new parents

Download as before: daily, full history, "Pobierz dane w pliku csv".

ASB, BOKF, CAC, CBU, CHCO, FFBC, FFIN, FHN, FNB, FRME, NBTB, ONB, OSBC,
PEBO, PRK, SFNC, TCBI, THFF, TRMK, UMBF, VLY; the links are printed below.

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
Pinned: match_tickers.py sha256 025c132c26c76070aa461894f5b3744976c5bf94d15c87155be9e6ca553da1ee
Pinned: ../replication/peura_keppo_2006/stage2.py sha256 477e32dea9a6064cc4aeaad0ac42956841dfd08947f30caf19929dca0127f0fa
Pinned: ../replication/peura_keppo_2006/stage1_barriers.py sha256 72d1942f940579564c087780cee8968083c07c6a2f494f6964d05a505b97afa6
Pinned: ../06_empirical_starter/distress_targeting.py sha256 03453a5bf21429bf68f74cf79aa06d0e5f48b5fcfc1af60a35dbfff7e6bd9783
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/nasdaqlisted.txt sha256 df221177204f1c56add0c52adcd8f19c7371bde662eb34c8b32c5bba8bf4f5b0
Pinned: /home/anuragr/.cache/wealth_garch/nasdaqtrader_20261020/otherlisted.txt sha256 cab8d0e5cb077acb38ebe201113a909b7ee3784c32249281d83399c0b0d5732f

Reproduce: python3 match_tickers.py

```
sample B: 61 banks

covered parents: 32, banks 38
   1199563 ASB    NIC ['ASSOCIATED BANC-CORP'] | listed 'Associated Banc-Corp Common Stock' | banks 5296 (RSSDHCR)
   1073757 BAC    NIC ['BANK OF AMERICA CORPORATION'] | listed 'kept from PREREG_comparison_9.md' | banks 3510 (RSSDHCR), 25178 (RSSDHCR)
   3587146 BNY    NIC ['BANK OF NEW YORK MELLON CORPORATION, THE'] | listed 'kept from PREREG_comparison_9.md' | banks 7946 (RSSDHCR)
   1883693 BOKF   NIC ['BOK FINANCIAL CORPORATION'] | listed 'BOK Financial Corporation - Common Stock' | banks 4214 (RSSDHCR)
   1951350 C      NIC ['CITIGROUP INC.'] | listed 'kept from PREREG_comparison_9.md' | banks 7213 (RSSDHCR)
   1130249 CAC    NIC ['CAMDEN NATIONAL CORPORATION'] | listed 'Camden National Corporation - Common Stock' | banks 4255 (RSSDHCR)
   1048867 CBU    NIC ['COMMUNITY FINANCIAL SYSTEM, INC.'] | listed 'Community Financial System, Inc. Common Stock' | banks 6989 (RSSDHCR)
   1076262 CHCO   NIC ['CITY HOLDING COMPANY'] | listed 'City Holding Company - Common Stock' | banks 17735 (RSSDHCR)
   2277860 COF    NIC ['CAPITAL ONE FINANCIAL CORPORATION'] | listed 'kept from PREREG_comparison_9.md' | banks 4297 (RSSDHCR)
   1071276 FFBC   NIC ['FIRST FINANCIAL BANCORP'] | listed 'First Financial Bancorp. - Common Stock' | banks 6600 (RSSDHCR)
   1102312 FFIN   NIC ['FIRST FINANCIAL BANKSHARES, INC.'] | listed 'First Financial Bankshares, Inc. - Common Stock' | banks 3066 (RSSDHCR)
   1094640 FHN    NIC ['FIRST HORIZON CORPORATION'] | listed 'First Horizon Corporation Common Stock' | banks 4977 (RSSDHCR)
   3005332 FNB    NIC ['F.N.B. CORPORATION'] | listed 'F.N.B. Corporation Common Stock' | banks 7888 (RSSDHCR)
   1208559 FRME   NIC ['FIRST MERCHANTS CORPORATION'] | listed 'First Merchants Corporation - Common Stock' | banks 4365 (RSSDHCR)
   1068191 HBAN   NIC ['HUNTINGTON BANCSHARES INCORPORATED'] | listed 'kept from PREREG_comparison_9.md' | banks 6560 (RSSDHCR)
   1039502 JPM    NIC ['JPMORGAN CHASE & CO.'] | listed 'kept from PREREG_comparison_9.md' | banks 628 (RSSDHCR), 23702 (RSSDHCR)
   1068025 KEY    NIC ['KEYCORP'] | listed 'kept from PREREG_comparison_9.md' | banks 17534 (RSSDHCR)
   1139279 NBTB   NIC ['NBT BANCORP INC.'] | listed 'NBT Bancorp Inc. - Common Stock' | banks 7230 (RSSDHCR)
   1098303 ONB    NIC ['OLD NATIONAL BANCORP'] | listed 'Old National Bancorp - Common Stock' | banks 3832 (RSSDHCR)
   1206911 OSBC   NIC ['OLD SECOND BANCORP, INC.'] | listed 'Old Second Bancorp, Inc. - Common Stock' | banks 3603 (RSSDHCR)
   1070578 PEBO   NIC ['PEOPLES BANCORP INC.'] | listed 'Peoples Bancorp Inc. - Common Stock' | banks 6544 (RSSDHCR)
   1069778 PNC    NIC ['PNC FINANCIAL SERVICES GROUP, INC., THE'] | listed 'kept from PREREG_comparison_9.md' | banks 6384 (RSSDHCR)
   1142336 PRK    NIC ['PARK NATIONAL CORPORATION'] | listed 'Park National Corporation Common Stock' | banks 6653 (RSSDHCR)
   1094828 SFNC   NIC ['SIMMONS FIRST NATIONAL CORPORATION'] | listed 'Simmons First National Corporation - Common Stock' | banks 3890 (RSSDHCR)
   2706735 TCBI   NIC ['TEXAS CAPITAL BANCSHARES, INC.'] | listed 'Texas Capital Bancshares, Inc. Common Stock' | banks 34383 (RSSDHCR)
   1208595 THFF   NIC ['FIRST FINANCIAL CORPORATION'] | listed 'First Financial Corporation  - Common Stock' | banks 4382 (RSSDHCR)
   1079562 TRMK   NIC ['TRUSTMARK CORPORATION'] | listed 'Trustmark Corporation - Common Stock' | banks 4988 (RSSDHCR)
   1049828 UMBF   NIC ['UMB FINANCIAL CORPORATION'] | listed 'UMB Financial Corporation - Common Stock' | banks 8273 (RSSDHCR)
   1119794 USB    NIC ['U.S. BANCORP'] | listed 'kept from PREREG_comparison_9.md' | banks 6548 (RSSDHCR)
   1048773 VLY    NIC ['VALLEY NATIONAL BANCORP'] | listed 'Valley National Bancorp - Common Stock' | banks 9396 (RSSDHCR)
   1120754 WFC    NIC ['WELLS FARGO & COMPANY'] | listed 'kept from PREREG_comparison_9.md' | banks 3511 (RSSDHCR), 13718 (RSSDHCR), 27389 (RSSDHCR)
   1027004 ZION   NIC ['ZIONS BANCORPORATION'] | listed 'kept from PREREG_comparison_9.md' | banks 2270 (RSSDHCR), 24107 (RSSDHCR), 20626 (RSSDHCR)

not covered parents: 22, banks 23
   1857108 NIC ['HSBC HOLDINGS PLC'] | country UNITED KINGDOM  (OTHER) | banks 57890 (RSSDHCR)
   1238565 NIC ['TORONTO-DOMINION BANK, THE'] | country CANADA | banks 18409 (RSSDHCR), 33947 (RSSDHCR)
   2961897 NIC ['MITSUBISHI UFJ FINANCIAL GROUP, INC.'] | country JAPAN | banks 22826 (RSSDHCR)
   1231333 NIC ['BANK OF MONTREAL'] | country CANADA | banks 16571 (RSSDHCR)
   1027518 NIC ['CITY NATIONAL CORPORATION'] | 0 candidates [] | banks 17281 (RSSDHCR)
   1145476 NIC ['WEBSTER FINANCIAL CORPORATION'] | 0 candidates [] | banks 18221 (RSSDHCR)
   2389941 NIC ['TCF FINANCIAL CORPORATION'] | 0 candidates [] | banks 28330 (RSSDHCR)
   1070804 NIC ['FIRSTMERIT CORPORATION'] | 0 candidates [] | banks 13675 (RSSDHCR)
   1066209 NIC ['LAURITZEN CORPORATION'] | 0 candidates [] | banks 5452 (RSSDHCR)
   1090987 NIC ['MB FINANCIAL, INC.'] | 0 candidates [] | banks 3628 (RSSDHCR)
   1117026 NIC ['NATIONAL PENN BANCSHARES, INC.'] | 0 candidates [] | banks 7414 (RSSDHCR)
   2605968 NIC ['MERCANTIL SERVICIOS FINANCIEROS, C.A.'] | country VENEZUELA | banks 22953 (RSSDHCR)
   2368106 NIC ['SNBNY HOLDINGS LIMITED'] | country GIBRALTAR | banks 26876 (RSSDHCR)
   1064278 NIC ['INTRUST FINANCIAL CORPORATION'] | 0 candidates [] | banks 4799 (RSSDHCR)
   1121340 NIC ['OTTO BREMER TRUST'] | 0 candidates [] | banks 12923 (RSSDHCR)
   4199229 NIC [] | country ? | banks 23220 (RSSDHCR)
   1981384 NIC ['BANCO DE SABADELL, S.A.'] | country SPAIN | banks 21837 (RSSDHCR)
   1107205 NIC ['AMARILLO NATIONAL BANCORP, INC.'] | 0 candidates [] | banks 14531 (RSSDHCR)
    114260 NIC ['FIRST NATIONAL BANK ALASKA'] | 0 candidates [] | banks 16130 (itself)
   1106879 NIC ['BROADWAY BANCSHARES, INC.'] | 0 candidates [] | banks 15797 (RSSDHCR)
   1895007 NIC ['A.N.B. HOLDING COMPANY, LTD.'] | 0 candidates [] | banks 23474 (RSSDHCR)
   2325350 NIC ['LONE STAR NATIONAL BANCSHARES--TEXAS, INC.'] | 0 candidates [] | banks 24347 (RSSDHCR)

Stooq pages for the new tickers
  https://stooq.com/q/d/?s=asb.us&i=d
  https://stooq.com/q/d/?s=bokf.us&i=d
  https://stooq.com/q/d/?s=cac.us&i=d
  https://stooq.com/q/d/?s=cbu.us&i=d
  https://stooq.com/q/d/?s=chco.us&i=d
  https://stooq.com/q/d/?s=ffbc.us&i=d
  https://stooq.com/q/d/?s=ffin.us&i=d
  https://stooq.com/q/d/?s=fhn.us&i=d
  https://stooq.com/q/d/?s=fnb.us&i=d
  https://stooq.com/q/d/?s=frme.us&i=d
  https://stooq.com/q/d/?s=nbtb.us&i=d
  https://stooq.com/q/d/?s=onb.us&i=d
  https://stooq.com/q/d/?s=osbc.us&i=d
  https://stooq.com/q/d/?s=pebo.us&i=d
  https://stooq.com/q/d/?s=prk.us&i=d
  https://stooq.com/q/d/?s=sfnc.us&i=d
  https://stooq.com/q/d/?s=tcbi.us&i=d
  https://stooq.com/q/d/?s=thff.us&i=d
  https://stooq.com/q/d/?s=trmk.us&i=d
  https://stooq.com/q/d/?s=umbf.us&i=d
  https://stooq.com/q/d/?s=vly.us&i=d
```
