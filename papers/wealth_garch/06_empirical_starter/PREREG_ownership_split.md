# Pre-registration: do E2 and E3 hold apart from holding-company subsidiaries?

Written and committed 12 October 2026, before the ownership data below
were fetched. Nothing in this file may change after the first run. A
change is a new file with a new date, and both are reported.

## Question

E2 (exact targeting fails) and E3 (dividends paid below $y^*$) pool every
bank. For a subsidiary of a multibank holding company, a dividend is a
transfer to the parent and capital may come down from it (Holod and Peek,
HP-Q1 to HP-Q4; Houston, James and Marcus, HJM-Q1 to HJM-Q3). Such a bank
is not the single decision-maker of the model. The question: do E2 and E3
fail for stand-alone banks too, or only for subsidiaries?

## Data

- The pinned panel of E2 and E3, `fdic_distress_panel_2015_2024.csv.gz`,
  sha256 `3cb78c0aa33691037f275eaf7bea5a75d02958161e0f8f7f2318fae4b0cc879b`,
  unchanged. Every flow, episode and proxy is computed from it exactly as
  in `distress_targeting.py` and `dividend_location.py`.
- New: FDIC BankFind API, endpoint `financials`, every institution, report
  dates 2015Q1 to 2024Q4, fields `CERT`, `REPDTE`, `RSSDHCR`, `HCTMULT`.
  Saved compressed and pinned by sha256 in the results file. Joined to the
  panel on (`CERT`, `REPDTE`). Nothing from 2025 or later is fetched.
- Definitions: the FDIC workbook (lit record `fdic_2026`): `HCTMULT` flags
  membership of a multibank holding company, 1 = yes, 0 = no; `RSSDHCR` is
  the Federal Reserve identifier of the regulatory high holder.

## Groups

Classified per bank-quarter:

- M, multibank subsidiary: `HCTMULT` = 1.
- S, stand-alone: `HCTMULT` = 0. This covers banks with no holding company
  and banks in a one-bank holding company, the split of Holod and Peek
  (HP-Q11).
- Unclassified: any other value, or no ownership row. Counted, excluded
  from both groups.

An E2 episode takes the group of its pre-issue quarter. An E3 bank-quarter
takes its own quarter's group.

Reported, not used for a verdict: within S, banks with no holding company
(`RSSDHCR` empty or 0) apart from one-bank holding companies; and the
number of banks whose group changes within 2015-2024.

## Tests

E3 per group: the share of bank-quarters below $y^*=1.571518$ with a
dividend paid in the quarter, with the bins of `dividend_location.py`.
The model predicts 0%.

- holds: the share is at most 5%;
- fails: the share exceeds 5%.

E2 per group: the within-bank regression of `distress_targeting.py` on
episodes of that group, for the proxies valid in E2 (P1 and P2), with the
same enough-data rule (at least 30 episodes in banks with at least two
classified episodes, across at least 15 banks) and the same verdict rule
on $b_{\text{post}}$. Group verdict: holds or fails if both proxies with
enough data give that verdict; not measured otherwise or if neither has
enough data. Proxy validity within each group is reported, not used.

Also reported for E2, per group: the share of the episodes' total
recapitalisation coming from stock sales (`EQCSXQ`) and from transactions
with the parent (`EQCBHCTR`).

## Reading fixed in advance

- S fails E2 and E3: the failures are not an artefact of subsidiaries'
  internal transfers. E2 and E3 stand, and are stated for stand-alone
  banks as well as all banks.
- S holds where M fails: the failure comes from subsidiaries. E2 or E3 is
  restated as a statement about subsidiaries, and the empirical analysis
  moves to stand-alone banks or to the holding company.
- Otherwise: reported as found; no restatement.

## Known limits

- If the API returns the current ownership for past quarters rather than
  the ownership at each quarter, banks that changed structure are
  misclassified in some quarters. The count of banks whose group changes
  is reported; zero changes would suggest a current-structure field.
- A one-bank holding company's subsidiary also pays its dividends to a
  parent; Holod and Peek treat it as stand-alone, and so does this test.
