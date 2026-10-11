# Pre-registration: do distress recapitalisations target exactly?

Written and committed 11 October 2026, before the data below were fetched.
Nothing in this file may change after the first run. A change is a new
file with a new date, and both are reported.

## Question

The narrower question agreed with the author. When a US bank recapitalises
because of distress, does it behave like the model's impulse?

With a fixed issuance cost charged per unit of liabilities and a linear
proportional cost, the impulse goes to one target $y_{\text{post}}$ with
$V'(y_{\text{post}})=1+\kappa$, whatever the starting point. Within a bank,
with its parameters fixed, every recapitalisation ends at the same $x$. The
prediction holds at every parameter value, so it does not depend on
$\sigma$.

## Data

- FDIC BankFind API, endpoint `financials`, every institution, report dates
  2015Q1 to 2024Q4. Fields `CERT`, `REPDTE`, `ASSET`, `LIAB`, `NETINC`,
  `EQCSXQ`, `EQCBHCTR`, `EQCMRG`, `EQCDIV`, `RBCT1J`, `RWAJT`.
- Definitions: the FDIC workbook `All Financial Reports.xlsx`, sheet
  "Reference-Variables&Definitions", sha256
  `6413c93064c16c63d4850221b25181f03ea51d086cd8f68d365cb070f383ab65`.
- Amounts are in thousands of dollars (checked on CERT 628, assets
  3,395,126,000).
- The fetched panel is saved compressed and pinned by sha256 in the results
  file.

## Flows

- `NETINC`, `EQCBHCTR`, `EQCMRG` and `EQCDIV` are year to date. Quarterly
  flow: Q1 is the reported value; Q2 to Q4 are the reported value less the
  previous quarter's in the same year. A missing previous quarter makes the
  flow missing, never zero.
- `EQCSXQ` is quarterly as reported.
- Institutions under $100 million file the year-to-date equity items only
  in December. A bank-quarter enters only if `ASSET` is at least 100,000 in
  it and in the preceding quarter.

## Episodes

- Recapitalisation in quarter $q$: $\text{recap}_q=\texttt{EQCSXQ}_q+\Delta\texttt{EQCBHCTR}_q$,
  stock sales plus capital from the parent.
- Episode: $\text{recap}_q\ge0.005\,\texttt{ASSET}_q$, $\Delta\texttt{EQCMRG}_q=0$
  (no business-combination change), and the preceding quarter is present.
- $x=\texttt{ASSET}/\texttt{LIAB}$. $x_{\text{pre}}$ at the preceding quarter
  end, $x_{\text{post}}$ at the end of $q$, $f=\text{recap}_q/\texttt{ASSET}_q$.
  Episodes with $x_{\text{pre}}$ or $x_{\text{post}}$ outside $(1,2)$ are
  dropped.

## Candidate distress proxies

Each uses only information up to the pre-issue quarter. None uses
$x_{\text{post}}$.

| ID | Proxy | Definition |
|---|---|---|
| P1 | Losses | Quarterly `NETINC` summed over the four quarters ending at the pre-issue quarter is negative |
| P2 | Capital decline | Book $E/A=1-1/x$ at the pre-issue quarter is at least 1 percentage point below its value four quarters earlier |
| P3 | Low regulatory capital | `RBCT1J`/`RWAJT` at the pre-issue quarter is in the bottom decile of all bank-quarters in the sample |
| P4 | Shrinking | `ASSET` at the pre-issue quarter is no higher than four quarters earlier |

## Validity criterion

The marker is used by no proxy and does not involve $x_{\text{post}}$: no
dividend (`EQCDIV` quarterly flow zero) in the issuing quarter and in the
quarter before it. A distressed bank does not pay dividends while raising
capital.

A proxy is valid if its classified episodes have a higher no-dividend share
than the episodes it does not classify, by a one-sided two-proportion test
at 5%.

A proxy has enough data if at least 30 classified episodes fall in banks
with at least two classified episodes, across at least 15 banks.

## Test

- Within-bank regression on banks with at least two classified episodes,
  each variable demeaned by bank: $x_{\text{post}}$ on $f$ gives
  $b_{\text{post}}$, and $x_{\text{pre}}$ on $f$ gives $b_{\text{pre}}$.
- Standard errors clustered by bank.
- Exact targeting predicts $b_{\text{post}}=0$. A fixed-size top-up predicts
  $b_{\text{pre}}=0$. $\bar x$ is the mean of $x_{\text{pre}}$, and
  $b_{\text{post}}-b_{\text{pre}}\approx\bar x$ is the accounting check.

## Verdict rule

For each valid proxy with enough data:

- holds: the 95% interval for $b_{\text{post}}$ contains 0 and excludes
  $\bar x/2$;
- fails: the 95% interval for $b_{\text{post}}$ excludes 0;
- not measured: otherwise.

Overall: holds or fails if every valid proxy with enough data gives that
verdict. Otherwise, or if no proxy is valid with enough data, not measured.

## Reported in full

For P1 to P4, for the episodes no proxy classifies, and for all episodes:
the number of episodes and banks, the validity statistics, $b_{\text{pre}}$,
$b_{\text{post}}$, their clustered standard errors, and the verdict. Also
reported, and not used for the verdict: the same table for 2015 to 2019 and
for 2020 to 2024.

## Known limits

- Parameters are taken as constant within a bank over ten years. A bank
  whose costs or risk changed has a moving target, which the test reads as
  a failure.
- $x_{\text{post}}$ includes the rest of the quarter's earnings and asset
  changes.
- `EQCBHCTR` also records dividends paid to the parent in the form of
  property, which enter with a negative sign.
