# Reconstruction: Baron (2020)

## Source read

- Matthew Baron, "Countercyclical Bank Equity Issuance", accepted
  manuscript dated 13 January 2020, "Forthcoming at Review of Financial
  Studies" (SSRN 2902505), with its Online Appendix: 127 PDF pages. Read in
  full: the paper, the Online Appendix text and the captions of every
  appendix figure and table. Evidence tag [F]. The published article,
  *Review of Financial Studies* 33(9):4186-4230, 2020, was not read.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `ssrn-2902505.pdf`, id
  `1lr4JhDO1p7MNSd2Wd5AiVzKOW9Nffjdh`), uploaded 2026-10-12. Cached at
  `~/.cache/wealth_garch/baron_2020.pdf`, sha256
  `5052f9fc20402a9e208b7aced7897504341233b0b0f7addd47a36e98c63a0891`.

## Primitives

Page references in this record are PDF pages (BAR-D1).

- Model (Online Appendix A): three periods, risk-neutral bankers and
  creditors; uniform asset shock; default losses split $(1-\gamma)$ to
  creditors, $\gamma$ to government; below the requirement $\Phi$ the bank
  is forced to recapitalise to $\Phi$ at a proportional cost $c$; a credit
  supply shock $\delta$ at $t=1$.
- Data: Compustat and CRSP commercial banks and bank holding companies,
  quarterly 1980-2012, sorted by size rank; flows normalised by book
  equity; mergers excluded (p. 15).
- Credit expansion: the standardised year-over-year change in bank credit
  to GDP (p. 15).

## Derivation chain

1. Model: creditor discipline makes issuance procyclical; guarantees
   blunt it, so bankers postpone equity and pay out in the boom, and are
   forced to recapitalise in the bust (pp. 7-13; A9, A13, A14).
2. Panel regressions (Table 2): net issuance minus dividends falls with
   credit expansion, most at the largest banks (pp. 19-23).
3. Magnitudes (Table 3) and the size of issuance (Table 4): small banks
   issue often and in large amounts (pp. 23-25).
4. Fitch support ratings (Table 5) absorb most of the size factor
   (pp. 25-29).
5. Landesbanken (Tables 7-8): after guarantees were removed, debt yields
   respond to issuance and payouts, and the banks that lost more
   guarantees issued and retained more equity (pp. 29-37).
6. Alternatives (governance, CEO optimism, market timing, VaR targeting)
   do not explain the size factor (pp. 38-41, Online Appendix H).

## Roles in this bundle

- A model with forced recapitalisation to the minimum at a proportional
  cost (BAR-Q2, BAR-Q3): the minimal top-up, as in Løkka and Zervos, not
  the impulse to a target above the minimum of Peura and Keppo.
- Evidence that recapitalisation at large banks is concentrated in busts
  and payouts in booms, and that small banks issue frequently and in
  large amounts (BAR-Q5 to BAR-Q8).

## What could not be reconstructed

- The calibrated figures of the model and every regression estimate.
- The appendix tables' cells beyond those quoted.
