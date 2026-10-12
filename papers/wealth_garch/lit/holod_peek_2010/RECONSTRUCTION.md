# Reconstruction: Holod and Peek (2010)

## Source read

- Dmytro Holod and Joe Peek, "Capital Constraints, Asymmetric Information,
  and Internal Capital Markets in Banking: New Evidence", working paper
  dated 5 December 2006 (SSRN 891563), same title as the article in
  *Journal of Money, Credit and Banking* 42(5):879-906, 2010. 45 PDF pages:
  an unnumbered title and abstract page, then pages 1 to 44 (printed page =
  PDF page - 1). Read in full. Evidence tag [F] for this version. The
  published article was not read; the author accepted working papers on
  2026-10-12.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `ssrn-891563.pdf`, id
  `1Tm8uz1LBytkh3tNu-skQrbCc2cGaN6TG`), uploaded 2026-10-12. Cached at
  `~/.cache/wealth_garch/holod_peek_2006.pdf`, sha256
  `04094a0fcf50d073d452201a50551224d056931055f8fb7da2687414539a9f01`.

## Primitives

- Unit: an FDIC-insured commercial bank, from the Call Reports. A bank is
  a subsidiary of a multibank holding company (MBHC) or stand-alone, which
  covers both no holding company and a one-bank holding company (n. 8).
- Capital ratio: the unweighted leverage ratio, because the risk-based
  ratio is not available for much of the period (n. 10).
- CapTrans: dividends with a negative sign, plus capital sales and
  acquisitions, plus other transactions with the parent (p. 14), scaled by
  the previous period's assets. Positive means capital to the bank.
- Net loan sales: loan sales less loan purchases, quarterly, reported only
  1987:Q2 to 1993:Q4 (p. 14).

## Derivation chain

1. Two motives move funds inside an MBHC: each subsidiary's own capital
   requirement (regulatory) and differing loan opportunities (efficiency)
   (p. 3).
2. Two mechanisms: capital moves to the subsidiaries that need it, and
   loans move from low-capital to high-capital subsidiaries through an
   internal secondary loan market that avoids the lemons problem (pp. 2-3,
   10).
3. Preliminary evidence, Tables 1 to 5: higher-capital subsidiaries pay
   more dividends to the parent and lower-capital subsidiaries receive more
   capital through capital sales and other transactions (p. 15).
4. Equation (1), annual 1987-2004, random effects with time effects:
   CapTrans falls with the bank's own capital ratio and rises with the
   rest of the MBHC's (Table 6).
5. Equations (2) and (3), quarterly 1987:Q2-1993:Q4: net loan sales fall
   with the bank's own capital ratio, and almost three times as steeply for
   MBHC subsidiaries as for stand-alone banks (Table 8); the gap survives
   for non-publicly traded banks (Table 10).

## Roles in this bundle

- The paper's CapTrans uses the same Call Report schedule (RI-A) as our
  recapitalisation measure, whose parent-transfer field `EQCBHCTR` is one
  of its components. Their Table 1 shows that, for MBHC subsidiaries,
  dividends rise with the capital ratio and capital flows in at low ratios:
  internal transfers, not decisions of a stand-alone firm.
- Footnote 8 gives the split this bundle adopts for the ownership test:
  MBHC subsidiaries against stand-alone banks (no holding company or a
  one-bank holding company).

## What could not be reconstructed

- The regressions (Tables 6 to 10): the data construction, random effects
  and the Hausman test are described but not reproducible from the text.
- The published version's changes.
