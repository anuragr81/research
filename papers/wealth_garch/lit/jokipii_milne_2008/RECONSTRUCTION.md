# Reconstruction: Jokipii and Milne (2008)

## Source read

- Terhi Jokipii and Alistair Milne, "The cyclical behaviour of European
  bank capital buffers", *Journal of Banking & Finance* 32(8):1440-1451,
  2008. Published version, 12 PDF pages (printed page = PDF page + 1439).
  Read in full. Evidence tag [F]. The regression tables are garbled in the
  text layer and were not read; Table 1 was read on the page image.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file
  `The_cyclical_behaviour_of_European_bank.pdf`, id
  `1oK3xBkcFdOSlzktl1l9rrEUSqgnXcotS`), uploaded 2026-10-12 from
  academia.edu. Cached at `~/.cache/wealth_garch/jokipii_milne_2008.pdf`,
  sha256 `99bb1800ec43831d043f189312da5cfcf94d1e3fb0148ffbf9b0375e0394da85`.
  It supersedes the Bank of Finland working paper 17/2006 found earlier.

## Primitives

- Unit: European commercial, savings and co-operative banks, Bankscope,
  annual 1997-2004; 468 banks (p. 1443).
- Buffer BUF: total (Tier 1 plus Tier 2) risk-weighted capital ratio less
  the national minimum, 8% or higher (Table 2).
- Partial adjustment with quadratic adjustment costs (1a)-(1b); two-step
  Arellano-Bond GMM.
- Cycle: real GDP growth, domestic or EU-wide.

## Derivation chain

1. Buffers are large (Table 1): EU15 average 2.93 points, RAM10 5.14.
2. Hypotheses on co-movement with the cycle, positive or negative
   (p. 1441).
3. Estimates (Tables 5-7): negative co-movement for EU25, EU15, EA and
   DK-SE-UK, and for large, commercial and savings banks; positive for
   RAM10, small and co-operative banks; adjustment around 66% a year.
4. Reading: small and co-operative banks rely on retained earnings because
   issuance is costly or impossible (p. 1450).

## Roles in this bundle

- The authors state the asymmetry of adjustment costs, easier to reduce
  capital than to raise it, and do not test it (JM-Q1). Our model's
  asymmetry is this one.
- Partial adjustment toward a desired buffer, the same family as Berger et
  al., on European data.

## What could not be reconstructed

- The GMM estimates (Tables 5-7, garbled in the text layer).
