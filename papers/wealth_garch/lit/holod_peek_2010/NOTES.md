# Notes: Holod and Peek (2010)

## Verified

- Every quotation in `CLAIMS.md` is found verbatim on its stated page of the
  pinned PDF (offset -1). Controls: a fabricated quotation, a true
  quotation on the wrong page, and a page before the first PDF page are all
  refused.
- The Lean results in `CLAIMS.md` are declared in
  `lean/mathlib/HolodPeek.lean`, which builds with no `sorry` and audits
  within propext, Classical.choice and Quot.sound. Every total in Table 1
  is the exact sum of its components; Table 2 agrees within rounding.

## Not verified

- The regressions of Tables 6 to 10 and every coefficient.
- The published version.

## Discrepancies

- HP-D2 (rounding in Table 2), HP-D3 (quartiles for quintiles), HP-D4
  (a repeated coefficient index) and HP-D5 (a misspelt name). None changes
  a result.

## Controls

- `HolodPeek.control_dividends_enter_negatively`.
- `HolodPeek.control_not_three_times`.

## Consequences for this bundle

- For a subsidiary of a multibank holding company, a dividend is a
  transfer to the parent, and the parent decides it with the subsidiary's
  capital ratio in view (HP-Q2 to HP-Q4). Such a bank is not the single
  decision-maker of the payout models (Peura and Keppo, Bolton, Chen and
  Wang). The dividend timing test (E3) and the recapitalisation test (E2)
  pool these banks with stand-alone banks.
- The recapitalisation measure of E2 includes `EQCBHCTR`, a component of
  CapTrans: part of what E2 calls recapitalisation is the parent moving
  capital.
- The split of n. 8 (HP-Q11) is the one adopted for the ownership test.
