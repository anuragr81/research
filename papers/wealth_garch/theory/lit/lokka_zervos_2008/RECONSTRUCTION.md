# Reconstruction: Løkka and Zervos (2008)

## Source read

- Arne Løkka and Mihail Zervos, "Optimal dividend and issuance of equity
  policies in the presence of proportional costs", preprint dated 14 July
  2005, King's College London. 14 pages, read in full. Evidence tag [F]
  for this version. BCVW cites the published version, *Insurance:
  Mathematics and Economics* 42(3):954-961, 2008, which was not read.
- Supplied by the author on Google Drive (folder
  `1pnev5GIF5BsJMmmNV2xmpm_pwcOOlbEb`, file `Lokka-Zervos.pdf`), uploaded
  2026-10-12. Cached at `~/.cache/wealth_garch/lokka_zervos_2008.pdf`,
  sha256 `2385581e05465444599f8d59bed5cfb6fded7522b0e8b92d7bce246f67083e45`.
  Printed page = PDF page.

## Primitives

- Reserves $dX=\mu\,dt+\sigma dW-dL+dG$, with dividends $L$ and equity
  issuance $G$ both singular. Bankruptcy at the first entry into
  $(-\infty,0)$.
- Objective $E[\int e^{-rt}dL-\beta\int e^{-rt}dG]$, with $\beta>1$ the
  proportional cost of issuance (eq. 2.1). No fixed cost.

## Derivation chain

1. Section 3, no issuance (Jeanblanc and Shiryaev). Dividend barrier
   $x^{**}$ from smooth pasting (3.8)-(3.10); $x^{**}>0$ iff $\mu>0$
   (3.12). Theorem 3.1.
2. Section 4, reserves kept nonnegative. Issue at zero, dividends at
   $x^*$, with $x^*$ the unique root of (4.9) and $c_1,c_2$ of (4.10).
   Lemma 4.1 (uniqueness, concavity), Theorem 4.2.
3. Section 5, the general problem. The value function is the larger of the
   two auxiliary solutions, chosen by conditions (5.3)-(5.4) on the data.
   Theorem 5.1, proved by an Itô-Tanaka verification argument.

## Roles

- A firm with reserves, not a bank. No risk choice, no regulatory cap, no
  delay and no fixed cost.
- The proportional-cost benchmark BCVW names (BCVW p. 2, [21]).

## What could not be reconstructed

- Nothing in the text read. The published version was not read.
