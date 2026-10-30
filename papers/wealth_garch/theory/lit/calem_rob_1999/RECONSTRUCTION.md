# Reconstruction: Calem and Rob (1996)

## Source read

- From the author's Google Drive (folder `wealth_garch`, file
  `199612pap.pdf`, id `1z8zrRG_3t5mRRr1zucUrNmVcYoRJYx8O`), downloaded on
  30 October 2026 and pinned as `~/.cache/wealth_garch/calem_rob_1996.pdf`,
  sha256 `022ee640da497d4f9e2f1799bdf1f5112e6c8015c11477479e1477087f7713eb`.
- Paul S. Calem and Rafael Rob, "The Impact of Capital-Based Regulation on
  Bank Risk-Taking: A Dynamic Model", Federal Reserve Board, Finance and
  Economics Discussion Series 96-12, March 1996. Read in full, 47 PDF pages.
- Published as "The Impact of Capital-Based Regulation on Bank Risk-Taking",
  Journal of Financial Intermediation 8(4), 1999 (OpenAlex W1974244766), not read.

## The model

- Discrete time, infinite horizon. Assets fixed at one, funded by deposits D
  and capital C. Each period the bank chooses the risky share R of a safe asset
  with gross return x and a risky asset that promises y0 and loses a random
  fraction u. Deposits cost a premium that is higher below the requirement C*
  (eq. 1).
- The net return z(C, R, u) (eq. 2). Above C* the excess is paid to
  stockholders; between 0 and C* nothing is paid and the return becomes next
  period's capital; at or below 0 the bank fails (pp. 8 to 9).
- Value V(C) by eq. (5), solved on a grid of capital in steps of 0.002 and
  twenty risky shares (pp. 10 to 11), calibrated to Call Report data for 1984
  to 1993 (Section 3).

## Results read

- Section 4: the risky share is roughly U-shaped in capital (Tables 3a, 3b).
- Section 5: a requirement of 0.07 in place of 0.06 raises the risk taken by a
  well-capitalized bank (Table 4); a premium surcharge below the requirement
  widens maximal risk-taking among undercapitalized banks and leaves
  well-capitalized banks unchanged (Table 5).
- Section 6: the risk-based rule C* = 0.06 + n(R - R0) raises risk when lax and
  lowers it when stringent (Table 6).
