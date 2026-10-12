# Notes — Corcoran (1984), pass 1 (read in full)

Run `python3 lit/corcoran_1984/verify_corcoran.py` from the repository root.
It checks the quotations verbatim, on the page named, against the cached text
(pinned by sha256), pins the page image of p.92 for the three quotations whose
text layer is garbled, runs a reversed-quote control, and measures
`lean/mathlib/Corcoran.lean` (see `LEAN`).

## Verdict for C5, C6 and C13

It is a free-entry paper, and the closest of the three unread ones to our
entry rule: entry is decided by comparing the return with the outside return.
But the players are identical, their number is continuous and there is no
cost spread or wealth. So:

- **C5** (the sign rule of C1 is new): nothing in the paper signs a spread of
  wealth or of entry costs. Its entry rule is a threshold on a common return,
  which is the rule of M5 for identical players.
- **C6** (count invariance with heterogeneous costs): the long-run count
  `r/(r-1)` does not depend on the prize, which is a count invariance of a
  kind, but it is invariance in the prize at identical costs, and the count
  is not an integer. It says nothing about invariance when costs differ.
- **C13** (the share of the prize taken by those who enter): a player who
  does not enter does not compete, so there is no share to compute for
  non-entrants.

## What it is useful for

The long-run count `r/(r-1)` does not depend on the prize, because what a
player spends is proportional to the prize (`aStar` is proportional to `R`),
so profit per player is proportional to `R` and the count at which it is zero
does not move. In the model here the cost of taking part, `kappa`, does not
scale with the prize, so a larger `V` raises the gain from entering at every
count and `k*` is non-decreasing in `V` (M11); an entrant at index `m` has
`(m+1) kappa <= V` (M10), where Corcoran's total equals the discounted prize
at the long-run count. The two results answer different questions: what a
prize does to the count when the cost of taking part is proportional to it,
and when it is fixed.

## Findings from the formalisation

1. Eq. (3) gives `T < r R`, strictly increasing in the count, with limit `r R`
   (`Tot_lt`, `Tot_strictMono_n`, `Tot_tendsto`). So for `r <= 1`, where the
   paper says that whether total expenditure rises with entry "cannot be
   determined" (CO-9), it does rise towards `r R <= R` within eq. (3). What is
   left open is the entry path, not the sign (CO-D1).
2. For `r > 1`, `T > R` exactly when the count is above `r/(r-1)`, where profit
   is negative (`Tot_vs_prize`, `profit0_cmp`), and the first-order point is
   then not an equilibrium: spending nothing pays more (`not_nash_of_loss`).
   This is the exit rule of CO-5 as an exact statement.
3. The first-order point is a best reply for `r <= 1` at every `n > 1`
   (`nash_of_le_one`) and at the long-run count for every `r > 1`
   (`nash_longrun`). The paper uses only the first-order condition. For
   `r > 1` and a count strictly below `r/(r-1)` we have not proved it, and
   do not assert it.
4. Footnote 4 is proved as a derivative (`profit0_hasDerivAt_r`,
   `profit0_deriv_neg`).
5. The offset behind eq. (10) is proved as two monotone statements
   (`aStar_strictMono_r`, `nLong_strictAnti`) and as the exact product
   (`aStar_nLong`, `Tot_nLong`).
