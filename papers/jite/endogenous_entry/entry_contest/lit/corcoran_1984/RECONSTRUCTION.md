# Reconstruction of Corcoran, "Long-run equilibrium and total expenditures in rent-seeking"

**Source read.** William J. Corcoran, *Public Choice* 43(1), 89-94 (1984), a
JSTOR copy added by the author on Drive; read in full [F] and checked on the
page images [P]. The text was extracted with `pdftotext -layout` and cached
at `~/.cache/entry_contest/corcoran_1984.txt`; pages are the printed pages
(the printed page is the PDF page plus 87). The equations do not survive
extraction, so they were read from the page images, cached in
`~/.cache/entry_contest/corcoran_pages/` and pinned by sha256.

**Why this paper.** It is cited in our other documents as an entry paper and
was not read, so C5, C6 and C13 carried it as unverified.

## Model (Sections 2 and 3)

Identical players, their number `n` taken as a real number, each spending `a`
on a Tullock lottery: the chance of winning is `a^r / (sum of all the a^r)`,
the prize is `P` paid after a lag `t`, discounted at the opportunity rate
`rho`, so its present value is `R = P e^(-rho t)`. Cost is the amount spent.
`r` is the elasticity of the chance of winning in the amount spent.

## Results

1. **Short run (eqs. 1-3, p.90).** The first-order condition at the symmetric
   point gives individual expenditure `(n-1) r R / n^2` (eq. 2) and total
   expenditure `(n-1) r R / n` (eq. 3). For two players and `r = 1` each
   spends `R/4` (Tullock) and the total is `R/2`; for one player the total
   is zero. So Tullock's model is a short-run model (p.90).
2. **Long run (eqs. 4-7, pp.91-92).** The rate of return on the rent-seeking
   option exceeds, equals or falls short of `rho` as `(n-1) r` is below, at or
   above `n`. Players enter while it exceeds `rho` and exit while it falls
   short. For `r > 1` entry stops at `n = r/(r-1)` (eq. 7), which does not
   depend on the prize. For `r <= 1` entry is unbounded (p.92).
3. **Eq. (10), p.92.** At the long-run count total expenditure is `P e^(-rho
   t)`, the discounted prize, and does not depend on `r`: individual
   expenditure `R (r-1)/r` rises with `r` and the count `r/(r-1)` falls, and
   the two offset exactly. This is the result named in the title.
4. **Footnote 4, p.91.** At `t = 0`, profit `R (n + r - n r)/n^2` falls in
   `r` with derivative `R (1-n)/n^2`.
5. **Implications (Section 4, p.93-94).** Tullock's advice to hold the number
   of players down stands. His advice to raise the marginal cost of improving
   the chance of winning must be qualified: with free entry the total does
   not change with it. Where entry is unbounded, whether the total rises with
   entry is left open. The last paragraph reads the result as welfare: a
   transfer displaces an opportunity worth at least as much.

## What could not be reconstructed

Nothing material for our use. The paper has no entry cost that differs between
players, no wealth, and no group of players who stay out; a player who does
not enter does not compete. The count is a real number, not an integer.
