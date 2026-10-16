# Cole, Mailath and Postlewaite (1991), claims

Evidence tag [F]: the text of every page of CARESS Working Paper 91-14 (PDF
pages 1 to 36) was read on 16 October 2026, with the equations read from page
images. Pages are PDF pages. PDF page `n` carries the printed number `n − 1`
from PDF page 3 on.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CM-Q1 | 4 | "We interpret an agent's status as a ranking device which determines how well he fares with respect to non-market decisions." | Status is a rank that allocates a non-market good, not a belief about a hidden type |
| CM-Q2 | 5 | "Agents will have common preferences over potential matches." | Preferences over mates are vertical and shared. Nothing depends on the chooser's own type, unlike Paper A's squared gap |
| CM-Q3 | 5 | "In our model, people only care about status because in equilibrium it may affect variables that enter the utility function." | Status is valued only through equilibrium matches. The precedent for deriving a social preference rather than assuming it, as Paper A derives group avoidance |
| CM-Q4 | 7 | "We assume that matching is voluntary and takes place based upon complete current information." | No characteristic is hidden and no one infers anything. The paper has none of Paper A's inference from a marker |
| CM-Q5 | 7 | "The role of status in our model will be to rank the men. This ranking determines the outcome of the matching stage." | The same point as CM-Q1, at the definition of a social norm |
| CM-Q6 | 13 | "A man's match in period t depends, then, only on his relative position in the capital distribution of period t." | Wealth-is-status matching is assortative on observed capital |
| CM-Q7 | 14 | "bequests will be larger when a son's match is more sensitive to the level of capital on the margin." | The paper's mechanism, from eq. (10). Not used by Paper A |
| CM-Q8 | 17 | "if a man matches with a woman with an endowment not equal to his status, the family line from that point on has zero status" | The aristocratic norm is enforced by loss of status, eq. (12), formalised through (13) as `ColeEtAl1991.eq13` |
| CM-Q9 | 22 | "groups, [0,i) and [i,1] called respectively the upper group and lower group." | The printed labels, reversed in reading CM-D3 |
| CM-Q10 | 23 | "For men and women in the upper group, matching is to be random" | Within the upper group a match is a random draw from the group, the setting of `ColeEtAl1991.upper_group_mean` |
| CM-Q11 | 23 | "Since the expected value of a match for any male in the upper group is (1-i)/2" | The printed value, read as `(1 + i)/2` (CM-D2, `ColeEtAl1991.control_printed_value_not_mean`) |
| CM-Q12 | 24 | "men with low status compete on the basis of wealth while men with high status compete on the basis of birth." | The hybrid equilibrium. Group membership by birth coexists with ranking by wealth |
| CM-Q13 | 26 | "why is he giving away goods that people care about rather than selling them?" | The authors chose matching to avoid modelling an allocator who could sell access. Paper A's venue that prices access is the case they set aside |
| CM-Q14 | 24 | "as shown in Proposition I, in the wealth-is-status equilibrium for the" | The cross-reference recorded as CM-D4 |

## Lean results

`lean/mathlib/ColeEtAl1991.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `ColeEtAl1991.eq13` | Condition (13). `V*(k) + j ≤ V*(k) + i/(1 − β)` if and only if `(1 − β) j ≤ i` |
| `ColeEtAl1991.control_zero_status_deviates` | Control. (13) fails for a man of status 0 and any woman with `j > 0` |
| `ColeEtAl1991.L_ge_k` | The compensating capital `L_t(j)`. `V*(L) − V*(k) = β j/(1 − β)`, so `L ≥ k` |
| `ColeEtAl1991.control_L_eq_k_at_zero` | Control. At `j = 0` the compensating capital is `k` itself |
| `ColeEtAl1991.upper_group_mean` | The hybrid equilibrium. A random match in `[i, 1]` is worth `(1 + i)/2` |
| `ColeEtAl1991.banishment_loss` | Banishment costs at least `(1 − i)/2` |
| `ColeEtAl1991.control_printed_value_not_mean` | Control. The printed `(1 − i)/2` is not the group mean for `0 < i < 1` |
| `ColeEtAl1991.spread_blind` | With `j` linear in utility, a random match in a group is worth its mean whatever its spread |
| `ColeEtAl1991.stationary_ratio` | Eq. (6) at a stationary point gives `(1 − λ*)^γ = A^{1−γ} β` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CM-D1 | The version read is CARESS Working Paper 91-14, June 1990, revised June 1991, and it is cited as such, with the key `ColeEtAl1991` | The cover, PDF p. 1. The cache file name says 1992, the year of the journal version, which is not read |
| CM-D2 | The expected value of a match in the upper group is read as `(1 + i)/2`, not the printed `(1 − i)/2` | Women in the upper group are uniform on `[i, 1]`. The same page gives the loss from banishment as `(1 − i)/2` when the best lower-group match is below `i`, and PDF p. 25 calls `(i + 1)/2` the average for the upper group. `control_printed_value_not_mean` |
| CM-D3 | `[i, 1]` is read as the upper group and `[0, i)` as the lower group, the reverse of the printed labels (CM-Q9) | PDF p. 23 says "for any man in [i,1]" in the upper group, and Proposition III puts `j ≥ i` under the aristocratic rule |
| CM-D4 | The claim that the consumption share in the wealth-is-status equilibrium goes to 0 for `γ > 1` is read as Property 6, not Proposition I (CM-Q14) | Proposition I is the existence result. Property 6 is the limit statement |
| CM-D5 | Footnotes 11 and 15 speak of "type I" and "type II" agents, read as sons and women | A vestige of the earlier paper named in the cover footnote. The text otherwise says men and women |
| CM-D6 | PDF page 36 repeats the reference list of PDF page 35 in another style, and a scan mark on PDF page 2 is extracted as the word "may" | Page images. Nothing is attributed to either |
| CM-D7 | The text layer garbles the equations, so every equation is read from page images at 110 dpi | The listed pages in `RECONSTRUCTION.md` |
