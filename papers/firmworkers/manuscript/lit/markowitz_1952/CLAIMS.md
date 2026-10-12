# Claims about Markowitz (1952)

**Source.** MK, *JPE* 60(2) pp.151–158 (Cowles Foundation Paper 57 reprint),
read in full [F] through a local OCR of the scan and Drive's OCR of pp.152–158.
See `RECONSTRUCTION.md`. Cited in `claims.yaml` as row L23 and in `refs.bib` as
`Markowitz1952`. Pages are the printed pages. Quotations are checked verbatim
on their page in the local OCR by `verify_mk.py`. Results are proved in
`lean/Lit/Markowitz1952.lean` (see `LEAN`), namespace `Markowitz1952`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MK-1 | 151 | "explained the existence of insurance and lotteries by the following joint hypothesis" | The Friedman–Savage hypothesis restated |
| MK-2 | 151 | "be convex below A and above B; concave between A and B" | Friedman–Savage shape in their terms (MK-D1) |
| MK-3 | 152 | "Thus the Friedman-Savage hypothesis explains both the buying of insurance and the buying of lottery tickets." | What it explains |
| MK-4 | 152 | "Friedman and Savage go so far as to suggest that these may represent different social classes." | The class reading noted |
| MK-5 | 152 | "Those with wealth between C and D will take some fair bets." | Gamblers between the tangency points |
| MK-6 | 153 | "In general, it seems to me that circumstances in which a moderately wealthy person is willing to risk a large fraction of his wealth at actuarially unfair odds will arise very rarely." | The critique |
| MK-7 | 154 | "above present wealth is first concave and then convex" | Shape above present wealth (his terms) |
| MK-8 | 154 | "going from present wealth downward is first convex and then concave" | Shape below present wealth (his terms) |
| MK-9 | 154 | "The function is concave immediately above present wealth; convex, immediately below." | The middle inflection at present wealth |
| MK-10 | 154 | "Conversely, if the chooser were rather poor, I should expect him to act as if his first and third inflection points were closer to the origin." | Inflection distances scale with wealth |
| MK-11 | 154 | "Generally people avoid symmetric bets." | Loss aversion, observed |
| MK-12 | 154 | "assume that the utility function is bounded from above" | Bounded utility |
| MK-13 | 155 | "the two differ, I shall let “customary wealth” (i.e., the second inflection point) remain at the origin of the graph" | The reference is customary wealth |
| MK-14 | 155 | "first inflection point is below, the third inflection point is above, customary wealth." | Inflections around the reference |
| MK-15 | 155 | "bounded; it is first concave, then convex, then concave, and finally convex" | The full shape (his terms) |
| MK-16 | 155 | "will be essentially the same whether he is poor or rich" | Scale-free behaviour |
| MK-17 | 155 | "there are no levels of wealth where people prefer large symmetric bets to any other fair bet" | Against Friedman–Savage |
| MK-18 | 156 | "The effect of playing conservatively when losing, liberally when winning, is to make the frequency distribution of final outcomes skewed to the right." | Gaming strategy and skew |
| MK-19 | 156 | "This implication holds true whatever be the level of customary wealth of the individual." | Independent of wealth |
| MK-20 | 157 | "ignorant of the way customary follows present wealth or how long it takes to catch up" | Adaptation left open |
| MK-21 | 157 | "I have assumed that asymmetric bets are undesirable." | Symmetric-bet assumption |
| MK-22 | 157 | "In particular, when millionaires play poker together, they play for pennies" | The fun-of-participation reductio |
| MK-23 | 158 | "Nor can any hypothesis which assumes that people maximize expected utility." | Multiple prizes unexplained |

## Results in Lean (`lean/Lit/Markowitz1952.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Symmetric bets (pp.154–155) | `Markowitz1952.avoids_symmetric_bets_iff`, `Markowitz1952.falls_faster_to_the_left` | with `U(0) = 0` at customary wealth, a fair symmetric bet is refused exactly when `U(X) < −U(−X)`, which is `|U(−X)| > U(X)` |
| Fun of participation (§3.1, pp.157–158) | `Markowitz1952.symmetric_bet_value_falls_with_stake` | with `U` concave, the expected utility of a fair bet `±s` does not rise with `s` |
| Customary wealth (pp.154–156) | `Markowitz1952.same_behaviour_rich_or_poor`, `Markowitz1952.windfall_moves_from_middle_inflection` | utility of the form `g((W − w)/s)` ranks bets scaled by `s` identically at every `w`; a windfall moves present wealth off customary wealth by its size |

Controls: `Markowitz1952.control_printed_p154_inequality_favours_bets`,
`Markowitz1952.control_stakes_need_concavity`,
`Markowitz1952.control_customary_needs_adaptation_rule`.

## Readings recorded

- MK-D1: Markowitz uses Friedman and Savage's terms: "convex" means concave and
  "concave" means convex in current usage (his footnote 5, p.151, pairs
  "convex below A" with a negative second derivative). MK-2, MK-7 to MK-9 and
  MK-15 are read accordingly.
- MK-D2 (p.154): the sentence after MK-11 prints `U(X) > |U(−X)|, X > 0`, which
  would make symmetric bets attractive (`control_printed_p154_inequality_favours_bets`)
  and contradicts the same sentence's "falls faster to the left". Page 155
  states `|U(−X)| > U(X)`. Read as an inverted inequality on p.154; confirmed on
  the page image.
