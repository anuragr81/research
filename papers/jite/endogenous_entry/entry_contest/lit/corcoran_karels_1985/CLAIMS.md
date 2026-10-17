# Claims about Corcoran and Karels (1985)

**Source.** William J. Corcoran and Gordon V. Karels, "Rent-seeking behavior in the
long-run", *Public Choice* 46: 227-246 (1985), the journal pages (the paper was
reprinted in *Efficient Rent-Seeking*, Springer 2001, pp.27-45, which is not
used). The author uploaded it to Drive as `rent_seeking_corcoran_karels.pdf`,
20 pages of page images with no text layer. Read in full from the page images
[P]. Pages are the printed pages; the printed page is the PDF page plus 226.
Cited in `skeleton_who_pays.tex` (row L29) and in `refs.bib` as `CorcoranKarels1985`.

The text was made by OCR (`ocr_corcoran_karels.py`, RapidOCR on the page images
upscaled 2.5 times) and is pinned by sha256. Every row not marked P is checked
verbatim, on the page named, against that text. Spaces are ignored, because the
OCR sometimes joins words. Every such quotation was also read on the page image.
Rows marked P (equations and table cells) were read from the page images, which
`verify_corcoran_karels.py` pins by sha256. The results we use are proved in
`lean/mathlib/CorcoranKarels.lean` (see `LEAN`), which imports
`lean/mathlib/Corcoran.lean` for eqs. (3) and (4).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CK-1 | 227 | "Corcoran (1983) extended Tullock's model to a long-run setting, i.e., free entry, and found that under these conditions rents will be dissipated." | The question |
| CK-2 | 228 | "Since each player is assumed identical this solution holds for all players." | Identical players |
| CK-3 | 228 | "Corcoran, building on Tullock's work, assumed entry would continue until profits are reduced to zero for each firm." | Free entry |
| CK-4 | 228 | "A second difficulty is the equilibrium number of players is not necessarily integer valued when r is greater than one." | The count |
| CK-5 | 229 | "If we require all players to consider the alternative of a zero loss with a zero bet then it must be the case that the value of expenditures in rent-seeking by all participants can never exceed the prize." | Participation |
| CK-6 | 229 | "We point out at this stage that all numerical examples utilized in the paper assume a payoff of $100." | X = 100 |
| CK-7 | 230 | "First, no matter how many competitors there are, no firm ever incurs an expected loss." | r ≤ 1 |
| CK-8 | 231 | "One possible solution to this degeneration of the game is to impose a minimum bet on all participants." | The minimum bet |
| CK-9 | 231 | "If the value from the C-N solution is less than, or equal to, the minimum bet, the minimum bet is the optimal value." | Rule (2) |
| CK-10 | 231 | "actually 49 or more players forces a minimum bid" | When the minimum binds |
| CK-11 | 232 | "These profits exist until 100 players are in the game." | The count at a minimum of $1 |
| CK-12 | 232 | "it should become apparent that all the rent will be dissipated if the size of the payoff is integer divisible by the minimum bet." | Dissipation |
| CK-13 | 232 | "In such a case, instead of a minimum bet requirement there is a fixed entry fee" | A fixed entry fee |
| CK-14 | 233 | "We define such a bet as a 'pre-emptory' bet." | Pre-emptive bets |
| CK-15 | 233 | "Although the low end declines as the number of incumbents increases, it does so at a slower rate than the upper end." | The range narrows |
| CK-16 | 233 | "This suggests an equilibrium number of incumbents." | The count with pre-emption |
| CK-17 | 236 | "For example, at r = 1.4 the maximum number is eight and declines to two when r = 3.0." | Counts |
| CK-18 | 236 | "For one player the minimum is 50.00 at an r-value of 2.0." | The one-player minimum |
| CK-19 | 236 | "Within the pre-emptory range an infinite number of solutions exists." | Many bets deter |
| CK-20 | 236 | "Thus for r = 1.1 in Table 3, between 5 and 11 players are viable in the long-run when they place a C-N bet." | The Cournot-Nash band |
| CK-21 | 238 | "Hit and run competition allows multiple equilibria in the number of incumbents both when they collude and when they settle at the C-N solution." | The count is not pinned |
| CK-22 | 240 | "Thus, an equilibrium number of players exist where the lower end of the pre-emptory range equals the upper end of the range." | The hardball count |
| CK-23 | 240 | "For instance, with r = 1.1, the hardball equilibrium would have 28 players; with r = 1.5, 6 players; with r = 2.0, 4 players; etc." | Hardball counts |
| CK-24 | 242 | "For both cases the hit and run entry assumption allows various numbers of incumbents to be a stable equilibrium." | Several counts |
| CK-25 | 243 | "Since all players were assumed identical, the long-run results could also be considered the solution where each possible player has time to consider alternatives" | Identical players |
| CK-26 | 243 | "For the sake of simplicity here and throughout the paper, the probability of winning is expressed assuming only two players." | Note 1 |
| CK-27 | 245 | "When entry occurs the expected value of an incumbent betting for maximum expected profits while preventing hit and run entry, is negative." | Note 10 |

## Read from page images

| ID | Page | Transcription | What we rely on |
|---|---|---|---|
| CK-P1 | 228 | "(3) a = (n − 1) r X / n²; (4) n = r/(r − 1); (5) r = n/(n − 1)" | The Cournot-Nash bet and count |
| CK-P2 | 230 | "Table 1: r = .5, n = 1000: optimal bet .0004995, expected profit .0995005; r = .75, n = 4: 14.0675, 10.9375; r = .75, n = 1000: .074925, .0025075; r = 1, n = 2: 25, 25; r = 1, n = 3: 22.22, 11.11" | Table 1 |
| CK-P3 | 232 | "Table 2: r = .5, n = 110: Cournot-Nash solution .492, minimum bet 1, optimal bid 1, expected profit −.09" | Table 2 |
| CK-P4 | 234 | "Table 3: r = 1.2, one player: 100.00 − 67.73; r = 1.2, 14 players: 7.14 − 7.07; r = 1.4, two players: 50.00 − 33.51 (35.00); r = 1.4, five players: 20.00 − 17.42; r = 1.4, eight players: 12.50 − 12.45; r = 1.4, nine players: 11.11 − 11.44; r = 1.1, 28 players: 3.571 − 3.565; r = 1.1, 29 players: 3.448 − 3.453" | Table 3 |
| CK-P5 | 237 | "Table 4: r = 1.2: 63.73 for one player, 98.98 for 14 players; r = 2.5: 98.94 for three players" | Table 4 |
| CK-P6 | 241 | "Table 6, incumbents collude: r = 1.1, 28 players; 1.2, 14; 1.3, 10; 1.4, 8; 1.5, 6; 1.6, 5; 1.8, 4; 2.5, 3 (98.64); 3.0, 2; 4.0, 2; 5.0, 1; 6.0, 1; at r = 2.0, 100.00 at 4 players" | The hardball counts |
| CK-P7 | 244 | "F'' < 0 if a^{−1}(r − 1) < 2[r a^{r−1}(a^r + b^r)^{−1}]. Assuming a, b > 0 then r ≤ 1 is a sufficient condition for this." | Note 3 |
| CK-P8 | 245 | "b = ((r − 1)/r) X/[n(r − 1)]^{1/r}" and "the optimal bet by an entrant (derived from 1) and 2) in note 8) is (r−1)X/r" | Notes 8 and 10 |

## Results in Lean (`lean/mathlib/CorcoranKarels.lean`, namespace `CorcoranKarels1985`)

| Result | Lean | What is proved |
|---|---|---|
| Section 1 (pp.228-229) | `eq5_iff`, `n_at_r16`, `r3_numbers` | eq. (5) inverts eq. (4); r = 1.6 gives n = 8/3; at r = 3 the count is 1.5, and two players each bet 75 and lose 25 |
| Tables 1 and 2 (pp.230, 232) | `table1_r05_n1000`, `table1_r075_n4`, `table1_r075_n1000`, `table2_r05_n110` | four misprinted cells, against eq. (3) |
| Splitting bets (p.230) | `split_r05`, `split_fails_r1`, `split_neutral_r1` | the r = .5 example; at r = 1 splitting loses under the paper's own computation and gains nothing with the rival's bet held fixed |
| The minimum bet (pp.231-232, note 4) | `minbet_binds`, `minbet_count_1`, `minbet_count_099`, `minbet_dissipation`, `minbet_best_reply` | the minimum binds from 49 players; the counts 100 and 101 of Table 2; with every entrant at the minimum Z the count is ⌊X/Z⌋ and the rent is used up exactly when X/Z is whole; for r ≤ 1, when the rivals bet Z and the Cournot-Nash bet is at most Z, no bet of at least Z beats Z |
| Note 3 (p.244) | `F_hasDerivAt`, `F'_hasDerivAt`, `note3_iff`, `note3_sufficient` | the second derivative of the winning probability, its sign exactly as printed, and r ≤ 1 sufficing |
| Pre-emption (Section 3, notes 8 and 10) | `key_amgm`, `ent_le`, `ent_zero`, `ent_pos_of_lt`, `note8`, `deter_iff`, `low_end_slower` | against committed bets the entrant's best reply earns at most zero exactly when the sum of the bets' r-th powers is at least a critical value, with zero attained at (r−1)X/r; note 8's bet attains that value; a common bet deters exactly when it is at least note 8's; the low end falls more slowly than X/m |
| The one-player minimum (p.236) | `blow_one_ge_half`, `blow_one_r2` | the lowest pre-emptive bet for one player is at least X/2 for every r > 1, and equals X/2 at r = 2 |
| Counts (Sections 4-5) | `range_nonempty_iff`, `sust_iff_pow`, `count_r11`, `count_r12`, `count_r13`, `count_r14`, `count_r15`, `count_r16`, `count_r18`, `count_r2`, `count_r25`, `count_r3`, `count_r4`, `count_r5`, `count_r6`, `two_sustained_r42` | the range for m incumbents is non-empty exactly when (m(r−1))^{r−1} ≤ r^r, i.e. m ≤ r^{r/(r−1)}/(r−1); this gives every hardball count of Table 6 and two sustained players at r = 4.2 |
| Numbers in the text (pp.236-240) | `cn4_r14`, `cn_band_r11`, `blow_r2_m4`, `table3_r12_m1`, `text_r12_m14`, `text_r25_m3` | the Cournot-Nash bet at r = 1.4 for four players; the end points of the r = 1.1 band; the single bet 25 at r = 2 and four players; three printed numbers against note 8's formula |

Not formalised: note 4's Kuhn-Tucker derivation beyond rule (2), and the claim
that the maximum number of incumbents falls as r rises (the counts above check it
at the table's values). Also not formalised: the effect of transactions costs and
licences on aggregate expenditure (pp.240-242), and the policy list of Section 6.

## Reading recorded

| ID | Page | Text | Our reading |
|---|---|---|---|
| CK-D1 | 228 | "For example, when r is equal to 1.6, the solution value for n is 2.6." | Eq. (4) gives 8/3 ≈ 2.67 (`n_at_r16`) |
| CK-D2 | 230 | "Second, it will always pay for each player to split its bets, assuming the other firm maintains its current bet." | The example re-solves every bet at the Cournot-Nash level for the new count; it does not hold the rival's bet fixed. Under that computation splitting loses at r = 1: two of three bets earn 22.22 against 25 for one of two (Table 1, `split_fails_r1`). With the rival's bet fixed, two bets earn what one bet of their sum earns (`split_neutral_r1`). So "always" fails at r = 1, which the text says Table 1 confirms |
| CK-D3 | 238 | "For example, at r = 1.2 one player bets $63.73 while 14 players in aggregate bet $98.18." | Tables 4 and 6 print 98.98 (14 × 7.07). Note 8's aggregate is between 98.9 and 99 (`text_r12_m14`), so 98.18 is a misprint |
| CK-D4 | 238 | "and thereafter increase to $98.84 at r = 2.5" | Table 4 prints 98.94 and Table 6 98.64 (3 × 32.88). Note 8's aggregate is between 98.6 and 98.7 (`text_r25_m3`), so both 98.84 and Table 4's 98.94 are misprints |
| CK-D5 | 240 | "For r-values larger than 4, only one player can be sustained." | Two players are sustained at r = 4.2 (`two_sustained_r42`). The statement holds at the table's values 5.0 and 6.0 (`count_r5`, `count_r6`) |
| CK-D6 | 245 | "This can also be determined by multiplying equation (30) by n." | Note 9: the paper has no equation (30). Eq. (3) times n gives the aggregate (n − 1)rX/n |

Misprinted table cells (rows P): Table 1 has three (`table1_r05_n1000`,
`table1_r075_n4`, `table1_r075_n1000`), Table 2 one (`table2_r05_n110`), Table 3
one (r = 1.2, one player, 67.73 for 63.73, `table3_r12_m1`), and Table 4 one
(r = 2.5, three players, 98.94 for 98.64, `text_r25_m3`).

## Comparison with the model rows

The players are identical and bet continuously in a Tullock lottery, with free
entry. The count is set by the exponent r: n = r/(r − 1) for Cournot-Nash bets,
and m ≤ r^{r/(r−1)}/(r − 1) for pre-emption (`range_nonempty_iff`). When a
minimum bet binds, the count is set by the minimum instead. Those who do not bet
cannot win.

- **The fixed fee.** The minimum-bet case is a fixed entry fee (CK-13). With
  every entrant at the minimum Z, each wins with probability 1/n, so entry goes
  on while nZ ≤ X (`minbet_dissipation`). That is M10's bound (m+1)κ ≤ V with
  κ = Z and V = X. It binds there because an entrant who pays gains the whole
  1/n and one who does not gains nothing. In our model outsiders compete on the
  base score, so the gain from paying can be smaller.
- **Several counts.** Under hit-and-run entry several numbers of incumbents are
  stable (CK-21, CK-24). The reason is that incumbents choose among a range of
  pre-emptive bets, not that entrants differ. Here the fees differ and the rule
  is fixed, and the count is pinned (M6).
- **Wealth.** There is no wealth and no spread of entry costs. The comparative
  statics are in r, the minimum bet and the commitment assumed.
