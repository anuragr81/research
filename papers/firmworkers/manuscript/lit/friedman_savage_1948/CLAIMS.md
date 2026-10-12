# Claims about Friedman and Savage (1948)

**Source.** FS, *JPE* 56(4) pp.279–304, read in full [F] through Drive's OCR
of the scan; quotations are checked against a local OCR with one block per
page. See `RECONSTRUCTION.md`. Cited in `claims.yaml` as row L22 and in
`refs.bib` as `FriedmanSavage1948`. Pages are the printed pages. Results are
proved in `lean/Lit/FriedmanSavage1948.lean` (see `LEAN`), namespace
`FriedmanSavage1948`, in modern terms (concave = FS "convex from above").

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FS-1 | 279 | "an important class of reactions of individuals to risk can be rationalized by a rather simple extension of orthodox utility analysis" | Scope |
| FS-2 | 279 | "That is, he is choosing certainty in preference to uncertainty." | Insurance as a choice of certainty |
| FS-3 | 279 | "Occupations differ greatly in the variability of the income they promise" | Occupational choice as choice under risk |
| FS-4 | 280 | "Diminishing marginal utility plus maximization of expected utility would thus imply that individuals would always have to be paid to induce them to bear risk." | The puzzle |
| FS-5 | 282 | "the ordinal properties of utility functions can be used to rationalize riskless choices, the numerical properties to rationalize choices involving risk" | Cardinal content only under risk |
| FS-6 | 284 | "people must in general be paid a premium to induce them to undertake moderate risks instead of subjecting themselves to either small or large risks" | The premium for moderate risk |
| FS-7 | 286 | "It seems highly unlikely that there is a sharp dichotomy between the individuals who purchase insurance and those who gamble." | The same units insure and gamble |
| FS-8 | 287 | "About the only clear indication is that people are willing to enter into gambles that offer a small chance of a large gain" | Lotteries: small chance, large gain |
| FS-9 | 291 | "It is clear that the graphical condition for a consumer unit to be willing to pay something for certainty is that the utility function be above its chord" | The chord condition |
| FS-10 | 294 | "The simplest utility function (with a continuous derivative) that can rationalize all three statements simultaneously is one that has a segment convex from above followed by a segment concave from above and no other segments." | Concave then convex, in modern terms |
| FS-11 | 297 | "The optimum structure of prizes clearly consists therefore of a single prize" | The single-prize argument |
| FS-12 | 297 | "Statement 5, that lotteries typically have more than one prize, is therefore inconsistent with the utility function of Figure 2." | Why a terminal segment is added |
| FS-13 | 298 | "The hypothesis does not assert that individuals explicitly or consciously calculate and compare expected utilities." | As-if methodology |
| FS-14 | 298 | "A possible interpretation of the utility function of Figure 3 is to regard the two convex segments as corresponding to qualitatively different socioeconomic levels, and the concave segment to the transition between the two levels." | Segments as classes (FS's terms) |
| FS-15 | 298 | "increases in income that raise the relative position of the consumer unit in its own class but do not shift the unit out of its class yield diminishing marginal utility, while increases that shift it into a new class, that" | Within-class concavity |
| FS-16 | 299 | "give it a new social and economic status, yield increasing marginal utility" | Convexity at the move between classes (continues FS-15) |
| FS-17 | 299 | "An unskilled worker may prefer the certainty of an income about the same as that of the majority of unskilled workers to an actuarially fair gamble that at best would make him one of the most prosperous unskilled workers and at worst one of the least prosperous." | Within-class risk aversion |
| FS-18 | 299 | "Men will and do take great risks to distinguish themselves, even when they know what the risks are." | Gambling to change class |
| FS-19 | 299 | "should not the dividing points between the segments occur at roughly the same income for different consumer units in the same community?" | Common class boundaries |
| FS-20 | 301 | "If the concave segment is a border line between two qualitatively different social classes, one would expect relatively few consumer units to be between the two classes." | Few at the border |
| FS-21 | 301 | "Units in the intermediate segment are tempted by every small gamble and at least some large ones." | Instability at the border |
| FS-22 | 303 | "It is convex from above below some income, concave between that income and some larger income, and convex for all higher incomes" | The hypothesis, in FS's terms |
| FS-23 | 304 | "the segments of diminishing marginal utility correspond to socioeconomic classes, the segment of increasing marginal utility to a transitional stage between a lower and a higher socioeconomic class" | The class reading, restated |
| FS-24 | 304 | "On this interpretation the boundaries of the segments should be roughly similar for different people in the same community" | Boundaries common within a community |

## Results in Lean (`lean/Lit/FriedmanSavage1948.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Chord condition (p.291) | `FriedmanSavage1948.prefers_certainty_iff_above_chord`, `FriedmanSavage1948.concave_buys_insurance`, `FriedmanSavage1948.convex_buys_gamble` | certainty is preferred exactly when utility at the mean lies above the chord; strict concavity gives it, strict convexity gives the reverse |
| Income equivalent (p.290) | `FriedmanSavage1948.equivalent_below_mean` | with `U` strictly increasing, `U(I∗) = Ū < U(Ī)` gives `I∗ < Ī` |
| Conceptual experiment (p.292) | `FriedmanSavage1948.experiment_600`, `FriedmanSavage1948.experiment_10000` | `U(600) = 3/5` and `U(10,000) = 5` from the stated indifferences |
| Bounds on gambles (p.293) | `FriedmanSavage1948.never_pay_more_than_max_gain`, `FriedmanSavage1948.compound_gamble_refused` | expected utility never exceeds the utility of the larger outcome; a mixture of two refused gambles is refused |
| Single prize (pp.296–297) | `FriedmanSavage1948.single_prize_beats_two` | with `U` convex over the prize range, one prize of `2P` at probability `q` is at least as good as two prizes of `P` at probability `2q` |

Controls: `FriedmanSavage1948.control_insurance_needs_strict_concavity`,
`FriedmanSavage1948.control_compound_needs_both_refused`,
`FriedmanSavage1948.control_two_prizes_can_win`.

## Readings recorded

- FS-D1: "convex from above" is concave and "concave from above" is convex in
  current usage; FS-10, FS-14 and FS-22 are read accordingly.
- FS-D2 (p.292): the OCR garbles the indifference probabilities; the page image
  shows `2/5` and `4/5`, which give the printed `.60` and `5`.
- FS-D3: Robson (ROB-10) and Ray and Robson (RR-6) quote or paraphrase the class
  reading; FS-15 and FS-16 confirm Robson's quotation against the source.
