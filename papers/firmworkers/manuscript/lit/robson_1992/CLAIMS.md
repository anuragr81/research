# Claims about Robson (1992)

**Source.** ROB, *Econometrica* 60(4) pp.837–857, read in full [F]. See
`RECONSTRUCTION.md` for the copy, sha256 and cache paths. Cited in
`claims.yaml` as row L16 and in `refs.bib` as `Robson1992`. Pages are the
printed pages. Quotations are checked verbatim on their page in Drive's OCR text
by `verify_robson.py`. Results are proved in `lean/Lit/Robson1992.lean` (see
`LEAN`), namespace `Robson1992`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| ROB-1 | 837 | "This paper supposes an individual cares about his/her own wealth not only directly but also via the relative standing that this wealth induces." | Status enters utility besides wealth |
| ROB-2 | 837 | "what is essentially ordinal rank in the wealth distribution enters von Neumann-Morgenstern utility as an argument" | Status is global wealth rank, so it answers to market outcomes |
| ROB-3 | 837 | "it is typically assumed here that all individuals have identical utility functions which are concave in wealth but convex in status" | Identical preferences; shape from status, not from wealth |
| ROB-4 | 837 | "utility can nevertheless be convex in wealth over some range when the indirect effect via status is included" | The convex region of reduced-form utility comes from status |
| ROB-5 | 838 | "This example is shown to be capable of predicting that the proportion of individuals in each of the three relevant ranges of wealth will remain constant, despite an across-the-board scaling-up of wealth." | The convex region moves with the distribution |
| ROB-6 | 838 | "This is a distribution of wealth such that all free fair gambles are rejected." | Stability means fair gambles rejected |
| ROB-7 | 838 | "It is shown that an equal distribution of wealth can never be a stable wealth distribution." | Equality is unstable under rank status |
| ROB-8 | 839 | "This defines status associated with wealth w as the number of individuals who have less wealth than w, plus one half of the individuals who have exactly wealth w." | The status rule, ties at one half |
| ROB-9 | 839 | "This can be shown to treat total status as being fixed at one half, regardless of the distribution of wealth, whether there are mass points or not." | Total status is fixed |
| ROB-10 | 839 | "increases that shift it into a new class, that give it a new social and economic status, yield increasing marginal utility" | Robson quoting Friedman and Savage (1948, pp.298–299): the convex region as a change of class. To be checked in Friedman and Savage itself |
| ROB-11 | 839 | "it seems plausible that the difference between a gold medal and silver medal should be greater than that between a silver medal and a bronze medal" | The case for convexity in status |
| ROB-12 | 840 | "This example then yields the concave-convex-concave utility shape described by Friedman and Savage." | Example A gives the Friedman–Savage shape |
| ROB-13 | 840 | "A drawback of the original Friedman-Savage formulation is that it predicts that a scaling-up of every individual's wealth must be accompanied by a decrease in the proportion of the population in the lowest wealth range" | A fixed inflection point is the drawback |
| ROB-14 | 842 | "it is appropriate to suppose that this distribution is unaffected by any given individual's gambling since each individual has zero measure" | A continuum: gambling does not move the distribution |
| ROB-15 | 842 | "there can be essentially no gambling in the equilibrium as defined" | The equilibrium has no gambling |
| ROB-16 | 843 | "There can be no mass points in F at positive levels of wealth." | Lemma 2 |
| ROB-17 | 843 | "a necessary and sufficient condition for F to be an SWD is that V be concave where there are people and not too convex where there are none, roughly speaking" | Theorem 1 in words |
| ROB-18 | 844 | "Friedman at the same time proved the startling result that such an equilibrium wealth distribution would have no middle class" | The contrast with Friedman (1953) |
| ROB-19 | 845 | "an SWD exists, for all levels of total wealth, if utility is strictly concave in wealth itself and wealth and status do not become infinitely strong complements" | Theorem 2 in words |
| ROB-20 | 847 | "a Pareto efficient distribution cannot allow any equality at positive wealth levels, given that individuals are risk-preferring for gambles over status" | Lemma 3 in words |
| ROB-21 | 848 | "whereas individual incentives to gamble derive from the function" | Private and social marginal utility differ |
| ROB-22 | 848 | "fails to reflect changes in status, either for better or for worse, induced upon other individuals by a change in a given individual's wealth and status" | The status externality |
| ROB-23 | 849 | "What is, perhaps, less obvious is that Pareto inefficiency can also arise from too little gambling." | Example C |
| ROB-24 | 851 | "One is to extend the model to encompass the possibility that the market for gambles could clear with a positive amount of gambling." | Positive gambling is left open |
| ROB-25 | 852 | "Another simple possibility is to have utility dependent on the overall average level of wealth." | An average-wealth reference |
| ROB-26 | 852 | "In the present context, however, with exogenous total wealth, the status externality would have no impact." | With fixed total wealth an average reference does nothing |

## Results in Lean (`lean/Lit/Robson1992.lean`)

| Result | Lean | What is proved |
|---|---|---|
| Status rule, total status one half (p.839) | `Robson1992.tieWeight_add_swap`, `Robson1992.total_status`, `Robson1992.mean_status_half` | in a finite population with ties weighted one half, statuses sum to `n²/2`, so mean status is one half whatever the wealths |
| Lemma 2, limiting step (p.843) | `Robson1992.mass_point_bet_gains` | if `U` is strictly increasing in status and status jumps from `F` to `F⁺`, some `p ∈ (0, 1)` makes `pU(F⁺) + (1 − p)U(F)` exceed `U(½F + ½F⁺)` |
| Corollary to Theorem 1, finite gambles (p.843) | `Robson1992.concave_rejects_fair_gambles` | a concave `V` rejects every fair finite gamble (Jensen) |
| Example A, derivatives (p.840) | `Robson1992.exampleA_first_deriv`, `Robson1992.exampleA_second_deriv` | for `w > 0`, `V = w^α/α + kw^β` has derivative `w^{α−1} + kβw^{β−1}` and second derivative `(α − 1)w^{α−2} + kβ(β − 1)w^{β−2}` |
| Example A, inflection (p.840) | `Robson1992.exampleA_convex_iff`, `Robson1992.inflection_lt_one` | the second derivative is positive exactly above `w̃ = [(1 − α)/(kβ(β − 1))]^{1/(β−α)}`, and `w̃ < 1` when `1 − α < kβ(β − 1)` |
| Example A′ (p.845) | `Robson1992.exampleA'_inflection` | with `α = 0`, `β = 2`, `k = ¾`, `w̃ = √(2/3)` |
| Scaling (p.840) | `Robson1992.scaled_inflection`, `Robson1992.log_case_proportion_constant`, `Robson1992.proportion_rises` | uniform on `[0, γ]` acts as `k·γ^{−β}`; `w̃_γ/γ = w̃·γ^{α/(β−α)}`; constant for `α = 0`; strictly increasing in `γ` for `0 < α < 1` |
| Remark p.852, closed economy | `Robson1992.total_fixed_under_reassignment` | total wealth, hence average wealth, is unchanged by any reassignment |
| Lemma 3, core step (p.847) | `Robson1992.status_lottery_beats_equal` | for `g` strictly convex on `[0, 1]`, a fair lottery between the statuses `(3a + b)/4` and `(a + 3b)/4` beats the shared status `(a + b)/2` |

Controls: `Robson1992.control_total_status_needs_half_ties`,
`Robson1992.control_mass_point_needs_jump`,
`Robson1992.control_rejection_needs_concavity`,
`Robson1992.control_convex_region_needs_status`,
`Robson1992.control_fixed_inflection_proportion_falls`,
`Robson1992.control_total_fixed_needs_whole_population`,
`Robson1992.control_status_lottery_needs_strict_convexity`.

## Readings recorded

- ROB-D1 (p.845): Drive's OCR drops the coefficient of Example A′, printing
  `ln w + N²`; the page image has `ln w + ¾N²`. With `¾` the stated inflection
  `√(2/3)` follows (`exampleA'_inflection`); without it, it would be `√2/2`.
- ROB-D2 (p.843): the limiting step of Lemma 2 lets the bet size `d → 0` and
  uses continuity of `U` in wealth; `mass_point_bet_gains` proves the inequality
  at the limit, not the continuity argument.
- ROB-D3 (p.840): the scaling formula is stated for uniform `F`; the
  generalisation to `ln w + s(N)` and any `F ∈ C²` is asserted, not proved, in
  the paper and is not formalised.
