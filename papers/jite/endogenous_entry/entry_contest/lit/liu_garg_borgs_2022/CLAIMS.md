# Claims about Liu, Garg and Borgs (2022)

**Source.** Lydia T. Liu, Nikhil Garg and Christian Borgs, "Strategic Ranking", *Proceedings of
the 25th International Conference on Artificial Intelligence and Statistics (AISTATS)*, PMLR 151
(2022), 2489-2518. Read in full from arXiv:2109.08240v2 [F, arXiv version]. Cited in `refs.bib`
as `LiuGargBorgs2022`. Pages are PDF pages.

Every quotation is checked verbatim, on the page named, against the PDF's text layer in layout
and in reading order, ignoring case, punctuation and spaces. The PDF is pinned by sha256. The
results we use are proved in `lean/mathlib/LiuGargBorgs.lean` (see `LEAN`).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LGB-1 | 1 | "we introduce strategic ranking, in which the (designed) individual reward depends on an applicant’s post-effort rank in a measurement of interest" | The question |
| LGB-2 | 1 | "Even if every applicant improves substantially, due to (perceived) capacity constraints the number accepted would not commensurately increase." | A fixed number of places |
| LGB-3 | 3 | "The applicants are modeled as a continuum of players" | A continuum, not finitely many challengers |
| LGB-4 | 3 | "Each applicant chooses an effort level e ≥ 0, the result of which is an observed, post-effort score" | A continuous effort, not a binary choice |
| LGB-5 | 4 | "such that in expectation it admits a number of applicants equal to a capacity constraint" | The capacity `ρ` |
| LGB-6 | 4 | "where the effort cost function p is non-negative, continuous, and strictly convex" | Convex effort cost (`no_overtaking`) |
| LGB-7 | 4 | "Applicants are assumed not to personally benefit from increasing their score v, except through the corresponding increase in their rank and reward." | Effort is wasted for the applicant, as our fee is |
| LGB-8 | 2 | "the competition effect results in rank preservation: in equilibrium, the ranks (and thus rewards) of applicants after applicant effort is the same as before." | Effort does not reorder applicants (`lemma_C4`, `no_overtaking`) |
| LGB-9 | 18 | "In every equilibrium, λ(θpost(θpre)) = λ(θpre), up to sets of measure 0." | Proposition 2.1 |
| LGB-10 | 7 | "Because a favorable environment results in a higher rate of return for effort, we model the environment as a multiplicative factor in the score" | Advantage enters the score, not the cost of entry |
| LGB-11 | 8 | "Access is the overall probability of admission of the disadvantaged group." | The construct nearest our entrants' share |
| LGB-12 | 8 | "Pure randomization has higher A than any two-level policy." | Proposition 4.4, first part (`access_lt_rho`, `access_pure`) |
| LGB-13 | 9 | "is convex, A for two-level policies is non-increasing in c." | Proposition 4.4, second part (`access_antitone`) |
| LGB-14 | 9 | "as one cannot compare such access across mechanisms that accept different numbers of people overall" | Access needs a fixed number of places, as our share does |
| LGB-15 | 26 | "Since thresB(c) > c for any c ∈ (0, 1−ρ], we have that A < ρ." | The proof's first step |
| LGB-16 | 26 | "On the other hand, A = 0 for a pure randomization policy." | Misprint: access under pure randomization is `ρ` (LGB-D3) |
| LGB-17 | 26 | "because thresB(c) ≤ c for any c ∈ [0, 1 − ρ]" | Misprint: LGB-15 has `thresB(c) > c` (LGB-D3) |

## Readings recorded

| ID | Point | Our reading |
|---|---|---|
| LGB-D1 | LGB-3, LGB-4, LGB-10 | Every applicant chooses a continuous effort, and advantage multiplies the score. There is no fixed fee, no binary choice, no outsider who competes without paying, and no wealth. The population is a continuum. |
| LGB-D2 | LGB-8, LGB-9 | Rank preservation: effort does not reorder applicants, so who is admitted is set by skill (and environment), and the design only sets how much effort it costs. In ours the bought component changes ranks: an entrant can overtake an outsider whose base component is higher. The two models differ at this point. |
| LGB-D3 | LGB-12, LGB-15 to LGB-17 | The proof of Proposition 4.4 (p.26) has two slips that do not affect the result. Pure randomization admits everyone with probability `ρ`, so its access is `ρ`, as the proposition says, not `0` (`access_pure`). The bound `(1 − thresB(c))/(1 − c) ≤ 1` needs `thresB(c) ≥ c`, which the proof stated two lines earlier as `thresB(c) > c`; the printed `≤ c` is the wrong way round. `access_antitone` proves the monotonicity from `thresB(c) ≥ c` and a slope of at least one. |
| LGB-D4 | LGB-11, LGB-13, LGB-14 | Access is a group's admission probability in a contest for a fixed number of places, and it does not rise as the rule makes the ranking more decisive (the cut-off c). That is the nearest result found to our entrants' share, which does not rise as the rule weighs the base component more (M31). The objects differ: their group is defined by environment, not by a choice to pay; effort is continuous; and the lever is the randomization of the reward, not the weight on a component of the score. |
