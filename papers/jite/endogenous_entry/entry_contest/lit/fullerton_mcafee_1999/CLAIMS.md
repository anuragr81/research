# Claims about Fullerton & McAfee (1999)

**Paper.** Richard L. Fullerton and R. Preston McAfee, "Auctioning Entry into
Tournaments", *Journal of Political Economy* 107(3), 573-605, 1999. The
source read is the published article, printed pages 573 to 603, from
`auction_entry.pdf` (Drive id `1QvwSRLL7LDxhtK_8yIkEo3SyQt4goBTf`). Evidence
level **[F]**, except printed pages 604 and 605, which the upload lacks.
Locators are printed page numbers.

## Claims our documents made before the read

| ID | Claim | Where | Verdict | Source locator |
|---|---|---|---|---|
| FM-A | Fullerton and McAfee "study contests where agents are heterogeneous and entry is determined by an auction" (quoted from Morgan, Orzen and Sefton). | `LITERATURE.tex` §sec:entry and §sec:unread; `lit/morgan_orzen_sefton_2012/NOTES.md` F6 | Correct. Contestants differ in research cost (Section II) or in a general quality attribute (Section III), and the sponsor restricts entry by auction. | p.578, p.584, p.590 |
| FM-B | "If that auction selects entrants by type, entrant identity is already a property of the agents there." | `LITERATURE.tex` §sec:entry and §sec:unread | The condition holds. The contestant selection auction "is an efficient mechanism for selecting the best-qualified contestants" (Theorem 5), and in Section II "entry of the m lowest-cost firms is an equilibrium" (Theorem 2). Entrant identity is a property of the agents' types in both. | p.591, Theorem 5; p.580, Theorem 2 |

## Claims this pass adds, each with its locator

| ID | Claim | Source locator | Lean |
|---|---|---|---|
| FM-1 | Effort is a number of independent draws, and the chance of holding the best innovation is `z_i / Σ_j z_j`. "This probability structure would arise if the firm took z_i identical, independent draws from F(x)". | p.578, eq. (1) | `win_prob` |
| FM-2 | "Given a set of firms, there is a unique equilibrium in the subgame that involves positive z_i for the lowest-cost firms." Efforts and profits are eqs. (3) and (4). | p.579, Theorem 1; p.596-597 | `zstar_isNash`, `nash_char`, `nash_unique`, `active_prefix`, `profit_zstar`, `all_active` |
| FM-3 | "either any equilibrium will involve the efficient firms or, when a low-cost firm is excluded, the firm's cost will be nearly that of the firms that enter", with the bound `c_i ≥ [(m²−m)/(m²−m+1)] c_k`, which "binds exactly". | p.579-580, Lemma 1; p.597 | `lemma1_deviation`, `lemma1_bound`, `lemma1_case_out`, `lemma1_exact` |
| FM-4 | "There is a unique number of firms, m, that is efficient for entry into the tournament and entry of the m lowest-cost firms is an equilibrium." | p.580, Theorem 2; p.597-598 | `thm2_iff`, `thm2_iff_needs_sign`, `thm2_step` |
| FM-5 | In the symmetric case the total cost of buying effort `Z` is `cZ + mγ`, "which is minimized at m = 2". | p.580 | `symmetric_case` |
| FM-6 | "If Δ_m is nondecreasing, then the total procurement cost of obtaining a fixed level Z is minimized at m = 2." | p.581, Theorem 3; p.598-599 | `TC_formula`, `TC_step` |
| FM-7 | Lemma 2: "Suppose that m ≤ m̄ and [c_m/c_{m+1} ≤ 1/m + ((m−1)/m)(c_{m−1}/c_m)]. Then Δ_m ≤ Δ_{m+1}." Both constant and proportional cost increments satisfy it. | p.581, Lemma 2; p.599 | `lemma2_step`, `lemma2_single_m_fails`, `lemma2_constant_increment`, `lemma2_proportional` |
| FM-8 | "there is no efficient equilibrium in which costs are uniformly distributed on any interval [0, c̄]". | p.583 | `uniform_bid_scale`, `uniform_bid_constant`, for two entrants |
| FM-9 | Theorem 4: "If there exists w̃ ∈ [w̲, w̄] such that, for all w < w̃, Ψ(w, w) ≥ Ψ(w̃, w̃), a symmetric, pure-strategy bidding equilibrium does not exist for the discriminatory-price or uniform-price entry auction." | p.586 | `thm4_hyp_always`, `thm4_hyp_with_increasing_bid`, `thm4_interior` |
| FM-10 | Lemma 4: "If the value of the best entrant's starting innovation is such that H(w_max) ≥ e^{−c/P}, then none of the tournament contestants will conduct additional research following entry." | p.589; p.601-602 | `lemma4`, `lemma4_sharp` |
| FM-11 | The contestant selection auction is efficient with independent types (Theorem 5) and its expected cost does not depend on `K` (Theorem 6). | p.591-592; p.603 | not formalised |

## A claim of ours that the read makes possible

| ID | Claim | Lean |
|---|---|---|
| FM-X | In Fullerton and McAfee's entry stage the number of entrants is not the same in every equilibrium. With costs `(2.1, 2.3, 2.5, 2.6)`, prize `1` and fixed cost `19/250`, both `{2.1, 2.3}` and `{2.1, 2.5, 2.6}` are entry equilibria in the sense of p.579. | `two_sizes` |
