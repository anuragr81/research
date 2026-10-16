# Kim and Loury (2012), claims

Evidence tag [F]: the article, PDF pages 4 to 18 of the issue (journal pp. 1
to 15), was read in full on 16 October 2026, from the text layer and from page
images of PDF pages 4 to 18 for the equations and figures. Pages below are PDF
pages of the whole issue. The journal page is the PDF page minus 3.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KLI-Q1 | 5 | "when group membership is endogenous then the logic of individuals' identity choices leads there to be a positive selection of higher ability individuals into the group with a better reputation" | The passing result as the abstract states it. The formal statement conditions on the return to skill, not on the reputation (KLI-Q11, KLI-D2) |
| KLI-Q2 | 5 | "This happens because those for whom human-capital-investment is less costly are also those who stand to gain more from joining the favored group." | The mechanism. The gain from a group depends on the chooser's own type, on the side of the person who passes rather than the side of the person who evaluates |
| KLI-Q3 | 7 | "If a worker's productivity is not perfectly observable, employers have an incentive to use the collective reputations of the identity groups to which job applicants belong in the screening process." | Unseen type inferred from a visible group, in a labor market. The evaluator is an employer who sets a wage, not a person deciding on contact |
| KLI-Q4 | 7 | "may consider "passing" into the group with a better reputation when the return for such "passing" (e.g., better treatment in the labor market) outweighs its cost" | Passing is the authors' motivating case, and its return is a labor-market return |
| KLI-Q5 | 8 | "The usage of the observable cultural traits in the screening process can to some extent cure the social inefficiency caused by imperfect information about the true characteristics of workers." | A chosen marker adds information to screening. This runs opposite to Paper A1's test, in which showing class removes the group penalty |
| KLI-Q6 | 8 | "It is also the case that the better-regarded group will, in equilibrium, come to consist disproportionately of high ability/low human capital investment cost types." | Ability is a low cost of investing in skill. Skill is chosen, not given |
| KLI-Q7 | 9 | "(That is, a person's identity orientation cannot be used to predict their economic abilities.)" | The affect cost `k` and the skill cost `c` are independent, so any association between group and ability is produced by selection |
| KLI-Q8 | 9 | "Employers set the wage for each worker to be proportional to the worker's expected productivity using both the productivity-related noisy signal and the identity type." | The evaluator sees an individual signal as well as the group, unlike Paper A1's group sufficiency, and values only the posterior mean |
| KLI-Q9 | 10 | "and that of an unskilled worker is zero. We assume that the wage is proportional to the expected skill level" | Eq. (1). The wage is linear in the posterior, so the group's spread carries no penalty |
| KLI-Q10 | 13 | "An equilibrium with affective stereotyping (ASE) is defined as a pair of investment rates for the affective groups" | The consistency condition. Each group's believed investment rate equals the actual rate of the members who chose it, `IsASE` |
| KLI-Q11 | 14 | "The disproportionately more talented workers, whose human capital investment costs (c) are relatively lower, choose the 'affect' that corresponds to the greater return to human capital investment" | Proposition 2, formalised in the finite case as `prop2` |
| KLI-Q12 | 14 | "Intuitively, we can say that the existence of non-trivial ASE is guaranteed given the multiple PSE." | The existence of a non-trivial ASE is argued informally and not proved (KLI-D5) |
| KLI-Q13 | 14 | "This implies that the inequality between endogenous groups in some non-trivial ASE can be greater than inequality between exogenous groups in any PSE." | The main result in the body says "can be greater", against "at least as great" in the abstract and introduction (KLI-D5) |
| KLI-Q14 | 15 | "Those who will go straight are more willing to join the program than those who will return to crime because those going straight have greater gain from having a job in the market." | A costly certificate that screens an unseen type by self-selection, the nearest the article comes to certification by price |

## Lean results

`lean/mathlib/KimLoury2012.lean`, in the paper's notation, with a finite cost
distribution in place of the continuous `G` and with `V₀`, `V₁` taken as given
functions.

| Theorem | What it checks |
|---|---|
| `KimLoury2012.eq_15` | Eq. (15). `U(π, c) = max {R(π) − c; 0} + V₀(π)` |
| `KimLoury2012.lemma1_low` | Lemma 1. Below both returns, `ΔU = V₁(πA) − V₁(πB)` |
| `KimLoury2012.lemma1_high` | Lemma 1. Above both returns, `ΔU = V₀(πA) − V₀(πB)` |
| `KimLoury2012.lemma1_mid` | Lemma 1, eq. (16). Between the returns, with `R(πA) ≥ R(πB)`, `ΔU = V₁(πA) − V₀(πB) − c` |
| `KimLoury2012.dU_antitone` | Lemma 1 as read on p. 14. `ΔU` is non-increasing in `c` when `R(πA) ≥ R(πB)` |
| `KimLoury2012.prop1_pointwise` | Every cost type gains from the group in which both wages are higher |
| `KimLoury2012.prop1` | Proposition 1. With symmetric, strictly increasing `H`, more than half the workers choose that group |
| `KimLoury2012.selRate_ge` | The core of Lemma 2. Weights higher on investing types give an investment rate of at least `G(r)` |
| `KimLoury2012.prop2` | Lemma 2 and Proposition 2. With `R(πA) ≥ R(πB)`, `φ(πA, πB) ≥ G(R(πA))` and `φ(πB, πA) ≤ G(R(πB))` |
| `KimLoury2012.trivial_ase` | Every PSE `x` gives the trivial ASE `(x, x)` |
| `KimLoury2012.control_no_selection_equal_return` | Control. With `R(πA) = R(πB)` there is no selection, `φ(πA, πB) = G(R(πA))`, even when `πA ≠ πB` |
| `KimLoury2012.control_reputation_without_return` | Control. With `πA > πB`, both wages higher in A and `R(πA) < R(πB)`, A's investment rate falls below `G(R(πA))` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KLI-D1 | The version read is the KDI Journal of Economic Policy article (2012). The published version in the Journal of Public Economics (2019) is not read | Results may have changed between versions. Rows citing this record cite the 2012 article as such |
| KLI-D2 | The positive selection runs toward the group with the greater return to skill, `R(πA) > R(πB)`, and coincides with the group with the better reputation only where `R` rises in `π` | Lemma 2 and Proposition 2 (p. 14) condition on `R`. Proposition 1 conditions on `π`. Figure 3, Panel A, right graph (p. 18), draws `πa > πb` with `R(πa) < R(πb)`, where `ΔU` rises in `c`. The abstract (KLI-Q1) and the conclusion (p. 15) say "better reputation". `R` is concave with `R(0) = R(1) = 0` (eq. 6, p. 11), so it falls above its peak. `control_reputation_without_return` |
| KLI-D3 | In Lemma 2, `φ(πA, πA)` is read as `G(R(πA))`, and in Proposition 2 the indices `i, j` range over the beliefs | The remark on p. 13 gives `φ(πA, πA) = G(R(πA))` when beliefs are equal. Proposition 2 writes `R(i)`, `φ(i, j)` with `i, j ∈ {πA, πB}` |
| KLI-D4 | The signal is read as supported on `[0, 1]` | The text says `t ∈ R+` (p. 10), but every integral in eqs. (2) to (8) runs from 0 to 1. Not formalised |
| KLI-D5 | The existence of a non-trivial ASE and the comparison of inequality are informal claims of the authors and not results of the article | The argument on p. 14 begins "Intuitively" (KLI-Q12), speaks of entering cohorts and stability without a law of motion, and concludes "can be greater" (KLI-Q13), while the abstract and introduction say "at least as great" |
| KLI-D6 | The strict inequalities of Proposition 1 and Proposition 2 are read as needing `H` strictly increasing and positive mass on both sides of the threshold | `prop1` assumes `StrictMono H`. `prop2` proves the weak inequalities, and `control_no_selection_equal_return` shows equality when the returns are equal |
| KLI-D7 | Typographical slips are read as intended | "argm ax" (p. 11), "identify choice" (p. 9), "Mulitple" (Figure 1, p. 17) |
| KLI-D8 | `kl_identity.txt` in the cache is an earlier extract that starts at the issue's contents page. It is not used | Quotations are pinned to `kl_kdi.txt` and pages to `kl_kdi.pdf` |
