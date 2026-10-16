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
| KLI-Q15 | 7 | "methods that are known to be used for "partial passing" are: affections of speech, dressing up rather than wearing casual clothes, spending more on conspicuous consumption, and migration to affluent residential areas" | Markers correlated with standing that a person can buy or adopt. Spending and residence enter as signals, not as a budget that limits whom one can meet |
| KLI-Q16 | 7 | "outweighs its cost (e.g., loss of ties to one's own kind.)" | The value of ties to one's own group enters as the exogenous affect cost `k`, independent of ability (KLI-Q7) |
| KLI-Q17 | 8 | "Thus, the cultural group is treated preferentially by employers due to the higher fraction of skilled workers, even though the cultural traits of the group are not relevant for productivity." | A trait irrelevant to productivity is used in screening because it is correlated with the unseen type. The article reports this from Fang (2001) |
| KLI-Q18 | 8 | "Skilled and unskilled workers have different incentives to join a group with unique cultural traits that are expensive to obtain." | An expensive trait screens by differential gain, the mechanism of the ex-convict programme (KLI-Q14) |

## Lean results

`lean/mathlib/KimLoury2012.lean`, in the paper's notation. Eqs. (1) to (8) are
derived for a finite signal space (`Signal`). The selection results take `V₀`
and `V₁` as given functions and use a finite cost distribution in place of the
continuous `G`.

| Theorem | What it checks |
|---|---|
| `KimLoury2012.rhoRaw_strictMonoOn` | Below eq. (1). `ρ(π, t)` rises strictly in `π` |
| `KimLoury2012.rhoRaw_mono_ratio` | Below eq. (1). Under MLRP, `ρ(π, t)` rises with the likelihood ratio of the signal |
| `KimLoury2012.V_strictMonoOn` | p. 7. `V_e` rises strictly in `π` on `[0, 1]` |
| `KimLoury2012.R_zero_one` | p. 8. `R(0) = R(1) = 0` |
| `KimLoury2012.eq_4` | Eq. (4) |
| `KimLoury2012.eq_7_8` | Eqs. (7) and (8), as derivatives |
| `KimLoury2012.eq_5` | Eq. (5), as a derivative |
| `KimLoury2012.eq_6` | Eq. (6), as a derivative |
| `KimLoury2012.eq_6_neg` | Eq. (6). `R″ < 0` for an informative signal |
| `KimLoury2012.R_strictConcaveOn` | p. 8. `R` is strictly concave on `[0, 1]` |
| `KimLoury2012.Rp_zero` | p. 8. `R′(0) = ω Σ (f₁ − f₀)²/f₀ > 0` |
| `KimLoury2012.Rp_one` | p. 8. `R′(1) = −ω Σ (f₁ − f₀)²/f₁ < 0` |
| `KimLoury2012.Vp_ends` | p. 8. `V₀′(0) = ω` and `V₁′(1) = ω` |
| `KimLoury2012.Vp_cross` | p. 8. `V₁′(0) > ω` and `V₀′(1) > ω` |
| `KimLoury2012.R_lt_omega` | p. 9. `ω − R(π) = ω Σ f₁f₀/f > 0`, investment below the social optimum |
| `KimLoury2012.U_antitone_c` | Eq. (9). `U` is non-increasing in `c` |
| `KimLoury2012.eq_15` | Eq. (15). `U(π, c) = max {R(π) − c; 0} + V₀(π)` |
| `KimLoury2012.lemma1_low` | Lemma 1. Below both returns, `ΔU = V₁(πA) − V₁(πB)` |
| `KimLoury2012.lemma1_high` | Lemma 1. Above both returns, `ΔU = V₀(πA) − V₀(πB)` |
| `KimLoury2012.lemma1_mid` | Lemma 1, eq. (16). Between the returns, with `R(πA) ≥ R(πB)`, `ΔU = V₁(πA) − V₀(πB) − c` |
| `KimLoury2012.dU_antitone` | Lemma 1 as read on p. 14. `ΔU` is non-increasing in `c` when `R(πA) ≥ R(πB)` |
| `KimLoury2012.dU_swap` | p. 9. `ΔU(πB, πA; c) = −ΔU(πA, πB; c)` |
| `KimLoury2012.prop1_pointwise` | Every cost type gains from the group in which both wages are higher |
| `KimLoury2012.dU_pos_iff` | p. 10. `ΔU > 0` for every `c` if and only if `πA > πB` |
| `KimLoury2012.prop1_signal_wages` | In the signal model, `πA > πB` raises both wages, so Proposition 1 applies |
| `KimLoury2012.prop1` | Proposition 1. More than half the workers choose the better-reputed group |
| `KimLoury2012.sigmaB_eq` | Eq. (12). `Σ^B = 1 − Σ^A` |
| `KimLoury2012.prop1_both` | Proposition 1. `Σ^A > .5 > Σ^B` |
| `KimLoury2012.selRate_ge` | The core of Lemma 2. Weights higher on investing types give an investment rate of at least `G(r)` |
| `KimLoury2012.selRate_gt` | The same, strict |
| `KimLoury2012.prop2` | Lemma 2 and Proposition 2, weak form |
| `KimLoury2012.prop2_strict` | Lemma 2 and Proposition 2, strict, as the article states them |
| `KimLoury2012.lemma2_equal` | Lemma 2, last sentence. Equal returns give no selection in either group |
| `KimLoury2012.equal_beliefs` | p. 10. With equal beliefs, `φ(π, π) = G(R(π))` |
| `KimLoury2012.trivial_ase` | p. 10. Every PSE `x` gives the trivial ASE `(x, x)` |
| `KimLoury2012.not_ase_of_two_pse` | Section IV. Two PSE with different returns do not form an ASE |
| `KimLoury2012.cohort_gap` | Section IV. The gap between the groups' entering members exceeds the gap between their reputations |
| `KimLoury2012.rhoRaw_revealing` | Our extension, not a result of the article. At a signal only the skilled send, the posterior is 1 whatever the reputation |
| `KimLoury2012.control_group_matters_interior` | Control. At a signal both types send, the posterior depends on the reputation |
| `KimLoury2012.control_no_selection_equal_return` | Control. With `R(πA) = R(πB)` there is no selection, even when `πA ≠ πB` |
| `KimLoury2012.control_reputation_without_return` | Control. With `πA > πB`, both wages higher in A and `R(πA) < R(πB)`, A's investment rate falls below `G(R(πA))` |
| `KimLoury2012.control_prop1_needs_symmetry` | Control. Without the symmetry of `H`, fewer than half choose the better-reputed group |
| `KimLoury2012.control_uninformative_flat` | Control. With `f₁ = f₀`, `R` is zero everywhere, so the concavity results need an informative signal |
| `KimLoury2012.control_strict_needs_mass_above` | Control. Without mass above `R(πA)`, the strict selection of `prop2_strict` fails |

## Coverage of the source's results

Pages are journal pages, as the article numbers them.

| Result (as the source numbers it) | Lean name | Status | Reason for anything short of general |
|---|---|---|---|
| Eq. (1) and the claim below it, `ρ` rising in `π` and `t` under MLRP | `rhoRaw_strictMonoOn`, `rhoRaw_mono_ratio` | special case | Pointwise in the signal, for densities of full support. MLRP is used as a comparison of two signals |
| Eqs. (2) to (4) | `eq_4`, definitions `V`, `Rs` | special case | Finite signal space in place of densities on `[0, 1]` |
| `V_e′(π) > 0` (p. 7) | `V_strictMonoOn`, `eq_7_8` | special case | Finite signal space |
| Eqs. (5) and (6), concavity of `R`, `R(0) = R(1) = 0` (p. 8) | `eq_5`, `eq_6`, `eq_6_neg`, `R_strictConcaveOn`, `R_zero_one` | special case | Finite signal space. Differentiation under an integral is replaced by a finite sum |
| `lim R′ > 0` at 0 and `< 0` at 1 (p. 8) | `Rp_zero`, `Rp_one` | special case | Finite signal space. Proved directly, as the article gives no argument |
| Eqs. (7), (8), `V₀′(0) = V₁′(1) = ω`, `V₁′(0) > ω`, `V₀′(1) > ω` (p. 8) | `eq_7_8`, `Vp_ends`, `Vp_cross` | special case | Finite signal space |
| "It is more likely that `V₀′` tends to increase" (p. 8) | none | not formalised | Hedged in the source, and not used |
| Eq. (9), `U` rising in `π`, non-increasing in `c` | `U_antitone_c`, `prop1_pointwise` | general | |
| PSE, `π̂ = G(R(π̂))` (p. 8) | `IsPSE` | special case | Finite `G` |
| "either one or three equilibria" (p. 9) | none | not formalised | A remark about an S-shaped `G`, not a result |
| Investment below the social optimum, `G(R(π̂)) < G(ω)` (p. 9) | `R_lt_omega` | special case | Proves `R(π) < ω`. The step to `G` needs `G` strictly increasing at `ω`, which is not formalised |
| Eqs. (10) to (14) | `SigmaA`, `phi`, `sigmaB_eq` | special case | Finite `G` |
| Equal beliefs leave investment unchanged (p. 10) | `equal_beliefs` | special case | Finite `G` |
| ASE, and every PSE a trivial ASE (p. 10) | `IsASE`, `trivial_ase` | special case | Finite `G` |
| Eq. (15) | `eq_15` | general | |
| Lemma 1, eq. (16) | `lemma1_low`, `lemma1_high`, `lemma1_mid`, `dU_antitone` | general | The case `R(πA) < R(πB)` follows from `dU_swap` |
| "`ΔU > 0` for any `c` iff `πA > πB`" (p. 10) | `dU_pos_iff` | general | |
| Proposition 1 | `prop1`, `prop1_both` | special case | Finite `G` |
| Lemma 2 | `prop2`, `prop2_strict`, `lemma2_equal` | special case | Finite `G`. The strict form needs `H` strictly increasing and mass on both sides of the thresholds (KLI-D6) |
| Proposition 2 | `prop2_strict` | special case | As Lemma 2 |
| `(π_h, π_l)` not stable (p. 11) | `not_ase_of_two_pse` | special case | Read as "not an ASE". The article's "stable" has no law of motion behind it (KLI-D5) |
| Existence of two non-trivial ASE (p. 11) | none | not formalised | Not proved in the article (KLI-Q12). A proof needs a fixed-point theorem in two dimensions that the article does not supply |
| Entering-cohort gap exceeds the reputation gap (p. 11) | `cohort_gap` | special case | Finite `G` |
| Endogenous-group inequality "can be greater" / "at least as great" (pp. 2, 6, 11) | none | not formalised | Not proved in the article, and stated two ways (KLI-D5) |
| Ex-convict programme (p. 12) | none | not formalised | Stated in words, without a model |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| KLI-D1 | The version read is the KDI Journal of Economic Policy article (2012). The published version in the Journal of Public Economics (2019) is not read | Results may have changed between versions. Rows citing this record cite the 2012 article as such |
| KLI-D2 | The positive selection runs toward the group with the greater return to skill, `R(πA) > R(πB)`, and coincides with the group with the better reputation only where `R` rises in `π` | Lemma 2 and Proposition 2 (p. 14) condition on `R`. Proposition 1 conditions on `π`. Figure 3, Panel A, right graph (p. 18), draws `πa > πb` with `R(πa) < R(πb)`, where `ΔU` rises in `c`. The abstract (KLI-Q1) and the conclusion (p. 15) say "better reputation". `R` is concave with `R(0) = R(1) = 0` (eq. 6, p. 11), so it falls above its peak. `control_reputation_without_return` |
| KLI-D3 | In Lemma 2, `φ(πA, πA)` is read as `G(R(πA))`, and in Proposition 2 the indices `i, j` range over the beliefs | The remark on p. 13 gives `φ(πA, πA) = G(R(πA))` when beliefs are equal. Proposition 2 writes `R(i)`, `φ(i, j)` with `i, j ∈ {πA, πB}` |
| KLI-D4 | The signal is read as supported on `[0, 1]` | The text says `t ∈ R+` (p. 10), but every integral in eqs. (2) to (8) runs from 0 to 1. Not formalised |
| KLI-D5 | The existence of a non-trivial ASE and the comparison of inequality are informal claims of the authors and not results of the article | The argument on p. 14 begins "Intuitively" (KLI-Q12), speaks of entering cohorts and stability without a law of motion, and concludes "can be greater" (KLI-Q13), while the abstract and introduction say "at least as great" |
| KLI-D6 | The strict inequalities of Proposition 1 and Proposition 2 are read as needing `H` strictly increasing, `R(πA) ≠ R(πB)`, and positive mass on a type that invests in both groups and on a type that invests in neither | The article states them without these conditions. `prop1` and `prop2_strict` assume them, and `control_no_selection_equal_return` and `control_strict_needs_mass_above` show equality when the returns are equal or when no mass lies above the threshold |
| KLI-D7 | Typographical slips are read as intended | "argm ax" (p. 11), "identify choice" (p. 9), "Mulitple" (Figure 1, p. 17) |
| KLI-D8 | `kl_identity.txt` in the cache is an earlier extract that starts at the issue's contents page. It is not used | Quotations are pinned to `kl_kdi.txt` and pages to `kl_kdi.pdf` |
