# Coate and Loury (1993), claims

Evidence tag [F]: the full text was read on 15 October 2026, with the
equations read from page images (`RECONSTRUCTION.md`). Pages are the
journal's.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CL-Q1 | 1221 | "Employers form beliefs about the correlation between group identity and productivity which, in the equilibria of our model, must be correct." | In equilibrium the belief about a group equals what the group does. Paper A's re-learned prior (D1) has the same property at a fixed point of the composed map (M5, M6) |
| CL-Q2 | 1221 | "it is possible that employers' negative beliefs about a group are confirmed in equilibrium, even when all groups are ex ante identical." | A low belief can be self-confirming. Paper A's closure trap is a low standing that reproduces itself, for any group (M5, M6) |
| CL-Q3 | 1224 | "Equilibrium is then a pair of employer beliefs which are self-confirming." | Definition 1. The parallel to Paper A is to this equilibrium, not to the patronizing one |
| CL-Q4 | 1226 | "However, not all solutions of (3) are locally stable under the obvious adjustment process" | Their beliefs follow a one-dimensional map with several fixed points of different stability, as the standing does in Paper A (M5, M6) |
| CL-Q5 | 1226 | "the belief that no one in a group is qualified must be self-confirming, since it leads employers to assign everyone in that group to task zero, and no one would want to invest under those circumstances." | Where the parallel ends. In their model zero is always an equilibrium. In Paper A the map sends 0 to `μ > 0`, so standing 0 is never a fixed point (M4) |
| CL-Q6 | 1222 | "Our paper builds on the statistical literature, being close in spirit to Arrow's work." | Their own placement, used in the literature row |
| CL-Q7 | 1232 | "Whenever an employer assigns a failing worker to task one, we say the employer is patronizing that worker." | Patronizing needs the affirmative-action constraint. Paper A has no such constraint, so its trap is not their patronizing equilibrium |
| CL-Q8 | 1232 | "Thus, the only stable equilibrium is the patronizing one." | Proposition 3 in the uniform example, formalised as `converges_to_patronizing` |
| CL-Q9 | 1235 | "we call it a patronizing equilibrium under affirmative action if employers have (correct) beliefs about the inferiority of B's and therefore use a lower standard in order to be sure that B's are assigned to task one at the same rate as W's." | The definition, which names the constraint |
| CL-Q10 | 1237 | "no marginal subsidy can ever completely eliminate the stereotype against B's." | A small intervention returns to the original equilibrium. Paper A's analogue is a push that stays below the tipping point (M6) |
| CL-Q11 | 1237 | "a marginal subsidy cannot shift any solution of (3) outside of the "basin of attraction" of the original, locally stable, self-confirming belief." | Footnote 25. Basins of attraction are the object of Paper A's M6 |
| CL-Q12 | 1238 | "A sufficiently large worker subsidy will overcome the stereotype by eliminating all locally stable nonzero self-confirming beliefs except the one on the upward-sloping part of the WW curve" | A large intervention on the worker side removes the low equilibria. Paper A's analogue is mobility above the threshold (M9, M11) |
| CL-Q13 | 1227 | "group identity conveys information only because employers expect it to." | In their model the group carries no exogenous information. In Paper A the group carries the standing, which is a fact about the group's history |

## Lean results

`lean/mathlib/CoateLoury1993.lean`, in the paper's notation.

| Theorem | What it checks |
|---|---|
| `CoateLoury1993.xi_eq` | Eq. (1), the two forms of the posterior agree |
| `CoateLoury1993.assign_iff` | Eq. (2). Task one is optimal exactly when `r ≥ [(1 − π)/π] φ` |
| `CoateLoury1993.EE_locus` | The EE locus of Proposition 1's proof, `π = φ/(r + φ)` |
| `CoateLoury1993.zero_self_confirming` | `π = 0` solves (3) when `G(0) = 0` and `β(1) = 0` |
| `CoateLoury1993.g_sub_self` | Eq. (9). `g(π) − π = (π − π_ℓ)(π − (1 − π_ℓ))/(1 − π)` |
| `CoateLoury1993.g_fixed_iff` | The equilibrium beliefs about B's under affirmative action are `π_ℓ` and `1 − π_ℓ` |
| `CoateLoury1993.converges_to_patronizing` | Proposition 3's dynamics. With `π_ℓ > 1/2`, every start below `π_ℓ` converges to `1 − π_ℓ` |
| `CoateLoury1993.control_start_at_colorblind` | Control. From `π_ℓ` beliefs stay at `π_ℓ` |
| `CoateLoury1993.control_half_needed` | Control. At `π_ℓ = 1/4` the orbit of 0 does not converge to `1 − π_ℓ` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| CL-D1 | The text layer renders `π`, `φ`, `θ` and `ξ` as other characters, for example `π` as `r` or `7r`. Equations are read from the page images | Quotations are taken only from passages without symbols, so each is checked against the text layer |
| CL-D2 | Proposition 3 concerns the stability of `1 − π_ℓ` under eq. (9), from the start `π_c`. Our Lean result holds for every start below `π_ℓ` | The paper says "the solution of (9) converges to `1 − π_ℓ`" for the start `π_b^0 = π_c`, and `π_c < π̂ < π_ℓ`, so the paper's case is covered |
| CL-D3 | "Patronizing equilibrium" is used only under the affirmative-action constraint | Definition on p. 1235 (CL-Q9) and the term's introduction on p. 1232 (CL-Q7) |
