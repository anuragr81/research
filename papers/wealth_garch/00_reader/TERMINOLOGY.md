# Terminology

Definitions for the terms used across the summary, the pitch, and any document written from here on. No term here is new; each is the name already used in the proofs document or in the standard literature it draws on. The symbol column gives the notation used in the proofs document, and is left blank where that document names the concept without assigning it a symbol.

| Term | Symbol | Definition |
|---|---|---|
| Capital buffer | X (continuous time), z (discrete time) | The bank's capital measured relative to the level it treats as required. The state variable of the model. In continuous time the state is a ratio on (1, infinity); in discrete time it is the log capital-buffer ratio, zero exactly at the requirement. |
| Book equity | E | The bank's equity in the discrete-time model. |
| Required capital | Q (level), q (ratio requirement) | The capital level the requirement sets. Exogenous and deterministically growing in the discrete-time model; in the state-mapping result it is a fixed ratio q of liabilities. Renamed from K to keep it distinct from the fixed issuance cost. |
| Reference level | R | The capital level separating what the bank treats as surplus from what it treats as deficit. Shortfalls are measured against it. |
| Surplus / deficit | z > 0 / z < 0 | The states above and below the reference level. |
| Asymmetry parameter | lambda-S | The weight the objective places on outcomes below the reference relative to equivalent outcomes above it. Equal to one when the bank is indifferent to the direction, and larger when shortfalls are penalised more heavily. |
| Shortfall penalty | Lambda(x) | The penalty term in the objective, equal to the excess weight times the shortfall below the reference, and zero above it. |
| Kinked-linear penalty | — | The form the shortfall penalty takes: proportional to the size of the shortfall, switching slope at the reference level and applying no penalty above it. Its graph has a corner at the reference but no curvature on either side. |
| S-shaped penalty | — | The alternative form used in prospect-theory models, curved in both directions and convex below the reference. Not the form used here; the contrast matters because it is what makes the present model tractable. |
| Running payoff | minus Lambda | The flow term in the objective, before any payout or injection. Concavity of the running payoff is a property of the objective, not of the value function. |
| Risk-neutral benchmark | lambda-S = 1 | The same control problem with the asymmetry parameter set to one, so the shortfall penalty vanishes identically. What the model predicts from regulation and cost structure alone, without any asymmetric preference. |
| Exact nesting | — | The property that the model reduces to the risk-neutral benchmark precisely, not approximately, at an asymmetry parameter of one. |
| Risky exposure | pi | The classical control: the share of the balance sheet carried in the risky asset. |
| Regulatory cap | pi-bar(x) | The state-dependent upper limit on risky exposure, tightening as the buffer erodes. |
| Capped exposure | u(x) | The exposure actually taken where the cap binds, equal to the smaller of a solvency-regime and a liquidity-regime branch. |
| Solvency / liquidity regime | x below / above x-bar | The two branches of the cap, separated by the level at which they cross. |
| Cap geometry | a-1, a-2, a-3 | The parameters describing how the regulatory cap varies with the buffer. Distinct from preference: quantities determined by cap geometry carry no information about the asymmetry parameter. |
| Dividends | D | The singular control, acting at the upper barrier. |
| Dividend barrier | y-star | The upper threshold at which the bank pays out. Also called the payout barrier. |
| Recapitalisation | I | The impulse control: lump-sum capital injections at the lower trigger. |
| Recapitalisation trigger | x-L | The lower threshold at which the bank raises capital. Also called the injection trigger or the recapitalisation boundary. |
| Injection target | y-post | The capital level the bank raises to when it recapitalises. Distinct from the trigger whenever raising capital carries a fixed cost. |
| Injection size | xi | The amount raised in a single recapitalisation. |
| Trigger-to-target gap | y-post minus x-L | The distance between the recapitalisation trigger and the injection target. The observable that responds to the fixed cost rather than to the preference. |
| Inaction region | the interval from x-L to y-star | The range of buffer levels between the recapitalisation trigger and the dividend barrier, within which the bank neither pays out nor raises capital. |
| Singular control | — | A control exercised in infinitesimal amounts at a boundary, with no cost to acting. The form dividend payment takes here. |
| Impulse control | — | A control exercised as discrete lump-sum interventions, made discrete by a fixed cost per intervention. The form recapitalisation takes here. |
| Fixed issuance cost | K | The component of the cost of raising capital that does not scale with the amount raised. What makes recapitalisation an impulse control, opens the trigger-to-target gap, and removes concavity of the value function. |
| Proportional issuance cost | kappa | The component of the cost of raising capital that scales with the amount raised. |
| Impulse operator | script-M | The operator expressing the value of recapitalising optimally from the current state, net of both cost components. |
| Value function | V | The optimised value of the objective as a function of the current buffer. |
| Verification theorem | — | The result establishing that a constructed solution of the optimality conditions is in fact the value function. Not proved here; stated as open. |
| Variance ratio | lambda-V to the fourth power | The ratio of the conditional variance of the buffer deep in deficit to its conditional variance deep in surplus. Also called the volatility ratio in plain-English summaries. The natural second-moment measure of asymmetry, and the object shown to be uninformative about the preference. |
| Induced variance asymmetry | lambda-V | The asymmetry parameter as it would be read off the variance ratio, as distinct from the asymmetry parameter in the objective. Its logarithm appears in the variance recursion, written out rather than given a symbol of its own. |
| Saturation limits | kappa-1(c), kappa-3(c) | The constant volatilities the buffer approaches deep in deficit and deep in surplus. Their ratio is what the variance ratio evaluates to, and both are functions of cap geometry and the return parameters alone. |
| Definitional evaluation | — | The variance ratio evaluated as a ratio of limits, from the model's structure directly. Distinguished from an estimate computed from simulated or observed paths, which is a different quantity. |
| Saturation scale | theta | The parameter governing how sharply the conditional variance transitions between the two regimes. The variance ratio is invariant to it. |
| Sojourn time | expected T | The expected length of time the buffer spends below the reference before returning to it. |
| Return on equity | r-sub-E | The return the bank earns on equity in the discrete-time model, pinned to growth over retention by the accounting identity. Renamed from rho to keep it distinct from the discount rate. |
| Discount rate | rho | The rate at which the continuous-time objective discounts future flows. |
| Persistence asymmetry | phi-minus, phi-plus | The maintained modelling assumption that deficits decay more slowly than surpluses, expressed as differing persistence coefficients on the two sides. |
| Correlation | c | The correlation between the risky return and the liability shock. Now the only use of this letter in the document. |
| Comparative statics | — | How the model's thresholds and value respond to changes in a parameter. The source of the claim that the thresholds move with the preference. |
| Identification | — | Whether a parameter can in principle be recovered from a given set of observable quantities. The paper's identification claim concerns thresholds versus variances, and is a property of the model, not a claim about any dataset. |
| Discrete-time model | — | The first of the paper's two formulations, in which the buffer is a sequence indexed by time period rather than a continuous-time process. Closed completely before the continuous-time formulation is introduced. |
| Continuous-time formulation | — | The paper's second formulation, in which the buffer evolves continuously and the bank exercises three controls of distinct type. Builds on, and is later reconciled with, the discrete-time model. |
| Accounting identity | — | The result that balanced growth of equity and required capital pins the bank's return on equity to growth divided by retention. |
| Classical control | pi | A control exercised continuously and without cost, applied here to risky exposure. Distinct from a singular control, which is costless but exercised only at a boundary, and an impulse control, which is exercised as discrete lump sums with a cost per intervention. |
| Asymmetric volatility specification | — | A volatility model, of the EGARCH type, in which the same shock produces a different conditional variance depending on whether the state is in surplus or deficit. The discrete-time recursion is shown to be observationally equivalent to one, which is what makes the variance ratio estimable from returns without observing required capital directly. |
| Liquidation payoff | — | The payoff realised if the bank liquidates rather than continues, used to construct a lower bound on the value function. |
| Distress boundary | — | The point at which the buffer exactly matches required capital, the lower end of the buffer's range. Where the apparent breakdown of the continuous-time model is shown to be a coordinate artefact rather than a real feature. |
| Coordinate artefact | — | A feature that appears to break the model down in one choice of coordinate but disappears entirely in another, and is therefore a property of the coordinate rather than of the model. Established here for the apparent degeneracy at the distress boundary. |
| Regularity | — | Whether the value function is continuously differentiable, and not merely continuous, at a given point. Assessed here at the recapitalisation trigger and left unresolved at the solver's current resolution. |
| Expected discounted shortfall | W | The shortfall accrued below the reference over time, discounted, under the optimal policy. Equals minus the sensitivity of value to the asymmetry parameter, so every threshold response reduces to a property of it. |
| Discounted injection count | N | The discounted number of recapitalisations under the optimal policy. Plays for the fixed issuance cost the role W plays for the asymmetry parameter. |
| Envelope identity | — | The result that the sensitivity of value to a parameter equals minus the corresponding accrued quantity, available because each parameter enters the objective linearly and neither restricts the admissible set. |
| Smooth fit | — | The condition that the value function's derivative matches across a free boundary. First-order at the trigger and the target; second-order at the barrier, where the first-order condition is degenerate. |
| Transversality condition | — | That the discount rate exceeds the maximal attainable portfolio return. Signs the barrier derivative, supplies the implicit function theorem's non-degeneracy at the barrier, and gives the boundary term in verification. |
| Joint local identification | — | That the two structural parameters can be recovered from the two thresholds, because the Jacobian of the thresholds in the parameters is non-singular. Turns on the two parameters moving the trigger in opposite directions, not on their magnitudes. |
| Liability-net discount rate | rho-L | The discount rate applying to the reduced, per-unit-liability problem: the gross rate less liability growth. What discounts the equation actually solved. |
