# Reconstruction: Glover, Johnson, Evatt and Cheng (2023)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/glover_johnson_evatt_cheng_2023.pdf`, sha256 `18dce30123a04a5259355e3f0508f1a464fa3a334e8723f6b137bd7522d83b73`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed in the PDF:** Kristoffer J. Glover, Paul V. Johnson, Geoffrey W. Evatt, Mingliang Cheng, "Capital Ideas: Optimal Capital Accumulation Strategies for a Bank and its Regulator." The PDF is the **preprint**: page 1 footer reads "Preprint submitted to The European Journal of Finance, December 1, 2022". It prints no volume, issue, page range or DOI. The PDF metadata title matches. (The 2023 European Journal of Finance publication comes from the task description. This PDF does not show it.)
- **File:** /root/.cache/wealth_garch/glover_johnson_evatt_cheng_2023.pdf
- **PDF pages:** 41.
- **Page mapping:** printed page = PDF page (offset 0). PDF page 1 (title, abstract, acknowledgements) has no printed number. Printed numbers 2 to 41 sit at the foot of PDF pages 2 to 41. Pages 35 to 41 hold Table 1 and Figures 1 to 9.
- **Coverage:** read in full, PDF pages 1 to 41, including Appendices A to D, the references, the table and the figure captions.

## 2. Reconstruction

**Agents and objective.** There are two participants (p. 6). The bank maximises the market value of equity V. The regulator fixes a capital adequacy ratio ω at t = 0. The regulator's objective (eq. 3, p. 7) is min over ω of αP + βZ* − γL*, where P is the closure probability, Z the expected loss on closure and L the loan book. The authors stay agnostic on the weights and only trace each metric across ω (p. 7). The regulator's problem is therefore never solved as an optimisation; ω is swept exogenously.

**State and dynamics.** The state is X = (t, L, E): loans and book equity, with deposits D held constant (eq. 10, p. 9). Loans follow dL/L = (l − a)dt − (µ + λK)dt − dQ̂ (eq. 4, p. 8). Defaults combine a continuous rate µ with compound-Poisson jumps of intensity λ and truncated-exponential sizes on [0,1] with mean K (eq. 5, p. 8). **Notation warning:** here λ and K are the jump intensity and the mean jump size. They are not the theory paper's λ_S and K. Equity follows dE = (Φ − u + f + f_ω)dt − L dQ̂ (eq. 8, p. 9), with expected profit Φ = r_R E + (r_L − r_R)L − (r_D − r_R)D − (running cost) − (µ + λK)L (eq. 11, p. 9). The running cost is convex in loans: a fixed part plus (1/2)·c₁·L² (eq. 21, p. 14). Its symbol does not render in the text layer.

**Controls** (p. 6): the loan-issuance rate l ∈ {0, l̄}, the dividend rate u, the voluntary equity-issuance rate f, and the closure time ν. Optimal controls are bang-bang (p. 17), and the free region splits into dividend, loan-issuance, equity-issuance and inaction subregions (pp. 17, 20–21; Fig. 4, p. 37).

**Regulatory constraint and frictions.**
- Capital constraint E/L ≥ ω (eq. 1, p. 6). If a loss pushes the bank below ωL while it is still solvent, the regulator **forces an immediate issue** f_ω of max{ωL − E, 0}, restoring the ratio exactly (pp. 6, 10).
- Insolvency (E < 0): resolution, and shareholder value is zero (boundary condition V = 0 on E ≤ 0, p. 11).
- Endogenous closure on a free boundary L = C*(E), where V = E − φL, with φ a proportional liquidation cost (eq. 12, p. 9).
- **Issuance cost:** ψ = ψ₀(f + f_ω). It is proportional to the amount raised and applies equally to voluntary and forced issues (p. 10). The authors say a fixed cost "can also be included" but would only enlarge the parameter space without altering insights (p. 10). The issuance criterion is f* = 0 if ∂V/∂E < 1 + ψ₀ (eq. 28, p. 18). Appendix D writes a general ψ(E_n) with first-order condition ∂V/∂E_n = 1 + ψ′(E_n) (eqs. D.5–D.6, p. 31), but with proportional costs the bank issues "the moment" equity hits the financing line (p. 31).
- **Cost of carry:** r_R < ρ, so internally held equity is costly (pp. 15, 18).
- Agents are risk-neutral and discount at ρ (p. 11).

**Value function.** Value (eq. 13, p. 11) solves a stationary partial integro-differential HJB (eq. 14, p. 11; condition 15, p. 11). The closure probability and expected loss solve linear PIDEs (eqs. 16–19, p. 12). The authors solve numerically with a semi-Lagrangian scheme (p. 18; Appendix D, pp. 30–31).

**Deterministic benchmark** (λ → ∞, K → 0, λK fixed; pp. 14–17). The bank sits at a stationary point and never issues equity (f* = 0, p. 14).
- Proposition 1 (eq. 23, p. 15) gives the optimal balance sheet, piecewise in ω, with thresholds ω₀ and ω_c (eq. 24).
- Corollary 2 (p. 16): ω_c rises in r_L and falls in D, c₁, λ, K and r_R.
- Proposition 3 (eq. 25, p. 16) gives the closure threshold ω_max.
- Corollary 4 (p. 17) gives the comparative statics of ω_max.
- The issuance cost ψ₀ does not enter any of these, because the deterministic bank never issues.

**Calibration** (pp. 18–19; Table 1, p. 35). Everything comes from external sources or simple ratio matching:
- D = 100 $bn; r_R = 1%; ρ = 3.5%; r_D = 2.5% (following Hugonnier and Morellec).
- ψ₀ = 5%, the average underwriter spread for bank seasoned equity offerings 1996–2007 (Boyson, Fahlenbrach and Stulz). φ = 5% "of the same magnitude".
- µ + λK = 2%, the FRED loan-loss reserve mean for 1984–2020.
- r_L = 7.5%, set to match a 3% net interest margin.
- Running cost / D = 2.2%, set to match a 0.8% return on assets.
- λ = 0.8 per year; l̄ = 5.5%.

**Main results (numerical, stochastic model)**
- Result 1 (p. 20): loss uncertainty plus costly issuance create a precautionary equity buffer.
- Result 2 (p. 20): high ω leads the bank to hold cash.
- Result 3 (p. 21): for small ω (≤ ~10%) the constraint does not bind. The endogenous capital ratio is about 13.7% (p. 22).
- Result 4 (p. 22): a higher ω raises the capital ratio but shrinks the buffer above the requirement. The buffer stays positive even at high ω.
- Result 5 (p. 22): closure probability is minimised at an interior ω* ∈ (25%, 30%). The headline result.
- Result 6 (p. 23): that ω* is robust to the loss frequency λ.
- Result 7 (p. 24): the capital ratio is non-monotone in loss variance.
- Result 8 (p. 25): the lending-maximising ω is robust to λ.
- Deposit-size scaling (pp. 25–27).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a capital requirement.** There is no running or graduated shortfall penalty. A breach of E ≥ ωL triggers an immediate regulatory **forced recapitalisation** back to exactly ωL (pp. 6, 10). The bank is never allowed to stay below the line; the region below it is where the regulator forces recapitalisation (p. 20). The cost of a breach is therefore the issuance cost on the forced amount, ψ₀·(ωL − E). That is proportional to the size of the shortfall but is paid **once, as a lump at the breach**, not as a flow while below (p. 10; eq. 28, p. 18). It is asymmetric: it is charged only below the line, and shareholders bear it. Below zero equity, the consequence is resolution with total loss of shareholder value (pp. 6, 11). The deadweight φL and any negative equity enter the regulator's loss metric Z (eq. 18 and fn. 12, p. 12). There is no fine, stigma, or dividend restriction short of the forced issue. On the surplus side, the cost of carry (r_R < ρ) makes equity above need costly (p. 15).

**(b) Costs of issuing equity.** The cost is proportional only: ψ = ψ₀(f + f_ω), ψ₀ = 5%, the same for voluntary and forced issues (p. 10; Table 1, p. 35). There is no fixed cost. The authors say explicitly that one could be added but would not change the insights (p. 10). Issues are therefore not lumpy in the impulse-control sense. The bank issues "just enough new equity to regain equality" (p. 18) and raises equity "the moment" it drops below the financing line (p. 31). Issue rates may be unbounded over an infinitesimal interval, giving finite amounts (fn. 16, p. 17).

**(c) Both frictions; interaction; comparative statics; identification.**
- The paper does **not** have both frictions as the theory paper defines them. There is no graduated shortfall penalty and no fixed issuance cost. Its two frictions at the bottom are a hard constraint enforced by costly forced issuance, plus proportional issuance costs.
- Threshold comparative statics are analytic only in the deterministic model. They are for the regulatory thresholds ω_c and ω_max, in r_L, D, c₁, λ, K, r_R, ρ and φ (Corollaries 2 and 4, pp. 16–17), not for dividend or issuance boundaries.
- In the stochastic model the dividend, loan and issuance regions are shown only graphically, for ω = 0, 10, 20, 30% (Fig. 4, p. 37; discussion pp. 20–21). Comparative statics are reported in ω, λ and D (pp. 19–27).
- **No comparative statics in ψ₀ are reported**; ψ₀ is held at 5% throughout.
- The paper does not ask whether its frictions can be told apart. It does not identify or estimate any parameter from the policy. Calibration uses external moments: underwriter spreads, FRED loan-loss reserves, net interest margin, return on assets (pp. 18–19).

**Buffers and variance.**
- Buffers above the requirement are central (Results 1, 3, 4; pp. 20–22): about a 13.7% endogenous ratio when unconstrained (p. 22), and a buffer that shrinks but stays positive as ω rises (p. 22).
- Loss variance (∝ λK², p. 23) is varied with the mean fixed. The capital ratio is non-monotone in it (Result 7, p. 24).
- There is **no analysis of the variance or distribution of the capital or buffer process itself**. The risk metrics are closure probability and expected loss (pp. 12, 22–25).
- Exposure is not capped by a regulatory cap. The loan-issuance rate is capped at l̄ because of customer demand (p. 17), and loan size is limited by convex costs (p. 13).

**Stated limitations and future research** (pp. 25, 27–28):
- constant deposits, so no liquidity risk; capital-liquidity interaction;
- risky or convertible debt;
- multiple asset classes and risk weights;
- fixed prices (r_L, r_D, r_R, ψ constant);
- a banking network or equilibrium;
- the leverage ratio versus risk weights (p. 25).

## 6. Verdict for the theory paper

1. The paper does **not** contain a graduated running shortfall penalty. A breach of the capital ratio triggers immediate forced recapitalisation back to the requirement, and the only cost is the proportional issuance cost on the forced amount (pp. 6, 10). Insolvency means resolution with zero shareholder value (pp. 6, 11).
2. It does **not** contain a fixed issuance cost. The cost is proportional, ψ₀ = 5%, and the authors explicitly set a fixed cost aside (p. 10). Issuance is therefore barrier-type rather than lumpy (pp. 18, 31).
3. Its threshold comparative statics are analytic only for regulatory ratios in the deterministic model (Corollaries 2 and 4, pp. 16–17). Stochastic results vary ω, loss frequency and deposit size, never the issuance cost (pp. 19–27). It reports nothing on how a dividend barrier or an issuance trigger moves with either friction.
4. It does **not** ask whether frictions can be told apart, and it identifies nothing from the policy. Parameters are calibrated to underwriter spreads, loan-loss reserves, net interest margin and return on assets (pp. 18–19). One parameter, ψ₀, prices both voluntary and forced issues (p. 10).
5. It is relevant prior art on precautionary buffers above a requirement (Results 1, 3, 4, pp. 20–22). It does not study the variance of capital or a regulatory cap on exposure (pp. 12, 17, 22–25).
