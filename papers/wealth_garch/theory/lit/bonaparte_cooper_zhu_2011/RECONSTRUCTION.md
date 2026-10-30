# Reconstruction: Bonaparte, Cooper and Zhu (2011)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/bonaparte_cooper_zhu_2011.pdf`, sha256 `9079f17d381bc621301cadb0684b104a66c058c338804770745e62eb57903fdd`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** Yosef Bonaparte, Russell Cooper and Guozhong Zhu, "Consumption Smoothing and Portfolio Rebalancing: The Effects of Adjustment Costs", NBER Working Paper 16957, http://www.nber.org/papers/w16957, National Bureau of Economic Research, 1050 Massachusetts Avenue, Cambridge, MA 02138, April 2011 (PDF p. 1). The abstract page gives NBER Working Paper No. 16957, April 2011, JEL No. E21, G11, on separate lines (PDF p. 2). The title page of the paper itself is dated "April 10, 2011" (PDF p. 3).
- **File.** `/root/.cache/wealth_garch/bonaparte_cooper_zhu_2011.pdf`. pdftotext prints xref/syntax warnings but extracts every page.
- **PDF pages.** 27 (pdfinfo).
- **Page mapping.** PDF p. 1 is the NBER cover and PDF p. 2 is the NBER abstract page. Neither has a printed number. Printed p. 1 (title page, abstract and start of Section 1) is PDF p. 3, so **printed page = PDF page − 2**. The last page, printed p. 25 (references), is PDF p. 27.
- **Coverage.** Read in full: all 27 PDF pages. That covers the text (printed pp. 1–19) and Appendix 6, which comprises 6.1 Data, pp. 20–22; 6.2 Computation, pp. 22–23; and 6.3 Parameter Sensitivity with Table 7, pp. 23–24. It also covers the references (pp. 24–25).
- **Symbols.** The text layer drops the underline on the borrowing limit. Eqs. (2) and (4) print "Ab ≥Ab", where the second term is the lower bound on bonds (printed p. 4).

## 2. Reconstruction

**Agent and assets** (pp. 3–4). An infinite-horizon household holds two assets:
- **bonds**, which are riskless and trade at zero cost, with gross return 1.02;
- **stocks**, which are risky and costly to trade.

Paper 1 (Bonaparte and Cooper, cited as 2010) had a single asset; this model extends it (footnote 1, p. 3).

**State.** (y, A₋₁, R₋₁): labour income, last period's bond and stock holdings, and last period's returns (p. 3).

**Bellman equations** (p. 4). "Adjustment" means trading stocks.
- Eq. (1): v = max{vᵃ, vⁿ}.
- Eq. (2), adjust: vᵃ = max over (A_b ≥ lower bound, A_s ≥ 0) of u(c) + βE v(y′, A, R).
- Eq. (3), budget when adjusting: c = Σ_{i=b,s} R₋₁ⁱ A₋₁ⁱ + y·ψ − Σ Aⁱ − C(·).
- Eq. (4), do not adjust: vⁿ = max over A_b of u(c) + βE v(y′, A, R).
- Eq. (5), budget when not adjusting: c = R₋₁ᵇ A₋₁ᵇ + y − A_b.
- Eq. (6), law of motion when not adjusting: A = (A_b, A₋₁ˢ R₋₁ˢ). Stocks, with dividends, are reinvested at no cost.
- The household can therefore smooth consumption through bonds without paying the stock adjustment cost.

**Adjustment costs.** Two, both paid only when stocks are traded.
1. **Direct cost C(·)**, paid to an intermediary (p. 4).
   - Eq. (7), buying: Cᵇ = ν₀ᵇ + ν₁ᵇ ΔA_s + ν₂ᵇ ΔA_s².
   - Eq. (8), selling: Cˢ = ν₀ˢ + ν₁ˢ(A₋₁ˢ − A_s) + ν₂ˢ ΔA_s².
   - The coefficients are taken from Paper 1 (pp. 5–6; Table 1, p. 6, reproduces Paper 1's Table 1). The paper says that only the fixed costs of trade are economically significant (the sentence runs across pp. 5–6) and that the actual fixed costs may be lower than estimated, because the cost data (1991–96) predate the PSID sample (p. 6).
2. **Opportunity (time) cost ψ** in eq. (3): a fraction 1 − ψ of stochastic labour income is lost in any period with a stock trade (pp. 2, 4). The paper calls it "the total time cost of making an optimal choice about stock holdings" and says it is a cost of trading, not of becoming informed (footnote 3, p. 4). It is a **fixed cost per adjustment, scaled by income**.

There is no probability-of-adjustment friction.

**Borrowing limit.** A lower bound A_b on bonds is estimated jointly. The best fit is the corner A_b = 0, meaning no borrowing (p. 8).

**Preferences.** CRRA with curvature γ and discount factor β (p. 7).

**Exogenous processes** (p. 5; appendix pp. 20, 22).
- Income for stockholders follows an AR(1) from the PSID, with ρ = 0.842 and σ = 0.29, discretised to 5 states.
- Stock returns are i.i.d., with two states: −9.17% and 21.83%.

**Estimation** (Section 3.2, pp. 6–9).
- **Method.** Simulated method of moments for (γ, β, ψ), with C(·) and the exogenous processes fixed.
- **Objective.** Eq. (9): Γ = min (Mˢ − Mᵈ)′W(Mˢ − Mᵈ), where W is the inverse of a diagonal matrix of moment variances. Covariances are set to zero because the moments come from different data sets (p. 7).
- **Four moments** (pp. 6–7; Table 2, p. 9), so the design is over-identified by one:
  1. The **net stock adjustment rate**, 0.467 per two years (PSID 1999–2007; Table 6, p. 21). The paper says it is "particularly informative about the opportunity cost of stock trading".
  2. The **stock share** of financial wealth, 0.6902 (SCF; Table 5, p. 21). It is informative about γ and ψ.
  3. The **"EIS"** α₁: the response of shareholders' consumption growth to returns, 0.2984, from Vissing-Jorgensen's data (p. 7; 2SLS per p. 22).
  4. The **median wealth/income ratio**, 2.127 (SCF). It matters for β and γ, and the paper gives directions: higher β and higher γ each raise it (p. 7).
- **Simulated panel.** 500 households over 800 periods, with the adjustment rate computed biennially as in the PSID (p. 8).
- **Standard errors** come from the numerical derivatives of the moments with respect to the parameters (p. 8, citing Adda and Cooper 2003).
- **Table 7** (p. 24; Appendix 6.3, p. 23) reports the **elasticities of the moments with respect to the parameters** and is described as "informative about identification".

**Results** (Table 2, p. 9).

| | β | ψ | γ |
|---|---|---|---|
| Estimate | 0.7048 | 0.9882 | 6.4313 |
| Standard error | (0.125) | (0.0147) | (0.1945) |

| Moment | Simulated | Data |
|---|---|---|
| Adjustment rate | 0.466 | 0.467 |
| Stock share | 0.739 | 0.6902 |
| α₁ | 0.223 | 0.2984 |
| Wealth/income | 2.139 | 2.127 |

- The adjustment cost is 1 − ψ = 0.012, about $850 at average income. Together with the broker fixed cost, the total fixed cost is about $905 (p. 9).
- The fit is Γ = 0.8675 with one degree of freedom, p-value 0.352 (p. 9).
- The robustness rows of Table 2 (high β, less volatile income, less persistent income, risky bonds, serially correlated returns) are simulations at the estimated parameters, not re-estimations (p. 9).

**Policy functions and the inaction region** (Figure 1, p. 11; Section 4.1, p. 12).
- Figure 1 plots future stock and bond holdings against current stock holdings (including the return) for low, median and high income. The current bond holdings and the return are held at their means.
- Inaction is where the stock policy lies on the 45° line. There is a sizable inaction region in every income state, and adjustment happens only in the tails.
- The **critical values of current stockholdings defining the inaction region** depend on income: in the low-income state the band is narrower and both critical values are lower. The paper attributes this partly to the lower opportunity cost of trading when income is low, and partly to larger gains from adjusting (p. 12).
- Inside the band, bonds fall as stocks rise. When the household adjusts, bonds move opposite to stocks. The bond policy is non-linear even though the costs apply only to stocks (p. 12).
- The thresholds are **described qualitatively only**: the paper gives no numerical values and no comparative statics in ψ or γ.

**Other results.**
- Small and large income shocks: Figures 2–3, pp. 13–14.
- Return shocks at the individual and aggregate level: Figures 4–5, pp. 15–16. The individual stock–bond correlation is −0.103 and the aggregate one is 0.94 (pp. 15–16).
- Regressions of decision rules on simulated data: Table 3, p. 18.
- PSID evidence on the stock–bond substitution: Table 4, p. 19. Corr(ΔB, ΔS) is −0.338 in the data and −0.467 in the model (p. 18).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a capital requirement or threshold.** No. This is a household paper, with no capital requirement and no shortfall penalty.
- The only threshold is a hard **borrowing limit on bonds**, estimated at the corner A_b = 0 (pp. 4, 8).
- It is a constraint, not a graduated or asymmetric cost.

**(b) Costs of adjusting (the analogue of issuance costs).** Yes, on stock trades only. Bonds trade at no cost (p. 3).
- **Broker cost (eqs. 7–8, p. 5; Table 1, p. 6).** Fixed per trade (about $56 to buy, about $61 to sell), plus proportional and quadratic terms. The parameters are imported from Paper 1, and the paper says only the fixed part matters economically (pp. 5–6).
- **Opportunity cost of time (eq. 3, p. 4).** A fixed cost per adjustment equal to a fraction 1 − ψ of labour income (pp. 2, 4). It is estimated at 0.012 of income, about $850 (p. 9).
- **Lumpiness.** Adjustment is lumpy: "large scale changes to their stock account" (p. 2), plus the rebalancing dynamics described on p. 14.

**(c) Both frictions, interaction, comparative statics, identification.**
- *Two frictions in the sense of (a) and (b)?* No. The paper estimates **two preference parameters (γ, β) jointly with one adjustment cost (ψ)** from four moments by SMM (pp. 6–8).
- *Identification argument.* There is a moment-by-parameter narrative (pp. 6–7):
  - the adjustment rate is "particularly informative" about ψ;
  - the stock share "contains information" on γ and ψ, since a smaller γ together with a modest ψ cost induces bond holdings;
  - the wealth/income ratio is important for β and γ, and both raise it.
- *Sensitivity matrix.* **Table 7 (p. 24)** is a 3×4 matrix of the elasticities of the moments with respect to (β, ψ, γ). It is described as "informative about identification as it indicates which moments are particularly sensitive to variations in each of the parameters" (pp. 8, 23). The paper does **not** call it a Jacobian, does not discuss its rank or conditioning, and does not say whether identification is local or global. Being built from numerical derivatives at the estimate (p. 8), it is a local object.
- *Same or opposite directions? (my reading of Table 7, D2).* Write the cost as 1 − ψ, so a positive elasticity with respect to ψ means the cost *lowers* the moment.
  - **Adjustment rate:** a higher cost and a higher γ both lower it (ψ: +194.463; γ: −7.584). Same direction.
  - **Stock share:** a higher cost and a higher γ both lower it (ψ: +27.953; γ: −6.945). Same direction.
  - **α₁:** a higher cost and a higher γ both lower it (ψ: +47.322; γ: −1.976). Same direction.
  - **Wealth/income ratio:** this is the only moment on which they move in **opposite** directions. A higher cost lowers it (ψ: +4.922) and a higher γ raises it (+20.255). β moves it most (+31.944).
  - The paper itself makes none of these sign comparisons.
- *Hard to separate?* No statement that any parameter is hard to separate. The paper only flags that the low β (0.7048) is driven by the wealth/income moment (pp. 8–9).
- *Thresholds.* Identification uses **only moments of behaviour**: the adjustment frequency, the portfolio share, the consumption–return coefficient and the wealth ratio. The **thresholds of the inaction region are not used**. They are shown qualitatively after estimation (pp. 11–12).
- *Comparative statics of thresholds in the frictions.* None. The paper reports only state dependence on income (p. 12) and, in Table 3, regressions on simulated data in which adjustment falls with income in the pooled sample and the signs reverse in the high-income sample (p. 17).
- *Bias finding.* This paper is more guarded than Paper 1 and never uses the word *bias*.
  - p. 2: with non-convex adjustment costs "the standard Euler equation does not hold even for stockholders".
  - p. 8: the estimated γ ≈ 6.4 "is not the same as the inverse of the estimated EIS from the data". With adjustment costs "there is no direct link between the inverse of the EIS and preference parameters". The aggregate response depends on both the extensive margin (adjusting or not) and the intensive margin.
  - p. 7: the existing literature needs large risk aversion to generate bond holdings, whereas here a smaller γ plus a modest adjustment cost does so.

**Buffers and variance.** This is a buffer-stock model (p. 3): impatient households hold financial wealth for precautionary reasons, with a median wealth/income ratio of 2.14 (p. 3).
- Bonds act as the buffer against small income shocks. Stocks are adjusted only for large shocks (pp. 3, 13–14, 19).
- Stock share and wealth/income are sensitive to income volatility and persistence (pp. 9–10).
- None of this concerns a regulatory capital buffer.

**Stated limitations and future work.**
- Life-cycle considerations "may be of additional interest" (footnote 2, p. 3).
- No durable goods, so no collateralised borrowing (footnote 4, p. 4).
- Housing is ignored (p. 8).
- The weighting matrix "assumes that covariances are all zero" (p. 9).
- The broker fixed costs may be overstated for the PSID period (p. 6).
- Estimating with risky bonds is "very time consuming", so the baseline keeps a constant bond return (footnote 7, p. 12).
- Re-estimating with serially correlated beliefs did not improve the fit (footnote 8, p. 12).
- The model abstracts from participation: every household is a stockholder (p. 5).

## 6. Verdict for the theory paper

1. This paper is the closer of the two precedents. It jointly estimates preference parameters (γ, β) and a fixed, income-scaled adjustment cost (ψ) by over-identified SMM (pp. 6–9), and its policy has an explicit inaction region with income-dependent critical values (pp. 11–12).
2. It does **not** use those thresholds for identification. The moments are the adjustment frequency, the stock share, a consumption–return coefficient and the wealth/income ratio (pp. 6–7), and the thresholds appear only qualitatively after estimation (p. 12).
3. Its only identification device is Table 7, an elasticity (sensitivity) matrix of moments to parameters. The paper describes it as "informative about identification" (pp. 8, 23–24) but never discusses its rank, local versus global identification, or which parameter moves which moment in which direction.
4. Read off Table 7, the adjustment cost and γ push the adjustment rate, the stock share and α₁ in the **same** direction. Only the wealth/income ratio moves in opposite directions (p. 24; my reading, D2). This is unlike the theory paper's opposite-direction effect on a threshold.
5. Its bias statement is that the γ of about 6.4 is not the inverse of the measured EIS, because with adjustment costs "there is no direct link" (p. 8).
6. It has no shortfall penalty or capital requirement, only a hard borrowing limit at zero (p. 8).
