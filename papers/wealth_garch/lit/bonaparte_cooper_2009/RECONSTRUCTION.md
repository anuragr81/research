# Reconstruction: Bonaparte and Cooper (2009)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/bonaparte_cooper_2010.pdf`, sha256 `487ed16b1aea7f2f352e5275da069479a5f7325bbc6262bc985c34fd7b4000b2`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** Yosef Bonaparte and Russell Cooper, "Costly Portfolio Adjustment", NBER Working Paper 15227, http://www.nber.org/papers/w15227, National Bureau of Economic Research, 1050 Massachusetts Avenue, Cambridge, MA 02138, August 2009 (PDF p. 1). The abstract page gives NBER Working Paper No. 15227, August 2009, JEL No. E21, E44, G11, on separate lines (PDF p. 2). The title page of the paper itself is dated "August 1, 2009" (PDF p. 3).
- **Version.** This is the August 2009 NBER working paper. The file name says 2010, and Bonaparte, Cooper and Zhu (2011) list this paper in their references as Bonaparte and Cooper (2010), NBER Working Paper #15227 (BCZ printed p. 24). Nothing in this PDF is dated 2010. The cover page gives Bonaparte's affiliation as Claremont McKenna College (PDF p. 2), but the footnote on PDF p. 3 gives The University of Southern Mississippi.
- **File.** `/root/.cache/wealth_garch/bonaparte_cooper_2010.pdf`
- **PDF pages.** 20 (pdfinfo).
- **Page mapping.** PDF p. 1 is the NBER cover and PDF p. 2 is the NBER abstract page. Neither has a printed number. Printed p. 1 (the title page, abstract and start of Section 1) is PDF p. 3, so **printed page = PDF page − 2**. The last page, printed p. 18 (references), is PDF p. 20.
- **Coverage.** Read in full: all 20 PDF pages. That covers the text (printed pp. 1–17), the conclusion (p. 17) and the references (pp. 17–18). The paper has **no appendix**.
- **Identification section?** None. The identification statements are in Section 3 (p. 6), Section 3.3 (pp. 10–11) and footnotes 17 and 19 (pp. 10–11).

## 2. Reconstruction

**Agent and timing.** One household solves an infinite-horizon dynamic discrete-choice problem. Each period it either adjusts its portfolio or does not (p. 4).

**State.** (y, s₋₁, R₋₁): current labour income y; the vector of existing asset holdings s₋₁, which already includes past capital gains and purchases; and last period's return vector R₋₁, which carries information about next period's returns (p. 5).

**Bellman equations** (p. 5).
- Eq. (2): v = max{vᵃ, vⁿ}.
- Eq. (3), adjust: vᵃ = max over s of u(c) + βE v(y′, s, R).
- Eq. (4), budget when adjusting: c = Σᵢ R₋₁ⁱ s₋₁ⁱ + y·ψ − Σᵢ sⁱ − C(s₋₁, s).
- Eq. (5), do not adjust: vⁿ = u(y) + βE v(y′, s, R). Consumption equals labour income, and the portfolio is reinvested at no cost. Eq. (6) is the law of motion when there is no adjustment: sⁱ = R₋₁ⁱ s₋₁ⁱ (p. 6).
- There is no saving out of income without adjusting. Income variations "are met by variations in consumption, not in the holdings of assets" (p. 4).

**Adjustment costs.** There are two, and both are paid only in a period with a trade.
1. **Direct trading cost C(·)** paid to a broker. It is separable across assets and asymmetric between buying and selling (p. 7).
   - Eq. (7), buying: Cᵇ = ν₀ᵇ + ν₁ᵇ(sⁱ − s₋₁ⁱ) + ν₂ᵇ(sⁱ − s₋₁ⁱ)².
   - Eq. (8), selling: Cˢ = ν₀ˢ + ν₁ˢ(s₋₁ⁱ − sⁱ) + ν₂ˢ(sⁱ − s₋₁ⁱ)².
   - So the cost has a fixed part per trade, a proportional part and a quadratic part.
   - It is estimated **outside the model**. The authors regress the commission on trade value and trade value squared, using Barber–Odean discount-broker account data for about 78,000 households, 1991–96 (pp. 7–8). The estimates are in **Table 1 (p. 8)**: ν₀ = 56.10 (buy) and 61.44 (sell) dollars; ν₁ = 0.0012 and 0.0014; ν₂ = −1.01e−10 and −1.28e−10. The paper reports "a fixed cost of trading of around $60.00" and says the quadratic term is negligible (p. 8).
2. **Time cost ψ** (p. 5). It is a multiplicative loss of labour income in any period of adjustment (y·ψ with ψ < 1). It is "lost income due to time spent on portfolio adjustment" and is distinguished from the broker's costs.
   - It is therefore a **fixed cost per adjustment**, independent of the size of the trade but **proportional to income**.
   - The paper says it could instead have used a direct, additive and stochastic utility cost à la Rust (footnote 10, p. 7). It chose not to.
   - There is **no random probability of adjustment** (no Calvo-type adjustment).

**Preferences** (p. 7). Utility is CRRA, u(c) = c^{1−γ}/(1−γ). The paper notes that for this form "our estimate of risk aversion is the inverse of the elasticity of intertemporal subsitution" (sic). β = 0.96 is fixed.

**Quantitative simplifications** (pp. 6, 9, 11).
- I = 1 (a single composite asset). With one asset, the decisions of portfolio adjustment and intertemporal consumption are the same.
- Income is a common AR(1) times an idiosyncratic AR(1), both from the PSID. Returns are an AR(1) fitted to Shiller real returns including dividends.
- 50% of households are added as non-participants who consume their income.
- The grid has 5 return states, 3 idiosyncratic and 3 aggregate income states, and 200 asset points, with a zero lower bound on assets (footnote 18, p. 11).

**Estimation** (Section 3.3, pp. 10–12).
- **Method.** Simulated method of moments for (γ, ψ), with ν taken from Table 1 and β = 0.96.
- **Two moments, just identified, identity weighting matrix** (footnote 19, p. 11):
  1. The annual SCF portfolio adjustment rate, 0.71 (p. 10). The SCF questions are cited on p. 2, footnote 3.
  2. The coefficient α₁ in the aggregate regression of log consumption growth on the log return, eq. (9), p. 10, estimated at 0.0138 for 1967–92. The paper calls it the "aggregate EIS" and says it offers "no structural interpretation" of it.
- **Simulated panel.** 500 participant households and 500 non-participants over 500 periods (p. 11).

**Results.**
- **Table 2 (p. 11)**, baseline "Both (γ, ψ)": γ = 1.00067 and ψ = 0.992, giving a simulated adjustment rate of 0.894 and α₁ = 0.534, with fit 0.305.
- The time cost is $576 at average income (p. 12).
- The other columns are:
  - "No Adjustment Costs": γ ≈ 1.0008, adjustment rate 0.962.
  - "Only γ" (ψ = 1): adjustment rate 0.953, α₁ 0.622.
  - "Boundary": ψ = 0.980.
- **Table 3 (p. 13), robustness:**
  - Alternative inaction definition: γ = 1.343, ψ = 0.901 (the text says 0.8348).
  - S&P returns: γ = 3.75, ψ = 0.939.
  - Shareholders only: γ = 4.34, ψ = 0.919.
- **Table 4 (p. 16)** compares a single agent, shareholders and the baseline (aggregation).
- **Table 5 (p. 16)** is a probit on simulated data. Wealth raises the probability of adjustment and income lowers it, "presumably due to the role of ψ" (p. 16).

**Policy structure.**
- The model is a discrete choice between adjusting and not adjusting.
- The paper **never characterises, plots or reports an inaction region or its thresholds**. Section 5 promises "We then look at the policy functions of a household using our baseline estimates" (p. 15), but Section 5.2 reports only the probit in Table 5 (p. 16).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a capital requirement or threshold.** No. This is a household paper, with no capital requirement and no shortfall penalty.
- The only threshold-like object is a **hard zero lower bound on assets** (footnote 18, p. 11). It binds in about 5% of observations in the baseline and about 11% in the "Boundary" column (p. 12).
- The 5% limit is imposed in estimation as a side constraint (footnote 20, p. 12). It is a constraint, not a graduated cost, and nothing in it is asymmetric.

**(b) Costs of adjusting (the analogue of issuance costs).** Yes, on portfolio trades rather than equity issues.
- **Direct broker cost, eqs. (7)–(8), p. 7.** It has a fixed part per trade (about $56 to buy, about $61 to sell), a proportional part (about 0.12–0.14% of trade value) and a negative quadratic term (Table 1, p. 8). Buying and selling costs differ.
- **Time cost, eq. (4), p. 5.** It is a fixed cost per adjustment in the form of a fraction 1 − ψ of labour income, independent of trade size but scaled by income (p. 5).
- Adjustment is lumpy: in the model a household adjusts when it trades and otherwise reinvests passively.
- No probability-of-adjustment friction is used (footnote 10, p. 7, mentions only an optional choice-specific shock).

**(c) Both frictions, interaction, comparative statics, identification.**
- *Two frictions in the sense of (a) and (b)?* No. Section 3.3 estimates **one preference parameter (γ) and one adjustment cost (ψ)** jointly, with β fixed and C(·) estimated outside the model (pp. 6, 10).
- *Identification argument* (p. 10 and footnote 17). It is informal and one-sided:
  - The adjustment rate "is very informative about the cost of trade, ψ".
  - α₁ "is responsive to variations in both γ and ψ".
  - The regression coefficient is "sensitive to variations in γ" (footnote 17).
  - The paper gives **no derivative matrix, no Jacobian and no rank or sensitivity table**, says nothing about local versus global identification, and gives no standard errors for γ and ψ.
  - The design is **just identified** (footnote 19, p. 11).
- *Separating the costs.* The paper separates the time cost from the broker cost by estimating C(·) outside the model: "This allows us to identify the utility costs of trade separately from actual trading costs" (p. 6). This separates two adjustment costs. It does not separate a cost from a preference parameter.
- *Directions.*
  - The only direction stated is for ψ: with ψ estimated below 1 (0.992), "As a consequence the adjustment rate is much lower", and consumption growth is "less responsive to interest rate movements" (p. 12; Table 2, p. 11). So a higher cost lowers both moments.
  - **No direction is stated for γ** on either moment, and the paper never says whether γ and ψ move a moment in the same or in opposite directions (see D3).
  - No statement that the parameters are hard to separate.
- *Thresholds.* Identification uses **only moments of adjustment behaviour** (the frequency of adjustment and an aggregate consumption–return regression coefficient). It does not use the thresholds of an inaction region, which the paper never computes (pp. 10–16).
- *Interaction or comparative statics.* There are no comparative statics of any threshold. Table 2 compares nested specifications (no costs, broker cost only, both), and the probit in Table 5 gives the state dependence of adjustment (pp. 11, 16).
- *Bias finding.*
  - Abstract (p. 1): the adjustment costs imply that inferences about risk aversion and the EIS "are biased". Household risk aversion is lower than other estimates and is not the inverse of the EIS.
  - p. 3: the direction of the bias on the EIS "is not clear a priori". Inaction pulls the estimated EIS down, but adjusters respond more.
  - p. 12: γ is close to 1 in all of Table 2's cases, and one cannot read the EIS or risk aversion off 1/α₁. The near-zero α₁ "reflects the inaction of households in their portfolio adjustment as well as the presence of non-participants in aggregated data".
  - p. 17, conclusion: the risk aversion estimate is "much lower than the one inferred from estimation based upon the representative agent model with continuous adjustment".
  - The robustness estimates are higher (γ = 3.75 and 4.34; Table 3, p. 13).

**Buffers and variance.** Nothing on capital buffers. The closest content is the zero-wealth bound and the 50% non-participation share (pp. 9, 12).

**Stated limitations and future work** (p. 17).
- The model has a single asset.
- Asset-specific trading costs are left for future work, for example to distinguish a savings account from complex options trades.
- With a costless asset, Euler equation (1) would hold for that asset.
- Adding further assets and housing "remains for future work".
- Section 4.1 notes that "the meaning of inaction in this portfolio adjustment problem is not entirely clear" (p. 13).

## 6. Verdict for the theory paper

1. This paper is a weak precedent for the theory paper's identification argument. It estimates one preference parameter (γ) and one fixed, income-scaled adjustment cost (ψ) jointly by just-identified SMM (pp. 10–11).
2. It identifies them from two **moments of adjustment behaviour**: the annual adjustment frequency and an aggregate consumption-growth/return regression coefficient. It does **not** use the thresholds of an inaction region, which it never computes or plots (pp. 10–16).
3. Its identification argument is informal: the adjustment rate is "very informative" about ψ, and α₁ responds to both γ and ψ (p. 10). It gives no Jacobian, no sensitivity table, no sign for γ's effect, no claim that the two parameters move a moment in opposite directions, and nothing on local versus global identification.
4. The separation it does claim is between the in-model time cost and the broker costs. It gets this by estimating the broker costs outside the model (p. 6). It does not separate a cost from a preference parameter.
5. Its substantive finding is that ignoring adjustment costs biases the inference of risk aversion and the EIS from 1/α₁ (pp. 1, 3, 12, 17). That is a bias claim, not an identification-from-thresholds result.
6. It has no shortfall or capital-requirement penalty of any kind (p. 11, footnote 18; p. 12, footnote 20).
