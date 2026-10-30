# Reconstruction: Behn, Daminato and Salleo (2019)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/behn_daminato_salleo_2019.pdf`, sha256 `25d58c20f4f7d1d394c456477b16c7e8f5006fee26b0db1f52ff3015fe07393c`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed:** Markus Behn, Claudio Daminato, Carmelo Salleo, "A dynamic model of bank behaviour under multiple regulatory constraints", European Central Bank, Working Paper Series, No 2233 / January 2019. PDF ISBN 978-92-899-3495-4, ISSN 1725-2806, doi:10.2866/943267 (back page, PDF p. 63).
- **PDF file:** `/root/.cache/wealth_garch/behn_daminato_salleo_2019.pdf`
- **PDF pages:** 63 (pdfinfo).
- **Page mapping:** printed page = PDF page − 1. The cover (PDF 1) and the back page with acknowledgements and imprint (PDF 63) have no printed number. The abstract is printed p. 1 (PDF 2). The main text runs printed pp. 4–35, references pp. 35–40, figures pp. 41–52, tables pp. 53–58, appendix tables A.1–A.3 pp. 59–61.
- **Coverage:** read in full. I read every page of the text layer (`pdftotext -layout`), and I viewed the figure pages (printed pp. 41–52) as rendered images, because their content is graphical and the text layer holds only the captions.

## 2. Reconstruction

**Agents and setting.** Risk-neutral bank managers maximise shareholder value in discrete time over an infinite horizon. The model is partial equilibrium: banks take prices as given and there are no bailouts (pp. 9, 11, fn. 6 p. 12). The balance sheet has two asset classes: loans L, and liquid assets F, which is the residual. It has four liability classes: exogenous deposits D, long-term debt LT, short-term debt ST, and equity E (p. 9, Fig. 1 p. 41).

**Choices.** Each period the bank chooses Γ = (div, g^L, g^F, g^LT, g^ST): payout div, where div < 0 is an equity issue; new loans; the adjustment of liquid assets; new long-term debt; and the adjustment of short-term debt (pp. 9–10, 13–14).

**Objective, Eq. (1), p. 10, and Bellman equation, Eq. (19), pp. 18–19.**

E_t Σ β^{s−t} [ div − Ω·1(CR < θ_CR) − Ψ·1(LR < θ_LR) − Φ1(−div)^{Φ2}·1(div < 0) ].

The paper labels the four terms "future dividends", "cost of breaching capital requirements", "cost of breaching liquidity requirements" and "cost of raising fresh equity" (p. 10).

**Frictions.**
- *Breach costs (item a):* Ω and Ψ are charged through an indicator whenever the risk-weighted capital ratio CR is below θ_CR, or the liquidity ratio LR is below θ_LR (Eq. 1, p. 10; requirement statement p. 18).
- *Equity issuance cost (item b):* Φ1(−div)^{Φ2}, paid only when div < 0. Retained earnings carry no cost (p. 10).

**Exogenous processes.** Loan and liquid-asset returns are normal i.i.d. around the policy rate plus a spread (Eqs. 2–3, p. 11). The liquid-asset price follows a unit root (Eq. 4, p. 11). The deposit rate is policy rate plus mark-up (Eq. 5, p. 11). The long- and short-term debt rates carry leverage terms κ (Eqs. 6–7, p. 12), and κ is set to zero after the first-step regressions (p. 21, Table 3 p. 55). There is a 1% chance of a tenfold short-term rate spike, i.e. a run (p. 12). Deposits follow a random walk (Eq. 12, p. 14).

**Laws of motion.** Loans: Eq. 8 (p. 13). Liquid assets: Eq. 9 (p. 13). Long- and short-term debt: Eqs. 10–11 (p. 14). Profit: Eq. 13, with operating cost exp(ι1 + ι2 log A) (p. 15). Budget constraint: Eq. 14 (p. 15). Equity: E_{t+1} = E_t + Π_{t+1} − div_t (Eq. 15, p. 16). Risk weight: Eq. 16 (p. 16). CR = E/(RW·A) (Eq. 17, p. 17). LR = F/(w_ST·ST + w_D·D), an LCR proxy (Eq. 18, p. 17).

**Solution.** There are five state variables: the capital ratio, the liquidity ratio (printed as a second "CR", a typo), equity, the loan share and the deposit share (p. 18). The model is solved by backward induction and value-function iteration. A terminal liquidation at T = 100 is a computational device, and the paper iterates until the policy functions converge (p. 19, fn. 10). The grids are discrete (Table A.1, p. 59):
- dividend choice ∈ {−0.1, 0, 0.01, 0.02, 0.05, 0.1}, as a fraction of capital;
- CR grid from 0.12 to 0.20;
- Tauchen quadrature and linear interpolation (p. 19).

There are no analytical results, propositions or threshold characterisations.

**Estimation.** Data: 116 SSM significant institutions, 2014–2016, supervisory COREP/FINREP data (pp. 19–20; Table 1 p. 53, 761 observations). Estimation has two steps (p. 20).
1. Exogenous parameters and shock variances are estimated directly from the data (pp. 20–22, Table 2 p. 54). Calibrated requirements: θ_CR = 0.12 (Pillar 1 + Pillar 2 + buffers, p. 22, fn. 14) and θ_LR = 0.9 (pp. 22–23).
2. Simulated Method of Moments for (β, Ω, Ψ, Φ1, Φ2, ζ). Each evaluation simulates 2000 banks over 200 periods and keeps periods 150 onwards (p. 23, fn. 15). The 13 targeted moments are in Table A.3 (p. 61) and Table 5 (p. 57): medians of CR, LR, log assets and RWA/assets; the mean dividend/capital ratio; the mean and standard deviation of profits/assets; and six growth-regression slopes β1–β6.

**Identification argument (pp. 23–24).**
- β: the dividend/equity ratio and the levels of assets and equity.
- Ω and Ψ: the size of voluntary buffers, plus the asset-growth versus risk-weight-growth slope.
- Φ: the asset-growth versus capital-growth (and liability-growth) relation, plus the levels of capital and assets.
- ζ: the median risk weight.

**Estimates (Table 5, p. 57; text pp. 25–26).** β = 0.986 (quarterly). Ω = 0.46 and Ψ = 0.28, which the text reports as 46% and 28% "of total capital". Φ1 = 1.09, Φ2 = 1.04, ζ = 0.0011. No standard errors are reported.

**Main results.**
- Banks hold precautionary voluntary buffers (pp. 25, 34).
- Fewer than 1% of banks issue equity each period at baseline (p. 26).
- When the requirement rises from 12% to 13%, banks rebuild the buffer one-for-one, reaching about +1 pp within a year (p. 27). The model's diff-in-diff gives 0.62 pp against 0.39 pp in the data (p. 28, Table 6 p. 58).
- Retained earnings are the dominant adjustment channel (p. 29).
- The adjustment mode differs by initial capital (pp. 29–30, Figs. 5–6 pp. 46–47):
  - well-capitalised banks cut dividends;
  - mid-range banks also cut loans;
  - the banks closest to the minimum (buckets 1–8) raise external equity.
- A higher liquidity requirement produces larger and more persistent cuts in loans (pp. 31–33).
- Higher expected impairments leave the target capital ratio virtually unchanged (p. 34).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a requirement.**
- *Present?* Yes, for both the capital and the liquidity requirement (Eq. 1, p. 10; Eq. 19, pp. 18–19).
- *Form:* a lump-sum charge, Ω for capital and Ψ for liquidity, triggered by the indicator 1(CR < θ_CR) or 1(LR < θ_LR). It is a fixed penalty per period of breach. It does **not** grow with the size of the shortfall, and it is not liquidation or closure.
- *Motivation given:* bail-in risk for shareholders, possible restrictions on dividend payments, or other supervisory measures (p. 10). The paper calls it "reminiscent of the bankruptcy cost in classical trade-off theory models" (p. 10) and links it to trade-off theory (p. 18).
- *Asymmetric?* Yes, by construction: it is charged only below the requirement, and nothing is charged above.
- *Who bears it?* Shareholders (p. 9). Footnote 5 (p. 10) adds that managers may personally face removal.
- *Size:* Ω is estimated at 46% and Ψ at 28% "of total capital" (p. 25; Table 5 p. 57, labelled "(in % of capital)" but printed as 0.46 and 0.28). The lower Ψ is read as an expected milder supervisory reaction to liquidity breaches (p. 25).

**(b) Cost of issuing equity.**
- *Present?* Yes: Φ1(−div)^{Φ2}·1(div < 0), with div < 0 being an equity issue (Eq. 1, p. 10).
- *Fixed component?* None. It is a power function of the amount raised, which the paper calls convex (pp. 23, 25).
- *Estimates:* Φ1 = 1.09 and Φ2 = 1.04 (Table 5, p. 57), so it is only slightly convex and close to proportional. The paper says the cost "can be interpreted as a fraction of the capital raised" (p. 25).
- *Motivation given:* direct transaction costs plus debt-overhang and signalling costs (p. 10).
- *Retained earnings:* exempt, in line with pecking order (p. 10).
- *Lumpiness:* no lumpiness in the specification. In the numerical solution, however, the only issuance choice on the grid is div = −0.1 of capital (Table A.1, p. 59), so simulated issues all have one size (see D4).
- *Frequency:* fewer than 1% of banks issue in a given period (p. 26).

**(c) Both frictions together, interaction, comparative statics and identification.**
- *Both present?* Yes, in one objective (p. 10). The paper says the interplay of profitability, the cost of equity, uncertainty and the breach costs "induces optimal buffers" (p. 18).
- *Comparative statics:* none of thresholds. The model has no dividend barrier, issuance trigger or target in closed form, and it reports no policy function plotted against Ω or Φ. The only directional statements are verbal:
  - buffers "can be expected to be larger the larger costs of breaching" (p. 18);
  - higher Ω leads to stronger targeting of the risk-weighted ratio and a more negative asset-growth versus risk-growth slope (p. 23);
  - lower Φ leads to a higher covariance between capital and asset growth and higher levels of capital and assets (p. 24).

  No counterfactual varies Ω or Φ: the three counterfactuals vary θ_CR, θ_LR and ζ (p. 27). The closest thing is p. 31, where the smaller-than-proportional liquidity response is attributed to the lower breach cost Ψ.
- *Identification:* the frictions are separated by assigning them to different moments (pp. 23–24), and they are estimated jointly by SMM from 13 moments (p. 23, Table A.3 p. 61). The paper presents this as a test: a zero estimate would reject trade-off theory (Ω) or pecking order (Φ) (pp. 10, 18). It reports no standard errors, no test statistics, no sensitivity or Jacobian matrix, and no formal identification result.
- *Interaction in the policy:* reported only descriptively. After a requirement increase, banks nearest the minimum choose costly issuance over large loan cuts (p. 30), and supervisors could make issuance more attractive than cutting loans (p. 31).

**Buffers and variance.**
- Voluntary buffers are the central object. Banks keep a "target" ratio above the minimum and restore it after a rise in requirements (pp. 27, 29).
- The model under-predicts capital ratios slightly, which the paper attributes to Pillar 2 guidance acting as a de facto requirement (p. 26, fn. 18).
- On variance, the only moment used is the standard deviation of profits/assets: 0.002 in the data against 0.001 in the model (Table 5, p. 57). The paper does not analyse the variance of the capital ratio or of buffers.
- Percentile bands in Fig. 4 (p. 44) show little dispersion in the adjustment of capital ratios (p. 27).
- The paper states without simulation that greater uncertainty would lead banks to keep larger precautionary buffers (p. 34).

**Stated limitations and future research.**
- Abstracts from the unweighted leverage ratio (pp. 32–33, fn. 4 p. 7).
- Positive risk weights on liquid assets drive the liquidity result (p. 32).
- No dividend-smoothing or signalling motive, so the lending response may be understated (fn. 20, p. 29).
- No agency problem (fn. 5, p. 10) and no bailouts (fn. 6, p. 12).
- A full cost-benefit analysis is out of scope (fn. 21, p. 29).
- The downturn is modelled as a shift in the mean only, not in the variance (p. 34).
- Future research: other sources of bank heterogeneity (p. 35); the real effects of liquidity rules and of interacting constraints (pp. 33, 35).

## 6. Verdict for the theory paper

1. BDS pair a cost of being below a capital requirement with a cost of issuing equity in one dynamic bank objective and estimate both (Eq. 1, p. 10; Table 5, p. 57). This is a close empirical precedent for using two frictions.
2. Their breach cost is a lump-sum indicator penalty, Ω·1(CR < θ_CR), that does **not** grow with the shortfall (p. 10). It is asymmetric only in the sense of being charged below the requirement. It is the bankruptcy-like step that the theory paper's graduated (λ_S − 1)(R − x)^+ replaces, not a precedent for it.
3. Their issuance cost is a power function with Φ2 ≈ 1.04 and **no fixed cost per issue** (pp. 10, 25). Lumpiness in their simulations comes only from the dividend grid (p. 59).
4. The paper does **not** derive or report comparative statics of any dividend, issuance or target threshold in either friction. It has no threshold policy (pp. 19, 59). The only directional claims are verbal: higher breach cost leads to larger buffers (p. 18), and lower issuance cost leads to more co-movement between capital and asset growth (p. 24).
5. It separates the two frictions only by assigning different moments in SMM (pp. 23–24), with no standard errors and no formal identification. It does **not** ask whether the two frictions can be told apart from the policy thresholds. Nor does it study how a breach cost moves the variance of capital or of the buffer (p. 57).
