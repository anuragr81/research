# Reconstruction: Lambertini and Uysal (2014)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/lambertini_uysal_2014.pdf`, sha256 `25f001a4e84022a8d64f61d77451a8b2bd65d63f760ef05cfc0e71ed7fd676a0`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- Citation as printed on the title page (PDF p. 1): Luisa Lambertini and Pinar Uysal, "Macroeconomic Implications of Bank Capital Requirements", EPFL, February 2014. Keywords: Macro & Finance; Capital Adequacy; Financial Regulation. JEL E44, E32, G21, G32. Funding: Swiss National Science Foundation, Grant CRSI11-133058. Working paper, no journal.
- File: `/root/.cache/wealth_garch/lambertini_uysal_2014.pdf` (pdfTeX, created 5 June 2014).
- PDF pages: 47.
- Page mapping: printed page = PDF page (offset 0). The title page carries printed "1", and every later page is numbered in sequence up to 47.
- Coverage: **read in full**, pp. 1-47. That covers the text (pp. 1-41), the references (pp. 41-43) and Appendix A tables (pp. 44-47). The figure pages (pp. 15-17, 28-29, 33, 38, 40) were read for their captions and legends. Their plotted numbers are not in the text layer.
- Layout: Sections 1-3 (pp. 2-17) are empirical: Y-9C bank holding company (BHC) data and a VAR. Section 4 sets out the DSGE model (pp. 18-25). Section 5 is calibration (pp. 25-27). Sections 6-8 give impulse responses, the steady state and second moments (pp. 27-35). Section 9 covers Basel III (pp. 35-38), Section 10 robustness (pp. 38-40) and Section 11 the conclusions (pp. 40-41).

## 2. Reconstruction

**Setting.** This is a quarterly DSGE model built on Gertler, Kiyotaki and Queralto (the paper cites "Gertler et al. (2012)", p. 18). It is solved by second-order perturbation around a "risk-adjusted steady state" (pp. 29-30). It is not a continuous-time stochastic-control model. It has no impulse control, no barrier policies and no thresholds.

**Agents and choices.**
- Households (eq. 1-8, pp. 18-19) choose consumption, labour, deposits D_t and bank shares X_t at price q_t. Deposits pay a predetermined R_D. Equity pays a state-contingent R_X.
- Capital producers face investment adjustment cost f(I_t/I_{t-1}) = 0.5χ(I_t/I_{t-1} − 1)^2 (eq. 9-10, p. 20; form on p. 25).
- Firms finance capital with state-contingent claims S_t that banks buy (eq. 11-16, pp. 20-21). A capital-quality shock Ψ_t follows an AR(1) (eq. 37, p. 25). There are also TFP and government-spending shocks (eq. 34, 36).
- Banks:
  - Balance sheet Q_t S_t = N_t + q_t X_t + D_t (eq. 17, p. 21).
  - Net worth (retained earnings) N_t = R_K Q S − [R_X + τ] X q − [R_D + ι] D + P (eq. 18, p. 22). Deposits cost ι per unit and outside equity costs τ per unit of value, with τ > ι ≥ 0 (p. 21).
  - Value V_t = max E Λ{(1−θ)N + θ V_{t+1}} over S_t and X_t (eq. 21, p. 23). θ is the survival probability; in the PDF the symbol is a missing glyph. On exit, net worth goes to households.
  - Dividends are not chosen. Payout happens only through random exit.
  - First-order conditions: eq. 23-24 (p. 23). Envelope condition: eq. 25. Definitions: eq. 26-31 (p. 24). Aggregate net worth: eq. 33 (p. 24).

**Friction (a): capital-requirement penalty, eq. (20), p. 23.**

P_t = φ [ log( (X_t q_t + N_t) / (γ Q_t S_t) ) − ζ (X_t q_t + N_t − Xq − N)/(Xq + N) ],  φ, ζ ≥ 0.

- P_t is added to next period's net worth (eq. 18, 22).
- γ = 0.08 is the required capital/asset ratio. Xq + N is steady-state bank capital.
- The φ multiplying the ζ term is confirmed by FOC (24), where φ multiplies [1/(Xq+N)_t − ζ/(Xq+N)] (see D1).
- The log term is negative below γ and positive above γ. It is smooth and concave in the capital ratio, with no kink at γ.
- The authors call it a penalty that bites below γ, and footnote 9 cites applications of "the barrier method".
- The ζ term is linear in capital relative to its steady-state value. It "penalizes capital-to-asset ratios above the steady state" (p. 23).
- Calibration discussion (p. 27):
  - ζ = 1 penalises deviations both above and below.
  - 0 < ζ < 1 "rewards banks for capital ratios above (but not too far above) the required value".
  - ζ = 0 penalises only shortfalls.
- Interpretation given on p. 23: restrictions on bonuses and capital distributions, more monitoring, higher funding costs.
- The penalty is borne by the bank's net worth. Its stated purpose is to allow variation in the ratio and to permit perturbation methods (p. 23).

**Friction (b): equity costs.**
1. A **proportional running cost τ** per unit of value of outstanding one-period equity raised the previous period (eq. 18, p. 22). Calibrated τ = 0.035, with ι = 0 (Table 12, p. 26). It is a per-period funding cost on the equity stock, not a flotation cost on new issues. Footnote 10 (p. 25) says intermediation costs are rebated lump-sum, and that treating τXq + ιD as wasted does not change results.
2. A **quadratic adjustment cost** 0.5 λ_0 (e_t/e − 1)^2, λ_0 ≥ 0 (p. 25), where e is the steady-state number of shares. It is "present only outside the steady state" and captures that "issuing and retiring equity is costly". λ_0 = 0.003 (Table 12: "Bank (quadratic) cost of equity").
   - It penalises the deviation of the share count from its steady state, not the per-period change.
   - The paper does not write where this cost enters the bank's problem or eq. (18) (D2).
3. There is **no fixed cost of issuance** anywhere in the paper.

**Solution.** The model is log-linearised and then solved to second order. The risk-adjusted steady state is found iteratively (p. 30). Second moments come from 1000 Monte Carlo runs of 100 quarters (pp. 33-34).

**Main results.**
- Shock response: a 2% capital-quality shock lowers asset prices, loans and output. Banks issue new shares despite low prices, and the equity/asset ratio rises (Fig. 4, p. 27). Without the requirement (φ near 0), banks would cut X instead, and the requirement "slightly amplif[ies]" output effects (p. 28).
- Steady state (Table 13, pp. 28-29). Bank equity X falls when φ falls (0.065 → 0.024 gives X 0.7265 → 0.0037, ratio 0.0821 → 0.0316). X rises when ζ falls (ζ = 0 gives X 1.32, ratio 0.1206). X rises when τ falls (τ = 0.02 gives X 1.36, ratio 0.1246).
- Risk-adjusted steady state (Table 15, p. 32): capital ratio 0.082 at low risk and 0.0831 at high risk (σ_Ψ = 0.0207), against γ = 0.08. Equity premium 0.07% (low risk) and 10.1% (high risk).
- Second moments (Table 16, p. 34): model std of the capital ratio is 2.98 versus 2.0 in the data. Retained earnings are far too volatile (21.39 vs 3.8). Share issuance is too volatile (15% vs 8.2%, fn. 14, p. 35).
- Basel III. The counter-cyclical γ̃ is logistic in credit deviation (eq. 40, p. 35). It lowers the volatility of bank variables (Table 17, p. 36). The steady-state ratio rises only to 9.2-9.3%, below the 10.5% requirement, mostly through lower lending (Table 18, pp. 36-37).
- Robustness (pp. 38-39): φ +10% or +20% raises the ratio (for example to 9% under Basel II and 10.21% under Basel III).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below the requirement.**
- Yes. It is a smooth running penalty in net worth, P_t = φ[log(capital/(γ·assets)) − ζ(capital − ss capital)/ss capital] (eq. 20, p. 23).
- It is not liquidation, closure, a fixed fine or a dividend ban. Dividend restrictions, monitoring and funding costs are named only as motivation (pp. 3, 23, 38).
- Form: logarithmic (a log barrier) in the ratio of capital to required capital. It is negative below γ and positive above γ, with no kink at the requirement. It is **not quadratic and not a one-sided (asymmetric, kinked) charge**: the same smooth function applies on both sides (D3).
- The second, ζ, component is linear in capital relative to steady state and lowers net worth for capital above steady state (pp. 23, 27).
- Who bears it: the bank, through net worth (p. 23).
- The "required ratio" is on equity/total assets with γ = 0.08 (p. 26). It is not risk-weighted.

**(b) Costs of issuing equity.**
- Proportional running cost τ = 0.035 per unit of equity value per period (eq. 18, p. 22; Table 12, p. 26), justified by citing Myers-Majluf and Berger-Herring-Szego (p. 21).
- Quadratic adjustment cost 0.5 λ_0 (e_t/e − 1)^2 with λ_0 = 0.003, on the number of shares relative to steady state (p. 25).
- No fixed cost and no lumpy issues. Issuance is continuous and adjusts every quarter (Figs. 4-8).
- The authors themselves suggest issuance may be costlier than modelled, because simulated share issuance is too volatile (p. 35, fn. 14).

**(c) Both frictions, interaction, thresholds, identification, calibration.**
- Both a capital penalty and equity costs are present. Their joint effect is shown only through steady-state comparative statics of portfolio quantities and the capital ratio (Table 13, pp. 28-29):
  - lower φ means less equity and a lower ratio;
  - higher ζ means less equity;
  - higher τ means less equity.
- The "trade-off" is stated verbally: "banks trade off capital for penalty" (p. 36).
- **No thresholds** (dividend, issuance trigger or target) exist in the model, so there are no comparative statics of thresholds.
- **No identification exercise.** The paper does not ask whether the penalty and the issuance cost can be told apart, and it does not estimate them.
- Parameters are **calibrated, not estimated** (Table 12, p. 26):
  - γ = 0.08 from Basel and Table 5.
  - φ = 0.065, ζ = 0.2, τ = 0.035, ι = 0, λ_0 = 0.003, θ = 0.75, ω = 0.03 are "set". The only general statement is that bank parameters are chosen "to come as close as possible to the aggregate summary statistics for banks reported in section 3" (p. 26). No moment is tied to φ or λ_0 specifically.
  - σ_a, ρ_a, σ_g, ρ_g are set to match output volatility. σ_Ψ, ρ_Ψ are set "to roughly match the standard deviation of bank assets" (p. 27). Table 12 gives σ_Ψ = 0.0017, p. 33 says 0.19%. The high-risk σ_Ψ = 0.0207 matches the 10.1% equity premium (p. 32).
  - Data: FR Y-9C BHC panel 1993Q1-2012Q1 merged with CRSP (pp. 7-8). Moments from 1993Q1-2007Q4 (pp. 30, 33).
- Estimating the model is listed as future work (p. 41).

**Capital buffer and variance of capital.**
- Empirical:
  - Most BHCs hold capital above the regulatory requirement (Tables 3-4, p. 9; Table A.2-A.3, p. 45).
  - The largest banks are over-represented among the significantly undercapitalised and among those 0-400 bp above "well capitalised" (p. 9).
  - Mean Tier 1+2/RWA is 14.40% (Table 5, p. 10).
  - The equity/asset ratio is counter-cyclical (Table 6, p. 12; Table 16, p. 34).
- Model:
  - The steady-state ratio sits just above γ: 8.21% deterministic (Table 13), and 8.2% / 8.31% risk-adjusted (Table 15, p. 32). The buffer is about 20-30 bp and rises with risk.
  - Under Basel III the bank sits *below* the 10.5% requirement at 9.2-9.3% (p. 36). The smooth penalty allows a persistent shortfall.
  - Std of the capital ratio is 2.98 (Basel II), 2.8 (Basel III, γ_2 = 3) and 3.32 (γ_2 = 10) (Tables 17, 19, pp. 36, 39).
  - No analytical result on buffer variance.

**Stated limitations and future research (pp. 34, 35, 41).**
- No loan-loss reserves or provisions, and no dividend smoothing (p. 34).
- Issuing stocks may be costlier than assumed (p. 35).
- Future work: risk-weighted asset classes, Poisson or binomial capital shocks, and estimating the model (p. 41).

## 6. Verdict for the theory paper

1. The paper pairs a smooth penalty for low capital with costs of equity, but neither matches the theory paper's frictions.
   - The penalty is a log barrier in capital/(γ·assets) plus a linear term in capital relative to steady state (eq. 20, p. 23). It is not a kinked, one-sided running cost (λ_S − 1)(R − x)^+, and it rewards ratios above γ (p. 27).
   - Equity costs are a proportional per-period cost τ on outstanding equity (eq. 18, p. 22) and a quadratic adjustment cost in the share count (p. 25). There is **no fixed issuance cost** and no lumpy recapitalisation.
2. The model is a perturbation-solved DSGE with no dividend barrier, issuance trigger or target. So it derives **no thresholds** and no comparative statics of thresholds (pp. 22-30).
3. It does **not** ask whether the penalty and the issuance cost can be separately identified, and it estimates nothing. All of φ, ζ, τ, λ_0 are calibrated by assertion against aggregate bank statistics (p. 26). Estimation is left to future work (p. 41).
4. The closest points of contact are these:
   - steady-state comparative statics showing that higher φ, lower ζ or lower τ raise equity and the capital ratio (Table 13, p. 29; p. 38);
   - a model capital ratio slightly above γ that rises with risk (Table 15, p. 32);
   - the empirical documentation of buffers well above the minimum (pp. 9, 40).
5. It can be cited as prior art for modelling the capital requirement as a graduated, net-worth penalty rather than a hard constraint. It is not prior art for the pairing with a fixed issuance cost, or for identification of the two frictions from the policy.
