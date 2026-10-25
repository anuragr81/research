# Reconstruction: Lambertini and Uysal (2013)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/lambertini_uysal_2013.pdf`, sha256 `4d3439e68a042ef1b5092c2a04b6fd461a68f35c083b8350e997a81c95314c18`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- Citation as printed on the title page (PDF p. 1): Luisa Lambertini and Pinar Uysal, "Bank Capital Requirements", EPFL, October 2013, "Preliminary and Incomplete". Keywords: Banking; Capital Adequacy; Financial Regulation. JEL E32, E44, G01, R31.
- File: `/root/.cache/wealth_garch/lambertini_uysal_2013.pdf` (pdfTeX, created 25 October 2013).
- PDF pages: 47.
- Page mapping: printed page = PDF page (offset 0). The title page carries printed "1", and the last page carries 47.
- Coverage: **read in full**, pp. 1-47. That covers the text (pp. 1-40) and the Appendix tables and references, which are interleaved: references on pp. 41, 45-47 and Tables A.1-A.7 on pp. 41-44. Figure pages (pp. 14, 17-18, 32, 39) were read for captions and legends only.
- Layout:
  - Sections 1-4 (pp. 2-18) are empirical, with the same Y-9C and VAR material as 2014.
  - Section 5 is the model (pp. 19-28). Section 6 is calibration (pp. 28-30).
  - Section 7 covers the capital-quality shock (pp. 30-33). Section 8 covers second moments (pp. 33-35).
  - Section 9 is the risk-adjusted steady state (pp. 35-37). Section 10 covers Basel III (pp. 37-40).
  - Section 11, "Conclusions" (p. 40), is only a four-item bullet list.

## 2. Reconstruction: what differs from 2014

The households, capital producers, firms, balance sheet (eq. 17, p. 23) and net-worth law (eq. 18, p. 24) have the same structure as in 2014.

**Penalty, eq. (19), p. 25.**
- Functional form: identical to the 2014 eq. (20): P_t = φ[log((X_t q_t + N_t)/(γ Q_t S_t)) − ζ(X_t q_t + N_t − Xq − N)/(Xq + N)], φ, ζ ≥ 0.
- It is added to net worth.
- Same text: the log term penalises capital below 8% of assets, and the second term "penalizes capita-to-asset ratios above the steady state". Footnote 8 cites the barrier method.
- Differences from 2014:
  - **Calibration.** φ = 0.0025 and ζ = 0.85 (Table 12, p. 29; text p. 30). In 2014 they are φ = 0.065 and ζ = 0.2. The 2013 penalty is about 26 times weaker in φ, and closer to the "both sides" case ζ = 1.
  - **Description of ζ (p. 30).** "ζ < 1 rewards banks for capital ratios above the required value (except at the steady state) while ζ = 0 penalizes banks for capital ratios above its required value." The 2014 text (p. 27) instead says ζ = 0 penalises ratios *below* the requirement, and adds the ζ = 1 two-sided case. See D1.
  - **No interpretive sentence** after the penalty. The 2014 sentence on bonus and distribution restrictions, monitoring and funding costs is absent here. The 2013 introduction (p. 3) does mention dividend limits, recapitalisation requirements and higher funding rates.
  - **No-requirement benchmark.** It is approximated by φ = 0.0001 (p. 31).

**Bank problem.**
- The Bellman equation is written with the max inside the expectation (eq. 20, p. 25).
- The bank guesses a linear value V = µ_s Q S − µ_x q X + µ_n N (p. 26). FOCs are eq. 22-23 with κ_t ≡ µ_n/(1 − P_N) and Ω_t ≡ (1 − θ + µ_n)/(1 − P_N). Coefficients are eq. 25-27 (p. 26).
- In 2014, Ω_t = 1 − θ + µ_n (eq. 31) and P_N enters µ_n (eq. 30).
- The aggregate net-worth equation (29, p. 27) weights (1 − θ) on incumbents. In 2014 (eq. 33) the weight is θ.

**Equity costs.**
- **Proportional cost τ.** Same form as 2014: [R_X + τ] on equity raised at t−1 (eq. 18, p. 24), with τ > ι ≥ 0 (p. 24).
  - Calibration is inconsistent within the draft. Table 12 (p. 29) lists τ = 0.02 and ι = 0.02. The text (p. 30) says "We set the cost of equity τ equal to 0.01".
  - These values contradict the stated τ > ι unless the text value is meant for ι.
  - 2014 uses τ = 0.035 and ι = 0.
- **Resource treatment.** In 2013, τ X q + ι D is a deadweight resource cost in the goods-market clearing condition (eq. 31, p. 27). In 2014 these costs are rebated lump-sum (p. 25, fn. 10).
- **No quadratic equity adjustment cost.** The 2014 term 0.5 λ_0 (e_t/e − 1)^2 does not appear anywhere in 2013. Table 12 has no λ_0 row.
  - 2013 therefore has only the proportional running cost on equity.
  - 2013 concludes only that "issuing stocks may be costly" (p. 35). 2014 adds the λ_0 term and says "more costly than assumed in our paper".
- **No fixed issuance cost** in either version.

**Other calibration differences (Table 12, p. 29).**

| Parameter | 2013 | 2014 |
|---|---|---|
| h | 0.85 | 0.75 |
| χ | 2 | 0.8 |
| θ | 0.3 | 0.75 |
| ω | 0.01 | 0.03 |
| σ_g | 0.0097 | 0.0077 |
| σ_a | 0.008 | 0.0064 |
| σ_Ψ | 0.002 | 0.0017 |

In 2013, bank parameters are again chosen "to come as close as possible to the aggregate summary statistics for banks reported in section 4" (p. 28). σ_Ψ is set to match the std of bank assets (pp. 30, 33). Nothing is estimated.

**Results that differ.**
- **Capital requirement and output.** In 2013 the requirement *de-amplifies* the output effect of a capital-quality shock (pp. 3, 33; abstract p. 1). Without it, bank equity falls by 100% (p. 33). In 2014 the requirement "slightly amplif[ies]" the impact (p. 28).
- **Steady-state ratio.** The risk-adjusted ratio is 0.08 at low risk and 0.081 at high risk (σ_Ψ = 0.01) (Table 15, p. 37). The buffer over γ is essentially zero. The 2014 values are 0.082 and 0.0831.
- **Ratio volatility.** The model std of the capital ratio is 4.2 versus 2.0 in the data (Table 13, p. 34). The 2014 value is 2.98.
- **Basel III.** The counter-cyclical γ̃ uses γ = 0.08, γ_1 = 0.05 and γ_2 = 20 (eq. 34, p. 37).
  - It *reduces* the real impact of the shock (p. 38). 2014 finds it fails to de-amplify.
  - The risk-adjusted steady state has far less capital and output (Table 17, p. 40).
- **No comparative-statics table** of steady-state X, D and ratio in (φ, ζ, τ). That is 2014 Table 13. No φ robustness section either (2014 Sec. 10.1).
- **Bankruptcy claim.** The introduction claims the model computes "the frequency of bankruptcy" and that lowering the requirement raises it (p. 3). No such computation appears in the body (D2).

## 3. Answers to (a), (b), (c)

**(a) Penalty below the requirement.**
- Yes. The smooth log-barrier-plus-linear penalty in net worth (eq. 19, p. 25) has the same functional form as 2014. It is not quadratic and not one-sided or kinked (pp. 25, 30).
- With φ = 0.0025 and ζ = 0.85 it is much weaker than in 2014, and close to the symmetric ζ = 1 case.
- It is borne by bank net worth.

**(b) Costs of issuing equity.**
- Only a proportional per-period cost τ on outstanding equity (eq. 18, p. 24). Its value is given inconsistently: 0.02 in Table 12 (p. 29) and 0.01 in the text (p. 30). It is a deadweight resource cost (eq. 31, p. 27).
- No quadratic adjustment cost (added only in 2014). No fixed cost.

**(c) Interaction, thresholds, identification, calibration.**
- No thresholds, no comparative statics of thresholds, and no identification or estimation. Calibration is by assertion (pp. 28-30).
- Unlike 2014, there is not even a steady-state comparative-statics table in φ, ζ, τ.
- Data: FR Y-9C BHC panel 1993Q1-2012Q1 with CRSP (pp. 7-8). VAR on the 15 largest BHCs (p. 13).

**Capital buffer.**
- Empirical: most BHCs hold more than the requirement (p. 9; Tables 3-4, A.1-A.2).
- Model: the risk-adjusted ratio equals or barely exceeds γ, at 8.0-8.1% (Table 15, p. 37).
- The ratio's model std is 4.2 versus 2.0 in the data (Table 13, p. 34).

**Limitations.**
- No reserves, no dividend smoothing (p. 34).
- Issuance "may be costly" (p. 35).
- Future work (p. 40): Poisson capital shocks, reserves, dividend smoothing, estimation.

## 6. Verdict for the theory paper

1. The 2013 draft has the same smooth log-barrier-plus-linear capital penalty as the 2014 version (eq. 19, p. 25). It has even weaker asymmetry: φ = 0.0025, ζ = 0.85, near the two-sided ζ = 1 case (pp. 29-30).
2. Its only equity friction is a proportional per-period cost τ on outstanding equity (eq. 18, p. 24), with an inconsistent value (D3). There is **no fixed issuance cost** and **no quadratic adjustment cost**; the latter appears only in 2014.
3. Like 2014, it derives **no thresholds**, reports no threshold comparative statics, and does **not** separate or identify the penalty from the issuance cost. All parameters are calibrated (p. 28).
4. Its model capital ratio sits at about the requirement (8.0-8.1%, p. 37), so it offers no model-based buffer result. The only buffer evidence is empirical (p. 9).
5. For the theory paper, the 2013 version adds nothing beyond the 2014 version except an earlier date for the "requirement as net-worth penalty" device. It is not prior art for a graduated shortfall cost paired with a fixed issuance cost.
