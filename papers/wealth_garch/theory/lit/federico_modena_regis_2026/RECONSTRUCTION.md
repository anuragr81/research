# Reconstruction: Federico, Modena and Regis (2026)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/federico_modena_regis_2026.pdf`, sha256 `e940148247a33976f4d65f323a0cdb41b18e443dc0844e5f0ea752dbf55253c3`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed:** Salvatore Federico, Andrea Modena, Luca Regis, "Coordinating Bank Dividend and Capital Regulation", SAFE Working Paper No. 487, July 2026 (SAFE cover, PDF p. 2). The title page says "This version: 14th July 2026" (printed p. 1). The EconStor cover (PDF p. 1) gives the suggested citation "Federico, Salvatore; Modena, Andrea; Regis, Luca (2026) : Coordinating bank dividend and capital regulation, SAFE Working Paper, No. 487, Leibniz Institute for Financial Research SAFE, Frankfurt a. M.", handle https://hdl.handle.net/10419/342550. The corresponding author is Andrea Modena (University of Mannheim). Federico is at Bologna and Regis at Turin / Collegio Carlo Alberto.
- **PDF file:** /root/.cache/wealth_garch/federico_modena_regis_2026.pdf
- **PDF pages:** 67 (pdfinfo).
- **Page mapping:** **printed page = PDF page − 2.** PDF p. 1 is the EconStor cover and PDF p. 2 is the SAFE series cover, neither numbered. PDF p. 3 is printed p. 1 (title page and abstract), and so on up to PDF p. 66 = printed p. 64 (end of Appendix A.4). PDF p. 67 is an unnumbered "Recent Issues" list for the SAFE series. Page numbers below are printed numbers unless marked "PDF".
- **Coverage:** read in full (PDF pp. 1–67). That covers the abstract, Sections 1–6, the references (pp. 45–50), Appendix A.1 (proof of Prop. 1), A.2 (sketch proof of Thm. 1), A.3 (proof of Prop. 2) and A.4 (dividend-ban case). The figures are images, so I used only their captions and the text describing them. The paper refers to a "supplementary material" that is not in this PDF. That material holds the β2 = 1 density, the full counter-cyclical solution, the pro-cyclical case and the MATLAB code (pp. 22, 33, 35, 41). Footnote 20 (p. 36) says "The appendix examines" a case where requirements are relaxed only in the bad state. No such appendix is in this PDF, which has only A.1–A.4.

## 2. Reconstruction

**Setting (pp. 8–12).** Time is continuous. A regime It ∈ {1, 2} follows a two-state Markov chain with intensities λi: state 1 is "good" and state 2 is "bad" (p. 8). A risk-neutral manager acts for the shareholders. The balance sheet is Et + D = Xt + L (eq. 1, p. 8). D is insured deposits paying ρ, L is a **fixed** stock of illiquid loans and Xt is liquid reserves that earn nothing. **The state variable is the reserve level Xt**, in money units with D = 1. It is not a ratio. The net cash flow to reserves is the arithmetic Brownian motion μIt dt + σIt dWt with μi = μ̄i − ρD (eq. 2, p. 9). Parameters satisfy μ̄1 ≥ μ̄2 > 0 and σ2 ≥ σ1 > 0. Nothing in the dynamics depends on the level of Xt.

**Capital requirement (eqs. 3–4, p. 9).** Equity must cover repayment of depositors if loans are sold at the fire-sale price α: E ≥ L(1 − α) + ΓIt. Here Γ > 0 is an extra regulatory buffer and Γ < 0 a subsidy or guarantee. In reserve terms the constraint is Xt ≥ D − αL + ΓIt =: xR,It (eq. 4). In the baseline xR,1 = xR,2 = xR (p. 12). Section 5.3 makes the requirement counter-cyclical, with xR,1 > xR,2.

**Controls (pp. 9–12).**
- *Dividends:* the bank chooses a cumulative, non-decreasing, right-continuous dividend process Zt (p. 9). A dividend of $1 paid in state i is worth βi ∈ [0, 1] to shareholders, with β1 = 1 ≥ β2. This proportional **dividend tax (1 − β2) applies in the bad macro state only** (p. 9). Admissibility (11) (p. 11) rules out any dividend that would push reserves to or below xR.
- *Recapitalisation or liquidation:* when Xt reaches xR,It at a random time τn (eqs. 6, 9), the manager chooses bn ∈ {0, 1} (p. 10). With bn = 0 the bank is liquidated. Shareholders "receive the residual buffer Γ+" and lose all future dividends (p. 10). With bn = 1, shareholders inject Gn > 0 and pay a fixed cost κ > 0 (p. 10). The jump is X̃τn = X̃τn− + Gn ≥ xR (eq. 7).
- *Objective:* J(x, i; A) = E[∫0^{τℓ} e^{−δt} βIt dZt − Σ_{n<ℓ} e^{−δτn}(Gn + κ)], with V = sup J (eqs. 13–14, p. 12). ℓ is the first liquidation index.

**Frictions.**
1. **Requirement and what happens below it.** Reserves are never allowed below xR. On hitting xR the bank must recapitalise at once (an impulse) or be liquidated (pp. 10, 14, eq. 19). There is **no running cost or penalty that grows with a shortfall**, and the state never lies below the requirement. The only cost of reaching the requirement is the lumpy issue cost Gn + κ, or else the loss of the franchise value at liquidation.
2. **Equity issuance cost.** A **fixed cost κ per issue** (eq. 13), in units of reserves with D = 1. The injected amount Gn is paid one for one, so there is **no proportional cost**. The extension (p. 39) lets the cost depend on the state, κ2 ≥ κ1.
3. **Dividend restriction.** This is the proportional tax 1 − β2 on payouts in the bad macro state (p. 9). A ban is the limit case: when β2 ≤ λ2/(λ2 + δ) the bank pays nothing in state 2, so the policy acts as a ban (Remark 1, p. 19; App. A.4, pp. 61–64).

**Solution structure (Section 4.1, pp. 13–19).** The value function solves the HJB variational inequality max{Li v − λi[v(·, i) − v(·, 3 − i)], βi − v′(·, i)} = 0 (eq. 15, p. 13), using guess-and-verify (p. 13). The authors postulate a dividend **barrier** x̃i = inf{x ≥ xR : v′(x, i) ≤ βi} (eq. 18, p. 14), with xR < x̃1 < x̃2 assumed (p. 14). The boundary condition at xR is v(xR, i) = max{max_{G∈[0, x̃i − xR]} [v(xR + G, i) − G − κ], Γ+i} (eq. 19, p. 14). The value function is a piecewise sum of exponentials (eqs. 22–25, p. 15), with coefficients from the linear system (26) (p. 16).
- **Proposition 1** (pp. 15–17) assumes β2 > λ2/(λ2 + δ), together with conditions (27) for concavity and (28). Under these, v is concave and solves (15). The bad-state target x*2 ∈ (xR, x̃2] is the unique solution of (29), which is v′(x*2, 2) = 1 (see also (83)–(84), p. 56). The thresholds x̃1 and x̃2 solve the algebraic system (30), in which κ enters (p. 17). Proof in App. A.1 (pp. 50–57).
- **Policy (pp. 17–18):**
  - *Trigger:* issuance happens exactly at the regulatory threshold xR. The trigger is **not** chosen.
  - *Target, good state:* the bank recapitalises up to the dividend barrier, **x*1 = x̃1**.
  - *Target, bad state:* the bank recapitalises up to **x*2 ≤ x̃2**, strictly below x̃2 when β2 < 1. The paper's reason is that the marginal value of dividends β2 is below 1 while v′(x*2, 2) = 1, and v is concave (pp. 17–18). With β2 = β1 = 1, x*2 = x̃2 (p. 21).
  - *Liquidation:* it is never optimal if κ ≤ κ̄ = min{v(x*1, 1) − (x*1 − xR), v(x*2, 2) − (x*2 − xR)} (eq. 31, p. 18).
- **Theorem 1** (verification, pp. 18–19, eq. 32) states that the optimal dividend is a reflection at x̃i and the optimal issue is Ĝn = x*I − xR. The proof is "only sketch[ed]" and points to Løkka and Zervos (2008) and Ferrari et al. (2022) (p. 57).
- **Remark 1** (p. 19) shows the structure breaks down for β2 ≤ λ2/(λ2 + δ). Then x̃2 = ∞ and C2 = (xR, ∞), so the bank never pays in state 2 (App. A.4, pp. 61–64). The ban-like cutoff λ2/(λ2 + δ) rises in λ2 and falls in δ (p. 19). This follows analytically from the formula.

**Stationary distribution (Section 4.2, pp. 19–23).** The derivation is "heuristic", backed by simulation (pp. 19–20). It solves Kolmogorov forward equations (35), with fluxes (33)–(34). The boundary conditions are no mass at xR (36), re-injection at x*2 (37)–(38), re-injection at x̃1 including the mass carried over from state 2 (39), and zero flux at x̃2 (40).
- **Proposition 2** (p. 22) gives π(x, i) as sums of exponentials (eqs. 42–47). Proof in App. A.3 (pp. 58–61).
- The paper defines a conditional mean Mi (48), a conditional **variance Si** (49), and unconditional versions M (50) and S (p. 29). It calls these "credit capacity" and its dispersion (p. 28).

**Calibration and numerical results (Section 5, pp. 24–43).** Table 1 (p. 25) gives the baseline values: μ1 = 0.05, μ2 = 0.02, σ1 = 0.25, σ2 = 0.3, κ = 0.087, δ = 0.03, xR = 0.0769, ρ = 0.0043, α = 0.6, β2 = 0.87, L/D = 1.5385, 1/λ1 = 10, 1/λ2 = 6.7, Γ = 0. Sections 5.2–5.4 report the comparative statics (Section 3 (c) below), the effect of counter-cyclical requirements (Section 5.3, eqs. 51–55, Tables 6–7) and the loan-adjustment extension (Section 5.4, eqs. 56–63, Tables 8–9).
- **Loan-adjustment extension.** The bank can cut loans in the bad state by a one-off fraction θ (eqs. 56–57, p. 39). It chooses θ to maximise W(θ) minus a quadratic adjustment cost ξ(Lθ)² (eq. 59, p. 40). Reserves earn ρ (eq. 58).
- Optimal θ* is 5% without the tax and 2% with it in the baseline. It is 6% and 3% with κ2 = 0.11 (Table 9, p. 43).

## 3. Answers to (a), (b), (c) and the caller's specific questions

### (a) Cost of being below a capital requirement

- **Form:** there is no graduated shortfall cost. The requirement xR (eq. 4, p. 9) is a hard floor. On hitting it the bank must either recapitalise by an impulse or be liquidated (pp. 10, 14, eq. 19). Under liquidation, shareholders get Γ+ and lose all future dividends (p. 10).
- The state never goes below xR. Admissibility (11) (p. 11) also forbids dividends that would breach it.
- **No running cost, fine or stigma** grows with the shortfall. The only cost is the lumpy cost of the issue, Gn + κ (eq. 13, p. 12), or the lost franchise value. Shareholders bear it.
- It is "asymmetric" only in the trivial sense that the floor acts at xR alone.
- In the counter-cyclical case (Section 5.3.1, p. 34), the band (xR,2, xR,1) is allowed in state 2. If the economy then switches to state 1, the bank must recapitalise at once or be liquidated (eq. 51, Fig. 5). That is a hard constraint that depends on the state, not a graduated cost.

**How the dividend restriction is modelled.**
- It is a proportional tax 1 − β2 on dividends paid in the bad **macro** state (p. 9), not a restriction tied to the bank's capital level.
- A ban is not imposed as a constraint. It emerges as optimal behaviour when β2 ≤ λ2/(λ2 + δ) (Remark 1, p. 19; App. A.4, pp. 61–64). It is not a cap on the payout rate either: dividends stay singular, as a reflection at x̃2.
- The restriction therefore does **not** act like a cost of being below a requirement. It is a wedge on payouts, felt at the dividend barrier and depending on the macro state, not on the capital shortfall (see D1).

### (b) Costs of issuing equity

- A **fixed cost κ per issue** (p. 10, eq. 13) is the only cost. It is **not scaled by issue size** and it is set in units of reserves, with D = 1.
- There is **no proportional cost**: the term Gn + κ shows the injected amount paid one for one.
- Issues are **lumpy impulses** at the trigger xR, up to target x*i (eq. 32, p. 19). The good-state target is the dividend barrier, x*1 = x̃1, and the bad-state target satisfies x*2 ≤ x̃2 (pp. 17–18).
- κ is **calibrated to 0.087** so that the price-to-book ratio at x̃1 in the good state is about 1.04, the US commercial-bank value in the NYU Stern database (p. 24).
- The extension (Section 5.4) lets the cost vary by state: κ2 = 0.11 > κ1 = 0.087, "following Arnold et al. (2018)" (p. 41).
- κ̄ (eq. 31, p. 18; Table 3, p. 30) is the largest κ at which recapitalising beats liquidating.

### (c) Both frictions, interaction, comparative statics, identification

**Does it have both?** No. There is a hard capital floor with liquidation as the outside option, a fixed issuance cost κ, and a proportional dividend tax in the bad state. A graduated shortfall cost is absent, so the paper cannot study how such a cost interacts with κ.

The dividend tax and κ do interact: higher taxes lower κ̄, "a 10% increase in dividend taxes is associated with a 28% reduction in the level of κ̄" (Table 3 and p. 30). This is numerical.

**Comparative statics.** All are numerical, from Tables 2–9. No comparative-static result in any parameter is proved. What is proved is structural:
- the barrier and impulse structure (Prop. 1, Thm. 1);
- x*1 = x̃1 and x*2 ≤ x̃2 (pp. 17–18);
- the ban cutoff λ2/(λ2 + δ) (Remark 1, App. A.4).

| Parameter change | x̃1 (good-state barrier) | x̃2 (bad-state barrier) | x*2 (bad-state target) | Trigger xR | κ̄ | Source |
|---|---|---|---|---|---|---|
| Higher dividend tax (β2 falls from 1 to 0.834) | falls (0.820 → 0.800) | rises (0.95 → 17.0, exploding as β2 → λ2/(λ2 + δ) ≈ 0.333) | falls (0.95 → 0.649) | unchanged (exogenous) | falls (0.1351 at β2 = 1 → 0.1043 at β2 = 0.85; Table 3 stops at 0.85) | Table 2 p. 27; Table 3 p. 30; pp. 25–27 |
| Γ1 = Γ2 = +0.05 (tighter requirement), at β2 = 1 / 0.87 | rises (0.821 → 0.871 / 0.801 → 0.851) | rises (0.954 → 1.007 / 1.700 → 1.750) | rises (0.954 → 1.007 / 0.655 → 0.705) | rises one for one (eq. 4) | "do not affect" (0.133 / 0.106) | Table 5 p. 32; p. 33 |
| Γ1 = Γ2 = −0.05 (looser requirement) | falls (0.769 / 0.751) | falls (0.923 / 1.650) | falls (0.923 / 0.605) | falls one for one | unchanged | Table 5 p. 32 |
| Counter-cyclical requirement (xR,1 up, xR,2 down), β2 = 0.87 | rises (0.801 → 0.824) | falls (1.700 → 1.656) | falls (0.655 → 0.609) | xR,1 rises, xR,2 falls by design | falls (0.106 → 0.087) | Table 6 p. 36; p. 38 |
| Higher bad-state issuance cost (κ2 = 0.11 > κ1 = 0.087), β2 = 1 / 0.87 | rises (0.821 → 0.832 / 0.801 → 0.813) | rises (0.954 → 1.012 / 1.700 → 1.752) | rises (0.954 → 1.012 / 0.655 → 0.707) | unchanged | not reported | Table 8 p. 42; p. 42 |
| Reserve remuneration (xρ > 0) | rises | rises | rises | unchanged | not reported | Table 8 p. 42 |
| σ2 = 0.35 (from 0.3) | rises | rises | rises | — | falls | Table 5 p. 32; p. 31 |
| μ1 = 0.07 or μ2 = 0.03 (higher drift) | falls | falls (slightly) | falls (slightly) | — | rises | Table 5 p. 32; p. 31 |
| λ1 = 0.05 (bad state visited less) | falls | unchanged | unchanged | — | rises | Table 5 p. 32; p. 31 |
| λ2 = 0.2 (faster exit from bad state) | falls with the tax | rises sharply with the tax (3.324) | rises slightly with the tax | — | rises | Table 5 p. 32; p. 31 |

- The **baseline κ (a common fixed cost in both states) is never varied** in any table. The only issuance-cost comparative static is the state-dependent κ2 in the extension (Table 8, p. 42), which also changes the model to remunerate reserves in rows (i) and (iii).
- The **trigger is never a choice**. It is always xR, so no parameter except Γ, which shifts xR by construction (eq. 4), can move it.

**Do two parameters move a threshold in opposite directions?** Yes, but the paper presents this only as policy trade-offs, not as identification.
- The dividend tax moves x̃1 down and x̃2 up (Table 2, p. 27; "backfires", p. 27).
- The counter-cyclical requirement moves x̃1 up and x̃2 and x*2 down (Table 6, p. 36).
- Across frictions, the dividend tax lowers x*2 while a higher κ2 raises it, and both raise x̃2 (Tables 2, 8; see D3).

**Identification or estimation.** The paper never asks whether parameters can be identified or told apart from the policy, and it estimates nothing. The calibration (p. 24, Table 1 p. 25) is:
- L/D = 1.5385 from S&P Global, US banks, Q4 2022. The text calls this the "deposit-to-loan ratio" but uses it as L with D = 1.
- ρ = 0.0043 from FRED.
- xR = 1 − 0.6 × 1.5385 = 0.0769.
- κ = 0.087 matched to price-to-book ≈ 1.04 at x̃1 (NYU Stern database). This is the single moment-matched parameter.
- μ̄i, σi and δ "similar to Guo et al. (2005)".
- λi and α from Hackbarth et al. (2006).
- β2 = 0.87 from a 10 p.p. tax increment, 0.3 → 0.4 (footnote 15).
- κ2 = 0.11 following Arnold et al. (2018) (p. 41).
- ξ set to match an annual commercial-loan drop of about 5% in the Great Financial Crisis, from FRED (pp. 42–43).

### Variance of capital and exposure caps

- **Variance of capital.**
  - Si (eq. 49) and S (p. 29) are the stationary variances of reserves.
  - Dividend taxes slightly reduce S1 and sharply raise S2 and S. For example S2 goes from 0.0418 to 0.2968 and S from 0.0344 to 0.1722 as β2 goes from 1 to 0.85 (Table 3, p. 30; text p. 29). The widening comes from the higher x̃2 (p. 29).
  - Counter-cyclical requirements lower S (0.098 → 0.092) and reduce the coefficient of variation √S/M by about 3.2% (Table 6 p. 36; p. 37).
  - These results are numerical, from the closed-form density.
  - No result says how any variance limit depends on a penalty parameter, because the model has none.
- **Exposure caps.** There is no regulatory exposure cap. Loans are fixed in the baseline (p. 8). In the extension the bank itself picks a one-off cut θ in the bad state, against a quadratic cost ξ(Lθ)² (eqs. 56–59, pp. 39–40). θ is chosen statically to maximise W(θ) and is not regulated.

### Stated limitations and future research

- Loan exposure is fixed, reserves are not remunerated, and issuance costs are constant. The extension relaxes all three (p. 38).
- There are no investments, and loans and deposits are fixed. A fuller analysis would adjust risky assets dynamically, citing Rochet and Villeneuve (2005) (p. 44).
- The bank perfectly anticipates the regulator's policy in each state. Regulatory uncertainty is left out: a policy enacted only with some probability, or a state the regulator observes imperfectly, would need numerical methods (pp. 44–45).
- The paper points to general-equilibrium work with heterogeneous banks (p. 45).
- It abstracts from share buybacks (footnote 5, p. 4).

## 6. Verdict for the theory paper

1. The paper is close in architecture to the theory paper, with a dividend barrier and impulse recapitalisation to an endogenous target under a fixed cost κ (pp. 10, 17–19). It does **not** have a graduated shortfall cost. The capital requirement is a hard floor where the bank must recapitalise at once or be liquidated, and the state never goes below it (pp. 10, 14).
2. Its "dividend restriction" is a proportional payout tax, or a ban in the limit, that depends on the **macro state**, not on the capital shortfall. It does not act as a cost of being below a requirement (pp. 9, 19).
3. The issuance trigger is fixed at the requirement, not chosen (pp. 17–18). The model has no proportional issuance cost, and the baseline fixed cost κ is never varied in a comparative static. The only one is the state-dependent κ2 in the extension, which raises all barriers and the bad-state target (p. 42).
4. All threshold comparative statics are numerical, at one calibration. They cover the tax, Γ, counter-cyclical requirements, κ2, drifts, volatility and intensities (Tables 2, 5, 6, 8; pp. 27–42). None is proved.
5. The paper never asks whether its frictions can be told apart or identified from the policy, and it estimates nothing. Its one moment-matched parameter is κ, matched to a price-to-book ratio of 1.04 (p. 24).
6. It computes the stationary variance of reserves and shows how dividend taxes and counter-cyclical requirements change it (pp. 29–30, 37). It has no exposure cap, and no result on how a penalty parameter enters the variance (pp. 8, 39–40).
