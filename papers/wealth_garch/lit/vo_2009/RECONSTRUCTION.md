# Reconstruction: Vo (2009)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/vo_2009.pdf`, sha256 `27d592a3c017bc3ba5e98090c87c5ddc5aaa1786d7bf2c050695852fccf2e207`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** VO Thi Quynh Anh (Norges Bank), "Optimality of Prompt Corrective Action in a Continuous - Time Model with Recapitalization Possibility", December 2009. Norges Bank Working Paper 2009 | 28, Research Department. ISSN 1502-8143, ISBN 978-82-7553-534-2 (online). The EconStor cover (PDF p. 1) gives: Vo, Thi Quynh Anh (2009): Optimality of Prompt Corrective Action in a Continuous - Time Model with Recapitalization Possibility, Working Paper, No. 2009/28, Norges Bank, Oslo, https://hdl.handle.net/11250/2497612.
- **File.** `/root/.cache/wealth_garch/vo_2009.pdf`
- **PDF pages.** 27 (pdfinfo).
- **Page mapping.** PDF p. 1 is the EconStor cover, PDF p. 2 the Norges Bank series cover, PDF p. 3 the series information page. Printed page 1 (title, abstract, start of the introduction) is PDF p. 4, so **printed page = PDF page − 3**. The last printed page, 24 (references), is PDF p. 27.
- **Coverage.** Read in full: all 27 PDF pages, including Appendices A to C (printed pp. 17–23) and the references (pp. 23–24). The text layer drops Greek letters, so the symbols were checked against page images of PDF pp. 9, 12, 16 and 18. The per-unit recapitalisation cost is α, cash-flow drift and volatility are μ and σ, and the banker's discount rate is ρ.

## 2. Reconstruction

**Agents and timing (pp. 5–7).** A risk-neutral banker and a risk-neutral Deposit Insurance Corporation (DIC) are in an infinitely repeated relationship. To open a bank, the banker transfers E0 to the DIC, collects D deposits and invests them in a long-term risky loan portfolio. Cumulative cash flows follow dR_t = μA_t dt + σ dZ_t^A, with effort A_t ∈ {0,1}. Shirking (A_t = 0) gives a private benefit B < μ. Effort is unobservable and cash flows are public (p. 6). The banker discounts at ρ and the DIC at r < ρ (p. 6, fn. 8). The banker can quit at any time for an outside utility W̃ ∈ [0, μ/ρ]. The loan portfolio is worth zero at termination (p. 6).

**Contract and frictions (p. 6, eqs. (1)–(5) on p. 7).** A contract specifies a liquidation time τ and cumulative transfers C_t to the banker. Transfers can be positive (dividends), zero or negative (capital injection, i.e. recapitalisation).
- **Recapitalisation cost:** the banker bears a **proportional** cost α > 0 for each unit of capital contributed. Her utility counts transfers as (1 + α·1{dC_t<0}) dC_t (pp. 6–7, eq. (3)). There is **no fixed cost** per issue.
- **Flow cap on injections:** dC_t ≥ −K dt (eq. (5)). K here is a bound on the *rate* of injection, justified by a borrowing constraint from capital-market imperfection (pp. 2–3, 6). It is not a fixed cost, despite the shared letter with the theory paper.
- **No running penalty** on low capital or low continuation utility. Closure at the lower boundary is the only terminal event.

The DIC maximises E∫ e^{−rt}(μ dt − dC_t) (eq. (1)) subject to incentive compatibility (2), promise keeping (3), participation (4) and the injection cap (5).

**Solution structure (Section 4, pp. 8–14).**
- Lemma 1 (p. 8, eq. (6)) and Proposition 1 (p. 9): high effort is incentive compatible if and only if the sensitivity G_t of the continuation utility W_t is at least Bσ/μ. The ODEs use G = Bσ/μ, which gives the term (B²/μ²)σ². Proof in Appendix A, pp. 17–18.
- The DIC's value function is F(W) (eqs. (11)–(12), p. 10). F is assumed concave and this is checked later (Appendix B).
- **Dividend threshold W\***: the smallest W with F′(W) = −1. Dividends are paid if and only if W_t > W\* (p. 10).
- **Recapitalisation threshold W̄**: the smallest W with F′(W) = −1/(1+α) (p. 11). Concavity gives W̄ < W\*.
- On (W̃, W̄] the banker injects at the maximal rate c_t = −K (p. 12). This assumes absolutely continuous injections (p. 11), so there are **no lumpy issues**. The bank closes when W_t reaches W̃ (p. 11), and F(W̃) = 0.
- **Proposition 2 (pp. 12–13).**
  - On [W̃, W̄]: rF1 = F1′(ρW + (1+α)K) + ½F1″(B²/μ²)σ² + μ + K.
  - On [W̄, W\*]: rF2 = F2′ρW + ½F2″(B²/μ²)σ² + μ.
  - Above W\*: F is linear with slope −1.
  - Six boundary conditions: F1(W̃) = 0; F1′(W̄) = F2′(W̄) = −1/(1+α); F1″(W̄) = F2″(W̄); F2′(W\*) = −1; F2″(W\*) = 0.
  - Existence, uniqueness and concavity are shown in Appendix B, pp. 18–22.
- Figure 1 (p. 13) shows three payment regions: negative payments on (W̃, W̄], zero payments on (W̄, W\*], positive payments above W\*.

**Comparative statics in the recapitalisation cost (p. 13).** The paper states: "We found that recapitalization threshold is decreasing with α while dividend threshold is increasing with it." Higher α shrinks the recapitalisation region and delays dividends "for prudent reason". **Corollary 1 (p. 13):** as α → 0, W̄ → W\* (the two thresholds merge); as α → +∞, W̄ → W̃ (no recapitalisation). No derivation, figure or numerical illustration of the monotonicity is given; only the two limits are argued, from the boundary conditions. There are no comparative statics in K, B, σ, r or ρ.

**Initial rent (p. 14).** The DIC charters from a competitive pool and sets W0 = argmax F(W0) subject to W0 ≥ W̃.

**Implementation (Proposition 3, pp. 14–15; proof in Appendix C, p. 23).**
- Bank capital is E_t = W_t − W̃. The thresholds are E\* = W\* − W̃ (dividends) and Ē = W̄ − W̃ (recapitalisation).
- Chartering requires E0 = W0 − W̃.
- A deposit-insurance premium dP_t depends on E_t and dR_t. It is decreasing in capital, i.e. risk-based with risk measured by capital (p. 15). When the bank recapitalises, the premium is cut by αK, which the paper says equals the total recapitalisation cost. It reads this as a subsidy (p. 15).
- Capital regulation:
  - above E\*: all excess capital is paid out as dividends;
  - on [Ē, E\*): dividends are prohibited and no recapitalisation is required;
  - below Ē: recapitalisation is ordered;
  - at E_t = 0: liquidation.
- The paper presents this as four PCA-like categories (p. 15). It argues that this mirrors the US PCA, with limited supervisory discretion and closure before economic capital turns negative (pp. 15–16). The triggers are book values, but the paper says this is not an argument for book over market value, because r and the default probability are fixed in the model (p. 16).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a capital requirement or threshold.**
- There is **no explicit cost function** of the shortfall (no fine, no running penalty that grows with the shortfall).
- The graduated intervention is **endogenous**. The thresholds E\* and Ē come from the concavity of the DIC's value function F under dynamic moral hazard. They are the points where F′ equals −1 and −1/(1+α) (pp. 10–11).
- The "costs" of low capital are the policy restrictions themselves:
  - dividend prohibition on (Ē, E\*] (p. 15);
  - mandatory recapitalisation at the maximal rate K below Ē (pp. 12, 15);
  - liquidation at E = 0, where the banker receives only W̃ and the portfolio is lost (pp. 6, 11, 15).
- These restrictions are applied only below the thresholds, so they are asymmetric in that sense. The liquidation loss falls on the DIC (F(W̃) = 0, p. 11).
- The recapitalisation cost is borne by the banker. In the implementation the DIC refunds it through a premium reduction, which the paper calls a subsidy (p. 15).
- The threshold is not an exogenous regulatory minimum. Ē and E\* are optimal contract boundaries.

**(b) Costs of issuing equity.**
- The cost is **proportional only**: α > 0 per unit of capital contributed, borne by the banker (pp. 2, 6).
- There is **no fixed cost** per issue.
- Issuance is a **flow**, capped at rate K per unit of time (dC_t ≥ −K dt, eq. (5), p. 6), and is assumed absolutely continuous in time (p. 11). There are therefore **no lumpy issues** and no impulse control.
- Below Ē, the optimal injection is at the cap (c_t = −K, p. 12).

**(c) Both frictions, interaction, comparative statics, identification.**
- The paper does **not** have both a graduated shortfall cost and a fixed issuance cost. It has a proportional issuance cost and an injection-rate cap.
- **Comparative statics** are reported only in α (p. 13): the recapitalisation threshold is **decreasing** in α and the dividend threshold is **increasing** in α. The limits are α → 0 (W̄ → W\*) and α → ∞ (W̄ → W̃) (Corollary 1, p. 13). The monotonicity is stated without a shown proof or figure.
- There are no comparative statics in K (the cap) or in any shortfall-cost parameter.
- The paper does not ask whether frictions can be told apart from the thresholds. There is no identification, estimation, calibration or numerical example anywhere (all 24 printed pages checked).

**Buffers and variance.**
- The intermediate region (Ē, E\*], where dividends are withheld but no recapitalisation is required (p. 15), is a capital buffer above the recapitalisation trigger. Its width rises with α by the p. 13 statement.
- Nothing is said about the variance or stationary distribution of capital.

**Stated limitations and future work.** Distinguishing book from market value "requires a richer setting than ours", because r and the loan default probability are fixed (p. 16). The conclusion (p. 16) gives no further agenda.

## 6. Verdict for the theory paper

- Vo derives a graduated, PCA-like intervention: no restriction above E\*, a dividend ban on (Ē, E\*], mandatory recapitalisation below Ē, liquidation at zero capital (p. 15). The gradation arises endogenously from moral hazard and the concavity of the DIC's value, not from a running shortfall cost (pp. 10–11).
- Its only issuance friction is a **proportional** cost α per unit injected, plus a cap K on the injection rate. There is **no fixed issuance cost** and no lumpy issue (pp. 6, 11–12).
- The paper reports that the recapitalisation threshold falls and the dividend threshold rises in α, with the thresholds merging as α → 0 and recapitalisation vanishing as α → ∞ (p. 13, Corollary 1). The monotonicity is stated without a displayed proof.
- Vo does **not** pair a graduated shortfall cost with a fixed issuance cost. It does **not** study how two frictions interact, and it does **not** ask whether the thresholds identify the frictions. There is no estimation, calibration or numerical work, and nothing on the variance of capital.
- At most it can be cited for the proportional-cost directions on the two thresholds (p. 13) and for an optimal-contract foundation of graduated PCA (pp. 1, 15).
