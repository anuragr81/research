# Reconstruction: Egami and Oryu (version of 8 November 2014)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/egami_oryu_2013_v2.pdf`, sha256 `522825d48acfc464aed85efa9b1fc5d5b1d185971716aa0b60adf979210c9e91`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- Citation as printed on the title page (PDF p. 1): Masahiko Egami and Tadao Oryu, "An Excursion-Theoretic Approach to Regulator’s Bank Reorganization Problem". Footnote on p. 1: "This version: November 8, 2014." Funding: Grant-in-Aid for Scientific Research (B) No. 26285069, Japan Society for the Promotion of Science. Key words: Prompt corrective actions; excursion theory; spectrally negative Lévy processes; scale functions. JEL D81, G21, G32. MSC (2010) Primary 60G40, Secondary 60J75. Both authors: Graduate School of Economics, Kyoto University (p. 24).
- Publication information: the PDF prints **no journal, volume or publisher**, and no arXiv identifier or stamp. It is a working-paper version. (The task says it is a later version of arXiv 1311.3019; the PDF itself does not say so.)
- File: `/root/.cache/wealth_garch/egami_oryu_2013_v2.pdf` (MiKTeX-dvipdfmx; PDF metadata creation date 12 Feb 2015).
- PDF pages: 24.
- Page numbers: the PDF **does print page numbers**. Page 1 has "1" at the foot. Pages 2-24 have the number in the running header. Printed page = PDF page (offset 0). All page references below are printed pages, which are also PDF pages.
- Coverage: **read in full**, pp. 1-24. That covers the text (pp. 1-21), the Appendix on scale functions (Section 6, pp. 21-22) and the references (pp. 22-24). Figures 1-7 were read for their captions and for the numbers given in the text. The plotted curves are not in the text layer.
- Layout: Section 1 Introduction (pp. 1-3); Section 2 Mathematical Model (pp. 3-6); Section 3 Solution (pp. 6-10); Section 4 Examples: 4.1 numerical example with comparative statics (pp. 10-15), 4.2 JP Morgan calibration (pp. 15-19); Section 5 Extension to Multiple PCA's (pp. 19-21); Section 6 Appendix (pp. 21-22). **There is no conclusion section and no future-research section.**

## 2. Reconstruction

**Whose problem.** The problem belongs to the **regulator**. "This threshold b′ should be determined by the regulator and we call it the PCA trigger level" (p. 4). The paper computes "the optimal threshold level that triggers PCA's and that minimizes the associated total cost" (p. 2). The bank makes no choices. Its balance-sheet policy is mechanical (pp. 2, 4). There is no bank value function, no dividend policy and no equity issuance decision by the bank.

**State variables (pp. 2, 4).**
- X is the log of the bank's total asset value. Y = e^X is total assets (p. 2; p. 4).
- X follows a spectrally negative Lévy process, switching between X^0 (normal regime, I = 0) and X^1 (PCA regime, I = 1), with Laplace exponents ψ_i (eq. 2.1, p. 4). The paper remarks that typically μ1 < μ0 and σ1 < σ0, "to reflect more conservative risk-return profile during the PCA period" (p. 4).
- S is the running maximum of X (p. 4).
- The bank keeps leverage Debt/Total Asset = e^{−b} whenever X = S. It "increases its asset base as long as" X = S (p. 2), so debt is e^{S−b}.
- The effective state is the **drawdown (excursion) S − X**. Leverage is e^{S−b}/e^X. Equity is wiped out when S − X = b (p. 2).
- So the state is **leverage-based**: the regulator watches the leverage ratio through the excursion S − X. The trigger is a drawdown level b′ ∈ [0, b]. At the trigger, leverage equals e^{b′−b} (p. 4).

**What the PCA does when triggered (pp. 4-5).**
- At T_{b′} = inf{t : S_t − X_t ≥ b′} (p. 4) the regime switches to I = 1, with dynamics ψ1.
- Assets are pushed up to e^{S_{T_{b′}} − a}, with a ∈ [0, b) a constant. This is done "by injecting funds (taxpayers’ money)", and leverage improves to e^{a−b} (p. 4).
- The injection is public equity: e^{S_{T_{b′}} − a} − e^{X} (p. 5). Here X is the pre-injection asset value, which includes any jump overshoot below S − b′.
- The PCA ends at τ = T_{b′} + (τ_a^+ ∧ τ_{a−b}^−)∘θ (eq. 2.2, p. 5).
  - Recovery: X^1 rises by a, back to S, and leverage returns to e^{−b}.
  - Insolvency: X^1 falls by b − a first.
- Section 5 lets the PCA recur after each recovery until insolvency (pp. 19-21).

**Total cost (eq. 2.3, p. 5).** The expected discounted cost of one PCA (discount rate q) has three parts.

C1(x,s;b′) = E[ e^{−qT_{b′}}(e^{S_{T_{b′}}−a} − e^{X}) + α ∫_{T_{b′}}^{τ} e^{−qt}dt + e^{−qτ} β e^{S_{T_{b′}}−b} 1{insolvency} ]

1. **Initial cost**: the cash infusion. It is proportional to the bank's size e^S, plus the overshoot (p. 5). The paper notes it could be generalised to c(X, S_{T_{b′}}). That general form might include cash set aside for insolvency, or the "present value of administration costs to alter bank’s risk-return profile from ψ0 to ψ1" (p. 5). This generalisation is not pursued.
2. **Running cost**: rate α ≥ 0 per unit time "in proportion to the duration of PCA being in place" (p. 5). In §4.2 the paper sets α = 0.05V0, "5% of the asset value per unit time" (p. 16).
3. **Social cost at insolvency**: β e^{S_{T_{b′}}−b} (β ≥ 0). This is β times the bank's assets when it becomes insolvent. It "reflects the fact that the cost of bank’s failure becomes greater as the size of the bank becomes larger" (p. 5).

The regulator chooses b′ for **a given a** (p. 5: "for a given value of a, we analyze the cost-minimizing PCA trigger level denoted by b∗"). The target a is not optimised jointly. Table 1 only compares b∗ across a grid of a values (p. 15).

**Solution structure (Section 3, pp. 6-10).**
- C1 is split into three cases (p. 6):
  - C1^0, for S0 − X0 ≥ b′: Proposition 3.1, eq. 3.1 (p. 6). Proof via eq. 3.2 (p. 7).
  - C1^1, for S0 = X0: Proposition 3.2, eq. 3.3 (p. 7). Proof via eq. 3.4 (p. 8), using Pistorius's excursion results (p. 9).
  - C1^2, for S0 − X0 ∈ (0, b′): Proposition 3.3, eq. 3.5 (p. 10).
- All are written in q-scale functions W_i^{(q)} and Z_i^{(q)}.
- Remark 3.1 (p. 9): C1^1 < ∞ iff 1 − W0^{(q)′}(b′)/W0^{(q)}(b′) < 0. This gives an upper bound on admissible triggers; in the example cost diverges for b′ > 0.6672 (p. 11).
- **b∗ is found by numerical minimisation over b′**. "the computation is numerical and is for a specific model" (p. 5). There is no first-order condition, smooth-fit condition or closed form for b∗.
- Multiple PCAs: recursive costs C_n. Proposition 5.1, eq. 5.1 (p. 20); Remark 5.1 (p. 20); Proposition 5.2, eq. 5.2 and Proposition 5.3, eq. 5.3 (p. 21), with proofs skipped (p. 21). The multiple-PCA case is not solved numerically.
- Appendix (pp. 21-22): scale-function facts, eqs. 6.1-6.7.

**Main results (all numerical except Remark 4.1).**
- Example (eq. 4.1, p. 10; scale function eq. 4.2, p. 11): Brownian motion with drift plus exponential jumps. Baseline: b = 1, a = 0.3, q = 0.1, α = β = 1, μ0 = 0.2, σ0 = 0.2, μ1 = 0.1, σ1 = 0.1, c0 = c1 = 1, ρ0 = ρ1 = 10 (p. 11). Result: b∗ = 0.5201 and C1(0,0;b∗) = 2.921 (Figure 1, p. 11).
- The trade-off (pp. 11-12):
  - A large b′ (late PCA) means the bank is large when the PCA begins, so the initial cost and the penalty get large.
  - b′ near a (early PCA) makes the running cost large.
- Comparative statics (Section 4.1, pp. 12-15). These are in the PCA-regime dynamics and the target a only; see (c).
- Bank size (Remark 4.1, pp. 18-19), with α proportional to asset value: C1(x,x;b′) = e^x G(b′). So **b∗ is independent of the initial asset level x**, and the minimal cost is proportional to e^x (p. 17; argument pp. 18-19).
- JP Morgan calibration (Section 4.2, pp. 15-18):
  - 2012-13: b∗ = 0.09457, minimal cost 1,469,060 million (p. 16).
  - 2006-07: b∗ = a = 0.05, a boundary solution, cost 688,187 million (p. 17).
  - Also the density of the "speed of market crash", the time from the last peak to the trigger (eq. 4.3, pp. 17-18; Figure 7, p. 18).

## 3. Answers to (a), (b), (c) and the task's further questions

**(a) Cost of being below a threshold.**
- Yes. Falling to the PCA trigger (drawdown b′, leverage e^{b′−b}) sets off three costs, all of them for the regulator and taxpayers or society (pp. 4-5, eq. 2.3).
- The intervention is a **forced recapitalisation with public funds**, not liquidation or a fine. A regime switch to more conservative asset dynamics comes with it (pp. 3-4).
- The **running cost is a constant rate α per unit time while the PCA is in force** (p. 5). **It does not grow with the size of the shortfall.** It depends only on the duration of the PCA.
  - The PCA lasts until full recovery to the normal leverage e^{−b} (drawdown 0), not merely until the bank is back above the trigger (eq. 2.2, p. 5). So the running cost is charged on [T_{b′}, τ), including time spent above the trigger level after the injection (D3).
  - In §4.2, α = 0.05V0 scales with bank size, not with shortfall (p. 16).
- The insolvency penalty β e^{S−b} is a **lump sum at failure, proportional to bank size** (p. 5). It is the "social cost to the economy" (p. 5).
- Who bears the costs:
  - The cash infusion is "taxpayers’ money" (p. 4), with "public funds to be injected (as equity)" (p. 5).
  - The running cost is described as "the administration cost of banks under the PCA provisions" (p. 16).
  - **None of the costs is borne by the bank's shareholders in the model.** The bank has no objective (D1).
- Is it asymmetric? Costs arise only once a PCA has been triggered: none accrue in the normal regime. But it is not a running penalty on the distance below a requirement.

**(b) Costs of issuing equity.**
- **No fixed cost, and no proportional issuance cost.** The bank never issues equity on its own. The paper says the bank lacks "enough creditworthiness to raise capital and hence taxpayers' money may have to be used" (p. 3).
- The "initial cost" is the injected amount itself, e^{S−a} − e^X (p. 5). It is one lump at the start of each PCA, with a fixed target a. Its size scales with bank size e^S, and it is larger after a jump overshoot (pp. 5, 8).
- In the paper's own taxonomy there is **no fixed cost of intervention or of recapitalisation**. There is no proportional cost on the injected amount beyond the amount itself, and no deadweight cost per unit raised.
- The paper only mentions a possible generalisation c(X,S) that could include "the present value of administration costs to alter bank’s risk-return profile" (p. 5). It is not modelled.
- The recapitalisation is lumpy. It is an impulse from the trigger to a fixed target S − a, with reset at each PCA (pp. 4-5, 19), but the target is exogenous.

**(c) Both frictions, interaction, comparative statics, identification.**
- **The paper has a duration-based running cost (α) plus a size-proportional injection and failure cost. It has no shortfall-graded running cost and no fixed issuance cost**, so it cannot study the pairing in the theory paper.
- **α and β are never varied.** They are fixed at α = β = 1 in §4.1 (p. 11) and at α = 0.05V0, β = 1 in §4.2 (pp. 15-16). So there are **no comparative statics of b∗ in either cost parameter**.
- Comparative statics actually reported (all numerical, single example, no proofs):
  - **σ1 ↑** (0.1, 0.2, 0.4): b∗ ↓ (earlier PCA), total cost ↓ (Figure 2(i), p. 12).
  - **μ1 ↑** (0.1, 0.2, 0.4): b∗ ↓, total cost ↓ (Figure 2(ii), pp. 12-13). Exact b∗ values for σ1 and μ1 are not printed.
  - **ρ1** (10, 15, 20; mean jump size 1/ρ1): b∗ = 0.5201, 0.5252, 0.5193, **non-monotone**; cost falls as ρ1 rises (p. 13; Figure 3(i) caption).
  - **c1** (jump intensity 1, 2/3, 0.5): b∗ = 0.5201, 0.5184, 0.5129, so **b∗ rises with c1** (p. 14). Lower intensity means lower cost and an earlier PCA (Figure 3(ii) caption, p. 13).
  - **Jumps versus no jumps**, with μ1 = μ0 etc.: b∗ = 0.441 with jumps versus 0.459 without; minimal costs 1.44250 versus 1.19319 (p. 14; Figure 4).
  - **Target a** (0.1 to 0.6): b∗ = 0.3918, 0.4841, 0.5201, 0.5410, 0.5544, 0.6, **increasing in a**. The infusion gap b∗ − a falls from 0.2918 to 0. Cost C1(0,0;b∗) = 1.887, 2.591, 2.921, 3.027, 2.977, 2.939, non-monotone with a local maximum near a = 0.4. At a = 0.6, b∗ hits the boundary (Table 1, p. 15; text p. 14).
  - **Bank size x**: b∗ is **independent of x** when α is proportional to asset value. Cost is proportional to e^x (Remark 4.1, pp. 18-19). This is an analytical argument, not numerical, and it is specific to the α-proportional set-up. The paper warns: "With different setting on α, we may obtain different results" (p. 17).
  - **JPM sub-periods**: the trigger moves from b∗ = 0.05 (2006-07, high σ) to 0.09457 (2012-13, low σ) (pp. 16-17). This is consistent with the σ comparative static (p. 17 refers back to 4.1.1).
- **Opposite directions.**
  - Yes, among dynamics parameters: higher μ1 and higher σ1 lower b∗ (p. 12), while higher c1 and higher a raise b∗ (pp. 14-15).
  - The paper notes that "a small c1 seems to have a similar effect to a large μ1" (p. 14).
  - Among cost parameters, nothing is reported (D5).
- **Identification and estimation.**
  - The paper **does not ask whether any parameter can be identified from the trigger**. It has no Jacobian or invertibility argument, and it does not try to tell the cost components apart from the policy.
  - It **estimates only the asset drift μ, volatility σ and initial asset value V0 of JP Morgan Chase**. The method is the option-theoretic maximum-likelihood approach (citing Duan 1994, 2000 and Lehar 2005; p. 16). The data are weekly equity prices from Datastream and year-end debt (short-term plus half of long-term, following Moody's KMV), over four two-year blocks of 2006-2013 (Table 2, p. 16).
  - b = 0.1054 is calibrated from JPM's leverage of about 0.89, derived from ROE 7.40% and ROA 0.65% (p. 16).
  - a = 0.05, α = 0.05V0, β = 1 and the weekly q = 0.001 are simply set (pp. 15-16).
  - **No cost parameter is estimated or calibrated to moments.**

**Capital buffers and variance of capital.**
- The bank holds a constant leverage e^{−b} at its running maximum (p. 2). The "buffer" is the drawdown room b′ before the PCA and b before insolvency.
- The PCA restores the buffer to a (leverage e^{a−b}, p. 4).
- Nothing on the variance of capital or of the buffer.
- Figure 7 (p. 18) gives the density of the time from the last peak to the trigger for b′ = 0.09. It spans roughly 0-100 weeks in 2012-13 and roughly 0-10 weeks in 2006-07 (p. 18).

**Stated limitations (no future-research section exists).**
- γ = 1 (all asset losses hit equity, no forced deleveraging) is assumed "for making our presentation simpler" (p. 2).
- The computation is numerical and specific to one model (p. 5).
- b = 1 "may not be realistic" (p. 11).
- The conclusion "may become different with different sets of parameters" (p. 13).
- Results may change with a different α setting (p. 17).
- The multiple-PCA cost is "not of an explicit form" (p. 19).
- The initial cost could be generalised to c(X,S) (p. 5).
- The "no jump" assumption is justified for large, diversified banks (p. 15).

## 6. Verdict for the theory paper

1. This paper is a regulator's cost-minimisation over a single leverage trigger b′ for publicly funded recapitalisation (pp. 4-5, eq. 2.3). Its running cost is a constant rate α per unit time of PCA duration, scaled by bank size in the calibration (pp. 5, 16). **It does not grow with the shortfall**, and it falls on the public side, not on the bank (D1, D3).
2. **It has no fixed issuance or intervention cost and no proportional issuance cost.** The "initial cost" is the injected amount e^{S−a} − e^X itself, proportional to bank size (p. 5), and the bank never issues equity privately (p. 3). So the pairing of a graduated shortfall cost with a fixed issuance cost is **absent**.
3. **α and β are held fixed** (p. 11; pp. 15-16), so it gives **no comparative statics of the trigger in any cost parameter**. Its numerical comparative statics concern the PCA-regime dynamics: b∗ falls in μ1 and σ1, rises in c1 and is non-monotone in ρ1 (pp. 12-14). They also cover the exogenous target a: b∗ rises in a (Table 1, p. 15). Bank size does not move b∗ when α is proportional to assets (Remark 4.1, pp. 18-19).
4. **It does not ask whether parameters can be identified from the trigger, and it estimates only μ, σ and V0 of JP Morgan's assets**, by the option-theoretic MLE (p. 16). No cost parameter is estimated.
5. For the theory paper, its most usable points are a citable precedent that the cost of a lumpy, regulator-forced recapitalisation scales with bank size (p. 5), and that the optimal trigger is invariant to size under proportional costs (pp. 17-19). It offers nothing on telling a shortfall cost apart from an issuance cost.
