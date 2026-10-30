# Reconstruction: Lang and Menno (2026)

## Source read

- ECB Working Paper 3188, fetched from ecb.europa.eu on 25 October 2026
  (ecb.wp3188~28f203e31a.en.pdf) and pinned as
  `~/.cache/wealth_garch/lang_menno_2026.pdf`, sha256
  `3299cc271b038351336ddb3d40b1eb85ca6902f6fe7cb14d3417f1644ed816cd`.
- Read in full, all 51 PDF pages.

## Reading notes for the theory paper's Section 3.1

The quotation IDs below are those of `CLAIMS.md` after the merge.

### 1. Source

- **Citation as printed:** Jan Hannes Lang, Dominik Menno, "A structural model of capital buffer usability", European Central Bank, Working Paper Series, No 3188. The imprint page says "© European Central Bank, 2026". ISBN 978-92-899-7640-4, ISSN 1725-2806, doi:10.2866/1442210, QB-01-26-028-EN-N (PDF p. 51). The cover and imprint print no month.
- **PDF file:** /root/.cache/wealth_garch/lang_menno_2026.pdf
- **PDF pages:** 51 (pdfinfo).
- **Page mapping:** printed page = PDF page − 1. PDF p. 1 is the unnumbered cover. PDF p. 2 is printed p. 1 (Abstract). PDF p. 50 is printed p. 49 (Appendix B). PDF p. 51 is the unnumbered acknowledgements and imprint page. Page numbers below are printed numbers unless marked "PDF".
- **Coverage:** read in full (PDF pp. 1–51), including the non-technical summary, Sections 1–8, the references, Appendix A (A1–A7, proofs) and Appendix B (Figure B1). The figures are images, so their plotted values could not be read from the text layer; only their captions and the text describing them were used.

### 2. Reconstruction

**Setting.** This is a two-period banking-sector model (p. 8) that cuts the dynamic model of Lang and Menno (2025) down to two periods "for tractability", which makes it like an overlapping-generations model (pp. 5, 8). It has a representative risk-neutral bank under monopolistic competition, with elasticity of substitution μ between banks' loans and an aggregate loan demand shifter λ with semi-elasticity ϵ (eq. 2, p. 10). The only shock is aggregate credit risk θ, which follows a log AR(1): ln θ′ = α0 + α1 ln θ + α2 ε′ (eq. 7, p. 12). There are no idiosyncratic shocks. The paper studies symmetric pure-strategy equilibria with L′ = L^A′ (p. 12).

**Choices.** In period 0 the bank has equity E, loans L and credit risk θ. It earns profit π and chooses new loans L′ and a dividend d, which gives next-period equity E′ = E + π − d. In period 1, θ′ is realised, the bank earns π′, exits, and pays out E′ + π′ (p. 8, Figure 2 p. 9). The bank maximises the discounted expected dividends at the required return on equity, with β = 1/(1+ρ) and ρ > i^D, so equity costs more than deposits (p. 10). The bank also makes a **discrete choice**: "use" the buffer (CR′ < R^cbr, value V^U) or "not use" it (CR′ ≥ R^cbr, value V^N), with V = max[V^U, V^N] (eqs. 3–5, pp. 10–11).

**Profit.** π = [i(L, L^A) − i^D − κ − θ − ξ·1{E < R^cbr ωL}] L + i^D E (eq. 1, p. 10).

**Frictions.**
1. **Minimum requirement R^min with costly liquidation.** If (E′+π′)/(ωL′) < R^min in period 1, the supervisor liquidates the bank and a **proportional liquidation cost ΔL′** is "borne by bank shareholders" (p. 9). The failure threshold is θ* in eq. (6) (p. 11). In the value functions this enters as −β∫_{θ*}^∞ ΔL′ f(θ′|θ)dθ′, that is ΔL′·PD(θ*) (eqs. 4–5, 25). Δ = 1%, chosen as a "reasonable ad-hoc parameter choice" (pp. 12–13). The cost scales with the loan book and not with the size of the shortfall below R^min.
2. **Capital buffer requirement (CBR) R^cbr ≥ R^min with stigma.** The bank may breach the CBR, but then "proportional stigma costs of ξL′ apply" (p. 9). In eq. (1) the cost is ξ·1{CR < R^cbr}·L: a per-unit-of-loans charge switched on by an **indicator** of being below R^cbr (also eqs. 6, 15, 26, 30–31). It is **charged only below the CBR** (asymmetric), it is a **step in the capital ratio**, and it does **not grow with the size of the shortfall** R^cbr − CR. The bank's shareholders bear it, because it is deducted from profit. There are two values: aggregate stigma ξ^{U|U}, when all banks use the buffer, and individual stigma ξ^{U|N}, when only the deviating bank does, with ξ^{U|N} > ξ^{U|U} (p. 10).
3. **Equity issuance constraint d ≥ 0.** This is the only equity-raising friction: "An equity issuance constraint is equivalent to a non-negativity constraint on bank dividends d" (footnote 4, p. 8). Issuance is either free (Sections 3–5, where the constraint is dropped or slack) or impossible (Section 6, where the constraint binds, so d = 0 and E′ = E + π, p. 42). The model has **no fixed cost and no proportional cost of issuing**. The equity premium ρ − i^D is a cost of *holding* equity funding, not of issuing it. ψ is the multiplier on the issuance constraint (p. 11).

**Solution structure.** FOCs for each discrete branch (eqs. 11–24, App. A1). The loan rate is a mark-up MU times marginal cost MC (eqs. 19, 24). The payoff is written as V = E + π + (1/(1+ρ))(MU − 1)MU^{−μ}MC^{1−μ}g(L^A′) (eqs. 50–51), and the two branches are compared.

**Main results.**
- *Prop. 1* (p. 14): with only R^min and no other friction, the bank holds no voluntary buffer, CR′ = R^min, and log L′ = λ − ϵ·(μ/(μ−1))[i^D + κ + E(θ′) + (ρ−i^D)ωR^min].
- *Prop. 2* (p. 16): with costly liquidation, if f(θ*m|θ) > (ρ−i^D)/(Δ(1+i^D)), the bank holds a voluntary buffer CR′ > R^min. The failure threshold solves f(θ*|θ) = (ρ−i^D)/(Δ(1+i^D)) (eq. 40), and PD > 0. The calibrated voluntary buffers are 1–2 pp and pro-cyclical. They rise with Δ, while PD falls with Δ (Figure 5, pp. 17–18).
- *Prop. 3* (p. 18): with no issuance constraint, the bank meets the CBR if ξ^{U|U} > (ρ−i^D)ω[R^cbr − CR^U′] + Δ[PD(θ*N) − PD(θ*U)] (eq. 8). It then sits **exactly at the CBR, CR′ = R^cbr, with no voluntary buffer above it**. The calibrated threshold for ξ is 0.5–3 bps (Figure 6, p. 19), and 0.5–4.5 bps across Δ and μ (p. 20).
- *Prop. 4* (p. 21): when the issuance constraint binds, a buffer-use equilibrium does not exist if ξ^{U|U} exceeds the Prop. 3 terms plus the forgone "excess profit" from deleveraging (eq. 9). The calibrated threshold is still 0.5–3 bps (Figure 8, p. 23).
- *Prop. 5* (p. 24): a no-use (deleveraging) equilibrium exists if ξ^{U|N} exceeds an analogous bound (eq. 10). The calibrated threshold is 10–30 bps, and it rises with deleveraging pressure (Figure 9, p. 26).
- *Prop. 6* (p. 28): in the usability-constrained equilibrium, CR = R^cbr. Loans are L^Ñ′ = s^Ñ L^N′ with s^Ñ = CR_L^N′/R^cbr < 1, so deleveraging is proportional to the capital-ratio shock. The loan rate is i^Ñ = i^N − log(s^Ñ)/ϵ, and PD is lower than under use.
- Figure 10 (p. 27) lists the cases of a unique equilibrium, two equilibria, or none, depending on which of the two stigma conditions holds.

### 3. Answers to (a), (b), (c)

**(a) Cost of being below a requirement.** There are two, at different thresholds.
- *Below R^min:* the bank is liquidated, with a proportional cost ΔL′ borne by shareholders (p. 9). This is closure. It scales with the loan book, not with the shortfall, and it is charged only when the bank is below R^min.
- *Below R^cbr:* a reduced-form "stigma" cost ξL′ (pp. 9–10, eq. 1 p. 10). It is **asymmetric** (charged only when CR < R^cbr) and is a **fixed charge per unit of loans switched on by an indicator**. It does **not grow with the size of the shortfall**: a bank 0.1 pp below the CBR pays the same ξL′ as one 2 pp below. The bank's shareholders bear it through profit. The paper says it stands in for supervisory scrutiny, a capital conservation plan, restrictions on dividends and AT1 coupons, and market stigma (pp. 6, 9, 30). The dividend restriction is **not modelled as an explicit constraint**; it is folded into ξ (pp. 9, 30).
- *Calibration of ξ:* ad hoc. ξ^{U|U} = 0.0003 and ξ^{U|N} = 0.0025 in Table 1 (p. 13), described as "low but sufficient to induce banks to fulfill the CBR". The text says 3 bps and 25 bps (p. 12). The paper says the specific values do not affect the derived results (p. 13). As a plausibility check it compares the required magnitudes with a meta-analysis it cites (Andreeva et al., 2020), which links lower capital ratios to higher funding costs, bond yields and CDS spreads (p. 7). It does not estimate ξ from bank data or match it to any moment.

**(b) Costs of issuing equity.** There are no fixed or proportional issuance costs. Issuance is either free (benchmark Sections 3–5: "Banks can always go to the market and raise new equity", p. 14) or ruled out by the constraint d ≥ 0 (footnote 4, p. 8; Section 6, pp. 21–28). There are no lumpy issues and nothing is scaled by issue size. The only cost of equity is the funding premium ρ − i^D > 0 on the stock of equity (pp. 10–11). ρ = 0.08 comes from cost-of-equity estimates, and i^D = 0.02 is empirical (Table 1, p. 13).

**(c) Both frictions, interaction, comparative statics, identification.**
- *Both present?* Yes in Section 6: a stigma cost below the CBR together with an issuance constraint d ≥ 0 (pp. 21–28). The issuance friction is a constraint, not a cost parameter, so there is **no issuance-cost parameter on which to run comparative statics**.
- *Interaction:* this is the paper's main subject. Once the issuance constraint binds, a bank that meets the CBR must deleverage. The trade-off is stigma against forgone "excess profit" on the lost loans (eqs. 9–10, pp. 21–25). The aggregate-stigma threshold only starts to rise once *using* banks also become equity-constrained (p. 24).
- *Comparative statics reported:* (i) voluntary buffers and the failure threshold rise with the liquidation cost Δ, while PD falls with Δ (Figure 5, p. 18). (ii) The minimum stigma needed to meet the CBR falls with current credit risk (p. 20) and is somewhat higher with more market power (pp. 20–21). (iii) The minimum individual stigma rises with deleveraging pressure, and market power barely matters for it (pp. 26–27). (iv) Higher stigma does **not** move the capital ratio smoothly. The response is a **switch**: below the threshold the bank sits at its interior use ratio CR^U < R^cbr, and above it the bank sits at CR = R^cbr exactly (Props. 3, 6; pp. 18, 28). The paper reports **no comparative statics of payout or issuance in ξ**, and none in an issuance cost, which the model does not have. Payout is the residual d = E + π − E′, and it is zero when the constraint binds (p. 42).
- *Telling frictions apart / identification:* the paper does **not** ask whether stigma and issuance frictions can be told apart from policy or data, and it **estimates neither**. The calibrated parameters (Table 1, p. 13) come from Lang and Menno (2025). Exceptions: ϵ = 8 comes from a regression of loan growth on the lending spread (Figure 3, p. 13), and μ = 100 targets a price-to-book ratio of 1.2. Δ and ξ are ad hoc.

**Buffers above the requirement / variance of capital.** Voluntary buffers above R^min arise only from costly liquidation. They are 1–2 pp and pro-cyclical, rising with credit risk (pp. 16–18). With a CBR and enough stigma, the bank holds **no voluntary buffer above R^cbr** (pp. 18, 28). Adding the CBR raises capital ratios by 0.5–1.3 pp, lowers PDs by 1.25–2 pp and lowers loans by 1–13 bps (p. 21). The paper has nothing on the variance or volatility of capital, since it has two periods and a representative bank.

**Stated limitations and future research.** The model is restricted to two periods for analytical tractability (p. 5). Bank heterogeneity and mixed-strategy equilibria are left for future research (p. 12). Limited liability is not modelled, because the calibration keeps net worth positive (footnote 10, p. 13). The paper offers only conjectures on releasable buffers (p. 29). Its own words on policy are that "further analysis regarding this question would be needed" (p. 7) and "further analysis of this would be needed to draw firm conclusions" (p. 29). The optimal composition of the capital stack is named as future research (p. 31).

### 6. Verdict for the theory paper

Lang and Menno pair an asymmetric cost of being below a buffer requirement with an equity-raising friction. But their stigma cost is a fixed charge per unit of loans, ξL′, triggered by an indicator, not one that grows with the shortfall (eq. 1, p. 10; p. 9). Their issuance friction is a hard non-negativity constraint on dividends with no fixed or proportional cost (footnote 4, p. 8). The paper does NOT have a graduated shortfall penalty like (λ_S − 1)(R − x)^+, and it does NOT have a fixed issuance cost K. So it reports no comparative statics of a dividend barrier, recapitalisation trigger or target in either friction (pp. 18–28). It does NOT ask whether stigma and issuance frictions can be told apart from the policy, and it estimates neither: ξ is set ad hoc, and the paper says its specific values do not affect the results (pp. 12–13). What carries over is the qualitative point that a buffer-breach cost acts as a threshold. With this step-function form, even 0.5–3 bps makes the bank sit exactly at the CBR with no buffer above it (Props. 3 and 6, pp. 18, 28). That contrasts with the interior buffer that a graduated cost produces, which can be cited as a reason for the graduated form. Its voluntary buffers above the minimum come only from a proportional liquidation cost and are 1–2 pp (pp. 16–18). The two-period design rules out any statement about the variance of capital (pp. 5, 8).
