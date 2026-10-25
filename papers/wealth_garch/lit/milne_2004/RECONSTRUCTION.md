# Reconstruction: Milne (2004)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/milne_2004.pdf`, sha256 `8ee31b60443a236414c49d43299c3d75e547925a8b62763cddf21fff8dc5a8d6`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** Alistair Milne (Faculty of Finance, Cass Business School, City University, London), "The inventory perspective on bank capital", August 2004. JEL G21. The footer reads "Electronic copy available at: https://ssrn.com/abstract=576062" (SSRN 576062). The title page (PDF p. 1) says: "This paper, and its companion Milne and Whalley (2004), have developed from a single original working paper (Milne and Whalley (1999))."
- **File.** `/root/.cache/wealth_garch/milne_2004.pdf`
- **PDF pages.** 43 (pdfinfo; A4; dvips/Ghostscript, text layer present).
- **Page mapping.** **Printed page = PDF page (offset 0).** PDF p. 1 is the title page and abstract and has no printed number. Printed pages 2 to 43 are PDF pp. 2 to 43. The body runs to p. 35, the Appendix to pp. 36–41 and the references to pp. 42–43.
- **Coverage.** Read in full: all 43 PDF pages, including the Appendix (A.1 proofs, A.2 numerical solution) and the references. Greek letters come through in the text layer (χ, ρ, σ, φ, θ, υ). Some figure text is garbled: in the Figure 1 legend "ρ" shows as "Ρ". On p. 39 the boundary conditions print as "(V (C ∗ ) = 1, V = (C ∗ ))", which from p. 16 means V_C(C\*) = 1 and V_CC(C\*) = 0.

## 2. Reconstruction

**Primitives (Section 2, pp. 10–14).**
- A risk-neutral bank holds a fixed stock A of non-tradeable assets. They produce cash flow with expected value RA per unit time and variance ½σ²A² per unit time (p. 11).
- The bank is funded by fully insured deposits D at rate r_d and by book equity C = A − D. C can be negative (p. 12).
- Shareholders discount at ρ > r_d. Equity is "relatively expensive", and the paper leaves this unexplained (p. 13, fn. 15).
- Capital follows dC = ((R − r_d)A + r_d C)dt + σA dz − dΘ_t (eq. (1), p. 12). Θ is cumulated dividends.
- There is one agent with choices, the bank (shareholder-value maximising, with no manager–owner conflict, fn. 5, p. 6). The regulator guarantees deposits and enforces the minimum C̄ (pp. 6, 12).

**Frictions.**
- **Section 2 (continuous observation).** "Regulators continuously observe bank capital C prohibiting any reduction of C below a required regulatory minimum C̄" (p. 12). At C = C̄ the bank must either recapitalise with a new issue κ = ΔC > 0 or liquidate. On liquidation shareholders receive nothing (p. 12). C̄ is therefore a hard absorbing or reflecting barrier, not a graduated cost.
- **Fixed issue cost χ** per issue, borne by shareholders (p. 12). It is "unrelated to the net worth of the bank or the amount of the capital issued" (p. 11). It is motivated by Myers–Majluf dilution (fn. 14, p. 12).
- **Objective (eq. (2), p. 13).** Γ_t = max E{∫ e^{−ρ(τ−t)} dΘ_τ − Σ_j e^{−ρ(τ_j−t)}(χ + κ(τ_j)) i(τ_j)}. The issued amount κ is charged at par. There is **no proportional issuance cost**: κ here is the amount issued, not a cost rate.
- **HJB (eq. (3), p. 14).** ρV = max_θ[θ + ((R − r_d)A + r_d C − θ)V_C + ½σ²A²V_CC]. This is linear in θ, so dividends follow barrier ("proportional") control (p. 36).
- **Value matching at an issue (eq. (4), p. 14).** V(C_t) = max V(C_t′) − (C_t′ − C_t + χ).

**Solution, Section 2 (Propositions 1–6, pp. 14–17; proofs pp. 36–38).**
- **Prop. 1 (p. 14).** Define F = ((R − r_d)A + r_d C̄)/ρ (eq. (5)). If F > 0, pay out C − C\* above a unique target C\* > C̄ and otherwise retain all earnings. If F ≤ 0, pay C − C̄ and liquidate at once.
- **Prop. 2 (eq. (6), p. 15).** V(C\*) = F + r_d(C\* − C̄)/ρ.
- **Prop. 3 (eqs. (7)–(8), p. 16).** V_C(C\*) = 1 and V_CC(C\*) = 0. These are the smooth-pasting and super-contact conditions, citing Dumas (1991) on p. 36.
- **Prop. 4 (p. 16).** For C̄ < C < C\*: V_C > 1, and the bank is averse to cash-flow risk with υ = −V_CC/V_C > 0, rising as C → C̄.
- **Prop. 5 (eq. (9), p. 17).** V(C̄) = max{0, V(C\*) − χ − (C\* − C̄)}. The bank liquidates if and only if V(C\*) − χ − (C\* − C̄) < 0.
- **Prop. 6 (eq. (10), p. 17).** ρV = ((R − r_d)A + r_d C)V_C + ½σ²A²V_CC on (C̄, C\*). It is solved with (7), (8) and (9); there is a closed form if r_d = 0 and a numerical solution otherwise (pp. 17, 40).
- Lemma A4 (p. 37): the bank never recapitalises or liquidates above C̄.
- **The policy is (s, S) with three points collapsed to two:**
  - issue point = C̄ (exogenous);
  - target after issue = C\* (eq. (9));
  - dividend barrier = C\*.

**Section 3 (endogenous lending, pp. 24–27, informal).**
- Loan demand is A(R) = A\* exp[−η(R − R\*)] (eq. (11)), with a fixed operating cost k.
- The loan rate is R = R\* + σ²A(R)υ (eq. (14)). Lending falls as capital falls below C\*.
- Desired capital is 1.6% of A\*, against 2.1% in the Figure 1 baseline (p. 27).

**Section 4 (random audit, pp. 27–32; model in Appendix pp. 38–39, informal).**
- Audits arrive as a Poisson process with intensity q, "as in Merton (1978)" (p. 28). Capital can fall below C̄ between audits.
- Paying dividends below C̄ ("looting") is illegal and can be recovered, except at a rate up to φ (p. 29).
- The bank chooses σ ∈ [σ1, σ2] (risk shifting, p. 29).
- Four regions, with ODEs on p. 39:
  - C ≤ Ĉ: loot at the maximal rate. The ODE is (ρ+q)V = −θ + …; the sign of θ is as printed.
  - Ĉ < C ≤ C̃: no dividends; **liquidate if audited**. The ODE is (ρ+q)V = drift and diffusion terms only, so the audit value is 0.
  - C̃ < C ≤ C̄: no dividends; **recapitalise if audited**. The ODE is (ρ+q)V = q[V(C\*) − χ − (C\* − C)] + ….
  - C̄ < C ≤ C\*: ρV = … as in Section 2, with barrier control at C\*.
- There are eleven unknowns and eleven boundary conditions (p. 39), including V(C\*) − χ − (C\* − C̃) = 0, which locates C̃.
- **Prop. 7 (p. 29).** Below C̄, V_CC > 0, so the bank is risk-loving and picks σ2. Above C̄ it picks σ1.
- **Prop. 8 (p. 30).** If C\* > C̄, there is a Ĉ < C̄ with V_C(Ĉ) = 1, and the bank loots at the maximal rate below it.
- **Prop. 9 (eq. (17), p. 30; r_d = 0).** Capital is retained (C\* > C̄) if and only if F > φ/(ρ+q) + sqrt((φ/(ρ+q))² + 2σ2²/q). Otherwise the bank pays down to C̄ and then loots.
- Figure 5 (pp. 31–32) shows the value function for q = 2, σ1 = 0.5%, σ2 = 2% and looting up to 6% of assets. It has multiple solutions of the optimality equation (p. 32).

**Calibration (pp. 17–18).**
- A = 1, R = 4%, σ = 1%, r_d = 2%, ρ = 5%, C̄ = 0 and χ = 4% of assets, so F = 0.4.
- The parameters are chosen so that "the banks will make an annual loss … approximately one year in 20" (p. 18).
- The figure captions print "χ = 1" (pp. 19, 26), which does not match the text's χ = 4%. The text on p. 21 also says "R = 0.4%" where the baseline is R = 4%.
- Nothing is estimated.

**Main numerical results (Section 2.3, pp. 18–24).**
- **Baseline.** The buffer C\* − C̄ is about 2% of assets, or two annual standard deviations (p. 18). V_C reaches about 10 at C̄ (p. 20).
- **Higher χ (4% → 20%).** C\* rises only modestly (p. 20).
- **Higher ρ.** The buffer falls (p. 20).
- **Lower R.** The buffer rises (p. 20).
- **Franchise value F.** C\* is non-monotonic (hump-shaped) in F (Figure 2, pp. 21–23).
- **Cash-flow volatility.** C\* is roughly proportional to σ (p. 23).
- **Capital requirement C̄.** The buffer C\* − C̄ is invariant to C̄ when r_d = 0. For r_d > 0 the effect is very small and negative at the baseline (Figure 3, pp. 23–24).

## 3. Answers to (a), (b), (c)

**(a) Cost of being below the requirement.**
- **Section 2.** No state below C̄ is allowed. At C̄ the bank must recapitalise (paying χ) or be liquidated, with shareholders getting nothing (p. 12; Prop. 5, p. 17). This is a hard barrier: liquidation or forced issue at the minimum.
- **Section 4.**
  - **Mechanism.** The bank can sit below C̄, and the cost is a Poisson audit with intensity q (p. 28). If audited, it must either recapitalise to C\* (paying χ plus the top-up C\* − C) on (C̃, C̄], or be liquidated (value 0) on (Ĉ, C̃] (p. 39). The paper calls this "the immediate threat of a regulatory audit and a consequent deadweight loss of recapitalization or liquidation" (p. 30).
  - **Restriction on dividends.** Dividends are also restricted below C̄: looting is illegal beyond the rate φ (p. 29).
  - **No running cost grows with the shortfall.** The only deadweight loss on audit is the fixed χ, or the loss of the bank. The top-up C\* − C is an injection at par, not a deadweight cost (pp. 13, 39).
  - **Who bears it, and is it asymmetric.** The cost is asymmetric: audit consequences apply only for C ≤ C̄ (p. 39). Shareholders bear it (pp. 12, 13).
  - **Intensity.** q is a constant intensity, independent of the size of the shortfall (pp. 28, 39).
- See D1 and D2.

**(b) Costs of issuing equity.**
- There is a **fixed cost χ per issue**, independent of the issue size and of net worth (pp. 11, 12–13). The issue amount is charged at par, and there is **no proportional issuance cost** (eq. (2), p. 13).
- Issues are **lumpy**: one jump from C̄ to C\* (eq. (9), p. 17), or from C to C\* on audit in Section 4 (p. 39).
- Equity also carries a holding cost through ρ > r_d (p. 13). Footnote 13 (p. 12) allows a rights issue or a cash acquisition by another bank as forms of "new issue".

**(c) Both frictions, interaction, comparative statics, identification.**
- **Both frictions.** Section 2 has the fixed cost χ with a hard minimum. Section 4 adds the random audit (q) below the minimum to χ, plus looting (φ) and risk shifting (σ1, σ2).
- **How they interact.**
  - The paper does not study the interaction of χ and q systematically.
  - The Section 4 numbers come from one example with q = 2 and looting and risk shifting all turned on at once (p. 31).
  - Section 4 is "informal" and "fully developed in Milne and Whalley (2004)" (pp. 27–28, fn. 18).
- **Comparative statics stated, with signs and pages:**

  | Threshold | Parameter | Sign | Page | Notes |
  |---|---|---|---|---|
  | Target / dividend barrier C\* | χ (4% → 20%) | Up, only a modest increase | p. 20 | Attributed to the thin tails of the normal density. Numerical (Figure 1, lower panel). |
  | Recapitalise-or-liquidate choice at C̄ | χ | High χ = 0.2 with F = 0.2 gives liquidation | p. 20 | From Prop. 5 (p. 17). The location of the issue point does not change; it stays at C̄. |
  | C\* | ρ | Down | p. 20 | |
  | C\* | R (through F) | Down when R rises at high F; up at low F (hump) | pp. 21–23 | |
  | C\* | σ | Roughly proportional | p. 23 | |
  | C\* − C̄ | C̄ | Zero if r_d = 0; small and negative at baseline; positive on the rising part of C\*(F) | pp. 23–24 | |
  | C\* (Section 3) | Endogenous lending | Lower, 1.6% vs 2.1% | p. 27 | |
  | C\* (Section 4) | Random audit q = 2, with looting and risk shifting at once | Lower, "around 2.5%" vs "around 4%" in the baseline | p. 31 | The 4% conflicts with "approximately 2%" on p. 18. See D4. |
  | Condition (17) for retaining capital | q | The gains from looting and risk shifting are higher the less frequent the audit; higher q makes retention easier | p. 31 | Only q → ∞ contains looting fully. |
  | Condition (17) | φ, σ2 | Higher φ or σ2 makes retention harder | p. 31 | |

- **No comparative static of the Section 4 thresholds.** No statement is made about how C̃ (the recapitalise-or-liquidate threshold on audit), Ĉ (the looting threshold) or C\* move with q or with χ separately.
- **Opposite-direction trigger effects.** No two frictions are shown to move the trigger in opposite directions. In Section 2 the issue point is pinned at C̄. In Section 4 the issue happens at the audit date, not at a chosen trigger, and no statics of C̃ are given (pp. 12, 31, 39).
- **Telling the frictions apart.** The paper never asks whether χ and q (or any other pair) can be told apart from the policy.
- **Estimation and calibration.** Nothing is estimated. The baseline is calibrated to a loss "one year in 20" (p. 18). Table 1 (p. 3) reports tier 1 ratios from BankScope descriptively. The fit to observed buffers is left as a question (p. 35); fn. 20 reports that Peura and Keppo (2004) find "a fairly good job".

**Relation to Milne and Whalley, as the paper itself states it.**
- The paper and "its companion Milne and Whalley (2004)" both grew from Milne and Whalley (1999) (p. 1).
- Section 2 generalises Milne and Robertson (1996) "(introducing an option to recapitalise)" (p. 2, fn. 2).
- "Sections 3 and 4 are extensions; the model of Section 4 is analyzed more fully in Milne and Whalley (2004)" (p. 2, fn. 2). Section 4 "is fully developed in Milne and Whalley (2004)" (p. 28, fn. 18). The Appendix refers to "Milne and Whalley (2002)" for details (p. 38); that version is not in the reference list.
- Milne and Whalley (1999) is described as "an early version of Milne and Whalley (2004), summarised in Section 4" (p. 2, fn. 1). They offer an exploratory steady-state distribution of capital (p. 35, fn. 22).
- The paper therefore presents Section 4 as a **summary** of the companion, not as new. It does not explicitly claim anything as new relative to Milne and Whalley. What it adds as written:
  - the continuously-observed Section 2 model with its numerical statics (Figures 1–3);
  - the endogenous-lending Section 3;
  - the empirical framing on excess capital (pp. 2–6).
- Its "new insights" (looting with insufficient franchise value; non-monotonic buffer in F) are claimed against "the standard static literature" (p. 8), not against Milne and Whalley. Footnote 8 sends readers to Milne and Whalley (2004) "for further discussion and development" of looting.
- The paper does not cite a 2001 version of Milne and Whalley.

**Capital buffers and the variance of capital.**
- The buffer above the minimum is the central object (pp. 2–6, 18, 33).
- The "trigger" and "target" ratios of UK supervision are discussed as regulatory, not model, objects (p. 5).
- There is no result on the variance or the stationary distribution of capital. The steady-state distribution is listed as future work (p. 35).

**Limitations and future research (pp. 7, 11, 24, 27, 35, 37).**
- Cash flows are not well approximated by diffusions (p. 7).
- There is no deterioration of expected cash flow and no non-performing loans (p. 11).
- Sections 3 and 4 are informal (pp. 24, 27).
- Lemma A1 needs positive expected cash flow (p. 37).
- Future work (p. 35):
  - test the size of buffers, the long-run versus short-run effects and the hump shape;
  - model the costs of recapitalisation more realistically;
  - add cyclical cash flows and requirements, and a loan maturity structure;
  - study the steady-state distribution and population dynamics of capital.

## 6. Verdict for the theory paper

- Milne (2004) pairs a **fixed cost of equity issue** χ per issue (pp. 11–13) with a cost of being below the minimum that is **either a hard barrier** (Section 2, continuous monitoring: recapitalise or be liquidated at C̄, p. 12) **or a Poisson audit** of intensity q that forces recapitalisation or liquidation (Section 4, pp. 28, 30, 39).
- It has **no running cost that grows with the shortfall**, no proportional issue cost, and the post-issue target equals the dividend barrier (eq. (9), p. 17).
- The only stated comparative statics in the frictions are:
  - C\* rises modestly in χ (p. 20);
  - the retention condition is easier to meet when audit is more frequent (p. 31);
  - one combined example where random audit with looting and risk shifting lowers C\* (pp. 31–32).
- There are no statics of the issue or liquidation thresholds C̃ and Ĉ in χ or q, and no case of two frictions moving the trigger in opposite directions.
- The paper does **not** ask whether the frictions can be told apart from the policy and estimates nothing: it calibrates one baseline (p. 18) and leaves empirical tests to future work (p. 35).
- By its own account, the random-audit part is a summary of Milne and Whalley (2004) (pp. 2, 28), so it adds nothing beyond the companion on the χ–q pairing.
