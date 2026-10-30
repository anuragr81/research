# Reconstruction: Alvarez, Le Bihan and Lippi (2014)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/alvarez_lebihan_lippi_2014.pdf`, sha256 `7c817424737889c9ceb2ba9b674e6087f59c98f3e172cf18815635e0a339e0ae`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** Fernando Alvarez, Hervé Le Bihan, and Francesco Lippi, "Small and Large Price Changes and the Propagation of Monetary Shocks", NBER Working Paper No. 20155, May 2014, JEL No. E3, National Bureau of Economic Research, 1050 Massachusetts Avenue, Cambridge, MA 02138, http://www.nber.org/papers/w20155 (PDF pp. 1–2). The abstract page refers to an online appendix at http://www.nber.org/data-appendix/w20155.
- **Version.** This file is the **NBER Working Paper 20155 (May 2014)**, not the Banque de France working paper. The theory paper's notes list this paper as "Banque de France WP 492". The PDF contains no Banque de France series number, and the string "492" does not appear anywhere in its text layer. Banque de France appears only in the acknowledgements (Fondation Banque de France support, and a disclaimer that the views are not the Banque's) and in Le Bihan's affiliation (PDF pp. 1–2). I cannot confirm from this file that WP 492 has the same content.
- **Identification section?** There is **no section or subsection titled "Identification"**. The identification material is spread over: the introduction (printed pp. 3–5); Propositions 1, 3, 4 and 5 (pp. 20–26); an untitled paragraph headed "The map between the fundamental parameters and observables" (p. 25); and Section 3.3 "On the implied cost of price adjustment" (pp. 25–28). The word "identification" appears in the sense of identifying parameters only on pp. 2 and 5, and "identified" on p. 37.
- **File.** `/root/.cache/wealth_garch/alvarez_lebihan_lippi_2014.pdf`
- **PDF pages.** 49 (pdfinfo).
- **Page mapping.** PDF p. 1 is the NBER cover and acknowledgements page. PDF p. 2 is the abstract page. Neither has a printed number. Printed page 1 (Introduction) is PDF p. 3, so **printed page = PDF page − 2**. The last page, printed 47 (end of the proofs of Propositions 8 and 9), is PDF p. 49.
- **Coverage.** Read in full: all 49 PDF pages. That covers the text (printed pp. 1–37), the references (pp. 37–40) and Appendix A "Proofs" (pp. 41–47). **Appendices B to S are not in this file.** The text cites them, but they belong to the separate online appendix: B (data), C, D, E (labelled "Online Appendix E" on p. 41), F, G, H, J, K, M, N, O, P, Q, R and S.
- **Symbols.** The text layer drops Greek letters, and it prints ∞ as "1". I recovered the symbols from page images of PDF pp. 23, 26 and 28 (printed pp. 21, 24 and 26): ψ is the menu cost, λ the arrival rate of free adjustments, σ² the variance of shocks, ℓ the fraction of free adjustments, φ ≡ λȳ/(nσ²), and Ψ(n, ℓ) the menu-cost function in eq. (11). The Taylor/Reis special case is n = ∞ (p. 24, image-checked), even though the text layer prints "n = 1" there and on p. 4.

## 2. Reconstruction

**Empirical part (Section 2, pp. 6–15).**
- **French data.** INSEE monthly CPI micro price records, 2003:4–2011:4, about 65% of CPI weights and about 11 million quotes. There are about 1.5 million non-zero price changes (1.1 million excluding sales). Price changes are standardised within about 1,500 cells (6-digit COICOP category × outlet type), pp. 7–8.
- **Moments of the French data** (Table 1, p. 10): frequency 17.1% per month; standard deviation of price changes 16.6%; kurtosis 12.8 raw and 8.9 standardised; E|z| = 0.70.
- **Measurement error** (Section 2.2, pp. 11–12). Spurious small changes enter as a mixture with weight 1 − ζ. As their size goes to zero, the observed kurtosis tends to k_u/ζ. Comparing CPI hypermarket and electronics data with Billion Prices Project scraped data (Table 2, p. 13) gives ζ ≈ 0.5, so the corrected French kurtosis lies between 4 and 5 (p. 12).
- **US comparison** (Section 2.3, pp. 13–15) uses Klenow–Kryvtsov CPI data, EJR scanner data, Symphony-IRI scanner data and Midrigan's figures. The conclusion: the standardised kurtosis is about 4 for the US and about 5 for France (p. 15).

**Model primitives (Section 3, pp. 16–25).**
- **n = 1 case** (Section 3.1, pp. 16–18).
  - The profit-maximising price follows dp* = σ dW with no drift. The price gap p = P − p* causes a flow loss B p².
  - A deliberate price change costs a **fixed menu cost ψ**.
  - Free adjustment opportunities arrive at Poisson rate λ.
  - Bellman equation in the inaction region (−p̄, p̄): r V(p) = B p² + λ[V(0) − V(p)] + (σ²/2) V''(p), with value matching V(p̄) = V(0) + ψ and smooth pasting V'(p̄) = 0 (p. 17).
  - The value function and the threshold depend on λ + r only, not on each separately (p. 17). For small ψ/B, p̄ ≈ (6σ²ψ/B)^{1/4} (p. 17).
  - The fraction of free adjustments ℓ = λ/N(Δp_i) depends only on φ = λp̄²/σ² (p. 17).
  - The distribution of price changes has mass (1 − ℓ) at ±p̄ and density ℓ g(p) inside, where g is eq. (2) (p. 18).
- **Multi-product firm** (Section 3.2, pp. 18–25).
  - A firm sells n goods with independent Brownian price gaps, each with variance σ². One fixed cost ψ resets all n prices, and a free opportunity also resets all n.
  - The state is y = ||p||², with dy = nσ² dt + 2σ√y dW (p. 19).
  - Bellman equation (4): r v(y) = B y + λ[v(0) − v(y)] + nσ² v'(y) + 2σ² y v''(y) on (0, ȳ), with v(0) + ψ = v(ȳ) and v'(ȳ) = 0 (p. 20).
- **Frictions in the sense of (a)/(b).** There is no shortfall or threshold penalty and no issuance cost. The only friction is the fixed adjustment cost ψ per (bundle) adjustment, with a random zero-cost opportunity at rate λ. The flow loss B‖p‖² is **symmetric**: positive and negative gaps are penalised equally, and there is no requirement level.

**Main results.**
- **Prop. 1, eq. (3), p. 20.** N(Δp_i)·Var(Δp_i) = σ² for **any** finite stopping time that treats the n gaps symmetrically, not only the optimal one. The proof uses optional sampling (p. 41).
- **Lemma 1, p. 20.** ȳ(r, λ) = ȳ(r + λ, 0): the problem with free adjustments is the λ = 0 problem with discount rate r + λ.
- **Prop. 2, p. 21 (comparative statics of the threshold).**
  - As ψ → 0: ȳ ≈ √(2(n+2)σ²ψ/B), the quartic-root rule. It rises with σ² (option value) and is insensitive to λ.
  - As ψ → ∞: ȳ ≈ (ψ/B)(r + λ), a square-root rule in the gap. It does **not** depend on σ and is increasing in λ.
- **Prop. 3, eq. (5), p. 21.** ℓ = λ/N(Δp_i) = L(φ, n) with φ ≡ λȳ/(nσ²). It is increasing in φ, and "for a given n there is a one to one and onto mapping" between φ and ℓ.
- **Invariant density and price-change distribution.** The invariant density f(y) is eq. (6), in Bessel functions, with power series in eq. (27) (pp. 22, 46). The price-change density w(Δp_i) is eq. (8), a mixture of ω(Δp_i; y) from eq. (7) (p. 22).
- **Prop. 4, eq. (9), p. 23.** The shape of the distribution of price changes depends only on (n, ℓ); the density is homogeneous of degree −1 in (Δp_i, Std).
- **Eq. (10), p. 24.** Var and Kur as mixtures over the invariant density f(y). A closed form for Kur is eq. (33) in Appendix D, which is in the online appendix (p. 24). Kur > 3n/(n+2), and it lies between 1 and 6. Benchmarks: Golosov–Lucas (n = 1, ℓ = 0) gives kurtosis 1; Taylor/Reis (n = ∞, ℓ = 0) gives 3; Calvo (ℓ → 1, any n) gives 6 (p. 24).
- **Prop. 5, eqs. (11)–(13), p. 26.**
  - Fix n ≥ 1 and let r ↓ 0. Then there is a unique triplet (σ², λ, ψ) for any ℓ ∈ [0, 1], Var > 0, N > 0, and ψ/n = B·[Var/N]·Ψ(n, ℓ).
  - Limits: Ψ(n, 0) = n/(2(n+2)); Ψ → ∞ as ℓ → 1; Ψ(1 − ℓ) → 0.
  - Eq. (14), p. 28: the yearly cost of adjustment as a share of revenue is (1/2)(Var/m)(1 − ℓ)Ψ(n, ℓ).
- **Props. 6–7, eqs. (20)–(22), pp. 31–32.** The cumulative output effect scales as Std/N times a function of (n, ℓ). For small shocks, M ≈ δ·Kur/(6εN). The proof of Prop. 7 for 0 < ℓ < 1 is a **numerical** check of the equality in eq. (29), using the Matlab file solveMp0.m (p. 46).
- **Prop. 8, p. 35.** Steady-state inflation has zero first-order effect on Kur, N and M'(0).
- **Prop. 9, p. 36.** The minimum shock for full flexibility is 2·Std·√(L⁻¹(ℓ; n)/ℓ). It is increasing in ℓ.

### The identification question (what the theory paper needs)

**Parameters.** The model has four fundamental parameters: the menu cost relative to the curvature of profits, **ψ/B**; the shock variance **σ²**; the number of products **n**; and the arrival rate of free adjustments **λ** (pp. 3, 25). B itself is **not** identified from price data. It is calibrated from markups through B = (1 + m)/(2m²) (p. 26).

**From which moments, and by which results.**
1. **σ² from N and Var.** Prop. 1, eq. (3), p. 20: σ² = N(Δp_i)·Var(Δp_i). This is **model-free with respect to the policy**: "equation (3) holds for any stopping rule, not just for the optimal one" (p. 20).
2. **λ from ℓ and N.** λ = ℓ·N(Δp_i) by definition (Prop. 3, eq. (5), p. 21).
3. **ψ/B from N, Var, n and ℓ.** Prop. 5, eq. (11), p. 26: ψ/n = B·(Var/N)·Ψ(n, ℓ), with a unique (σ², λ, ψ) for each (ℓ, Var, N) once n is fixed and r ↓ 0. The proof works by total differentiation of the threshold equation and uses elasticity relations from Alvarez and Lippi (2014): the elasticities of ȳ with respect to ψ/B, σ² and λ are tied together, and this gives a menu cost proportional to Var/N (p. 43).
4. **(n, ℓ) from the shape of the distribution of price changes.** Prop. 4, eq. (9), p. 23: the shape depends only on (n, ℓ). Kurtosis (eq. (10), p. 24) is a function of (n, ℓ) only, increasing in both. The authors add the **direct evidence on the costs of price adjustment**, using eqs. (11) and (14) with the Levy et al. (1997) and Dutta et al. (1999) figure of about 0.7% of revenues (pp. 27–28), and the **qualitative shape** (no mass at or near the thresholds requires n ≥ 6, p. 16; n ≥ 3 removes the U shape, p. 19). The summary appears on p. 4 and p. 25.

**Global or local; analytic or numerical.**
- The mapping (ψ/B, λ) ↔ (N, Var) given (n, ℓ) is **global** and **analytic**: "one-to-one and onto" for φ ↔ ℓ (Prop. 3, p. 21), and a "unique triplet" for every ℓ ∈ [0, 1], Var > 0, N > 0 (Prop. 5, p. 26). There is no Jacobian or rank argument, and no local identification statement anywhere.
- The claim that kurtosis is increasing in both n and ℓ is **not a proposition**. It is stated "as can be seen in Figure 5" (p. 24), so it rests on numerical evaluation. The proportionality of M to Kur/N in Prop. 7 is checked numerically for 0 < ℓ < 1 (Lemma 4, p. 46).

**Non-identification statements.**
- **(n, ℓ) are not separately pinned down by kurtosis.** Footnote 3 (p. 4) says a given kurtosis can be produced by different combinations of n and ℓ. The authors prefer high n on cross-sectional-shape grounds, because high n removes the mass of large price changes.
- **Kurtosis is invariant to (ψ/B, λ, σ²) once (ℓ, n) are fixed** (p. 24). N has no independent effect on the shape once ℓ is held constant (p. 23).
- **λ and r enter the decision rule only through λ + r** (p. 17; Lemma 1, p. 20), so the threshold alone cannot tell the discount rate from the free-adjustment rate. The authors set r ↓ 0 in Prop. 5 (p. 26).
- **Threshold insensitivities** (Prop. 2, p. 21): for small ψ the threshold does not respond to λ, and for large ψ it does not respond to σ.
- **Models where the distribution is not informative.** With no menu cost, or with fat-tailed infrequent shocks, the price-change distribution is not informative enough, and "additional information is needed to identify the parameters" (p. 2) or "more observations" are required (p. 37).

**Same versus opposite direction.** The paper never discusses this explicitly. The relevant facts are:
- Kurtosis rises in both n and ℓ (same direction, p. 24). This is why one moment cannot separate them (fn. 3, p. 4).
- The second moment they use, the implied menu cost Ψ(n, ℓ)/n, is increasing in ℓ for fixed n. In n it is increasing for very small ℓ and reversed for larger ℓ (p. 27).
- The yearly cost (1 − ℓ)Ψ(n, ℓ) falls to zero as ℓ → 1 (p. 27).
- They use the two pieces of information together to settle on n = 10 and ℓ ≈ 60% (p. 28), but they never state a rank or opposite-sign condition (see D3).

**What is estimated, from what data.** No structural estimation is performed. The paper:
- **estimates descriptive moments** (frequency, standard deviation, kurtosis, fractions of small and large changes) from French CPI micro data (INSEE 2003–2011), Billion Prices Project scraped data for French retailers, US CPI histograms (Klenow–Kryvtsov), and US scanner data (EJR, Symphony-IRI, Midrigan). See Tables 1–3, pp. 10–15.
- **estimates the measurement-error share** ζ ≈ 0.5 from CPI-versus-BPP kurtosis ratios (p. 12).
- **calibrates an illustration**: N = 1.5 per year, Std = 0.10, markup 25% (B = 10), and n = 10. Under these values, the model matches the observed yearly adjustment cost of 0.7% of revenues at ℓ ≈ 60%, which implies that one deliberate adjustment costs about 5% of profits (p. 28).

## 3. Answers to (a), (b), (c)

- **(a) Cost of being below a capital requirement or threshold. No.** The paper is about price setting, not banks. The only state penalty is the symmetric quadratic flow loss B p² (n = 1, p. 16) or B y with y = ||p||² (eq. (4), p. 20). It is charged equally for positive and negative gaps, and there is no requirement level and no asymmetry. The firm bears it as lost profit.
- **(b) Cost of issuing equity. No.** There is no financing. The analogue is the **fixed menu cost ψ per adjustment**. It is lump-sum, independent of the size of the change, paid per adjustment event, and in the multi-product case covers all n prices at once (pp. 16, 19–20). A Poisson opportunity sets the cost to zero (p. 16; pp. 29, 36 for the general equilibrium and CalvoPlus variants). Appendix S, which is not in this file, considers a cheaper rather than free random cost (p. 36). Adjustments are lumpy: all gaps are closed to zero.
- **(c) Both, interaction, comparative statics and identification. No banking content**, but the identification analysis is the paper's core and is summarised in Section 2 above.
  - **Threshold comparative statics** (Prop. 2, p. 21): ȳ rises in ψ (quartic root for small ψ, linear in ψ for large ψ); rises in σ² for small ψ, with no effect for large ψ; and rises in λ for large ψ, with no effect for small ψ.
  - **Identification**: σ² from N·Var (Prop. 1, p. 20); (ψ/B, λ) from (N, Var) given (n, ℓ), uniquely (Prop. 5, p. 26); (n, ℓ) from kurtosis plus menu-cost evidence and shape (pp. 4, 24–28). Kurtosis alone does not separate n from ℓ (fn. 3, p. 4).
  - **Estimation and calibration**: only descriptive moments are estimated, and the illustration in the last bullet of Section 2 is a calibration (p. 28).
- **Buffers or variance of capital.** Nothing on capital. The closest analogue is that the variance of adjustments times their frequency equals σ² under any symmetric stopping rule (Prop. 1, p. 20), and that the variance of price changes is given in closed form in eq. (10) (p. 24).
- **Stated limitations and future research** (p. 37):
  - Fat-tailed infrequent shocks break the sufficient-statistic result, and such models need more observations, such as the shape of cost shocks; this is left for future work.
  - Correlated cost shocks across a firm's goods are left for the future.
  - The model assumes zero inflation, but small inflation has only second-order effects (Prop. 8, p. 35).
  - General equilibrium feedback is ignored and is argued to be second order (p. 30).

## 6. Verdict for the theory paper

The paper has neither a shortfall penalty nor an issuance cost: its only frictions are a symmetric quadratic flow loss and a fixed menu cost ψ with random free adjustments (pp. 16, 20). It therefore says nothing directly about pairing a graduated shortfall cost with a fixed issuance cost. It is a real precedent for identifying the parameters of an inaction model from features of the observed policy. ψ/B, σ² and λ are identified globally and analytically from the frequency and variance of adjustments, given (n, ℓ) (Props. 1, 3 and 5, pp. 20–26), and (n, ℓ) are disciplined by kurtosis plus outside evidence on adjustment costs (pp. 4, 25–28). It identifies from moments of the distribution of adjustments, not from thresholds, and it uses no Jacobian or local-identification argument (pp. 20–26). It explicitly concedes that kurtosis cannot separate n from ℓ, because both raise it (fn. 3, p. 4; p. 24), and that the threshold depends on λ and r only through λ + r (p. 17; Lemma 1, p. 20). It never discusses parameters that move a moment in opposite directions, so it cannot be cited for the theory paper's opposite-sign argument. It can be cited only as an example of the same-direction problem, solved by adding a second moment (pp. 27–28).
