# Reconstruction: Constantinides and Richard (1978)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/constantinides_richard_1978.pdf`, sha256 `6194bc222dfdaf00a6162251ede6b7d74441681cd3762dd54dee814b0b8a5c36`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed.** George M. Constantinides and Scott F. Richard (Carnegie-Mellon University, Pittsburgh, Pennsylvania), "Existence of Optimal Simple Policies for Discounted-Cost Inventory and Cash Management in Continuous Time", *Operations Research*, Vol. 26, No. 4, July-August 1978, pp. 620–636. © 1978 Operations Research Society of America. Received May 1976; accepted October 1977.
- **File.** `/root/.cache/wealth_garch/constantinides_richard_1978.pdf`. This is a scan with no text layer. It was read from the OCR files `/root/.cache/wealth_garch/constantinides_richard_1978_ocr/page-001.txt` to `page-018.txt`.
- **PDF pages.** 18 (pdfinfo).
- **Page mapping.** PDF p. 1 is the JSTOR cover. Printed pp. 620–636 are PDF pp. 2–18, so **printed page = PDF page + 618**.
- **Coverage.** Read in full: all 18 OCR pages, including the Appendix (printed pp. 631–635) and the references (p. 636).
  - The OCR drops some lines, e.g. at the foot of p. 620 and in the middle of pp. 622, 623, 629 and 631, and garbles formulas.
  - To recover the missing lines and the formulas, page images (rendered at 100 dpi) were checked for printed pp. 620, 622, 623, 626, 627, 628, 629, 630 and 631.
- **Quotations.** Quotations are copied verbatim from the OCR file of their page and checked by whitespace-normalised substring match.

## 2. Reconstruction

**State and demand (pp. 622–623).** x(t) is the cash balance. Between interventions, dx = −μ dt − σ dw: cumulative demand for cash is a Wiener process with drift μ and variance σ² per unit time. At stopping times τ_i the controller applies impulses φ_i: x(τ_i) = x(τ_i⁻) + φ_i. Holding, penalty and transaction costs are *not* charged to the cash balance (p. 623).

**Frictions.**
- **Holding-penalty cost rate (p. 622):** C(x) = max[hx, −px] with h, p > 0. This is a running cost, piecewise linear with a kink at zero. Holding cost h per unit of positive balance; penalty p per unit of negative balance (shortfall). The slopes can differ: **asymmetric** if h ≠ p, and symmetry is explicitly not assumed (p. 621). In the inventory reinterpretation, h and p are holding and backlogging cost rates (p. 629).
- **Transfer cost (p. 622):**
  - B(x1 − x0) = K⁺ + (x1 − x0)k⁺ for x0 ≤ x1;
  - B(x1 − x0) = K⁻ + (x0 − x1)k⁻ for x1 < x0;
  - k⁺, k⁻, K⁺, K⁻ > 0.
  - So there is a **fixed cost per transfer** (K⁺ up, K⁻ down) **plus a proportional cost per unit** (k⁺, k⁻). Even a zero control incurs B(0) = K⁺ (p. 622).
- **Objective (p. 624):** minimise J_x(v) = E[Σ_i e^{−βτ_i} B(φ_i) + ∫ e^{−βs} C(x(s)) ds] with discount rate β.

**Solution method (pp. 624–626).** Impulse control (Bensoussan–Lions, extended by Richard [16], p. 622).
- Quasi-variational inequalities: V ≤ QV (1), βV + μV′ − ½σ²V″ − C ≤ 0 (2), complementarity (3), and regularity (4) (p. 625).
- Lemma 1 (p. 625): a V satisfying (1)–(4) yields an optimal impulse control. The proof is in [16].

**Simple policy (p. 626, eq. (5)).** Parameters d ≤ D ≤ U ≤ u:
- if x ≤ d, jump to D;
- if d < x < u, do nothing;
- if x ≥ u, jump to U.

Boundary conditions (p. 626):
- value matching (6) V(d) = V(D) + K⁺ + k⁺(D − d) and (7) V(u) = V(U) + K⁻ + k⁻(u − U);
- optimal targets (8) V′(D) + k⁺ = 0 and (9) V′(U) − k⁻ = 0;
- smooth pasting at the triggers (10) V′(d) + k⁺ = 0 and (11) V′(u) − k⁻ = 0;
- inside (d, u), the ODE (12) βV + μV′ − ½σ²V″ = C(x).

**Controllability condition (p. 627, eq. (13)).** h − βk⁻ > 0 and p − βk⁺ > 0. If p/β ≤ k⁺ (perpetuity of the penalty no larger than the proportional cost of raising cash), it is never optimal to increase cash. Symmetrically, if h/β ≤ k⁻, it is never optimal to decrease cash.

**Main results.**
- **Lemma 2 and Theorem 1 (p. 627).** Under (13) a C² solution of (12) with (4) and (6)–(11) always exists. The optimal policy exists and is simple, given by (5).
  - The value function is (14), with roots λ1, λ2 in (15).
  - Figures 1–2 (p. 628) show V′ and V. The fixed costs K⁺ and K⁻ equal the shaded areas between V′ and the levels −k⁺ and k⁻ (p. 635).
  - Verification is on pp. 628–629, existence of (c1, c2) in the Appendix (pp. 631–635), and an intermediate-value argument on p. 635.
- **Theorem 2 (p. 630), inventory.** The case where it is never optimal to decrease cash (ξ ≥ 0, cost K + kξ). If p − βk > 0, an optimal (S, s) policy exists ((16)–(19)).
- **Other cases (p. 630).** If h/β − k⁻ < 0, the model reduces to inventory. If p/β − k⁺ < 0 with h/β − k⁻ > 0, it is an "inverse inventory system". If both are negative, the system is never controlled.

**Explicit solutions and comparative statics (Section 4, pp. 630–631).**
- **Symmetric case, no fixed costs.** Assumptions μ = 0, K⁺ = K⁻ = 0, k⁺ = k⁻ = k, h = p. Result: u = U = −d = −D = ln[(h/β − {k(2h/β − k)}^{1/2})/(h/β + {k(2h/β − k)}^{1/2})] = (kσ²/h){1 − kβ/4h + O(β)} (p. 630). These are reflecting boundaries. They lie farther from the origin the higher k, the higher σ², the lower the holding cost and the lower β. With k = 0 the balance stays at zero (p. 631).
- **Asymmetric case with fixed and proportional costs.** Assumptions μ = 0, β → 0, D < 0 < U (p. 631):
  - h(u + U) = −p(d + D);
  - huU + pdD = σ²(k⁺ + k⁻);
  - (h/2σ²)(u − U)³ = K⁻;
  - (p/2σ²)(D − d)³ = K⁺.
- **Solution:**
  - U = γ/h − h⁻¹(3h²σ²K⁻/4)^{1/3} and u = γ/h + h⁻¹(3h²σ²K⁻/4)^{1/3};
  - D = −γ/p + p⁻¹(3p²σ²K⁺/4)^{1/3} and d = −γ/p − p⁻¹(3p²σ²K⁺/4)^{1/3};
  - γ² = [hpσ²(k⁺ + k⁻) + p(3h²σ²K⁻/4)^{2/3} + h(3p²σ²K⁺/4)^{2/3}]/(h + p), where γ is the average cost rate.
  - The paper credits these equations to its reference [6] (Constantinides 1976), "where these equations were first derived" (p. 631).
- **Stated comparative statics (p. 631):**
  - γ is increasing in all cost parameters and in σ²;
  - **u is increasing in p, K⁺, K⁻, k⁺, k⁻ and σ²**;
  - **U is increasing in K⁺, k⁺, k⁻ and p**;
  - u − U is increasing in σ² and K⁻, decreasing in h, and independent of the other parameters.
  - No directions are stated for d or D.
- **Caveats (p. 631).** The β → 0 limit "may be questioned" because the value function diverges, so it is treated as an approximation for small β. With μ = 0, the σ → 0 limit is degenerate.

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a threshold.**
- Yes. There is a **running penalty cost that grows linearly with the shortfall** below zero: C(x) = −px for x < 0 (p. 622).
- It is **asymmetric** in the sense that it applies only below the threshold, with its own slope p. A separate linear holding cost h applies above zero, and h ≠ p is allowed (pp. 621–622).
- The threshold is zero cash, not a regulatory requirement. There is no liquidation, closure, fine or stigma.
- The cost is borne by the cash manager or firm, the single decision maker. It is not charged to the cash balance (p. 623).
- In the inventory reading, p is a backlogging cost (p. 629).

**(b) Costs of transfers (the analogue of issuance).**
- There are **both a fixed cost per transfer and a proportional cost per unit transferred**, separately for increases (K⁺, k⁺) and decreases (K⁻, k⁻) (p. 622).
- Transfers are **lumpy impulses** at stopping times (pp. 622–623). An increase from d to D is the analogue of a recapitalisation from a trigger to a target.
- The fixed costs are per transfer, not scaled by size. The proportional costs are per unit.

**(c) Both frictions, interaction, comparative statics, identification.**
- Yes, both frictions are present: a linear asymmetric shortfall penalty p, and fixed-plus-proportional transfer costs (abstract, p. 620; p. 622).
- The general results are existence and form only (Theorems 1–2, pp. 627, 630). No general comparative statics are given.
- Comparative statics appear only in the special cases of Section 4 (pp. 630–631):
  - **u (the upper trigger) increases in both the penalty p and the fixed cost of raising cash K⁺**, as well as in K⁻, k⁺, k⁻ and σ²;
  - U increases in K⁺, k⁺, k⁻ and p;
  - u − U depends only on σ², K⁻ and h;
  - the symmetric no-fixed-cost case: the boundaries widen with k and σ² and narrow with h and β.
- **No statement is made on how d (the lower trigger) or D (the lower target) move** with p or K⁺.
- The paper does not ask whether the frictions can be told apart from the policy. It does not identify, estimate or calibrate any parameter, and has no data.

**Buffers and variance.** Nothing on the variance or distribution of the cash balance, and no cap on exposure.

**Stated limitations.**
- Explicit solutions are available only in special cases (μ = 0, β → 0, or symmetry with zero fixed costs) (pp. 630–631).
- The β → 0 limit is only an approximation (p. 631).
- With μ = 0, no implications can be drawn for a deterministic system (p. 631).

No future-research section.

## 6. Verdict for the theory paper

- C&R is the closest primitive structure to the theory paper's two frictions: a linear running penalty below a threshold (C(x) = −px for x < 0, asymmetric to the holding slope h) together with a fixed-plus-proportional cost per transfer (K⁺ + k⁺ξ) (p. 622). They prove an optimal simple trigger-target policy (d, D, U, u) always exists under these costs (Theorem 1, p. 627).
- Its comparative statics are confined to the μ = 0, β → 0 approximation (p. 631). There, the upper trigger u increases in both the penalty p and the fixed cost K⁺, paralleling the theory paper's dividend-barrier result.
- The paper states **no** direction for the lower trigger d or the target D. Any claim that d falls in K⁺ (D4) or that D − d separates the frictions (D5) is my derivation from printed formulas, not the authors'.
- C&R do **not** ask whether the penalty and the fixed cost are separately identified by the thresholds. They estimate nothing, and say nothing about the variance of the balance or about an exposure cap (pp. 620–636).
