# Bohren, Hull and Imas (2024), reconstruction

J. Aislinn Bohren, Peter Hull and Alex Imas, "Systemic Discrimination: Theory
and Measurement", NBER Working Paper 29820, March 2022, revised December 2024.

## Source read

- The NBER file of the December 2024 revision, 66 PDF pages. PDF page 1 is the
  NBER cover, PDF page 2 the abstract page, and the printed page is the PDF
  page minus 2 from PDF page 3 on. Cached at
  `~/.cache/paper_a/sources/bohren_hull_imas_wp.pdf`, sha256
  `0973ae89…184f7`. Text layer at
  `~/.cache/paper_a/sources/bohren_hull_imas_wp.txt`, sha256
  `f8c579e0…d40c1`, with a 67th, empty page.
- Read in full on 16 October 2026 from the text layer, every page, so the
  evidence tag is [F]. Page images at 110 dpi were read for PDF pages 19, 27
  and 52. Figures 1 to 7 are images and their plotted values are not
  attributed.

## The framework, from its own primitives

1. **A decision** (PDF p. 12). A worker has unseen productivity `Y*`, group
   `G ∈ {b, w}` and an observed signal `S*`. A manager's action rule
   `A*(G, S*)` is taken in reduced form, without modelling her decision
   problem.
2. **A system** (PDF pp. 12–13). Nodes `n = 1, …, N` and the focal node `n*`,
   each with its own productivity, signal, action rule and evaluator. Actions
   at one node can enter signals or productivity at another.
3. **Direct discrimination** (Definition 1, PDF p. 13). `Aⁿ(w, s) ≠ Aⁿ(b, s)`
   at a fixed signal. It covers tastes, beliefs about productivity and beliefs
   about the signal, so statistical discrimination is direct (BHI-Q3).
4. **Subsystem and reference qualification** (PDF pp. 14–15). The researcher
   chooses which nodes to include, `N′`, and a qualification `Y⁰` that groups
   comparable workers at entry. Disparities from outside the subsystem are
   left unclassified (BHI-Q5).
5. **Total discrimination** (Definition 2, PDF p. 15).
   `Δᵀ(y⁰) = μ*(y⁰, w) − μ*(y⁰, b)`, the gap in average focal actions between
   workers of the same qualification.
6. **Systemic discrimination** (Definition 3, PDF p. 16). With the
   counterfactual `μ̃*(y⁰, g)`, group `g`'s signal distribution under the
   other group's rule (eq. 2), `Δˢ(y⁰, w) = μ*(y⁰, w) − μ̃*(y⁰, b)` and
   `Δˢ(y⁰, b) = μ̃*(y⁰, w) − μ*(y⁰, b)`.
7. **Decomposition** (Proposition 1, PDF p. 19). With average direct
   discrimination `Δᴰ(y⁰, g) = ∫ δ(s) dσ*(s; y⁰, g)`, eqs. (4), (5) and (6).
8. **Discussion** (PDF pp. 20–23). The choice of `Y⁰` nests correspondence
   studies (`Y⁰ = S*`), disparate impact (`Y⁰ = Y*`) and unconditional gaps
   (constant `Y⁰`). A race-neutral rule gives only systemic discrimination
   (footnote 26, BHI-Q7). Systemic discrimination is informational or
   technological (BHI-Q8). It differs from statistical discrimination in
   working through the action distribution at a qualification level rather
   than through the rule at a signal (BHI-Q9), and it cannot be inaccurate
   (BHI-Q10).
9. **Identification** (PDF pp. 24–29). In a two-node subsystem with
   `Y⁰ = S¹`, `μ*(y⁰, g) = A*(g, S*(A¹(g, y⁰), y⁰))` and
   `μ̃*(y⁰, g) = A*(g′, S*(A¹(g, y⁰), y⁰))` (eqs. 7, 8). The treatment
   component is direct discrimination at each node, and the interaction
   component is how the initial action moves the focal action. The
   constructive Iterated Audit estimates the interaction piece by piece, and
   the experimental Iterated Audit simulates the focal signals directly.
10. **Constructive application** (PDF pp. 29–34). A correspondence study at
    automotive firms, 1,001 resumes. Callbacks 28.3 percent for White and 20.0
    for Black names. Without experience 25.6 against 13.2, a gap of 12.4
    points (BHI-Q11). With experience 31.1 against 26.9, a gap of 4.2 points,
    p = 0.20. Experience raises callbacks by 9.5 points. 107 hiring managers
    report conversion rates of 55 percent without and 71 percent with
    experience. Local openings at the 25th, 50th and 75th percentiles are 5,
    8 and 18. The systemic share of total discrimination is 11 percent at one
    opening, 44 percent at eight and 32 percent at 31 (BHI-D2).
11. **Experimental application** (PDF pp. 34–38). LLM-written recommendation
    letters, 396 hiring managers, 441 ratings. Total discrimination against
    women of about 29 and 31 percent of a standard deviation in hiring
    likelihood and wage. Direct discrimination small and insignificant.
    Systemic discrimination about 27 and 43 percent of a standard deviation.
    Four language indices do not account for the gap (Table 2).
12. **Appendix B** (PDF pp. 51–57). Differential signal precision yields both
    direct and systemic discrimination, with the systemic part hurting
    high-productivity workers of the noisier group and helping
    low-productivity ones. A lab experiment with 100 workers, 199 recruiters
    and 501 hiring managers finds total discrimination of 0.24 for high and
    0.03 for low qualification, with systemic parts 0.09 and −0.04 (Table 3).

## Derivation chain of what Paper A1 uses

- Eq. (4) follows by adding and subtracting `μ̃*(y⁰, w)`, eq. (5) by adding
  and subtracting `μ̃*(y⁰, b)`, and eq. (6) is their average (`prop1`, `eq5`,
  `eq6`).
- Footnote 26 follows from eq. (4) when `A*(w, ·) = A*(b, ·)`, since then
  `δ = 0` (`neutral_rule_all_systemic`).
- If the two groups' signal distributions coincide at `y⁰`, then
  `μ̃*(y⁰, b) = μ*(y⁰, w)` and `μ̃*(y⁰, w) = μ*(y⁰, b)` so both systemic terms
  vanish and total equals direct (`same_signal_all_direct`). This is our
  statement of a case the paper describes in words when `Y⁰ = S*` (PDF p. 21).
- A group-neutral rule applied to a signal that is distributed alike in both
  groups at the reference qualification gives no total discrimination at
  that qualification (`neutral_rule_same_signal_no_gap`). A group-neutral
  rule applied to a signal that differs by group at the same qualification
  gives total discrimination that is all systemic
  (`neutral_rule_all_systemic`, `control_audit_misses_systemic`).
- In the two-node subsystem, `Δˢ(y⁰, b)` depends on the group only through
  `A¹(g, y⁰)` (eqs. 7, 8), so it vanishes without initial-node direct
  discrimination (`no_initial_direct_no_systemic`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every numbered result, and the gaps are these.

- The measure-theoretic general case. Definitions 1 to 3, Proposition 1 and
  eqs. (3) to (6) are formalised on a finite signal space at one
  qualification level, where the integrals are sums. The algebra is the same.
- The estimates. Table values are transcribed from the text layer and only
  their internal arithmetic is checked (`table1_arith`, `table3_arith`). No
  data were read. Figure 5, which carries the systemic shares by thickness,
  is an image (BHI-D2).
- Appendix B.1.1 is formalised for any strictly increasing `Φ`, with the
  square roots restored (BHI-D5). The averages over productivity that give
  the "cherry-picking" and "lemon-dropping" conclusions need normal
  integrals and are not formalised.
