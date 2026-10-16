# Bohren, Hull and Imas (2024), claims

Evidence tag [F]. NBER working paper 29820, March 2022, revised December 2024,
read in full from its text layer on 16 October 2026, all 66 PDF pages, with
page images read for PDF pages 19, 27 and 52. Pages are PDF pages. The
printed page is the PDF page minus 2 (BHI-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BHI-Q1 | 2 | "Economists often measure discrimination as disparities arising from the direct effects of group identity. We develop new tools to model and measure systemic discrimination, which instead captures how discrimination in other decisions indirectly contributes to disparities." | The two notions the paper separates |
| BHI-Q2 | 9 | "conditioning on characteristics that are themselves affected by discrimination" | The bad-control concern. Conditioning on shown class removes any part of the gap that runs through class differences caused by discrimination |
| BHI-Q3 | 13 | "It can arise from the manager's preferences, beliefs about productivity, or beliefs about the signal distribution being dependent on group identity (e.g., taste-based or statistical discrimination)." | Statistical discrimination from the group marker is direct, not systemic |
| BHI-Q4 | 13 | "Because this definition conditions on the signal at the node where discrimination is being measured, it does not account for disparities that stem from decisions made at other nodes." | Direct discrimination is measured at a fixed signal |
| BHI-Q5 | 15 | "Importantly, we do not claim that the residual focal-node disparity is not discrimination; this remaining disparity may stem from decisions outside of the subsystem, non-discriminatory forces, or both." | A gap from group differences in class that arise outside the subsystem is left unclassified |
| BHI-Q6 | 19 | "total discrimination is the sum of average direct discrimination and systemic discrimination" | Proposition 1, eq. (4), `BohrenEtAl2024.prop1` |
| BHI-Q7 | 20 | "In our framework, any total discrimination that arises from a race-neutral action rule is systemic, as the direct component is zero by definition." | Footnote 26, `BohrenEtAl2024.neutral_rule_all_systemic` |
| BHI-Q8 | 22 | "Systemic discrimination can stem from an informational source, via group-based differences in the focal-node signal for workers who are equally productive at the focal node, or a technological source, via group-based differences in focal-node productivity for workers who are equally qualified at entry to the subsystem." | The two sources of systemic discrimination |
| BHI-Q9 | 23 | "Statistical discrimination differs from systemic discrimination, however: it arises from the impact of these distributions on the action rule for a given signal realization, rather than the action distribution for a given qualification level." | The line between A1's marker inference (statistical, direct) and a group-neutral rule on a correlated variable (systemic) |
| BHI-Q10 | 23 | "Unlike with statistical discrimination (e.g., Bordalo et al. 2019; Bohren et al. 2023) there is no scope for "inaccurate" systemic discrimination: only the true productivity and signal distributions contribute to systemic discrimination." | Inaccurate beliefs act through the action rule, hence through direct discrimination |
| BHI-Q11 | 31 | "Among resumes without experience, callback rates were 25.6% for White applicants and 13.2% for Black applicants, resulting in a significant 12.4 percentage point disparity (p < 0.01)." | First-round direct discrimination in the constructive IA study (Table 1, column 2) |
| BHI-Q12 | 32 | "More than half of this total discrimination is due to systemic discrimination against Black applicants" | The share at median thickness as stated on PDF p. 32, at odds with BHI-Q13 (BHI-D2) |
| BHI-Q13 | 33 | "but increases to 44% in intermediate markets with 8 jobs (50th percentile)." | The share at median thickness as stated on PDF p. 33 (BHI-D2) |
| BHI-Q14 | 48 | "For example, a lending algorithm which considers a person's zip code but does not use racial information when determining loan eligibility may be race neutral in design but discriminatory in practice." | A group-neutral rule on a correlated variable is the paper's example of systemic discrimination |
| BHI-Q15 | 50 | "evidence for a reversal of direct discrimination over time— such as the ones documented in Bohren et al. (2019) and Mengel et al. (2019)—may not imply that total discrimination has been mitigated or reversed." | A vanishing direct penalty need not mean a vanishing total gap |
| BHI-Q16 | 17 | "the framework can also capture systemic discrimination arising from the choice of the productivity signaling technology itself (i.e., choosing the evaluation criteria)." | Choosing which signals are read is itself a node of the system |
| BHI-Q17 | 17 | "our framework can also capture systemic discrimination driven by how anticipated actions at contemporaneous or future nodes impact the [...] decision." | Anticipated treatment elsewhere enters the focal decision. "focal-node" is elided because the text layer breaks it across a line |
| BHI-Q18 | 23 | "since signal inflation is a statistical bias in the signal, it can be offset via "reverse" direct discrimination in the action rule." | A rule that may not use the group cannot make this offset, `BohrenEtAl2024.signal_inflation_offset` |
| BHI-Q19 | 48 | "Mayhew (1968) argues that some organizations may have accepted Civil Rights legislation mandating [...] treatment because they were aware systemic discrimination could preserve the status quo." | Footnote 53. Discrimination expected to survive a colour-blind mandate through neutral channels. "color-blind" is elided because the text layer breaks it across a line |
| BHI-Q20 | 49 | "A facially neutral practice may therefore be found to be discriminatory under this doctrine even in the absence of explicit categorization or animus." | The disparate impact doctrine reaches some group-neutral rules |
| BHI-Q21 | 52 | "When the qualification is set to current productivity, [...] the former can lead to both direct and systemic forms of discrimination in the current decision, while the latter only leads to direct discrimination." | Appendix B. "The former" is a group difference in signal precision and "the latter" a group difference in the prior distribution of productivity. A group difference in class distribution read from the marker gives only direct discrimination |

## Lean results

`lean/mathlib/BohrenEtAl2024.lean`, in the paper's notation, with a finite
signal space at one qualification level.

| Theorem | What it checks |
|---|---|
| `BohrenEtAl2024.prop1` | Proposition 1, eq. (4). `Δᵀ(y⁰) = Δᴰ(y⁰, w) + Δˢ(y⁰, b)` |
| `BohrenEtAl2024.eq5` | Eq. (5). `Δᵀ(y⁰) = Δᴰ(y⁰, b) + Δˢ(y⁰, w)` |
| `BohrenEtAl2024.eq6` | Eq. (6), the average decomposition |
| `BohrenEtAl2024.neutral_rule_all_systemic` | Footnote 26. Under a group-neutral rule, average direct discrimination is zero and total discrimination is systemic |
| `BohrenEtAl2024.same_signal_all_direct` | When both groups have the same signal distribution at the reference qualification, systemic discrimination is zero and total discrimination is direct |
| `BohrenEtAl2024.no_initial_direct_no_systemic` | Eqs. (7) and (8). In the two-node subsystem, no direct discrimination at the initial node gives no systemic discrimination at the focal node |
| `BohrenEtAl2024.control_pairing_matters` | Control. Pairing `Δᴰ(y⁰, w)` with `Δˢ(y⁰, w)` instead of `Δˢ(y⁰, b)` fails, 2 against a total of 1 |
| `BohrenEtAl2024.control_audit_misses_systemic` | Control. With a group-neutral rule and different signal distributions, direct discrimination is 0 and total discrimination is 1 |
| `BohrenEtAl2024.neutral_rule_same_signal_no_gap` | A group-neutral rule on a signal distributed alike in both groups at the reference qualification gives no total discrimination |
| `BohrenEtAl2024.signal_inflation_offset` | Section 3.3. Signal inflation offset by reverse direct discrimination, total 0 with direct 1 and systemic −1 |
| `BohrenEtAl2024.two_node_decomp` | Section 4.1. Total discrimination in the two-node subsystem is the focal treatment component plus the systemic term |
| `BohrenEtAl2024.experimental_ia` | Section 4.3 and Figure 4. Direct plus systemic equals total in the experimental Iterated Audit |
| `BohrenEtAl2024.fn44_thin_thick` | Footnote 44 and Section 5.2. The experience gap is 0 with no openings, positive with one, and tends to 0 |
| `BohrenEtAl2024.control_fn44_no_direct` | Control. Without first-round direct discrimination there is no experience gap |
| `BohrenEtAl2024.table1_arith` | Table 1 and Section 5.2, printed differences and the 49 percent return to experience (BHI-D9) |
| `BohrenEtAl2024.table3_arith` | Table 3, total equals direct plus systemic and the difference column |
| `BohrenEtAl2024.b11_posterior` | Appendix B.1.1, the posterior mean `η s/(1 + η)` |
| `BohrenEtAl2024.b11_cutoff_order` | Appendix B.1.1, the noisier group faces the higher signal cut-off |
| `BohrenEtAl2024.b11_systemic_sign` | Appendix B.1.1, the sign of systemic discrimination above, below and at the cut-off, with the square roots restored (BHI-D5) |
| `BohrenEtAl2024.control_equal_precision_no_systemic` | Control. Equal precisions give no systemic discrimination |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| BHI-D1 | The version read is NBER working paper 29820, March 2022, revised December 2024. Any published version is not read. Pages are PDF pages, the printed page is the PDF page minus 2, and the text file has a 67th, empty page | The suite pins this file. Rows citing this record cite the revised working paper |
| BHI-D2 | At the median thickness of 8 openings, PDF p. 32 says "More than half" of total discrimination is systemic (BHI-Q12), PDF p. 33 says 44 percent (BHI-Q13), and the introduction (PDF p. 7) says "roughly half". The plotted values of Figure 5 are not in the text layer. No share is relied on | The three statements describe the same market |
| BHI-D3 | Footnote 59 (PDF p. 55) gives the median task-A performance as 4. The notes to Figure 7 (PDF p. 56) and to Table 3 (PDF p. 57) give the median as 3 | Appendix B is not relied on |
| BHI-D4 | Footnote 60 (PDF p. 55) calls eq. (5) the "average" decomposition. The average decomposition is eq. (6) (PDF p. 20), which the notes to Table 3 cite | `BohrenEtAl2024.eq5`, `BohrenEtAl2024.eq6` |
| BHI-D5 | In B.1.1 (PDF p. 52) the signal has precision `η_g`, so its standard deviation is `1/√η_g`, but the printed difference is `Φ(η_b(·)) − Φ(η_w(·))`. We read `Φ(√η_b(·)) − Φ(√η_w(·))`. The signs the paper draws are unaffected | Page image of PDF p. 52. Not relied on |
| BHI-D6 | Appendix B.1.3 (PDF p. 55) writes average direct discrimination as `τ(w, y⁰)` and total discrimination as `Δ(y⁰)`, against `Δᴰ(y⁰, w)` and `Δᵀ(y⁰)` in Section 3.2 | Notation only |
| BHI-D7 | Footnote 41 (PDF p. 27) lists `A*(w, S̃*(w, y⁰))` twice and `A*(w, S̃*(b, y⁰))` as the actions that identify `Δˢ(y⁰, b)` and `Δᵀ(y⁰)`. By Definitions 2 and 3 with eqs. (7) and (8) we read `A*(w, S̃*(w, y⁰))`, `A*(b, S̃*(w, y⁰))` and `A*(b, S̃*(b, y⁰))` | Page image of PDF p. 27 |
| BHI-D8 | The text layer renders "≠" as "6=" in Definitions 1 and 2 (PDF pp. 13, 15) | Quotations avoid these lines |
| BHI-D9 | The introduction (PDF p. 6) says White applicants without experience were "13 percentage points (90%) more likely" to be called back. The rates of Section 5.2 give 12.4 points, which is 94 percent of the Black rate of 13.2 | `BohrenEtAl2024.table1_arith`. Not relied on |

## Coverage of the source's results

| Result | Lean name | Status | Reason |
|---|---|---|---|
| Definition 1 | `direct` | special case | Finite signal space at one qualification level. The paper allows measurable spaces |
| Definition 2 | `total` | special case | As above |
| Definition 3, eqs. (1), (2) | `mu`, `muT`, `sysW`, `sysB` | special case | As above |
| Eq. (3) | `direct` | special case | As above |
| Proposition 1, eq. (4) | `prop1` | special case | As above. The proof is the same add-and-subtract |
| Eq. (5) | `eq5` | special case | As above |
| Eq. (6) | `eq6` | special case | As above |
| Footnote 26 | `neutral_rule_all_systemic` | special case | As above |
| Section 3.3, signal inflation and its offset | `signal_inflation_offset` | special case | A two-signal example |
| Section 3.3, differential screening and technological sources | `b11_systemic_sign` | special case | Only through the Appendix B.1.1 example |
| Eqs. (7), (8) | `sysB2`, `total2`, `two_node_decomp`, `no_initial_direct_no_systemic` | general | Deterministic two-node subsystem, as the paper writes it |
| Section 4.3, experimental Iterated Audit | `experimental_ia` | general | |
| Footnote 44 | `pExp`, `fn44_thin_thick` | general | |
| Section 5.2 and Table 1, printed arithmetic | `table1_arith` | general | |
| Systemic shares 11, 44 and 32 percent (PDF p. 33) | none | not formalised | They come from Figure 5 and data not in the text (BHI-D2) |
| Section 6 and Table 2 | none | not formalised | Estimates. No printed number follows from others |
| Appendix B.1.1 | `b11_posterior`, `b11_cutoff_order`, `b11_systemic_sign` | special case | Any strictly increasing `Φ`. The "cherry-picking" and "lemon-dropping" averages need normal integrals and are not formalised |
| Table 3 | `table3_arith` | general | |
