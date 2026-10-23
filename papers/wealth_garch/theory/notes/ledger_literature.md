# Ledger diagrams of three approaches in the literature, 23 October 2026

A planning note, not part of the paper. The author asked to see the gap the
theory paper closes, drawn as ledger diagrams of the ways the literature
addresses the paper's question, set against the paper's own ledger
(`notes/ledger.md`), and then asked for the diagrams to be in the manuscript.
They are Figures 1 to 4 of `wealth_garch_theory.tex`, drawn from
`figures/ledger_paper.tex`, `figures/ledger_issuance.tex`,
`figures/ledger_cap.tex` and `figures/ledger_preference.tex` with the styles of
`figures/ledger_styles.tex`. Section 3.2 of the manuscript gives the contrast
and Table `tab:approaches` the element-by-element comparison. In the
manuscript every label cites a row; the literature rows L22 to L34 were added
for the quotations the drawings rest on. The tables below give the quotation
IDs of the records behind each arrow, and the Lean theorem behind each solid
one. Every paper drawn was read in full, has a record under the parent `lit/`
and a Lean companion in `theory/lean/`. The form is Paper A's
(`papers/subjective_racism/paper_a/notes/ledger_literature.md`).

## 1. The question, and the three approaches

The question (manuscript, Section 1.1). When a bank's capital buffer is
explained by a fixed cost of issuing equity together with a penalty that grows
as capital falls below the requirement, does the bank's policy carry the weight
of that penalty as a parameter of its own, separate from the issuance cost, and
if so where, in the dividend barrier and the recapitalisation trigger or in the
variance of the buffer?

The papers read address it in three ways, each a position on three axes. What
creates the thresholds (a fixed cost, a cap, or nothing). What the cost of a
shortfall is (liquidation at the minimum, an exogenous threshold, or a
preference over losses). Where a parameter is read off (calibration, numerical
solve, or the variance of returns).

| Approach | Papers | Thresholds from | The cost of a shortfall | Parameters read off | Lean records |
|---|---|---|---|---|---|
| 1. Issuance-cost barrier models | Peura and Keppo (2006); Dai, Huang and Keppo (2019); Bolton, Chen and Wang (2011); Løkka and Zervos (2008); Milne and Whalley (2001) | a fixed cost or a delay on issues | liquidation at the minimum, or a random audit | calibration from issue cost and frequency; fit to equity values; imposed | `PeuraKeppo`, `DaiHuangKeppo`, `BoltonChenWang`, `LokkaZervos`, `MilneWhalley` |
| 2. Regulatory-cap control | Bayraktar, Chevalier, Ly Vath and Wang (2026); Black and Perold (1992) as the geometry | a cap on exposure; an exogenous distress threshold; proportional costs only | an intervention at the threshold, taken as given | an illustrative parameterisation | `BCVW`, `BlackPerold` |
| 3. Preference asymmetry read in volatility | Barberis, Huang and Santos (2001); Li, Yu and Zhang (2023); Engle and Siriwardane (2018) | none; no bank | a loss-averse preference over gains and losses | the volatility of returns, or the large-wealth limits of the controls | `BarberisHuangSantos`, `LiYuZhang`, `EngleSiriwardane` |

Left out, with the reason.

- **Berger et al. (2008).** A partial-adjustment regression, not a mechanism.
  It supplies the fact the question starts from (L15) and is drawn nowhere.
- **Hugonnier and Morellec; Bolton, Li, Wang and Yang; De Nicolò, Gamba and
  Lucchetta; Klimenko et al.** Read in full in the novelty passes
  (`notes/novelty_check_20261023.md`), cited by no row, so they have no
  record and are not drawn. Each would sit in approach 1 or 2.

## 2. Rules of the drawing

The manuscript's rules hold (`notes/ledger.md`, section 1). An entity acts or
is acted on and is not a role, an attribute or an action. A rule is not a node
and enters through the agent who enforces it. Constructs and the model's
outputs are not nodes. The status of an arrow is read against the source, not
the manuscript.

- **Solid.** The paper derives the action, and its Lean record proves it.
- **Dashed.** The paper assumes the action, or claims it without the record
  formalising the claim.
- **Dotted.** The paper states the action in words and does not model it.

Each page closes with a note on where the approach parts from the paper. Page 1
uses the manuscript's own statuses (settled, given, open) with the same three
line styles.

## 3. This paper (page 1)

The ledger of `notes/ledger.md`. Six entities of the bundle ledger, eight
actions, and the results T1 to T11 placed on the actions and on equity.

| From | To | Action | Status | Rows | Results |
|---|---|---|---|---|---|
| bank | investors | pays dividends at the dividend barrier | settled | M1 | T1, T4 |
| investors | equity | buy stock, from the trigger to the target | settled | M2, M4 to M7 | T2, T3, T4 |
| investors | equity | inject capital, the parent holding company | settled | M2, M7 | T2, T3 |
| insured creditors | bank | deposit and withdraw, the liability shock | given | A1 | T8 |
| uninsured creditors | bank | lend and withdraw, the liability shock | given | A1 | T8 |
| bank | bank | takes risky exposure under the regulatory cap | given | A2 | T6, T7, T9 |
| supervisor | bank | supervises, holds the bank to the cap and the requirement | given | A2 | T10 |
| supervisor | bank | assigns a capital category, with its restrictions | open | none | T11 |

The two headlines sit on the graph. K1 is a statement about the dividend edge
and the issue edges together (T4). K2 is a statement about the exposure edge
and the node it moves, equity (T6, T7), set against T1 and T2 where the
asymmetry parameter does move the thresholds. The shortfall penalty is drawn
nowhere; it is a construct that stands for the open edge.

## 4. Approach 1, issuance-cost barrier models (page 2)

**Entities.** Bank, with its owners, holding a capital ratio; Investors, the
capital market that supplies an issue; Regulator, who liquidates at the
minimum. The dividend barrier $u_2$ and the issue barrier $u_1$ are outputs
and not nodes.

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Bank | Investors | Pays dividends at the barrier $u_2$, to which every issue replenishes the ratio | PK-Q7, BCW-Q7, LZ-Q3 | Solid | `PeuraKeppo.value_at_barrier`, `BoltonChenWang.marginal_q_wedge`, `LokkaZervos.barrier_paste` |
| Investors | Bank | Supply an issue at a fixed proportion $K$ of size, after a delay, in larger and rarer lumps when $K$ is high | PK-Q5, PK-Q9, DHK-Q2, BCW-Q3, BCW-Q4 | Solid | `PeuraKeppo.table2_signs`, `DaiHuangKeppo.table21_K_signs`, `BoltonChenWang.investment_cash_sensitivity` |
| Investors | Bank | Without a fixed cost, top up minimally at zero, no lump and no target | LZ-Q3, LZ-Q4, BCW-Q8 | Solid | `LokkaZervos.xss_pos_iff`, `LokkaZervos.v_concave` |
| Investors | Bank | The fixed cost alone creates no positive issue barrier; the delay does | PK-Q8 | Solid | `PeuraKeppo.no_issue_barrier` |
| Regulator | Bank | Liquidates when the ratio falls below the minimum; owners take the minimum as a constraint | PK-Q6, DHK-Q13 | Dashed | |
| Regulator | Bank | Audits at random times; the buffer guards against a costly intervention or market response | MW-Q2, MW-Q3, MW-Q4 | Dashed | `MilneWhalley.buffer_eq_u0` for the no-issue buffer |
| Regulator | Bank | Its own asymmetry, the cost of liquidating against not liquidating, moves the liquidation barrier | DHK-Q9, DHK-Q10 | Dashed | |
| Analyst | Parameters | Pins the delay and the fixed cost from issue cost and frequency, imposes the rest by intuition, or fits them to equity values | PK-Q22, PK-Q28, DHK-Q11 | Dotted | |

Recorded as. Capital ratios and accounting returns of US banks (PK-Q11), issue
cost and frequency (PK-Q22), equity values (DHK-Q11).

**Against the paper.** The thresholds are the paper's (M1, M2, M7 in the same
form), and the fixed cost per unit of size is the paper's device (A5). The
penalty is liquidation at the minimum, which the authors call extreme and ask
to replace with one that grows as the ratio deteriorates (PK-Q6, PK-Q18, PK-Q30),
the paper's shortfall penalty (A4, M8). No parameter is read off the thresholds;
they are imposed or calibrated (PK-Q28, DHK-Q11), where the paper's M3 reads two
off the barrier and the trigger. The regulator's asymmetry in Dai, Huang and
Keppo is the regulator's, not the owners', and moves both barriers the same
way (DHK-Q9, DHK-Q10), where the paper's parameter moves the trigger against
the fixed cost (M2, M3).

## 5. Approach 2, regulatory-cap control (page 3)

**Entities.** Bank; Regulator, who sets the cap and the distress threshold;
Investors, who receive dividends and recapitalise at the threshold; Depositors,
whose liabilities carry a shock and run off. Black and Perold's investor stands
beside the bank as the same geometry with preferences in place of regulation.

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Regulator | Bank | Caps the risky exposure by the smaller of a solvency and a liquidity branch, with a switching point | BCVW-Q7, BCVW-Q8, BCVW-Q9, BCVW-Q10 | Solid | `BCVW.piBar_mul_y`, `BCVW.u_concave`, `BCVW.switching_point_baseline`, `BCVW.piBar_tendsto` |
| Bank | Bank | Holds exposure at the cap; at zero equity the cap forces zero exposure and the diffusion vanishes | BCVW-Q2, BCVW-Q3 | Solid for the cap's values, dashed for the optimum binding | `BCVW.piBar_at_one`, `BCVW.diffusion_vanishes_at_one`, `BCVW.drift_at_one` |
| Regulator | Bank | Forbids borrowing in the main banking case, under which the liquidity branch never binds | BCVW-Q15, BCVW-Q16, BCVW-Q17 | Solid | `BCVW.ceiling_tighter_than_lcr` |
| Bank | Investors | Pays dividends at a barrier | Theorem 3.1, Remark 3.2 (BCVW p. 12) | Dashed | |
| Investors | Bank | Recapitalise at an exogenous distress threshold to a post-issue state, at proportional costs only | BCVW-Q5, BCVW-Q6, BCVW-Q14 | Dashed | |
| Depositors | Bank | Liabilities with their own shock, correlated with the risky return; a fixed run-off share | BCVW-Q10, BCVW-Q14 | Solid for the diffusion | `BCVW.sig2_nondegenerate`, `BCVW.sig2_decomp` |
| BP investor | BP investor | Keeps exposure a constant multiple of the cushion up to a borrowing limit; log-cushion volatility constant on the multiplier branch | BP-Q1, BP-Q4, BP-Q5, BP-Q6, BP-Q11 | Solid | `BlackPerold.multiplier_branch_vol`, `BlackPerold.limit_vol_tendsto`, `BlackPerold.binding_wealth`, `BlackPerold.cap_is_cppi` |
| BP investor's preferences | BP investor | The multiple from impatience, the limit from risk tolerance | BP-Q8, BP-Q9, BP-Q10 | Dashed | |
| Liability | BP investor | The floor tracks a liability, so the safe asset hedges it | BP-Q3 | Dashed | |

Recorded as. Nothing; an illustrative parameterisation (BCVW p. 13).

**Against the paper.** The cap is the paper's cap (A2; `BCVW.piBar_mul_y`
equals `CapGeometry.capU`, and `BlackPerold.cap_is_cppi` is the same rule with
a floor of one). There is no fixed issuance cost and the threshold is
exogenous, both of which the authors name as changes that would move
recapitalisation (BCVW-Q5, BCVW-Q6); the paper's M2 and M3 take up that change.
There is no asymmetry parameter. The limits of buffer variance are never
evaluated, and the main banking case forbids borrowing, which is where the
paper's C3 puts the surplus limit. In Black and Perold the two slopes come
from preferences, so the volatility levels carry preference information
(L6); in the paper they come from regulation and carry none (M11, K2).

## 6. Approach 3, preference asymmetry read in volatility (page 4)

**Entities.** Investor, or agent, with a reference point and loss aversion;
Asset, the dividend claim or the wealth process; Firm, with leverage, whose
equity return is recorded. No bank and no regulator.

| From | To | Action | Source | Status | Lean |
|---|---|---|---|---|---|
| Investor | Asset | Prices the dividend claim; with constant loss aversion the volatility of log returns equals that of dividend growth at every level | QJE-Q1, QJE-Q2, QJE-Q5 | Solid | `BarberisHuangSantos.dispersion_free_of_f`, `BarberisHuangSantos.logReturn_eq`, `BarberisHuangSantos.v_concave` |
| Prior outcomes | Investor | Loss aversion varies with prior gains and losses, which is where excess volatility comes from | QJE-Q3 | Dashed | `BarberisHuangSantos.lam_after_ten_percent_fall` for the rule |
| Agent | Asset | Consumes and invests under an S-shaped utility over a reference; the large-wealth limits of the controls depend on the reference parameters | LYZ-Q1, LYZ-Q3, LYZ-Q4, LYZ-D1 | Solid | `LiYuZhang.merton_portfolio_limit`, `LiYuZhang.limitTerm_free_of_k`, `LiYuZhang.control_limitTerm_depends_on_k_when_equal` |
| Firm | Equity | Leverage multiplies asset variance into equity variance | ES-Q7 | Solid | `EngleSiriwardane.equity_variance`, `EngleSiriwardane.phi_zero_equity_is_asset` |
| Market | Equity | Volatility asymmetry, a response of variance to the sign of news, is mostly market exposure, 83 percent, and leverage 3 to 14 percent | ES-Q1, ES-Q3, ES-Q4, ES-Q5, ES-D1 | Solid for the shares' arithmetic | `EngleSiriwardane.share_market`, `EngleSiriwardane.share_gjr`, `EngleSiriwardane.gjr_news_asymmetry` |

Recorded as. Return volatility (QJE-Q2, 12 against 20.02 percent), the
responses of variance to negative and positive returns (ES-Q3).

**Against the paper.** The asymmetry is a preference of an investor over
gains and losses, not a weight on a regulatory shortfall in a bank's objective
(A4). Its mark is sought in the variance of returns. Two of the three find
none under their own constancy, Barberis, Huang and Santos with constant loss
aversion (QJE-Q1) and Engle and Siriwardane with leverage (ES-Q2), and the
third finds the mark in the large-wealth limits without a cap (LYZ-Q1). The
paper's K2 says that under a binding regulatory cap the variance limits carry
no mark of the asymmetry parameter (M11), and K1 says the thresholds do (M1 to
M3). The variance route of this approach is closed by the cap, and the
threshold route is the one the paper opens.

## 7. What the three drawings leave open, and the paper closes

| Element of the paper's ledger | Approach 1 | Approach 2 | Approach 3 | The paper |
|---|---|---|---|---|
| A fixed issuance cost that creates lumpy issues (A5, M6) | present | absent (BCVW-Q5) | absent | present |
| A graduated penalty below the requirement (A4, M8) | called for, not modelled (PK-Q18) | absent; an exogenous threshold (BCVW-Q6) | a preference over losses, no bank | present, with $R=1+q$ |
| A regulatory cap on exposure (A2, M10) | absent | present (BCVW-Q7) | absent | present |
| The trigger moved in opposite directions by two frictions (M2, M3) | the delay and the fixed cost move it the same way (DHK-Q6, DHK-Q7; PK Table 2 at small $K$ opposite, L8) | one friction | no trigger | the asymmetry parameter and the fixed cost (M3) |
| A parameter read off the thresholds (M3) | calibration and fits (PK-Q22, PK-Q28, DHK-Q11) | illustrative values | the variance of returns | local identification (K1) |
| Variance limits set by the cap, free of the asymmetry (M11, K2) | no variance | no limits evaluated; slopes from preferences in Black and Perold (L6) | limits carry the preference without a cap (LYZ-Q1); constant loss aversion leaves no mark (QJE-Q1) | present (K2) |

What no drawing has and the paper has not settled either. Whose cost the
penalty is (PK-Q31, the open edge of page 1), and whether the optimal exposure
binds the cap near both ends (the hypothesis of M11).
