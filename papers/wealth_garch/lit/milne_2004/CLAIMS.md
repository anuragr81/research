# Claims: Milne (2004)

Bib key `milne2004`. Version read: Cass Business School working paper, August 2004, SSRN 576062; 43 PDF pages, printed page = PDF page (p. 1 unnumbered). Read in full. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| MI-Q1 | 1 | This paper, and its companion Milne and Whalley (2004), have developed from a single original working paper (Milne and Whalley (1999)). | Relation to the companion paper. |
| MI-Q2 | 2 | Sections 3 and 4 are extensions; the model of Section 4 is analyzed more fully in Milne and Whalley (2004). | The random-audit model is a summary of the companion, not new here. |
| MI-Q3 | 11 | A fixed cost of raising external equity capital unrelated to the net worth of the bank or the amount of the capital issued. | (b): fixed cost per issue, not scaled by size. |
| MI-Q4 | 12 | In the case of liquidation the realized value of bank assets is assumed to be less than total deposits, so shareholders receive nothing. | (a): in Section 2 the consequence at the minimum is liquidation, borne by shareholders. |
| MI-Q5 | 18 | With these assumptions the banks will make an annual loss (negative cash flow) approximately one year in 20. | (c): calibration target; nothing is estimated. |
| MI-Q6 | 20 | The desire to avoid these costs leads to higher desired capital | (c): C\* rises in the fixed cost χ (4% to 20%). |
| MI-Q7 | 20 | This modest impact reflects the rapid reduction in the normal density function away from the mean. | (c): the effect of χ on C\* is small. |
| MI-Q8 | 28 | a random poisson regulatory audit, with expected probability of audit per unit time of | (a): the cost below the minimum is a Poisson audit with constant intensity q. |
| MI-Q9 | 30 | With random audit, whenever capital falls below the regulatory minimum, the bank is under the immediate threat of a regulatory audit and a consequent deadweight loss of recapitalization or liquidation. | (a): what happens on audit; the cost is asymmetric, applying only below the minimum. |
| MI-Q10 | 31 | The gains from this alternative are higher the greater the the ability to loot | (c): sign of the looting and risk-shifting alternative in φ, σ2 and q (the sentence continues "…and the less frequent the regulatory audit (q)"). The doubled "the the" is in the original. |
| MI-Q11 | 32 | and looting, substantially reduce the desired capital buffer. | (c): random audit with looting and risk shifting lowers C\*; a single combined example. |
| MI-Q12 | 39 | make no dividend payment and recapitalize in the event of a regulatory audit | (a), (b): in Section 4 the issue happens at the audit date, not at a chosen trigger. |
| MI-Q13 | 33 | Assets cannot be sold for full present value and there are fixed costs of raising new shareholder capital, required whenever bank capital declines to the regulatory minimum. | Summary of the two frictions in the conclusions: illiquid assets and a fixed issue cost at the minimum. |
| MI-Q14 | 35 | What is the relationship between expected cash flow and bank capital and does this take the hump-shaped form predicted by the inventory perspective? | Empirical testing left to future research. |
| MI-Q15 | 35 | There is also room for further theoretical development, including more realistic modelling of the costs of recapitalisation | Stated limitation: the cost of issue is crude. |

## Lean results

| Name | Statement |
|---|---|
| `Milne2004.issueCost_const` | The issue cost $\chi$ is the same whatever the amount issued. |
| `Milne2004.audit_loss_is_fixed` | With value of slope one between $C$ and $C^*$, the loss on an audit that forces recapitalisation at par is $\chi$, the same at every shortfall. |
| `Milne2004.control_free_issue_no_loss` | With $\chi=0$ the audit costs nothing. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| MI-D1 | pp. 28, 39 | The cost of being below the minimum is a Poisson audit of intensity $q$ that forces recapitalisation to $C^*$ or liquidation. The only deadweight loss on recapitalisation is the fixed $\chi$; the top-up is paid at par, so no part of the cost grows with the shortfall (`audit_loss_is_fixed`). | MI-Q8, MI-Q9, MI-Q12; the region-3 equation, p. 39. |
| MI-D2 | pp. 12, 17 | Under continuous monitoring the minimum is a hard barrier, where the bank recapitalises at cost $\chi$ or is liquidated; the issue point is the minimum and does not move with $\chi$. | MI-Q4; Prop. 5. |
| MI-D3 | pp. 14, 17 | With a fixed cost only and issues at par, the post-issue target is the dividend barrier $C^*$, with no separate target. My reasoning from eq. (9) and Props. 3 and 4, consistent with what is printed. | Eq. (9). |
| MI-D4 | p. 20 | $C^*$ rises modestly in $\chi$; a high $\chi$ against franchise value turns recapitalisation into liquidation at the minimum. | MI-Q6, MI-Q7. |
| MI-D5 | pp. 30-32 | The only statement on the audit intensity is that more frequent audit makes the condition for keeping capital easier to meet; the one example changes $q$, looting and risk shifting together, so no single-parameter effect on a threshold can be read from it. No two frictions are shown moving the trigger in opposite directions. | MI-Q10, MI-Q11. |
| MI-D6 | pp. 18, 27, 31 | The baseline buffer is given as about 2 percent (pp. 18, 27) and as around 4 percent (p. 31), and the figure captions give $\chi=1$ where the text gives 4 percent. | Text. |
| MI-D7 | pp. 1, 2, 28 | The random-audit model is a summary of the companion Milne and Whalley paper, so on the pairing of audit and fixed cost the paper adds nothing beyond Milne and Whalley (2001). | MI-Q1, MI-Q2. |
