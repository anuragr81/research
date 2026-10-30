# Claims: Bolton, Chen and Wang (2011)

Bib key `bolton2011`. Version read: the accepted manuscript marked "Forthcoming, Journal of Finance", 43 numbered pages after a title page. Evidence [F] for the main text; Appendices B and D are cited but not in the copy.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| BCW-Q1 | 3 | While this double-barrier policy is not new (it goes back to the inventory model of Miller and Orr 1966 in corporate finance), our model provides substantial new insight | The payout-and-financing double barrier is an established structure, by the authors' own account. |
| BCW-Q2 | 1 | The baseline model we propose introduces only the essential building blocks | A deliberately minimal model. |
| BCW-Q3 | 10 | Together these costs imply that the firm will optimally tap equity markets only intermittently, and when doing so it raises funds in lumps, consistent with observed firm behavior. | Fixed costs make issuance lumpy. |
| BCW-Q4 | 10 | To preserve the linear homogeneity of our model, we further assume that the firm's fixed cost of issuing external equity is proportional to capital stock K, so that Φ = φK. | The same device as $K$ per unit of liabilities in M4 to M6 and $KR$ in Peura and Keppo. |
| BCW-Q5 | 10 | Having said that, a weakness of our model is that it will be mis-specified as a structural model of firms outside equity issue decisions. | The authors' own limit on the device. |
| BCW-Q6 | 4 | The firm optimally chooses not to issue equity unless it runs out of cash. | Issuance only at the lower barrier. |
| BCW-Q7 | 18 | Second, since m is optimally chosen, the marginal value of the last dollar raised must equal one plus the marginal cost of external financing, 1 + γ. | One return target $m$ with $p'(m)=1+\gamma$: exact targeting (BCW-D1). |
| BCW-Q8 | 23 | Without fixed cost (φ = 0), the payout boundary drops further to w = 0.14, and the firm's return cash-capital ratio is zero, as the firm raises just enough funds to keep w above 0. | Without a fixed cost the lump disappears, as in Løkka and Zervos. |
| BCW-Q9 | 19 | The implied first-best q in the neoclassical model is q F B = 1.23, and the corresponding first-best investment-capital ratio is iF B = 15.1%. | Checked in Lean (`BoltonChenWang.ifb_baseline`). |

## Lean results

| Name | Statement |
|---|---|
| `BoltonChenWang.ifb_mm_value` | $i^{FB}$ of (7) satisfies $(r+\delta-i)(1+\theta i)=\mu-i-\theta i^2/2$, the Modigliani-Miller valuation with $q=1+\theta i$ (8). |
| `BoltonChenWang.ifb_baseline` | At $r=6\%$, $\delta=10.07\%$, $\mu=18\%$, $\theta=1.5$: $0.1511<i^{FB}<0.1512$ and $1.2266<q^{FB}<1.2268$. |
| `BoltonChenWang.investment_cash_sensitivity` | The derivative of (14) is $-p\,p''/(\theta p'^2)$, (21). |
| `BoltonChenWang.sensitivity_pos` | It is positive when $p>0$, $p''<0$. |
| `BoltonChenWang.marginal_q_wedge` | $q_m=p-wp'=q_a-(p'-1)w$, (23). |
| `BoltonChenWang.marginal_q_deriv` | $q_m'(w)=-p''(w)\,w$. |
| `BoltonChenWang.control_sensitivity_needs_concavity` | With $p''>0$ the sensitivity is negative. |
| `BoltonChenWang.control_ifb_needs_root_condition` | With a negative discriminant, (7) does not solve the valuation. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| BCW-D1 | pp. 18, 23 | Each equity issue takes the cash-capital ratio from 0 to the single return target $m$: exact targeting. | BCW-Q7, condition (19)-(20). |
| BCW-D2 | the copy read | Appendices B and D, cited in the text, are not in the copy. | Nothing from them is attributed. |
| BCW-D3 | title page | Accepted manuscript, "Forthcoming, Journal of Finance". BCVW cites the published version, 66(5):1545-1578, 2011. | Only this version's pages are attributed. |
