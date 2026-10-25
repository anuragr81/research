# Claims: Constantinides and Richard (1978)

Bib key `constantinides1978`. Version read: Operations Research 26(4), July-August 1978, pp. 620-636; a JSTOR scan with no text layer, 18 PDF pages, printed pages 620 to 636 (PDF page = printed page - 618). Read in full from the OCR text, with page images for the formulas. Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| CR-Q1 | 620 | model with both fixed and proportional transactions costs and with linear holding and penalty costs | Both frictions: fixed plus proportional transfer cost; linear holding and penalty. |
| CR-Q2 | 620 | When the proportional transactions cost of decreasing the cash balance is sufficiently high, it is never optimal to decrease the cash balance. | Reduction to the one-sided (inventory) case. |
| CR-Q3 | 620 | From a practical viewpoint, these policies are easy to implement. | Simple (d, D, U, u) policy motivation. |
| CR-Q4 | 621 | We do not assume symmetry in any of the costs or in the demand distribution. | Holding and penalty slopes, and up/down transfer costs, may be asymmetric. |
| CR-Q5 | 622 | where there are fixed costs of control and hence the optimal control is discontinuous and consists of impulses, applied at stochastic stopping times. | The fixed cost produces lumpy impulse control. |
| CR-Q6 | 622 | We distinguish between no control, which costs nothing, and a zero control, which does nothing but nevertheless has a cost | The fixed cost is charged per transfer, whatever its size. |
| CR-Q7 | 623 | Implicit in the state equations is the assumption that the holding-penalty costs and transactions costs are not charged to the cash balance. | Costs do not enter the state dynamics. |
| CR-Q8 | 626 | the marginal saving on future costs in increasing the cash balance by one unit | Optimality condition at the target D (eq. (8)). |
| CR-Q9 | 627 | is the present value of the holding cost of keeping one unit of cash from now to infinity. | Interpretation behind condition (13) linking running costs to proportional costs. |
| CR-Q10 | 630 | In this section and under additional assumptions we explicitly solve for the policy parameters and perform a comparative statics analysis. | Comparative statics only in special cases. |
| CR-Q11 | 631 | As expected, the boundaries lie farther away from the origin the higher the transaction cost | Symmetric no-fixed-cost case: the band widens with k. |
| CR-Q12 | 631 | We next consider the case where costs are not symmetric and there are fixed as well as proportional transactions costs. | Asymmetric case with both cost types. |
| CR-Q13 | 631 | For example, the average cost rate is an increasing function of all cost parameters and the variance of the demand for cash | Comparative statics of the average cost rate γ. |
| CR-Q14 | 631 | because the value function tends to infinity. Therefore, we consider this solution as an approximation when | Stated limitation of the β → 0 explicit solution. |
| CR-Q15 | 631 | the policy parameter u is an increasing function | The upper trigger rises in the penalty p and in the fixed cost K+, among others, in the approximation with no drift and vanishing discounting (the list of parameters is garbled in the OCR and read from the page image). |

## Lean results

| Name | Statement |
|---|---|
| `Constantinides.gap_area` | The area between $V'$ and the proportional cost on $[U,u]$, for $V'-k^-=(h/\sigma^2)(x-U)(u-x)$, is $(h/\sigma^2)(u-U)^3/6$, so $(u-U)^3=6\sigma^2K^-/h$ as in their printed solution (with `F_deriv`). |
| `Constantinides.F_deriv` | The antiderivative used in `gap_area`. |
| `Constantinides.gap_rises_in_K` | A gap $g$ with $g^3=6\sigma^2K/p$ rises in $K$. |
| `Constantinides.gap_falls_in_p` | The same gap falls in $p$. |
| `Constantinides.thresholds_pin_pK` | Given $h$ and $\sigma$, $h(u+U)=-p(d+D)$ and $(D-d)^3=6\sigma^2K^+/p$ admit one pair $(p,K^+)$. |
| `Constantinides.control_printed_factor` | Their printed equation's factor $1/2$ is not the area's $1/6$. |
| `Constantinides.control_gap_alone_does_not_pin` | The gap alone fits two slopes, $(p,K^+)=(1,1/6)$ and $(2,2/6)$. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| CR-D1 | pp. 622, 626 | Their lower side maps onto the theory paper's: the penalty slope $p$ below zero to $\lambda_S-1$, the threshold $0$ to $R$, $K^+$ and $k^+$ to $K$ and $\kappa$, the trigger $d$ and target $D$ to $x_L$ and $y_{\text{post}}$. They also have a holding cost $h$ above zero, which the theory paper does not. | CR-Q1, CR-Q4, CR-Q5. |
| CR-D2 | p. 631 | With $K^-=0$ the upper trigger and target coincide, $u=U$, a reflecting barrier like the theory paper's dividend barrier. | Substitution in $(u-U)^3\propto K^-$. |
| CR-D3 | p. 631 | Their stated comparative static, $u$ increasing in $p$ and in $K^+$, parallels the theory paper's dividend barrier rising in both frictions. It holds only in the $\mu=0$, $\beta\to0$ approximation. | CR-Q12, CR-Q13 and the sentence after CR-Q13. |
| CR-D4 | p. 631 | From the printed $d=-\gamma/p-p^{-1}(3p^2\sigma^2K^+/4)^{1/3}$ with $\gamma$ increasing in $K^+$, the lower trigger falls in $K^+$. The paper states no direction for $d$ or $D$; this is my algebra. | Printed solution. |
| CR-D5 | p. 631 | The gap $D-d$ rises in $K^+$ and falls in $p$ (`gap_rises_in_K`, `gap_falls_in_p`). The printed equations give $(D-d)^3=2\sigma^2K^+/p$, the printed solution $6\sigma^2K^+/p$; the area computation (`gap_area`) gives the solution's 6, so the printed equation carries a typo. The directions hold under either. | Lean; the 250 dpi page image confirms both as printed. |
| CR-D6 | p. 631 | In their approximation the four thresholds identify the penalty and the fixed cost once $h$ and $\sigma$ are known (`thresholds_pin_pK`). The paper never states or uses this; it asks only existence and form. | Lean; Theorems 1 and 2 and Section 4 contain no identification claim. |
