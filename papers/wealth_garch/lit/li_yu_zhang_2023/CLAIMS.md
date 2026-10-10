# Claims: Li, Yu and Zhang (2023)

Bib key `li2023`. Version read: arXiv:2108.02648v4 (28 Feb 2023). Evidence [F].

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| LYZ-Q1 | 13 | our asymptotic limits differ significantly from the ones in the Merton's problem, which now sensitively depends on the reference degree parameter | In an unconstrained loss-averse problem, preference reaches the large-wealth limit of the control (L4, C2). |
| LYZ-Q2 | 13 | our asymptotic results will coincide with the ones in the standard Merton's problem under the power utility | The limits reduce to Merton's when the reference vanishes. |
| LYZ-Q3 | 1 | We consider the concave envelope of the utility with respect to consumption, allowing us to focus on an auxiliary HJB variational inequality on the strength of concavification principle and dynamic programming arguments. | An S-shaped utility needs a concave envelope (H5). |
| LYZ-Q4 | 3 | The utility is an S-shaped function on R. | As LYZ-Q3. |

## Lean results

| Name | Statement |
|---|---|
| `LiYuZhang.roots_sum_prod` | Two distinct roots of $\eta^2-\eta-c=0$ have sum 1 and product $-c$. |
| `LiYuZhang.merton_portfolio_limit` | With $r_1r_2=-2r/\kappa^2$, $\kappa=(\mu-r)/\sigma$, $\gamma_1=\beta_1/(\beta_1-1)$: $2r(\gamma_1-1)/((\mu-r)r_1r_2)=(\mu-r)/(\sigma^2(1-\beta_1))$ (p. 34). |
| `LiYuZhang.limitTerm_free_of_k` | The term $\frac{k}{\beta_2}\lambda^{\beta_2}\mathbf 1_{\beta_2=\beta_1}+\frac{(1-\lambda)^{\beta_1}}{\beta_1}$ of the limits on p. 33 does not depend on $k$ when $\beta_2\ne\beta_1$. |
| `LiYuZhang.U_homogeneous` | With $\beta_1=\beta_2=\beta$, $U(tx)=t^\beta U(x)$ for $t>0$ (the homogeneity behind Remark 4.2). |
| `LiYuZhang.control_limitTerm_depends_on_k_when_equal` | At $\beta_1=\beta_2=1/2$ the term changes with $k$. |
| `LiYuZhang.control_roots_need_distinct` | $a=b=0$, $c=0$ solve both equations with $a+b\ne1$. |
| `LiYuZhang.control_U_not_concave` | At $\beta_1=\beta_2=1/2$, $k=1$, $U$ is not concave. |

## Readings recorded

| ID | Where | Reading adopted | Reason |
|---|---|---|---|
| LYZ-D1 | p. 13 against pp. 33 to 34 | The limits $L_1$, $L_2$ depend on $k$ only when $\beta_1=\beta_2$, through $k\,\mathbf 1_{\beta_2=\beta_1}$ (case $y_1=y_2$) and through $w(1)$ (case $y_1>y_2$, $\beta_2=\beta_1$). For $\beta_2\ne\beta_1$ they are free of $k$. | Remark 4.1 says the limits "sensitively" depend on $k$. The formulas of Section 5.6 contain $k$ only in those places (`LiYuZhang.limitTerm_free_of_k` and its control). Only the paper's dependence on $\lambda$, $\beta_1$, $\beta_2$ holds in general. |
| LYZ-D2 | p. 10 | Assumption (A1) reads $\beta_j<-r_2/r_1$. | The fraction is garbled in the text layer, and the next sentence is printed with the indices swapped. The reading is the one the stated consequence $r_1\beta_j+r_2<0$ needs. |
| LYZ-D3 | pp. 3 and 5 | $\lambda\in(0,1)$. | The introduction allows $\lambda\in(0,1]$, Section 2.1 and every result use $0<\lambda<1$. |
| LYZ-D4 | p. 34 | Merton's limit $\pi^*/x=(\mu-r)/(\sigma^2(1-\beta_1))$ follows from $r_1r_2=-2r/\kappa^2$. | `LiYuZhang.merton_portfolio_limit` with `LiYuZhang.roots_sum_prod`. |
