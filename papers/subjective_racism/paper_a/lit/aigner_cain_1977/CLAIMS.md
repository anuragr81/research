# Aigner and Cain (1977), claims

Evidence tag [F]: the full text of all 14 PDF pages was read on 16 October
2026, with PDF pages 3 and 8 also read from 110 dpi images for the
equations. Pages are PDF pages; PDF page `n` is journal page `173 + n`. The
cached text sets the two columns side by side, so each quotation is a
contiguous run within one column line, and the rest of the sentence is given
in the last column (AC-D1).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| AC-Q1 | 3 | "Employers can observe the test score, y," | The sentence continues "but they are interested in this only insofar as it gives them information about the unobservable variable, q". The unseen type is inferred from a signal and the group mean, eq. (2) |
| AC-Q2 | 4 | "employer behavior, "In an equilibrium the" | Quoting Spence, "In an equilibrium the subjective distribution and the one implied in the market mechanism are identical". The consistency of beliefs with outcomes that A1 imposes on imputed group standing |
| AC-Q3 | 4 | "bias. In particular, employers will not" | The group means "are estimated without bias. In particular, employers will not persist in believing that α^W > α^B if, in fact, α^W = α^B". An assumption justified by competition, not a derived fixed point |
| AC-Q4 | 7 | "or that the employer is risk-neutral with" | "This is equivalent to assuming that q enters the profit function linearly, or that the employer is risk-neutral with respect to q". With a linear payoff the dispersion of `q` given `y` is costless, `control_risk_neutral` |
| AC-Q5 | 7 | "is required to reflect risk aversion and to" | "only the conditional variance of q, written Var(q\|y) = Var(q)(1 − γ), is required to reflect risk aversion and to yield a theoretical explanation for economic discrimination". Footnote 15, `condVar` |
| AC-Q6 | 8 | "utility function will attempt to hire from" | "an employer with this utility function will attempt to hire from the group of workers that maximizes expected productivity, q, discounted for risk". A choice between groups penalised by the group's conditional variance, `eq8_order` |
| AC-Q7 | 8 | "This risk can arise from differing variances" | "in the distribution of q, or u, or both". The penalty is on `Var(q\|y)`, which mixes the group's own spread and the test's noise, footnote 18, `condVar_symm` |
| AC-Q8 | 8 | "symmetric with respect to Var(u) and Var(q) since," | Footnote 18, `Var(q\|y) = Var(q)Var(u)/(Var(q) + Var(u))`, `condVar_symm` |
| AC-Q9 | 8 | "to the larger R factor for blacks." | "the lower pay for blacks is entirely attributable to the larger R factor for blacks". Fig. 2, `fig2_black_paid_less` |
| AC-Q10 | 9 | "large to be rationalized by risk aversion." | The differential borne by black workers seems "too large to be rationalized by risk aversion". The authors' doubt about the size of the risk channel |
| AC-Q11 | 11 | "without recourse to risk aver" | The selection–truncation model "incorporates reliability differences but without recourse to risk aversion" (the word is broken at the column line). A penalty on the noisier group that does not need risk aversion |
| AC-Q12 | 11 | "tion is nullified because workers with low" | "The potential preference for blacks at the lower end of the y distribution is nullified because workers with low y scores are not hired". The truncation is what turns compression toward the mean into a penalty |
| AC-Q13 | 12 | "In the presence of perfect information" | "(γ = 1), the systematic difference in E(q\|y) or E(q\|q) for the two racial groups disappears". The labour-market form of contribution 4's prediction |
| AC-Q14 | 13 | "differential with risk aversion by" | "When we combine this reliability differential with risk aversion by employers, our model depicts economic discrimination". The conclusion ties the variance penalty to risk aversion |

## Lean results

`lean/mathlib/AignerCain1977.lean`, in the paper's notation. The last three
rows are contrasts with Paper A1's utility, not attributions to the paper.

| Theorem | What it checks |
|---|---|
| `AignerCain1977.gamma_mem` | Eq. (3). `0 < γ < 1` when both variances are positive |
| `AignerCain1977.condVar_symm` | Footnotes 15 and 18. `Var(q)(1 − γ) = Var(q)Var(u)/(Var(q) + Var(u))` |
| `AignerCain1977.eq8` | Footnote 17 and eq. (8). Under `q ~ N(m, v)`, `E[a − b e^{−cq}] = a − b e^{−cm + (c²/2)v}` |
| `AignerCain1977.eq8_order` | P. 181. For `b, c > 0`, eq. (8) ranks beliefs as `m − (c/2) v` does |
| `AignerCain1977.condVar_strictMono_Vu` | `Var(q\|y)` rises with `Var(u)` when `Var(q) > 0` |
| `AignerCain1977.fig2_black_paid_less` | Fig. 2. Equal means and `Var(q)`, `Var(u^B) > Var(u^W)`, `k > 0`: lower black pay at `y = α` |
| `AignerCain1977.zero_reliability` | With `γ = 0` the prediction is `α` and the risk factor is `k Var(q)` |
| `AignerCain1977.control_risk_neutral` | Control. With `k = 0` the two groups are paid the same at `y = α` |
| `AignerCain1977.control_no_spread` | Control. With `Var(q) = 0` there is no risk discount whatever `Var(u)` |
| `AignerCain1977.contrast_quadratic_ce` | Contrast. Under `v − κ(c − x)²`, expected utility is `v − κ((x̄ − c)² + σ²)` for any finite belief |
| `AignerCain1977.contrast_cara_common_ranking` | Contrast. Under `m − kV`, equal-variance groups are ranked by mean whatever `k` |
| `AignerCain1977.contrast_quadratic_ranking_depends_on_own_class` | Contrast. Under the quadratic loss, observers of class 0 and class 1 rank two equal-variance groups oppositely |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| AC-D1 | Quotations are limited to contiguous runs within one column line, and the rest of the sentence is given in the last column, as in PF-D1 | The cached text lays the two columns side by side line by line, so a sentence that crosses a line is interleaved with the other column |
| AC-D2 | Eqs. (1) to (8) and footnotes 15 to 18 are taken from the 110 dpi images of PDF pages 3 and 8, not from the text | The text renders eq. (7) as "U(q) = a becq" and eq. (8) as "a - bec E(qly)+c2/2 Var(qly)"; the image reads `U(q) = a − be^{−cq}` and `a − be^{−cE(q\|y)+c²/2 Var(q\|y)}` |
| AC-D3 | The step from eq. (8) to `max [E(q\|y) − k Var(q\|y)]` is read as monotonicity in the exponent, not as taking a logarithm | The paper says maximising `E[U(q\|y)]` "is equivalent to maximizing its logarithm". The logarithm of `a − be^{X}` is not `−X`. What holds is that `a − be^{X}` falls in `X` for `b > 0`, and `X = −c(E − (c/2)Var)` (`eq8_order`) |
| AC-D4 | The utility is `a − b e^{−cq}`, not `a + b exp(−cq)` | Fang and Moro (2010) restate eq. (7) with a plus sign (FM-D2). The image of PDF page 8 has the minus sign |
| AC-D5 | The penalty on the noisier group needs risk aversion only in the wage model, not in all of the paper's models | AC-Q5 and AC-Q14 tie the penalty to risk aversion, but the selection–truncation model obtains it "without recourse to risk aversion" (AC-Q11). A statement that the paper penalises a noisier group "only under risk aversion" holds for its risk model and its Phelps model, not for its truncation model |
| AC-D6 | `0 < γ < 1` is read with both variances positive | The paper writes "Clearly, 0 < γ < 1" (p. 3); `γ = 0` when `Var(q) = 0` and `γ = 1` when `Var(u) = 0` (`gamma_mem`) |
| AC-D7 | Footnote 27's account of Phelps's phrase is not checked | It concerns the wording of Phelps (1972), outside this source |
