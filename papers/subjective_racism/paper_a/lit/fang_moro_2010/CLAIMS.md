# Fang and Moro (2010), claims

Evidence tag [F]: the full text of all 78 PDF pages of NBER working paper
15860 (April 2010) was read on 16 October 2026, with PDF pages 9, 27, 57
and 66 also read from 110 dpi images. Pages are PDF pages; PDF page `n` is
printed page `n − 4` from PDF page 5. The text layer keeps the ligatures `ﬁ`, `ﬀ`,
`ﬂ`, and quotations copy them as they are (FM-D7).

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| FM-Q1 | 5 | "But, sometimes the group characteristics can also be endogenously chosen, such as club membership or language." | The survey allows the marker to be chosen, the setting of A1's passing |
| FM-Q2 | 7 | "The concept of statistical discrimination has been applied mostly to labor market examples where employers discriminate against one group of workers." | The scope of the literature surveyed. Contact and social settings are not its subject |
| FM-Q3 | 8 | "on average workers of the two groups receive the same average wage, unless average productivity is assumed to be exogenously diﬀerent as in Case 1" | In Phelps's Case 2 a noisier signal gives no average penalty without a further ingredient |
| FM-Q4 | 8 | "This assumption can be rationalized assuming some communication of language barriers between employers and minorities, see, e.g., Lang (1996)." | Footnote 2. The only place the survey ties signal noise to the pair of employer and worker groups. The noise depends on the pair, not the employer's preference (FM-D5) |
| FM-Q5 | 9 | "Aigner and Cain (1977) proposed to incorporate employers' risk aversion into the standard Phelps' setup." | Section 2.2.1, the variance penalty through risk aversion |
| FM-Q6 | 9 | "Employers are compensated for the risk factor incorporated in each" | "... B worker's higher uncertainty about productivity, measured by the term cVar(q\|θ)/2". `condVar_strictMono_noise` |
| FM-Q7 | 10 | "Their observation was that if one group has a more informative signal, then this group's variance of the expected productivity is higher." | Cornell and Welch (1996). In selection at the top, the group with the less noisy signal wins, a variance effect in selection with no risk aversion |
| FM-Q8 | 29 | "This paper therefore shows how search friction might generate group inequality even when employers have perfect information about their workers and would strictly prefer to hire workers from the discriminated group." | Mailath, Samuelson and Shaked (2000). Firms search only one group, and the group penalty survives full information, the rival prediction to contribution 4 |
| FM-Q9 | 33 | "a skilled worker is more willing than an unskilled one to endure disutility from activity" | Fang (2001). A chosen, a priori irrelevant activity becomes a marker of skill in equilibrium |
| FM-Q10 | 34 | "This parameterization allows the model to investigate the relationship between integration and discrimination. It shows that in some cases integration makes it impossible for negative stereotypes to be sustained." | Chaudhuri and Sethi (2008). Integration enters through peer effects on the cost of skill, not through contact choices |
| FM-Q11 | 35 | "Altonji and Pierret (1997) proposed and implemented a test of statistical discrimination based on the eﬀect of race on worker wages over time with employer learning." | Footnote 12. The labour-market test that the group effect falls as the individual becomes known, the counterpart of contribution 4 (FM-D6) |
| FM-Q12 | 36 | "This paper generates endogenous segregation in a dynamic environment where partners randomly match to play a repeated prisoner's dilemma game." | Eeckhout (2006), the one marriage-market model in the survey |
| FM-Q13 | 58 | "Vertically diﬀerentiated colleges compete for desirable students using ﬁnancial aid and admission policies." | Epple, Romano and Sieg (2008). Venues that screen on ability with prices, with income and race in the colleges' objective |
| FM-Q14 | 68 | "everybody is paid a wage equal to the average productivity in the pool of workers." | Schwab (1986). A pool valued at its average type, with the marginal worker leaving it |
| FM-Q15 | 71 | "we still have a relatively poor theoretical understanding on the evolution of stereotypes" | The survey's own statement of a gap, outside A1's static scope |

## Lean results

`lean/mathlib/FangMoro2010.lean`, in the survey's notation.

| Theorem | What it checks |
|---|---|
| `FangMoro2010.eq1_weights_sum` | Eq. (1). The weights on the signal and on the group mean sum to one |
| `FangMoro2010.condVar_strictMono_noise` | Section 2.2.1. `Var(q\|θ) = σ_j²σ_εj²/(σ_j² + σ_εj²)` rises with the noise when `σ_j² > 0` |
| `FangMoro2010.control_condVar_zero_spread` | Control. With `σ_j² = 0` the conditional variance is zero whatever the noise |
| `FangMoro2010.post_strictMono_prior` | Eq. (4). The posterior rises with the group's investment rate `π` |
| `FangMoro2010.eq7_iff` | Eqs. (6) and (7). Assignment to the complex task iff `f_q/f_u ≥ (1 − π)x_u/(π x_q)` |
| `FangMoro2010.control_uninformative_signal` | Control. With `f_q = f_u` the posterior is the prior |
| `FangMoro2010.pool_variance` | The law of total variance for the pooled population of Section 7.1 |
| `FangMoro2010.control_survey_pooled_variance` | Control. The survey's `λ²σ_B² + (1 − λ)²σ_W²` is not the pooled variance (FM-D3) |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| FM-D1 | The version read is NBER working paper 15860 of April 2010, cited as such, not the Handbook chapter of 2011 | PDF page 1 says the survey "was prepared for the Handbook of Social Economics" and is forthcoming. The slug and key name the 2010 working paper. The published chapter is not read |
| FM-D2 | Aigner and Cain's utility is read as `a − b exp(−cq)` | The survey prints `U(q) = a + b exp(−cq)` (PDF p. 9, image) but the next line has `a − b exp[...]`. Aigner and Cain's eq. (7) has the minus sign (AC-D4) |
| FM-D3 | The pooled variance of Section 7.1 is `λσ_B² + (1 − λ)σ_W² + λ(1 − λ)(μ_B − μ_W)²` | The survey writes `σ² = λ²σ_B² + (1 − λ)²σ_W²` (PDF p. 66, image), the variance of a weighted sum of independent draws, not of the pooled population (`pool_variance`, `control_survey_pooled_variance`). The pooled conditional mean is also not linear in `θ` when the group means differ |
| FM-D4 | "Phelps (1973)" on PDF p. 8 is read as Phelps (1972) | The reference list has only Phelps (1972) |
| FM-D5 | "Lang (1996)" in footnote 2 is read as Lang (1986) | The reference list has Lang (1986), "A Language Theory of Discrimination". Lang's model itself is not read |
| FM-D6 | "Altonji and Pierret (1997)" in footnote 12 is read as Altonji and Pierret (2001) | The reference list has only the 2001 article |
| FM-D7 | Quotations keep the ligature characters of the text layer and avoid passages with mathematical symbols | The text layer sets mathematics in Unicode italic and keeps `ﬁ`, `ﬀ`, `ﬂ`, which the normaliser does not expand |
| FM-D8 | PDF p. 57 is read from the image | The page reproduces Figure 1 of Chan and Eyster (2003) over the text layer, so the extracted text interleaves the two. No quotation is taken from it |
| FM-D9 | The fourth value relation on PDF p. 27 is read with `V_W` on the left | It prints `V_B` twice; the coefficient `ρ_F(γ_I + 2γ_F)` and `Z_W` on the right belong to white workers |
| FM-D10 | The survey's accounts of the models it cites are its own readings | Only Aigner and Cain (1977) and Eeckhout (2006) among them are read in full in this bundle. A claim about any other cited model rests on this survey and carries the tag [C] |
