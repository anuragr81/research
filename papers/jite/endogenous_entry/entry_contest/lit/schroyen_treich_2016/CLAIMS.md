# Claims about Schroyen & Treich

**Paper.** Fred Schroyen and Nicolas Treich, "The Power of Money: Wealth
Effects in Contests." Source read: TSE Working Paper 16-699. The WP series
cover page is dated September 2016; the manuscript itself is dated 11 April
2016. `LITERATURE.tex` describes it as "dated 11 April 2016", which is the
manuscript date, not the series date. Evidence level **[F]** (full read).

The published *Games and Economic Behavior* article supersedes this version
and has **not** been line-checked. Theorem numbers below are the WP's.

## Claims as stated in `LITERATURE.tex` (§ "The closest paper on the affordability channel")

| ID | Claim | Source locator |
|---|---|---|
| ST-1 | The three models are distinguished by whether rent and effort are commensurable with wealth inside `u`: privilege `U_i = u(w_i - x_i) + pi_i r`; ability `U_i = pi_i u(w_i + r) + (1-pi_i) u(w_i) - c(x_i)`; rent-seeking `U_i = pi_i u(w_i + r - x_i) + (1-pi_i) u(w_i - x_i)`. | Appendix A.4, A.5, A.6 |
| ST-2 | Theorem 3: in the symmetric privilege contest, the sign of the quadratic form is positive iff `2A(1 - m^2) > P`, where `A = -u''/u'` and `P = -u'''/u''`. | Theorem 3, eq. (A.16) |
| ST-3 | Under quadratic utility the condition reduces to `m < 1`. | Text following (A.16) |
| ST-4 | Under CARA the condition reduces to `m < 2^(-1/2) ~ 0.707`. | Text following (A.16) |
| ST-5 | Under CRRA with coefficient `gamma` the condition reduces to `gamma*(1/2 - m^2) > 1/2`. | Text following (A.16) |
| ST-6 | The result is **local**: a second-order effect of a *small* MPS evaluated at a symmetric equilibrium, obtained as a quadratic form in the Hessian of the best-response function (their Theorem 2). | Theorem 3 preamble; "invoke Theorem 2" |
| ST-7 | Its sign turns on the CSF decisiveness parameter `m` **and on the third derivative of `u`**. | (A.16) via `P` |
| ST-8 | Multiplying (A.16) by `(w-x)` replaces `A` and `P` by relative risk aversion and relative prudence. | Text following (A.16) |
| ST-9 | Our model is a *privilege* contest: entry cost paid out of resources, `V` separable and exogenous, so only the marginal-cost channel is live. | Comparison against `U_i = u(w_i - x_i) + pi_i r` |
| ST-10 | Making `V` money added to the resource would move the model to their *rent-seeking* contest, where the two wealth effects oppose and under CARA exactly cancel. | Rent-seeking model |
| ST-11 | They list an arbitrary number of players as an extension not undertaken. | Conclusion |
| ST-12 | **Separation claim.** P9-gen's sign rule needs no third derivative, no prudence condition, no decisiveness parameter and no functional-form restriction, and is global rather than a local second-order expansion. | Our own claim, contrasted with ST-2/6/7 |

## What is checkable here

ST-2 through ST-8 are arithmetic and are checked in `verify_st.py`.
ST-12 is checkable in the specific, falsifiable sense given as **ST-12a**:
*the truth value of (A.16) genuinely depends on the third derivative*, i.e.
there exist two utilities with the **same** `A` at a point but different `P`
that give opposite signs at the same `m`. If that failed, "no third
derivative" would be an empty distinction.

ST-1, ST-9, ST-10 and ST-11 are structural readings of functional forms and
prose. They are not arithmetic and are **not** given checks; ST-10's "exactly
cancel under CARA" is the one sub-claim that could be formalised later, and
is recorded in `NOTES.md` as not attempted.
