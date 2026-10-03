"""Grounds for entry C.19 (7.2) of notes/tools/corrections_entries.py.

Sources and conventions.  Coffman, Exley and Niederle pages are those of the
Harvard Business School working paper of 28 February 2020 (the copy the author
linked), not the Management Science pages.  Canay, Mogstad and Mountjoy pages
are those of NBER Working Paper 27802 as revised in June 2023.  Lean statements
use the manuscript's parametrisation: alpha = P(A=0), beta = P(B=0), q0 and r0
the delivered credences on A=0 and B=0, omega the adoption weight (delta in
the code), Z = alpha beta (1-alpha)(1-beta).  Every quotation was read on the
page.
"""

GROUNDS = {}

# ---------------------------------------------------------------------------
# C.19  Conclusion, third implication: an existing design that asks a
#       difference question and reads decisions
# ---------------------------------------------------------------------------
GROUNDS["C.19"] = [
    {
        "kind": "quote",
        "source": r"Coffman, Exley and Niederle 2020 (working paper), Section 2.2, p. 8",
        "text": r"""``employers predict the difference in average scores between these groups of
workers on a 10-question easy quiz and a 10-question hard quiz. Employers could indicate
any feasible average difference (from -10 to 10 problems solved correctly)''""",
        "note": r"The belief question is a difference between groups, not a level for either.",
    },
    {
        "kind": "quote",
        "source": r"Coffman, Exley and Niederle 2020, fn 17, p. 16",
        "text": r"""``We code the probability of a female-even-month worker being hired as 1 if she
is hired with certainty, 0.5 if chance determines who is hired, and 0 if a
male-odd-month worker is instead hired with certainty.''""",
        "note": r"The outcome read is a decision rate.",
    },
    {
        "kind": "quote",
        "source": r"Coffman, Exley and Niederle 2020, Section 3.2, p. 16",
        "text": r"""``If employers neither engage in belief-based nor taste-based discrimination, we
expect that they should hire female-even-month workers 50\% of the time. This is not
the case. Employers in the Gender treatment only hire female workers in 43\% of
decisions, significantly below the 50\% benchmark.''""",
        "note": r"Discrimination is read from the decisions themselves, against a 50\% benchmark.",
    },
    {
        "kind": "quote",
        "source": r"Coffman, Exley and Niederle 2020, Section 3.2, p. 17",
        "text": r"""``Column 2 shows that beliefs are highly predictive of decisions: employers who
believe the performance gap is larger are less likely to hire the worker from the
lower-performing group.''""",
        "note": r"The believed difference and the decision rate are read together, which is the pairing the entry uses.",
    },
    {
        "kind": "quote",
        "source": r"Coffman, Exley and Niederle 2020, fn 5, p. 4",
        "text": r"""``This of course limits our ability to test the stereotype formation channel of
Bordalo et al. (2016). We focus instead on decision-making given a certain set of
beliefs.''""",
        "note": r"Belief formation, where the paper's mechanism sits, is set aside by their design; the match is in the form of the question only.",
    },
    {
        "kind": "theorem",
        "source": r"CoffmanExleyNiederle.lean, gap\_is\_conditional\_difference",
        "text": r"""For a belief $Q$ over group $a\in\{0,1\}$ and a $0$--$1$ score $b$, the difference of
the groups' mean scores equals the conditional difference,
\[
\frac{Q_{11}}{Q_{11}+Q_{10}}-\frac{Q_{01}}{Q_{01}+Q_{00}}
 = P_Q(B{=}1\mid A{=}1)-P_Q(B{=}1\mid A{=}0).
\]""",
        "note": r"A believed difference between groups has the form of the conditional difference, which Proposition PRO classifies.",
    },
    {
        "kind": "theorem",
        "source": r"PropPRO.lean, propPRO\_protection",
        "text": r"""If $\nabla F=x\,J+y\,\nabla\assoc$ at $q\otimes r$, then
$\langle\nabla F,\;M_\lambda(\alpha,\beta,q_0,r_0)\rangle=0$ identically in
$(\alpha,\beta,\lambda)$, where $M_\lambda$ is the first-order direction of
$\Pbar_\lambda-\PB$; the aggregate first-order coefficient of $F$ is zero.""",
        "note": r"Proposition PRO (i): a statistic whose differential at independence lies in $\mathrm{span}\{J,\nabla\assoc\}$ is second order.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_PRO.py, protected class statistic by statistic (59/59)",
        "text": r"""Symbolic in $\alpha,\beta,q_0,r_0$ and $\lambda$:
for $\Pbar(B{=}1\mid A{=}1)-\Pbar(B{=}1\mid A{=}0)$, $dF\in\mathrm{span}\{J,d\,\assoc\}$ and the
$c^0$ and $c^1$ coefficients of $F(\Pbar_\lambda)-F(\PB)$ vanish identically; for
$\Pbar(B{=}1\mid A{=}1)$ alone the $c^1$ coefficient is $\lambda(q_0-\alpha)r_0r_1/Z$, zero
only when $\lambda=0$ or $q_0=\alpha$.""",
        "note": r"The difference question is second order; the level question it is built from is first order.",
    },
    {
        "kind": "theorem",
        "source": r"Decision.lean, volume\_flipSet",
        "text": r"""For every $c,\delta\in\mathbb R$, the set of evaluators whose action the sequence
changes, $\{u:\ \neg((0\le u)\leftrightarrow(0\le u+c\delta))\}$, has Lebesgue measure
exactly $|c\,\delta|$.""",
        "note": r"The band of changed decisions has width $|c\delta|$, which with a density positive at the threshold makes the share first order (Proposition SHR).",
    },
    {
        "kind": "theorem",
        "source": r"Ladder.lean, condDiff\_eq, ladder\_condDiff\_seqEffect, ladder\_condDiff\_coeff, ladder\_condDiff\_coeff\_eq\_zero\_iff; sympy/verify\_ladder.py, row (B) (99/99)",
        "text": r"""Under partial adoption of the second cue with weight $\omega$, the conditional
difference equals $\assoc/(m_{A0}m_{A1})$, so each route's is $c$ times its association factor
over its $A$-marginals, exactly; at $c=0$ the difference of those factors is
\[
\frac{(1-\omega)(r_0-\beta)(t_0-r_1)}{Z},\qquad t_0=(1-\omega)\beta+\omega r_0,
\]
zero exactly when $\omega=1$, $r_0=\beta$, or $t_0=1-r_0$.""",
        "note": r"The entry's claim is a full-adoption claim: a partly adopting population would in general not pass the difference audit.",
    },
    {
        "kind": "quote",
        "source": r"Canay, Mogstad and Mountjoy 2023 (NBER WP 27802, rev.), after Theorem 4.1, p. 21",
        "text": r"""``First, the marginal outcome test may conclude bias even if the judge is racially
unbiased. Second, the outcome test may conclude no bias even if the judge is locally
or globally racially biased.''""",
        "note": r"The form of the entry's last sentence, an audit that cannot rule out what it is meant to detect, has this precedent in economics; the paper does not yet cite it.",
    },
]
