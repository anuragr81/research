#!/usr/bin/env python3
"""Grounds for the related-literature entries C.11 (nine parts, with C.15
inside part 9), C.13 and C.14.

Every quote was checked against the local copy of the paper on 2026-09-30:
text-layer papers by grep on the page's pdftotext output; scans (Phelps 1972,
Arrow 1973, Good-Mittal 1987, Becker 1962, FGT 1984 eq. (3)) on the rendered
page.  Pages are as printed on the page.  Arrow is the 1971 Princeton working
paper (WP p. n = PDF page n+2 in the local copy); BIR is the January 2019 working paper (printed
page = PDF page - 1); Tao is the author's preliminary version ("pre-p. N").
Theorem statements follow the Lean hypotheses.  C.12 lists bibliography
entries and has nothing to ground.
"""

GROUNDS = {}

# ---------------------------------------------------------------------------
GROUNDS["C.11"] = [
    # ---- part 1: BHW / Banerjee -------------------------------------------
    dict(kind="quote",
         source=r"Bikhchandani, Hirshleifer and Welch 1992, p. 1000",
         text=r"""``DEFINITION. An informational cascade occurs if an individual's
action does not depend on his private information signal. If an individual $i$
is in a cascade, then his action conveys no information and individual $i+1$
draws the same inference from all previous actions.''""",
         note=r"""(part 1, BHW) A cascade is defined by the action's silence about
the signal; the action is binary (adopt or reject, p. 996), which is where
``coarse'' belongs."""),
    dict(kind="quote",
         source=r"Banerjee 1992, p. 809",
         text=r"""``The key reason why we get a different result is that in our
model the choices made by agents are not always sufficient statistics for the
information they have. \ldots It is evident that, for a wide range of signals,
the agents in our model will always choose the same option; this lack of
invertibility is what causes the sufficient property to fail.''""",
         note=r"""(part 1, Banerjee) The same mechanism as BHW, on a continuum of
options, so ``on the other hand'' has no contrast to mark."""),
    dict(kind="theorem",
         source=r"BHW.lean, lik\_ratio (with cascade\_uninformative)",
         text=r"""For every $p$ and every history $h$ with
$d(h) = \#\mathrm{adopt} - \#\mathrm{reject}$,
$L_1(h)\,c_0(d) = L_0(h)\,c_1(d)$, so the public likelihood ratio
$L_1/L_0$ is a function of $d$ alone: $1$ at $d=0$, $p/(1-p)$ at $d=1$,
$p(1+p)/((1-p)(2-p))$ at $d\ge 2$, and the reciprocals at $d=-1$, $d\le-2$.""",
         note=r"""(part 1, BHW) Two adoptions carry less than two $H$ signals,
since $p(1+p)/((1-p)(2-p)) < (p/(1-p))^2 \iff 1 < 2p$; and the ratio is
constant once $|d|\ge 2$, so later actions convey no private signal."""),
    # ---- part 2: the marginals analogy -------------------------------------
    dict(kind="quote",
         source=r"Bikhchandani, Hirshleifer and Welch 1992, p. 996",
         text=r"""``The gain to adopting, $V$, is also the same for all
individuals and is either zero or one, with equal prior probability 1/2.''""",
         note=r"""(part 2, marginals analogy) One binary state, so the sequence
effect in BHW falls on a belief about one variable, the counterpart of a
marginal."""),
    dict(kind="quote",
         source=r"Banerjee 1992, p. 802",
         text=r"""``Let us assume that there is a unique $i^{*}$ such that
$z(i) = 0$ for all $i \neq i^{*}$ and $z(i^{*}) = z$, where $z > 0$. \ldots
Of course, everybody, given these payoffs, would want to invest in $i^{*}$.
The trouble is no one knows which one it is.''""",
         note=r"""(part 2, marginals analogy) One unknown, the identity of
$i^{*}$; there is no second attribute and no association for the sequence
effect to fall on, which is the analogy the AFTER keeps."""),
    # ---- part 3: Dietrich ---------------------------------------------------
    dict(kind="quote",
         source=r"Dietrich 2021, Definition 1, p. 7",
         text=r"""``Definition 1 An EU preference aggregation rule \ldots is
linear-geometric if there exist individual weights \ldots such that for each
preference profile \ldots the group preference relation \ldots has \ldots
probability function \ldots given on states by \ldots up to a multiplicative
constant.''""",
         note=r"""(part 3, Dietrich) Definition 1 defines a rule on preference
profiles, so it is preference aggregation; the belief-pooling statement is
Theorem 2, which the AFTER cites."""),
    dict(kind="quote",
         source=r"Dietrich 2021, Theorem 2, p. 11",
         text=r"""``Theorem 2 A belief aggregation rule (on the domain of coherent
belief profiles) is dynamically rational, unanimity-preserving, and continuous
if and only if it is geometric.''""",
         note=r"""(part 3, Dietrich) The criterion is Dynamic Rationality, defined
on p. 16 as ``When information is learnt by everyone, the new group beliefs
equal the old ones conditional on the information''."""),
    dict(kind="theorem",
         source=r"Dietrich.lean, linPool\_dynRational\_on\_common\_prior",
         text=r"""Let $w_i \ge 0$ with $\sum_i w_i = 1$, let $p_i = \pi$ for every
$i$, and let $E$ be any event. Then
$\sum_i w_i\, p_i(\cdot \mid E) = \big(\sum_i w_i\, p_i\big)(\cdot \mid E)$.""",
         note=r"""(part 3, Dietrich) With a common prior the premise of Dynamic
Rationality forces unanimity, and linear pooling passes wherever the criterion
applies; Paper B's evaluators do not condition on a common event, so the
criterion is silent on them."""),
    # ---- part 4: Epstein / Ortoleva / Cripps -------------------------------
    dict(kind="quote",
         source=r"Epstein 2006, abstract, p. 413",
         text=r"""``The main result is a representation theorem that generalizes
(the dynamic version of) Anscombe--Aumann's theorem so that both the prior and
the way in which it is updated are subjective.''""",
         note=r"""(part 4, Epstein) What Epstein makes subjective is the
updating rule, not only the prior; nothing in the abstract concerns the
sequence of several updates."""),
    dict(kind="quote",
         source=r"Epstein 2006, p. 416",
         text=r"""``There are three periods---an ex ante stage 0, an interim period
1 when a signal $s_1 \in S_1$ is realized, and period 2 when the remaining
uncertainty is resolved through realization of some $s_2 \in S_2$.''""",
         note=r"""(part 4, Epstein) One interim signal, so the model has no
sequence of updates whose invariance could be at issue."""),
    dict(kind="quote",
         source=r"Ortoleva 2024, p. 546 and footnote 2",
         text=r"""``we do not discuss updating \ldots when subjects receive more
general types of information, such as vague, ambiguous, or imprecise
information or information of the form `Event A is more likely than event
B';'' with footnote 2: ``These cases have been extensively studied in several
disciplines and include classical approaches such as Jeffrey's rule or the AGM
theory of belief revisions''.""",
         note=r"""(part 4, Ortoleva) Jeffrey's rule is outside the survey's
scope, so the Hypothesis Testing model is not a theory of Jeffrey-type
sequence effects."""),
    dict(kind="quote",
         source=r"Ortoleva 2024, Section 4.1, p. 559",
         text=r"""``When new information $A$ arrives, individuals first test to
determine if they were using the right prior, using the threshold
$\epsilon$. If $A$ is not unlikely given $\pi$, that is, $\pi(A) > \epsilon$,
then $\pi$ is kept and updated following Bayes' rule \ldots when the new
information was unexpected given the prior, $\pi(A) \le \epsilon$, then the
prior is questioned''.""",
         note=r"""(part 4, Ortoleva) The departure is triggered by one unexpected
event, the AFTER's wording; the survey's restatement of Ortoleva (2012) is
the only source in hand."""),
    dict(kind="quote",
         source=r"Cripps 2021, p. 9 (axioms named on pp. 7--9)",
         text=r"""``Furthermore, symmetry implies that the order in which the
signals are revealed can be changed without an effect on the ultimate beliefs.
Hence, these axioms imply that reversing the order in which two signals arrive
has no effect on the ultimate beliefs.''""",
         note=r"""(part 4, Cripps) His four axioms are Axiom 1 (Uninformativeness)
and Axiom 2 (Symmetry), p. 7, Axiom 3 (Divisibility), p. 8, and Axiom 4
(Non-Dogmatic), p. 9; order invariance follows from Symmetry and
Divisibility."""),
    dict(kind="theorem",
         source=r"Cripps.lean, order\_invariance",
         text=r"""Let $U$ satisfy Symmetry and Divisibility, let $\mu$ have full
support, and let $x, y : \Theta \to (0,1)$. Writing $u(\mu,x)$ for the
first-signal update of $\mu$ on the binary experiment $(x, 1-x)$,
$u(u(\mu,x),y) = u(u(\mu,y),x)$.""",
         note=r"""(part 4, Cripps) The p. 9 remark as a theorem, with no Axiom 1
or 4 needed: two conditionally independent signals with fixed likelihoods
give the same posterior in either sequence."""),
    dict(kind="theorem",
         source=r"Cripps.lean, composite\_AB\_eq\_bayes",
         text=r"""Let $\mu$ be a full-support belief on $A \times B$, $q > 0$ with
$\sum_a q_a = 1$ and $\sum_b r_b = 1$. Then
$J_B\big(J_A(\mu,q),r\big) = \mathrm{Bayes}(\mu, \ell)$ with
$\ell(a,b) = \dfrac{q_a}{\mu_A(a)} \cdot \dfrac{r_b}{\big(J_A(\mu,q)\big)_B(b)}$.""",
         note=r"""(part 4, Cripps) Under Proposition IMM's reading the composite
is one Bayes update, a divisible rule meeting all four axioms, whose second
likelihood is matched to the intermediate belief; the other sequence feeds a
different experiment, so no axiom fails."""),
    dict(kind="theorem",
         source=r"Cripps.lean, rigid\_not\_uninformative",
         text=r"""For every $\varphi$, the rigid rule
$(\mu, p, s) \mapsto J_A\big(\mu, \varphi(p_s)\big)$ violates Axiom 1
(Uninformativeness): an experiment with $p_s(\theta) = p_s(\theta')$ for all
$\theta, \theta'$ still resets the $A$-marginal.""",
         note=r"""(part 4, Cripps) Under the other reading, delivered credence
fixed whatever the prior, the composite fails Uninformativeness (and
Non-Dogmatic), so ``of the four axioms only divisibility'' is wrong on either
reading."""),
    # ---- part 5: Pettigrew-Weisberg ----------------------------------------
    dict(kind="quote",
         source=r"Pettigrew and Weisberg 2025, p. 3, Equation (1)",
         text=r"""``But we'll see that commutativity instead favours upco, also
known as multiplicative pooling:
$P'(E) = \dfrac{P(E)\,Q(E)}{P(E)\,Q(E) + P(\overline{E})\,Q(\overline{E})}$. (1)''""",
         note=r"""(part 5, Pettigrew-Weisberg) The pooling step is multiplicative
and takes the prior $P(E)$ as one of its two arguments."""),
    dict(kind="quote",
         source=r"Pettigrew and Weisberg 2025, pp. 3--4",
         text=r"""``Suppose we begin with $P$, fix some pooling rule $f$, and use the
following two-step procedure for responding to $Q$'s opinion about $E$.
\ldots Step 1. Apply pooling rule $f$ to $P(E)$ and $Q(E)$ to obtain
$P'(E)$'' (p. 3); ``Begin with the case where $P$ pools with $Q$ first. Step 1
of Jeffrey pooling combines $P(E) = 4/10$ with $Q(E) = 8/10$ via upco, to
yield $P'(E) = 8/11$'' (p. 4).""",
         note=r"""(part 5, Pettigrew-Weisberg) The prior is pooled with each
source's opinion; successive inputs are never pooled with each other."""),
    dict(kind="theorem",
         source=r"PettigrewWeisberg.lean, upco\_eq\_self\_iff",
         text=r"""For $0 < p, q < 1$,
$\dfrac{pq}{pq + (1-p)(1-q)} = q \iff p = \tfrac12$.""",
         note=r"""(part 5, Pettigrew-Weisberg) Pooling the prior with a delivered
credence returns that credence only from an even prior, so their operation
is not the manuscript's marginal reset."""),
    dict(kind="theorem",
         source=r"PettigrewWeisberg.lean, PB\_is\_upco\_pooling",
         text=r"""Let $P$ be regular on the cells $E_i \cap F_j$ and $x_i, y_j > 0$.
With the matched opinions $m_E(x)_i \propto x_i / P(E_i)$ and
$m_F(y)_j \propto y_j / P(F_j)$, Jeffrey pooling $P$ with upco on $E$ using
$m_E(x)$ and then on $F$ using $m_F(y)$, in either sequence, gives
$\PB(\omega) \propto P(\omega)\, \dfrac{x_{E(\omega)}}{P(E_{E(\omega)})}\,
\dfrac{y_{F(\omega)}}{P(F_{F(\omega)})}$.""",
         note=r"""(part 5, Pettigrew-Weisberg) Their commuting procedure, fed the
likelihoods matched to the prior, is Paper B's benchmark; sequence dependence
belongs to the delivered credences."""),
    # ---- part 6: Phelps / Arrow --------------------------------------------
    dict(kind="quote",
         source=r"Phelps 1972, p. 659, Equation (1)",
         text=r"""``The employer is able to measure the performance of each
applicant in some kind of test, $y_i$, which, after suitable scaling, may be
said to measure the applicant's promise or degree of qualification, $q_i$,
plus an error term, $\mu_i$. (1) $y_i = q_i + \mu_i$ where $\mu$ is normally
distributed with mean zero.''""",
         note=r"""(part 6, Phelps) Qualification is a continuous variable read
through a normal error and a least-squares predictor (pp. 659--660); the only
binary variable is the race dummy, so Phelps supplies no binary-attribute
frame."""),
    dict(kind="quote",
         source=r"Arrow 1973, Section 4, working paper p. 26",
         text=r"""``Only some workers, however, are qualified to hold skilled jobs.
\ldots The employer cannot know of any given worker whether or not he is
qualified; however, he does believe that the probability that a random W
worker is qualified is $p_W$ and that a random B worker is qualified is
$p_B$.''""",
         note=r"""(part 6, Arrow) Binary qualification and two groups, with a
belief $p_W, p_B$ per group; this is the binary-attribute frame, and Arrow
stays in the AFTER (the local copy is the 1971 working paper, hence its
pagination)."""),
    dict(kind="theorem",
         source=r"CoateLoury.lean, negativeStereotype\_iff\_cov\_pos",
         text=r"""For $0 < \lambda < 1$ (the share of W's),
$\pi_b < \pi_w \iff 0 < \mathrm{Cov}(W, q)$, where
$\mathrm{Cov}(W,q) = \lambda\pi_w - \lambda\big(\lambda\pi_w + (1-\lambda)\pi_b\big)
= \lambda(1-\lambda)(\pi_w - \pi_b)$.""",
         note=r"""(part 6, Coate-Loury) A negative stereotype in the lineage the
AFTER cites is a believed positive association between group and
qualification, the binary frame the manuscript borrows."""),
    # ---- part 7: BCGS -------------------------------------------------------
    dict(kind="quote",
         source=r"Bordalo, Coffman, Gennaioli and Shleifer 2016 (May 2015 WP), p. 12",
         text=r"""``The DM has stored in memory the full conditional distribution
\ldots but he assesses this distribution by recalling only a limited and
selected set of types. Selective recall is driven by representativeness,
formalized below following GS (2010).''""",
         note=r"""(part 7, BCGS) One mechanism, representativeness-driven selective
recall, not ``representativeness-distortion or selective recall''; nothing
in it is a sampling distortion."""),
    dict(kind="quote",
         source=r"Bordalo, Coffman, Gennaioli and Shleifer 2016 (May 2015 WP), Section 4.3, p. 27",
         text=r"""``Again, there is a kernel of truth in these stereotypes, but also
an exaggeration of the correlation between education and being on welfare:
people neglect that most elements of the less educated group are not on
welfare''.""",
         note=r"""(part 7, BCGS) BCGS also produce an exaggerated cross-attribute
association, so the contrast rests on where it arises, across groups, and on
the mechanism, not on the concept."""),
    dict(kind="theorem",
         source=r"BCGS.lean, welfare\_within\_group",
         text=r"""With $\mathrm{cov}(p_{00},p_{01},p_{10},p_{11})
= p_{11} - (p_{10}+p_{11})(p_{01}+p_{11})$:
$\mathrm{cov}(0, \tfrac{9}{11}, 0, \tfrac{2}{11}) = 0$, while the true
within-group law has $\mathrm{cov}(\tfrac{21}{50}, \tfrac{9}{50},
\tfrac{9}{25}, \tfrac{1}{25}) = -\tfrac{6}{125}$.""",
         note=r"""(part 7, BCGS) In the Lean instance of the Section 4.3 example (BCGS give
no numbers there) the $d=2$ stereotype of a group recalls only welfare types, so the within-group education-welfare
association is zero; the exaggerated correlation lives in the population
pooled across groups."""),
    # ---- part 8: Becker -----------------------------------------------------
    dict(kind="quote",
         source=r"Becker 1962, p. 7",
         text=r"""``This analytical statement must be distinguished from the
frequently encountered arithmetical statement that a market would behave
rationally even if only a few households did \ldots Our statement goes beyond
arithmetic and stems from an analysis of the responses of rational and
irrational households.''""",
         note=r"""(part 8, Becker) Becker sets his result against the arithmetic
of aggregation, the opposite of ``protection intrinsic to the arithmetic of
aggregation''."""),
    dict(kind="quote",
         source=r"Becker 1962, p. 10",
         text=r"""``In both cases the word `survive' simply refers to a resource
constraint on behavior and does not literally distinguish `life' from
`death,' although some households and firms may actually die from trying to
`live' beyond their means.''""",
         note=r"""(part 8, Becker) His mechanism is a constraint on behaviour, so
``rather than imposed by an enforced constraint on behaviour'' inverts
him."""),
    dict(kind="quote",
         source=r"Becker 1962, p. 12",
         text=r"""``Indeed, the most important substantive result of this paper is
that irrational units would often be `forced' by a change in opportunities to
respond rationally.''""",
         note=r"""(part 8, Becker) Becker's own summary of the paper: rational
market responses are forced by the opportunity set."""),
    dict(kind="theorem",
         source=r"Becker.lean, constraint\_without\_averaging and impulsive\_mean",
         text=r"""With $I = p_2 = 1$ and $p_1$ rising from $1$ to $11/10$, the theorem
states that some $x \in [0, 1]$ and $x' \in [0, 10/11]$ have $x < x'$ (the
proof takes $x = \tfrac{1}{10}$, $x' = \tfrac{9}{10}$): one impulsive household
can buy more after the rise. If $X$ is
uniform on $[0, I/p_1]$ then $\mathbb{E}X = I/(2p_1)$.""",
         note=r"""(part 8, Becker) The downward market slope needs both
ingredients, the budget constraint through the support $[0, I/p_1]$ and
averaging over many households; neither alone delivers it."""),
    dict(kind="computation",
         source=r"grep on PAPER\_B\_MANUSCRIPT.tex, Concluding remarks (lines 873--939)",
         text=r"""Case-insensitive search of the concluding section for ``Becker'',
``constraint'', ``arithmetic'', ``enforced'', ``imposed'' and ``intrinsic'':
zero hits. The manuscript's only mention of Becker is line 255, the sentence
under correction.""",
         note=r"""(part 8, Becker) The ``lineage distinction the concluding section
trades on'' is not drawn there, so the forward reference dangles and the
AFTER deletes the sentence."""),
    # ---- part 9: Good-Mittal, and C.15 (Heckman / BIR) -----------------------
    dict(kind="quote",
         source=r"Good and Mittal 1987, Definition 1.1, p. 695",
         text=r"""``DEFINITION 1.1. We say that the amalgamation (or aggregation)
paradox, or simply the paradox, occurs if
$\max_i \alpha(\mathbf{a}_i) < \alpha(\mathbf{A})$ or
$\alpha(\mathbf{A}) < \min_i \alpha(\mathbf{a}_i)$.'' \ldots ``in Yule's
formulation the subpopulations each had zero association in which case there
is no reversal of sign. Hence we prefer the name `amalgamation paradox.'\,''""",
         note=r"""(part 9, Good-Mittal) The paradox covers amplification beyond
the maximum and association created from none, not only ``erased or
reversed''."""),
    dict(kind="quote",
         source=r"Good and Mittal 1987, pp. 695--696",
         text=r"""``a drug can be judged to be beneficial, as measured by $\alpha$,
for both men and women considered separately, but can seem to be harmful for
the population at large, by looking only at the amalgamated table. This can
happen even though $N_i \propto p_i$. We claim that such a situation can
arise only if not enough care is used in the design of the experiment.''""",
         note=r"""(part 9, Good-Mittal) Population-share weights do not prevent
the paradox; its cause is uneven treatment allocation across
subpopulations."""),
    dict(kind="theorem",
         source=r"GoodMittal.lean, equalSize\_reversal",
         text=r"""For $\mathbf{a}_1 = [8, 2; 60, 30]$ and $\mathbf{a}_2 = [20, 70; 1, 9]$,
$N_1 = N_2 = 100$, $\pi_R(\mathbf{a}_1) = \tfrac{2}{15} > 0$,
$\pi_R(\mathbf{a}_2) = \tfrac{11}{90} > 0$, but
$\pi_R(\mathbf{a}_1 + \mathbf{a}_2) = -\tfrac{33}{100} < 0$, where
$\pi_R = \dfrac{a}{a+b} - \dfrac{c}{c+d}$.""",
         note=r"""(part 9, Good-Mittal) Two equally large subpopulations, the
treatment beneficial in each and harmful in the aggregate: the p. 696 remark
as an instance."""),
    dict(kind="theorem",
         source=r"GoodMittal.lean, piR\_amalg\_general",
         text=r"""For positive tables $\mathbf{a}_i$ with $\mathbf{A} = \sum_i \mathbf{a}_i$,
$\pi_R(\mathbf{A}) = \dfrac{\sum_i (a_i+b_i)\,\frac{a_i}{a_i+b_i}}{\sum_i (a_i+b_i)}
- \dfrac{\sum_i (c_i+d_i)\,\frac{c_i}{c_i+d_i}}{\sum_i (c_i+d_i)}$.""",
         note=r"""(part 9, Good-Mittal) Each row averages the subpopulation rates
with its own row totals, not with $N_i/N$; the paradox is driven by the
difference between those two weightings, that is, by uneven treatment
allocation."""),
    dict(kind="quote",
         source=r"Heckman 1998, p. 109",
         text=r"""``Assumption b is a strong one, and it is the key identifying
assumption of the audit method as currently practiced. Nothing guarantees
that it will be satisfied.''""",
         note=r"""(part 9, C.15 Heckman) ``nothing guarantees'' is on p. 109, and
assumption b is equal means of unobserved productivity across groups, as the
AFTER glosses it."""),
    dict(kind="quote",
         source=r"Heckman 1998, p. 102",
         text=r"""``The audit method can find discrimination when in fact none
exists; it can also disguise discrimination when it is present.''""",
         note=r"""(part 9, C.15 Heckman) The p. 102 sentence is Heckman's general
summary; the AFTER attributes no sample-size claim to him."""),
    dict(kind="quote",
         source=r"Bohren, Imas and Rosenberg 2019, working paper p. 1",
         text=r"""``As prior work has noted, it is difficult to identify the
underlying source of discrimination from such static settings, as different
sources generate the same patterns of observable behavior (Fang and Moro
2011).''""",
         note=r"""(part 9, C.15 BIR) BIR credit the sentence to Fang and Moro, so
the AFTER paraphrases it (``as they note'') rather than quoting it as
theirs."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO\_protection",
         text=r"""For all real $x, y$ and all $(\alpha, \beta, \lambda, q_0, r_0)$,
$\langle xJ + y\,\nabla\assoc(q \otimes r),\; M_\lambda \rangle = 0$, where
$M_\lambda = \lambda\kappa R_1 + (1-\lambda)\kappa' R_2$,
$R_1 = q \otimes (1,-1)$ and $R_2 = (1,-1) \otimes r$.""",
         note=r"""(part 9, C.15) The first-order coefficient of a protected
statistic vanishes identically in the prior and in the mix, so it is zero in
the population itself; its silence is a failure of identification, which no
sample size repairs."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.13"] = [
    dict(kind="quote",
         source=r"Foster, Greer and Thorbecke 1984, p. 763, Equation (3)",
         text=r"""``For each $\alpha \ge 0$, let $P_\alpha$ be defined by (3)
$P_\alpha(y; z) = \dfrac{1}{n} \sum_{i=1}^{q} \Big(\dfrac{g_i}{z}\Big)^{\alpha}$.''""",
         note=r"""Every member divides by the population size $n$ and by the line
$z$; the sum runs over the $q$ poor only."""),
    dict(kind="quote",
         source=r"Foster, Greer and Thorbecke 1984, p. 763",
         text=r"""``The measure $P_0$ is simply the headcount ratio $H$, while
$P_1$ is $H \cdot I$, a renormalization of the income-gap measure. The
measure $P$ is obtained by setting $\alpha = 2$.''""",
         note=r"""$P_0$ is a ratio, not a count, and $P_1$ is the income-gap
measure renormalised, not the ``average of their distances'' among the
poor; FGT never say ``incidence'' or ``intensity''."""),
    dict(kind="theorem",
         source=r"FGT.lean, P1\_eq\_popMean\_normGap",
         text=r"""For any population $s$, incomes $y$ and line $z$,
$P_1(y; z) = \dfrac{1}{|s|} \sum_{i \in s} \dfrac{\max(z - y_i, 0)}{z}$.""",
         note=r"""$P_1$ is the whole-population mean of the normalised shortfall,
the non-poor contributing zero, which is the AFTER's description."""),
    dict(kind="theorem",
         source=r"FGT.lean, I\_not\_decomposable",
         text=r"""With $z = 1$ and incomes $(0, \tfrac12, 2)$: $I = \tfrac34$ on the
whole population, $I = 1$ on $\{0\}$ and $I = \tfrac12$ on $\{1, 2\}$, and
$\tfrac34 \neq \tfrac13 \cdot 1 + \tfrac23 \cdot \tfrac12 = \tfrac23$.""",
         note=r"""The mean shortfall among the poor (FGT's $I$ times $z$) is not
additively decomposable with population-share weights, so it is not a member
of the decomposable family the manuscript cites."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.14"] = [
    dict(kind="quote",
         source=r"Tao 2011, Theorem 1.7.15, pre-p. 200",
         text=r"""``Theorem 1.7.15 (Tonelli's theorem, incomplete version). Let
$(X, \mathcal{B}_X, \mu_X)$ and $(Y, \mathcal{B}_Y, \mu_Y)$ be $\sigma$-finite
measure spaces, and let $f : X \times Y \to [0, +\infty]$ be measurable with
respect to $\mathcal{B}_X \times \mathcal{B}_Y$. Then:''""",
         note=r"""Tonelli is stated for nonnegative $f$ with no integrability
hypothesis, which is what Step 3 of the LOS proof uses."""),
    dict(kind="quote",
         source=r"Tao 2011, Corollary 1.7.23, pre-p. 206",
         text=r"""``Corollary 1.7.23 (Fubini-Tonelli theorem). Let
$(X, \mathcal{B}_X, \mu_X)$ and $(Y, \mathcal{B}_Y, \mu_Y)$ be complete
$\sigma$-finite measure spaces, and let $f : X \times Y \to \mathbf{C}$ be
measurable with respect to $\mathcal{B}_X \times \mathcal{B}_Y$. If
$\int_X \big(\int_Y |f(x,y)|\, d\mu_Y(y)\big)\, d\mu_X(x) < \infty$ \ldots
then $f$ is absolutely integrable''.""",
         note=r"""The corollary the manuscript cites is Fubini-Tonelli for
complex-valued $f$ under an absolute-integrability hypothesis, not the
nonnegative Tonelli statement."""),
    dict(kind="quote",
         source=r"Tao 2011, Exercise 1.4.23(iii), pre-p. 93",
         text=r"""``(iii) (Downwards monotone convergence) If $E_1 \supset E_2
\supset \ldots$ are $\mathcal{B}$-measurable, and $\mu(E_n) < \infty$ for at
least one $n$, then $\mu(\bigcap_{n=1}^{\infty} E_n) = \lim_{n \to \infty}
\mu(E_n) = \inf_n \mu(E_n)$.''""",
         note=r"""Continuity from above is an exercise, stated for sequences and
with a finiteness hypothesis; the LOS proof lets $c \downarrow 0$ over a
continuum."""),
    dict(kind="theorem",
         source=r"Tao.lean, measure\_band\_tendsto\_zero",
         text=r"""Let $\mu$ be a finite measure and $(B_c)_{c>0}$ measurable sets with
$B_{c'} \subseteq B_c$ whenever $0 < c' \le c$ and $\bigcap_{c > 0} B_c =
\emptyset$. Then $\mu(B_c) \to 0$ as $c \downarrow 0$.""",
         note=r"""The real-parameter passage Step 4 needs, proved from the
sequential statement since the neighbourhood filter of $0^{+}$ is countably
generated; the AFTER applies 1.4.23(iii) along any $c_n \downarrow 0$."""),
]
