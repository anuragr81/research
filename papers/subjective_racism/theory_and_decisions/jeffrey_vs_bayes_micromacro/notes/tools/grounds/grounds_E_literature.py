"""Grounds for the literature entries E.5, E.6 and E.7 of plan_entries_E.py.

Every quotation was checked on the page before entry (Doring from the scanned
journal pages S379-S385; Asch pp. 262, 264, 270-272 and Diaconis-Zabell p. 828
from rendered pages; Hawthorne, Field, Wagner, Garber and Bohren-Imas-Rosenberg
from text layers whose printed page markers were located).  Page numbers are as
printed: journal pages for Hawthorne, Field, Garber, Diaconis-Zabell and Asch;
S-pages for Doring; the preprint's own page for Wagner 2002; the January 2019
working paper's printed page for Bohren-Imas-Rosenberg.  Lean statements are
given with their hypotheses; the paper's weight omega is the code's delta.
"""

GROUNDS = {}

# ---------------------------------------------------------------- E.5 ------
GROUNDS["E.5"] = [
    # Paragraph 1: the commutativity literature and Hawthorne's vocabulary
    {
        "kind": "quote",
        "source": "Hawthorne 2004, p. 96 (Section 5)",
        "text": r"``AMNESTIC UPDATE-FACTOR THESIS. For any state $e$ that directly affects an evidence basis $\{E_i\}$ and for any other state $d$ and sequence of states $\alpha$, $Q_{\alpha de}[E_i] = Q_{\alpha e}[E_i]$.'' And: ``Indeed Amnestic Updating is just Standard Sequential Updating -- Jeffrey's original approach to sequential updating.''",
        "note": "Para 1: the full-adoption premise is named by Hawthorne, and he identifies it with Jeffrey's sequential rule.",
    },
    {
        "kind": "quote",
        "source": "Hawthorne 2004, p. 97 and note 12 (p. 121)",
        "text": r"``a pair of states $e$ and $f$ will commute for $Q_\beta$ \ldots just in case neither $e$ nor $f$ can, on its own, influence (even indirectly) the basis sentences of the other -- i.e. just in case for each $E_i$, $Q_{\beta f}[E_i] = Q_\beta[E_i]$, and for each $F_j$, $Q_{\beta e}[F_j] = Q_\beta[F_j]$.'' Note 12: ``Theorem 3.2 in (Diaconis and Zabell, 1982).''",
        "note": "Para 1: he credits the commutation criterion to Diaconis and Zabell.",
    },
    {
        "kind": "quote",
        "source": "Hawthorne 2004, abstract p. 89 and section headings pp. 96, 99, 103",
        "text": r"``I will explore three models of sequential updating, the usual extension and two alternatives.'' The headings: ``5. The Amnestic Update Model'' (p. 96), ``6. The Normed-Likelihood Factor Model'' (p. 99), ``7. The Likelihood-Ratio Factor Model'' (p. 103).",
        "note": "Para 1: the taxonomy has three members, one amnestic model and two factor models; the normed-likelihood model is the middle one, not an end.",
    },
    {
        "kind": "quote",
        "source": "Hawthorne 2004, note 20 (p. 121)",
        "text": r"``In his most recent work Jeffrey favors updating based on Likelihood-Ratio factors as well. He thinks of them as ratios of new to old odds and calls them `Bayes factors', following Good (1950).''",
        "note": "Para 1: the name Bayes factor is given by Hawthorne to likelihood-ratio factors, which is why the entry identifies the paper's Bayes factor with his LR factor as a ratio of two NL factors.",
    },
    {
        "kind": "theorem",
        "source": r"Hawthorne.lean, NL\_jeffrey, factorUpdate\_seq, med\_NL\_denominator",
        "text": r"$\mathrm{NL}[Q,e,E_i] = Q_e[E_i]/Q[E_i]$ (p. 95); for a Basic Jeffrey update to targets $q$, $\mathrm{NL}[Q,e,E_i] = q_i/Q[E_i]$ with no side condition. Two factor updates on bases $u,v$ with $\sum_y w(u_y)Q(y) \neq 0$ compose to $Q_{ef}(x) = w(u_x)\,z(v_x)\,Q(x)\big/\sum_y w(u_y)\,z(v_y)\,Q(y)$ (pp. 102, 105). In his medical example the NL factors against the initial prior give $Q[C] = 1/2$ only after dividing by the normaliser $301/625 \neq 1$.",
        "note": r"Para 1: $\PB$ has the form of his extended update formula up to normalisation, so the entry says ``up to normalisation''.",
    },
    # Paragraph 2: the two grounds of dispute, and the information-theoretic ground
    {
        "kind": "quote",
        "source": "Doring 1999, S379 (Introduction) and S383",
        "text": r"``The following is an exercise in Bayesian rational psychology. Its contention is that Jeffrey conditionalization, a generalization of classical conditionalization, cannot be a complete account of rational belief change.'' On the reversed conditional probabilities of his example: ``This seems wholly unjustified if there is nothing essential about the order in which experiences were made.''",
        "note": "Para 2: Doring's objection is normative, about rational belief change, not about how people update (audit D1/P3).",
    },
    {
        "kind": "quote",
        "source": "Hawthorne 2004, pp. 115-116",
        "text": r"``Which extension of Basic Jeffrey Updating is the more plausible model of human agents? I'm a logician, not a psychologist. But Amnestic Updating seems psychologically less plausible than the more Bayesian approaches, not because it is un-Bayesian, but because it seems unlikely that we dismiss previous experiences so completely. However, I am mainly interested in whether these models capture useful normative conceptions of belief updating''",
        "note": r"Para 2: the entry's second fragment begins at ``because it seems unlikely'', dropping ``not because it is un-Bayesian, but''; the sentence runs onto p. 116 at ``of belief updating''.",
    },
    {
        "kind": "quote",
        "source": "Diaconis and Zabell 1982, p. 828, Theorem 5.1 and (5.3)",
        "text": r"``Let $Q$ be a probability on $\Omega$ such that $Q(E_i) = P^*(E_i)$. Then \ldots $I(Q,P) \geq \sum P^*(E_i)\log\big(P^*(E_i)/P(E_i)\big)$. (5.6) In (5.5) and (5.6) equality holds if and only if $Q(A) = \sum P(A \mid E_i)\,P^*(E_i)$.'' With (5.3): ``$I(Q,P) = \sum_\omega Q(\omega)\log\big(Q(\omega)/P(\omega)\big)$.''",
        "note": r"Para 2: Jeffrey's rule is the unique minimiser of the $I$-divergence from the prior among revisions that take on the delivered credence; the Lean statement is DiaconisZabell.lean, thm51\_KL\_eq\_iff.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_deviation",
        "text": r"Let $\mathrm{dampedB}(Q,r_0,\delta)$ be the Jeffrey step on $B$ to the target $(1-\delta)\,Q(B{=}1) + \delta\,(1-r_0)$. If $Q(B{=}1) \neq 0$ then $\mathrm{dampedB}(Q,r_0,\delta)(B{=}1) - (1-r_0) = (1-\delta)\,\big(Q(B{=}1) - (1-r_0)\big)$, an identity in $Q$, $r_0$ and $\delta$. The paper's $\omega$ is $\delta$.",
        "note": r"Para 2: the degree of adoption is recovered from the rating of one marginal before and after the second cue against the delivered credence $1-r_0$, namely $\omega = 1 - (\text{after} - (1-r_0))/(\text{before} - (1-r_0))$.",
    },
    # Paragraph 3: the two responses to sequence dependence
    {
        "kind": "quote",
        "source": "Doring 1999, S379 (abstract)",
        "text": r"``Jeffrey conditionalization is sensitive to the order in which the evidence arrives. This order effect can be so pronounced as to call for a belief adjustment that cannot be understood as an assimilation of incoming evidence by Jeffrey's rule.''",
        "note": "Para 3: Doring treats the sequence effect as a defect of the framework.",
    },
    {
        "kind": "quote",
        "source": "Doring 1999, S384-S385",
        "text": r"``The most straightforward approach would be to revert to the original probabilities and try to assimilate the later evidence all at once.'' (S384) ``Jeffrey conditionalizing in one step on the new assignments yields a posterior distribution in which both pieces of evidence receive equal weight'' (S385).",
        "note": "Para 3: his remedy is a single Jeffrey step from the original prior, which successive steps cannot supply (audit P2).",
    },
    {
        "kind": "quote",
        "source": "Field 1978, pp. 365-366 (eq. (7))",
        "text": r"``If we express the combined result of both changes in terms of the Jeffrey parameters $q$ and $q'$ of the individual changes, we get a very complicated law; moreover, it is an asymmetric law \ldots If however we express the combined result in terms of the parameters $\alpha$ and $\alpha'$ of the individual changes, we get a law that is both simple and symmetric''",
        "note": "Para 3: Field's input is the portable parameter alpha rather than the delivered credence q, and with it successive changes commute.",
    },
    {
        "kind": "quote",
        "source": "Wagner 2002, abstract (preprint p. 1)",
        "text": r"``When identical learning is properly represented, namely, by identical Bayes factors rather than identical posterior probabilities, then sequential probability-kinematical revisions behave just as they should.''",
        "note": "Para 3: identical Bayes factors make sequential revisions commute.",
    },
    {
        "kind": "quote",
        "source": "Garber 1980, p. 144",
        "text": r"``after nine repetitions of the same rather uninformative experience, S will become virtually certain that the ball is blue.'' And: ``If stimulation behaves in the way he supposes in (3), then practical certainty in $E$ is much too easily obtained.''",
        "note": "Para 3: the objection is that a portable factor compounds implausibly under repetition; Garber never discusses order.",
    },
    {
        "kind": "theorem",
        "source": r"Field.lean, field\_eq7\_comm",
        "text": r"For $E, E' : \Omega \to \mathrm{Bool}$, $\alpha, \alpha' \in \mathbb{R}$ and $P \geq 0$ with $\sum_\omega P(\omega) = 1$: $\mathrm{tilt}_{E'}(\alpha', \mathrm{tilt}_E(\alpha, P)) = \mathrm{tilt}_E(\alpha, \mathrm{tilt}_{E'}(\alpha', P))$, where $\mathrm{tilt}_E(\alpha,P)(\omega) = e^{\pm\alpha}P(\omega)\big/\sum_{\omega'} e^{\pm\alpha}P(\omega')$, the sign $+$ on $E$ and $-$ off it (Field's eq. (6)).",
        "note": "Para 3: Field's eq. (7) is symmetric, so his two updates commute.",
    },
    {
        "kind": "theorem",
        "source": "Wagner2002.lean, thm31",
        "text": r"Let $p$ be a probability, $q$ come from $p$ by probability kinematics on $\mathcal{E}$ and $r$ from $q$ on $\mathcal{F}$; let $q'$ come from $p$ on $\mathcal{F}$ and $r'$ from $q'$ on $\mathcal{E}$. If $\beta_{r',q'}(E_{i_1}{:}E_{i_2}) = \beta_{q,p}(E_{i_1}{:}E_{i_2})$ for all $i_1, i_2$ and $\beta_{q',p}(F_{j_1}{:}F_{j_2}) = \beta_{r,q}(F_{j_1}{:}F_{j_2})$ for all $j_1, j_2$, then $r' = r$ (finite partitions).",
        "note": "Para 3: Wagner's Theorem 3.1, with the primes as printed on the rendered page.",
    },
    {
        "kind": "theorem",
        "source": r"Garber.lean, garber\_nine",
        "text": r"With $\alpha_0$ fixed by the change $.3 \to .4$ (so $e^{2\alpha_0} = 14/9$) and Field's step iterated from $P_0(E) = .3$, $P_n(E) = 3\cdot 14^n/(3\cdot 14^n + 7\cdot 9^n)$; then $P_9(E) > .95$ and $P_8(E) < .95$.",
        "note": "Para 3: Garber's table (p. 144) is reproduced exactly; nine repetitions are the first to pass .95.",
    },
    # Paragraph 4: the insensitivity of the association under every extension
    {
        "kind": "theorem",
        "source": r"Hawthorne.lean, factorUpdate\_smul, NL\_factorUpdate, LR\_factorUpdate",
        "text": r"A factor update is $\mathrm{fu}(p,u,w)(x) = w(u_x)\,p(x)\big/\sum_y w(u_y)\,p(y)$. For $c \neq 0$, $\mathrm{fu}(p,u,c\,w) = \mathrm{fu}(p,u,w)$. If $Q[E_i] \neq 0$, $\mathrm{NL}[p,\mathrm{fu}(p,u,w),E_i] = w_i/\sum_y w(u_y)p(y)$; if also $Q[E_j], Q[E_k]$ and the normaliser are nonzero, $\mathrm{LR}[p,\mathrm{fu}(p,u,w),E_j,E_k] = w_j/w_k$, whatever the prior.",
        "note": "Para 4: amnestic, normed-likelihood and likelihood-ratio updates each multiply the basis cells by a factor that depends on that basis alone; they differ only in what fixes the factor.",
    },
    {
        "kind": "theorem",
        "source": r"LemmaSEP.lean, isSeparable\_applySteps, sep\_rescales\_association",
        "text": r"For any $N$, any prior $P$ on $\{0,1\}^N$ and any finite list of attribute-local steps (each multiplying cell $x$ by a factor of the single coordinate $x_a$ it reads), the result is $P(x)\prod_a g_a(x_a)$ for some $g$. For such $F$ and $i \neq j$: $F_{11}F_{00} - F_{10}F_{01} = g_i(1)g_i(0)g_j(1)g_j(0)\,\big(\prod_{a \neq i,j} g_a(x_a)\big)^2\,(P_{11}P_{00} - P_{10}P_{01})$.",
        "note": "Para 4: under every extension the believed association is rescaled by a positive factor and never shifted.",
    },
]

# ---------------------------------------------------------------- E.6 ------
GROUNDS["E.6"] = [
    {
        "kind": "quote",
        "source": "Asch 1946, p. 270 (Experiment VI)",
        "text": r"``The following series are read, each to a different group: A. intelligent---industrious---impulsive---critical---stubborn---envious B. envious---stubborn---critical---impulsive---industrious---intelligent There were 34 subjects in Group A, 24 in Group B. The two series are identical with regard to their members, differing only in the order of succession of the latter.''",
        "note": "Six stimulus terms read in either order; the eighteen traits of Table 7 are response items, none of which is a stimulus term (Asch.lean, stimulus\\_disjoint\\_checklist).",
    },
    {
        "kind": "quote",
        "source": "Asch 1946, p. 262 (Check List I) and p. 271",
        "text": r"``To this end we constructed a check list consisting of pairs of traits, mostly opposites. From each pair of terms in this list, which the reader will find reproduced in Table 1, the subject was instructed to select the one that was most in accordance with the view he had formed.'' (p. 262) ``The check-list data appearing in Table 7 furnish quantitative support for the conclusions drawn from the written sketches.'' (p. 271)",
        "note": "The check-list is a forced choice within a pair of opposites (audit A11); Table 7 reports, for each pair, the percentage choosing the listed member.",
    },
    {
        "kind": "computation",
        "source": r"Asch.lean, quoted\_all; literature/asch1946/sympy/check\_table7.py",
        "text": r"Table 7 (p. 271), Experiment VI, columns intelligent$\to$envious ($N = 34$) and envious$\to$intelligent ($N = 24$): restrained 64 / 9, good-looking 74 / 35, serious 97 / 100, persistent 82 / 87, reliable 84 / 91; also humorous 52 / 21, good-natured 18 / 0, important 85 / 90. Every quoted number is in the right column (proved by decide; script 16/16 PASS).",
        "note": "The four small moves are all of 7 points or less and all in the opposite direction to the large swings (Asch.lean, d\\_neg\\_exactly).",
    },
    {
        "kind": "quote",
        "source": "Asch 1946, p. 264 (Experiment I)",
        "text": r"``If we assume that the process of mutual influence took place in terms of the actual character of the qualities in question, it is not surprising that some will, by virtue of their content, remain unchanged.''",
        "note": "Asch's account of uneven effects is by content; it is given for Experiment I, and he offers no trait-by-trait account for Experiment VI (audit A15).",
    },
    {
        "kind": "quote",
        "source": "Asch 1946, pp. 271-272",
        "text": r"``the first terms set up in most subjects a direction which then exerts a continuous effect on the latter terms.'' (p. 271) ``It is not the sheer temporal position of the item which is important as much as the functional relation of its content to the content of the items following it.'' (p. 272)",
        "note": "Content rather than position: the entry's ``rather than to their position''.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_deviation",
        "text": r"With $\mathrm{dampedB}(Q,r_0,\delta)$ the Jeffrey step on $B$ to $(1-\delta)\,Q(B{=}1) + \delta\,(1-r_0)$ and $Q(B{=}1) \neq 0$: $\mathrm{dampedB}(Q,r_0,\delta)(B{=}1) - (1-r_0) = (1-\delta)\,\big(Q(B{=}1) - (1-r_0)\big)$. Hence $\omega = \delta$ is fixed by three readings of one marginal, before the cue, after it, and the credence it delivers.",
        "note": "Marginals in two sequences, however many, do not fix omega; one marginal read before and after the second cue and against what it delivers does.",
    },
    {
        "kind": "quote",
        "source": "Asch 1946, p. 262 (Technique) and p. 271 (design of the within-subject group)",
        "text": r"``Following the reading, each subject wrote a brief sketch.'' (p. 262) ``A new group ($N{=}24$) heard Series B, wrote the free sketch, and immediately thereafter wrote the sketch in response to Series A.'' (p. 271)",
        "note": "Nothing is elicited before the series is read, and neither the sketch nor the forced-choice list asks how strongly two traits are believed to go together, so no prior association is collected (audit P13).",
    },
]

# ---------------------------------------------------------------- E.7 ------
GROUNDS["E.7"] = [
    {
        "kind": "quote",
        "source": "Bohren, Imas and Rosenberg 2019, working paper p. 11 (Belief-Updating)",
        "text": r"``The evaluator learns about the worker's ability from the evaluation history. Her posterior belief about ability is derived using Bayes rule, given her model of inference. She combines this updated belief about ability with the signal to learn about the quality of the current task, also using Bayes rule to form her posterior belief about quality.''",
        "note": "Their map from partiality to behaviour is Bayesian by explicit assumption.",
    },
    {
        "kind": "quote",
        "source": "Bohren, Imas and Rosenberg 2019, working paper pp. 11-12, eqs. (2) and (3)",
        "text": r"``Then her optimal evaluation in period $t$ is $v_i(h_t,s_t,g) = \hat{E}_i[q_t \mid h_t,s_t,g] - c^i_g$. (2)'' (p. 11) ``Let $D_i(h,s) \equiv v_i(h,s,M) - v_i(h,s,F)$ (3) denote the difference between type $\theta_i$'s evaluation of a male and female worker conditional on observing history $h$ and signal $s$'' (p. 12).",
        "note": "With no taste ($c^i_g = 0$) discrimination is the gap in posterior expected quality at the same history and signal.",
    },
    {
        "kind": "quote",
        "source": "Bohren, Imas and Rosenberg 2019, working paper p. 16, Proposition 1",
        "text": r"``Proposition 1 (Subjectivity of Judgement). If the evaluator has belief-based partiality, initial discrimination is decreasing in the precision of the signal $\tau_\eta$ and otherwise, initial discrimination is constant with respect to $\tau_\eta$. As the signal becomes perfectly objective, $\tau_\eta \to \infty$, there is initial discrimination if and only if the evaluator has preference-based partiality.''",
        "note": "The belief gap is attenuated by the precision of the signal and vanishes as judgement becomes perfectly objective.",
    },
    {
        "kind": "quote",
        "source": "Bohren, Imas and Rosenberg 2019, working paper p. 18, Proposition 2",
        "text": r"``Proposition 2 (Impossibility of Reversal). Suppose there is a single type of evaluator with belief-based partiality and no preference-based partiality. Then fixing an evaluation history, discrimination decreases across periods but never reverses.''",
        "note": r"The belief gap is also attenuated along the history, so the entry says ``vanishes as'' rather than ``vanishes only as'' (audit P17b).",
    },
    {
        "kind": "computation",
        "source": r"literature/bohren\_imas\_rosenberg2019/sympy/check\_pinning\_kills\_partiality.py, checks (3)-(5)",
        "text": r"Group $g$ has the $2{\times}2$ joint with $P_g(A{=}1) = a_g$, $P_g(B{=}1) = b_g$ and covariance $c_g$. A Jeffrey step on $A$ to $1-q_0$ followed by a Jeffrey step on $B$ to $1-r_0$ gives $P_M(B{=}1) - P_F(B{=}1) = 0$ identically in $(a_M,b_M,c_M,a_F,b_F,c_F,q_0,r_0)$, so $D = 0$ exactly with $c^i_g = 0$. With the $B$-cue read first and the $A$-cue last, $D \neq 0$. All checks pass.",
        "note": "An evaluator who takes the impression of quality last evaluates two workers alike whatever her group priors; the silencing is exact and route-dependent.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_deviation",
        "text": r"For $Q$ with $Q(B{=}1) \neq 0$: $\mathrm{dampedB}(Q,r_0,\delta)(B{=}1) - (1-r_0) = (1-\delta)\,\big(Q(B{=}1) - (1-r_0)\big)$. Applied to two groups meeting the same delivered credence $1-r_0$, the difference of their $B$-marginals after the cue is $(1-\delta)$ times the difference before it.",
        "note": r"Under partial adoption with weight $\omega = \delta$ on the impression, a fraction $1-\omega$ of the belief gap survives.",
    },
    {
        "kind": "theorem",
        "source": r"BohrenImasRosenberg.lean, prop1\_decreasing, prop2\_decreasing",
        "text": r"With $\mathrm{eval}(\tau_q,\tau_\eta,\mu,c,s) = (\tau_q\mu + \tau_\eta s)/(\tau_q+\tau_\eta) - c$ and $D(\tau_\eta) = \mathrm{eval}(\tau_q,\tau_\eta,\hat\mu_M,0,s) - \mathrm{eval}(\tau_q,\tau_\eta,\hat\mu_F,c_F,s)$: if $\tau_q > 0$, $0 < \tau_{\eta,1} < \tau_{\eta,2}$ and $\hat\mu_F < \hat\mu_M$ then $D(\tau_{\eta,2}) < D(\tau_{\eta,1})$. Along any history $v : \mathbb{N} \to \mathbb{R}$, with $\tau_a,\tau_\varepsilon,\tau_\eta > 0$, $\hat\mu_F < \hat\mu_M$ and $c = 0$: $D_{n+1}(s) < D_n(s)$ for every $n$.",
        "note": "Their map transmits the belief gap attenuated by the signal precision and along the history; the paper's mechanism sits outside this map.",
    },
]
