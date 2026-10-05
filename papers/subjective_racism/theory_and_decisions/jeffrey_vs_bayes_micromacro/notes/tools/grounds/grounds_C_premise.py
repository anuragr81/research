# Grounds for C.6, C.7, C.9, C.10 (Introduction premise paragraph, the Domotor
# sentence, the Setup prior paragraph, and the invariance/justification passage).
#
# Every quote was checked against the paper on 2026-09-30: Jeffrey (2002 draft)
# on the rendered page for pp. 59 and 61 and in the text layer elsewhere; Jeffrey
# (1983), Domotor (1980) and Diaconis-Zabell (1982) on rendered scan pages;
# Hawthorne (2004) in the text layer (rendered-page check recorded in
# notes/citation_audit/verify_hawthorne_weisberg.md, H7, H12, H13). Page numbers
# are the printed ones. Theorem statements follow the Lean hypotheses.

GROUNDS = {}

# --------------------------------------------------------------------------
# C.6  Read as a Bayes factor: definition, provenance, and the D-Z uniqueness
# --------------------------------------------------------------------------
GROUNDS["C.7"] = [
    {
        "kind": "quote",
        "source": r"Domotor (1980), p.~395",
        "text": r"""``Along with Field (1978) we may argue that Jeffrey machines are inadequate because,
among other things, the commutativity of evidence application:
$[P_{(U,p)}]_{(V,q)} = [P_{(V,q)}]_{(U,p)}$ fails in general.''""",
        "note": r"Domotor supports bare non-commutativity and counts it against Jeffrey machines; the manuscript cites him for a mechanism and for the opposite lean.",
    },
    {
        "kind": "quote",
        "source": r"Domotor (1980), p.~397",
        "text": r"""``A moment's thought shows that Field's input space is embeddable into Jeffrey's
input space by the map: $h_P : \mathbf{F}_X \to \mathbf{E}_X$, defined by
$h_P(U,\alpha) = (U,p)$, where: $p(A) = e^{\alpha_A} P(A) : \alpha_X$.'' \ldots ``So,
whatever advantage may have been gained in commutativity is lost in probabilistic
independence.''""",
        "note": r"The passage nearest a mechanism: a fixed factor $\alpha$ yields a $p$ that depends on the state $P$, used to argue that Field's space is smaller, not to explain non-commutativity. Neither ``likelihood'' nor ``marginal'' occurs on the rendered pp.~394--397, the pages around the quoted passages.",
    },
    {
        "kind": "theorem",
        "source": r"Domotor.lean, field\_eq\_jeffrey\_embed; embed\_depends\_on\_state",
        "text": r"""For $P\ge0$ on finite $\Omega$, a partition $u:\Omega\to I$ and $\alpha:I\to\mathbb{R}$,
put $\alpha_X=\sum_y e^{\alpha_{u(y)}}P(y)$ and $h_P(\alpha)_i=e^{\alpha_i}P(E_i)/\alpha_X$.
Then Field's conditional $x\mapsto e^{\alpha_{u(x)}}P(x)/\alpha_X$ equals Jeffrey's rule on
$(u,h_P(\alpha))$. The image depends on the state: $\alpha=(\log2,0)$ on two atoms gives
$p=(2/3,1/3)$ at $P=(1/2,1/2)$ but $p=(2/5,3/5)$ at $P=(1/4,3/4)$.""",
        "note": r"Read in reverse, a fixed delivered credence corresponds to a factor that depends on the belief it meets; Domotor states the embedding but draws neither reading.",
    },
    {
        "kind": "theorem",
        "source": r"Cripps.lean, composite\_AB\_eq\_bayes",
        "text": r"""Let $\mu$ on $A\times B$ be strictly positive with $\sum\mu=1$, $q$ on $A$ strictly
positive with $\sum_a q_a=1$, and $r$ on $B$ with $\sum_b r_b=1$. Then
$J_B(J_A(\mu,q),r)=\mathrm{bayes}\big(\mu,\ x\mapsto
\tfrac{q_{x_1}}{\mu(A{=}x_1)}\cdot\tfrac{r_{x_2}}{(J_A(\mu,q))(B{=}x_2)}\big)$, where
$\mathrm{bayes}(\mu,\ell)(\theta)=\mu(\theta)\ell(\theta)/\sum_{\theta'}\mu(\theta')\ell(\theta')$
and $J_A(\mu,q)(x)=q_{x_1}\mu(x)/\mu(A{=}x_1)$. In the other sequence the $B$-likelihood is
$r_{x_2}/\mu(B{=}x_2)$.""",
        "note": r"The second cue's likelihood is matched to the intermediate belief, which the first cue has moved; this is the corrected sentence's mechanism, and it is the paper's own.",
    },
    {
        "kind": "computation",
        "source": r"sympy, recomputed symbolically (the instance is check 3 of verify\_soft\_vs\_hard.py)",
        "text": r"""After the $A$-step to $q_0$ from $P(\alpha,\beta,c)$, the $B$-marginal is
$\beta + c\,\dfrac{q_0-\alpha}{\alpha(1-\alpha)}$, so the first cue moves it whenever
$c\neq0$ and $q_0\neq\alpha$. At $\alpha=\beta=1/2$, $c=1/20$, $q_0=1/5$ it is $11/25$.""",
        "note": r"The delivered credence $r_0$ is then read against $11/25$ instead of $1/2$, which is the whole of the sequence effect.",
    },
    {
        "kind": "quote",
        "source": r"PAPER\_B\_MANUSCRIPT.tex, Proposition DIV (micro divergence), (i)",
        "text": r"""Let $Z:=\alpha\beta(1-\alpha)(1-\beta)>0$. ``At $c=0$ both reading sequences coincide
with the benchmark, $P^{J}_{AB}=P^{J}_{BA}=P^{B}=q\otimes r$.'' (i) For $c\neq0$,
$P^{J}_{AB}-P^{B}=c\,\kappa R_1+O(c^{2})$, $R_1=q\otimes(1,-1)$,
$\kappa=\frac{(\alpha-q_0)\,r_0(1-r_0)}{Z}$, ``with $\kappa=0\iff q_0=\alpha$ or
$r_0\in\{0,1\}$''.""",
        "note": r"The manuscript's own statement of the mechanism the sentence should cite.",
    },
]

# --------------------------------------------------------------------------
# C.9  The prior paragraph: the Hawthorne debate, and the bounds are right
# --------------------------------------------------------------------------
GROUNDS["C.9"] = [
    {
        "kind": "quote",
        "source": r"Hawthorne (2004), \S5, pp.~98--99",
        "text": r"""``The resulting Amnestic Model raises two related concerns. First, it seems
implausible that the most recent experience or non-propositional state should completely
dictate belief strengths for basis sentences, with no regard for the import of previous
experiences or states. There may be some specialized systems for which this model is
appropriate. But it seems wrong as a model of (idealized) human agents'' \ldots
``This order effect is very troubling.''""",
        "note": r"The source of the debate the corrected sentence names; Hawthorne's alternatives are factor models (\S\S6--8), not a partial weight, so the citation is for the objection only.",
    },
    {
        "kind": "quote",
        "source": r"Hawthorne (2004), \S5, p.~96",
        "text": r"""``AMNESTIC UPDATE-FACTOR THESIS. For any state $e$ that directly affects an evidence
basis $\{E_i\}$ and for any other state $d$ and sequence of states $\alpha$,
$Q_{\alpha de}[E_i]=Q_{\alpha e}[E_i]$.'' ``Indeed Amnestic Updating is just Standard
Sequential Updating -- Jeffrey's original approach to sequential updating.''""",
        "note": r"Full adoption, $\omega=1$, is this thesis under Hawthorne's name for it.",
    },
    {
        "kind": "computation",
        "source": r"Nonnegativity of the four cells of $P$ (checked cell by cell in sympy)",
        "text": r"""$\alpha\beta+c\ge0$, $\alpha(1-\beta)-c\ge0$, $(1-\alpha)\beta-c\ge0$,
$(1-\alpha)(1-\beta)+c\ge0$, hence
$-\min\{\alpha\beta,\,(1-\alpha)(1-\beta)\}\le c\le\min\{\alpha(1-\beta),\,(1-\alpha)\beta\}$,
the Fr\'echet--Hoeffding bounds for a $2\times2$ table with marginals $\alpha,\beta$; at
$\alpha=\beta=1/2$ the range is $[-1/4,1/4]$.""",
        "note": r"The manuscript's bounds sentence is correct and is kept.",
    },
]

# --------------------------------------------------------------------------
# C.10  Invariance versus origination; D-Z Theorem 2.2; mechanical updating
# --------------------------------------------------------------------------
GROUNDS["C.10"] = [
    {
        "kind": "quote",
        "source": r"Jeffrey (1983), ch.~11, \S11.3 p.~168 and \S11.7 p.~174",
        "text": r"""p.~168: ``the values of $PROB$ for all arguments ought to be deducible from a
knowledge of (a) the values of $prob$ for all arguments, (b) the value of $PROB$ for the
argument $B$, and the fact that (c) the change from $prob$ to $PROB$ \textit{originated}
in $B$''. p.~174: ``(11-8) $PROB(A/A_i)=prob(A/A_i)$ for each $i=1,2,\ldots,m$ which defines
what we shall mean by saying that the change from $prob$ to $PROB$ \textit{originated} in
the set (11-6).''""",
        "note": r"On the rendered pp.~168 and 174--175 Jeffrey's term for the condition is that the change originated in the partition, not ``invariance''.",
    },
    {
        "kind": "quote",
        "source": r"Jeffrey (1983), \S11.7, p.~174, and Example 7",
        "text": r"""``Note that since distinct sets of propositions can have the same set of atoms, there
is a certain latitude in the choice of a set (11-6) in which the change from $prob$ to
$PROB$ is viewed as originating.'' Example 7: the change of Example 6, originating in
$G,B,V$, ``might equally well be regarded as originating in the set $B_1=G\vee B$,
$B_2=G$''.""",
        "note": r"On Jeffrey's own account the originating set is not unique, against ``the partition satisfying the invariance condition''.",
    },
    {
        "kind": "quote",
        "source": r"Jeffrey (2002 draft), ch.~3, \S3.1 p.~56 and \S3.2 pp.~57--58",
        "text": r"""``Invariant conditional probabilities: (1) For all $H$, $new(H|D)=old(H|D)$''.
``As long as invariance holds, updating is valid by a generalization of conditioning to
which we now turn.'' ``If the invariance condition holds for each answer, $D_i$, we have the
updating scheme $new(H)=old(H|D_1)\,new(D_1)+old(H|D_2)\,new(D_2)+\ldots$'' ``This is
equivalent to invariance with respect to every answer: $new(H|D_i)=old(H|D_i)$ for
$i=1,\ldots,n$.''""",
        "note": r"``Invariance'' is the 2004 book's term, introduced in \S3.1 for one proposition and carried to partitions in \S3.2.",
    },
    {
        "kind": "quote",
        "source": r"Diaconis and Zabell (1982), \S2.2, p.~824",
        "text": r"""``To apply Jeffrey's rule, it is required to find a partition $\{E_i\}$ such that
$P(A\,|\,E_i)=P^*(A\,|\,E_i)$ for all $A$ and $i$. This is simply the problem of finding a
\textit{sufficient partition} for the two-element family $\mathcal{F}=\{P,P^*\}$''.
``A coarsest sufficient partition is said to be minimal sufficient. The following
(well-known) theorem gives an alternative version of Jeffrey's rule and states that there
is always a coarsest partition for which Jeffrey's rule is valid.''""",
        "note": r"Sufficiency is relative to one pair $\{P,P^*\}$, not to ``any candidate posterior''.",
    },
    {
        "kind": "quote",
        "source": r"Diaconis and Zabell (1982), Theorem 2.2, p.~824",
        "text": r"""``\textit{Theorem 2.2.} Let $P$, $P^*$ be probability measures with common support on
the countable set $\Omega$. If $\{E_i\}$ is a partition of $\Omega$ such that $P(E_i)>0$ and
$P(A\,|\,E_i)=P^*(A\,|\,E_i)$ for all subsets $A$ and elements of the partition $E_i$, then
for each $\omega\in\Omega$, $P^*(\omega)=\dfrac{P^*(E_i)}{P(E_i)}P(\omega)$, $\omega\in E_i$.
(2.2) If $R=\{x:P^*(\omega)/P(\omega)=x,\ \omega\in\Omega\}$, and
$E_x=\{\omega:P^*(\omega)/P(\omega)=x,\ \omega\in\Omega\}$, then $\{E_x:x\in R\}$ is a
minimal sufficient partition for $\{P,P^*\}$.''""",
        "note": r"The minimal sufficient partition is the likelihood-ratio partition, which depends on the pair; the manuscript assigns minimality to the cue's partition.",
    },
    {
        "kind": "quote",
        "source": r"Diaconis and Zabell (1982), \S5.2 and \S5.3, p.~828",
        "text": r"""``Mechanical updating allows the possibility of updating on collections of sets more
general than partitions.'' ``Theorem 5.1 suggests that Jeffrey's rule is an uncontroversial
form of mechanical updating in the sense that it agrees with virtually every
minimum-distance rule.''""",
        "note": r"D-Z give no axioms; ``axiomatic grounding'' mislabels what they call mechanical updating.",
    },
    {
        "kind": "theorem",
        "source": r"DiaconisZabell.lean, thm22\_lr\_minimal",
        "text": r"""Let $P,P^*>0$ on finite $\Omega$ and let $e:\Omega\to I$ satisfy (J):
$P^*(A\,|\,E_i)=P(A\,|\,E_i)$ for every $A\subseteq\Omega$ and $i$. Then
$e(\omega)=e(\omega')\Rightarrow P^*(\omega)/P(\omega)=P^*(\omega')/P(\omega')$: every cell
of a (J)-partition lies inside one level set of $P^*/P$; with the converse
(\texttt{lr\_jeffrey\_iff}) the likelihood-ratio partition is the coarsest sufficient one, and it
depends on the pair $\{P,P^*\}$.""",
        "note": r"Minimality belongs to the likelihood-ratio partition, not to whichever partition carries the cue.",
    },
    {
        "kind": "theorem",
        "source": r"DiaconisZabell.lean, jcond\_of\_refines (with jcond\_atoms, jcond\_trivial\_self)",
        "text": r"""Let $P,P^*>0$ on finite $\Omega$, let $e:\Omega\to I$ satisfy (J), and let
$e':\Omega\to K$ refine $e$, i.e. $e'(\omega)=e'(\omega')\Rightarrow e(\omega)=e(\omega')$.
Then $e'$ satisfies (J). In particular the partition into atoms satisfies (J) for every pair,
and if $P^*=P$ the one-cell partition does.""",
        "note": r"``The partition satisfying the invariance condition'' is not unique: every refinement qualifies, and the cue's partition is minimal only when the delivered credence differs from the prior marginal.",
    },
    {
        "kind": "theorem",
        "source": r"DiaconisZabell.lean, thm51\_KL\_eq\_iff; D-Z Theorem 5.1, (5.6)",
        "text": r"""For finite $\Omega$, $P>0$, $e:\Omega\to I$ surjective and $Q\ge0$ with
$Q(E_i)=p_i$ for all $i$: $\mathrm{KL}(Q\,\|\,P)=\sum_i p_i\log\frac{p_i}{P(E_i)}$ if and
only if $Q(\omega)=p_{e(\omega)}P(\omega)/P(E_{e(\omega)})$; with thm51\_KL\_le the
right-hand side is the minimum over the feasible set.""",
        "note": r"Supports the corrected second sentence: the rule is the unique $I$-divergence minimiser among beliefs with the delivered marginal.",
    },
]
