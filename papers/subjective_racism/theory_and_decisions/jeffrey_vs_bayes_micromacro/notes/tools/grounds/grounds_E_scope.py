"""Grounds for entries E.10-E.13 of notes/tools/plan_entries_E.py.

Sources and conventions.  Lean statements are rendered in the Lean's own
parametrisation: alpha = P(A=0), beta = P(B=0), q0 and r0 the delivered
credences on A=0 and B=0, delta the code's name for the adoption weight omega,
Z = alpha beta (1-alpha)(1-beta).  Hogarth-Einhorn pages are those of the
LaTeX transcription (T-p.N); Wagner pages are the preprint's; Asch, Hawthorne
and Epstein pages are the journals'.  Every quotation was read on the page
(Wagner's Theorem 3.1 on the rendered page, since the text layer drops the
primes).
"""

GROUNDS = {}

# ---------------------------------------------------------------------------
# E.10  Propositions ORD and ADJ
# ---------------------------------------------------------------------------
GROUNDS["E.10"] = [
    {
        "kind": "theorem",
        "source": r"PropORD.lean, lemmaORD\_gap",
        "text": r"""For $\alpha\neq0$, $1-\alpha\neq0$, $\beta\neq0$, $1-\beta\neq0$ and any
weight $v\in\mathbb R^{2\times2}$,
\[
\frac{d}{dc}\bigl\langle v,\;\PJ_{AB}(c)-\PJ_{BA}(c)\bigr\rangle\Big|_{c=0}
 =\kappa\,\langle v,R_1\rangle-\kappa'\,\langle v,R_2\rangle,
\]
with $\kappa=(\alpha-q_0)\,r_0(1-r_0)/Z$, $\kappa'=(\beta-r_0)\,q_0(1-q_0)/Z$,
$Z=\alpha\beta(1-\alpha)(1-\beta)$, $R_1=q\otimes(1,-1)$, $R_2=(1,-1)\otimes r$.
Here $\alpha=P(A{=}0)$, $\beta=P(B{=}0)$ and $q_0$, $r_0$ are the delivered
credences on $A{=}0$, $B{=}0$.""",
        "note": r"The display of Proposition ORD, assembled cell by cell from Proposition DIV's route derivatives.",
    },
    {
        "kind": "theorem",
        "source": r"PropORD.lean, propORD\_Amarg, propORD\_Bmarg, propORD\_const, propORD\_Amarg\_eq\_negKdrift",
        "text": r"""Same hypotheses. $\frac{d}{dc}\bigl[\PJ_{AB}(A{=}1)-\PJ_{BA}(A{=}1)\bigr]_{c=0}=\kappa'$,
$\frac{d}{dc}\bigl[\PJ_{AB}(B{=}1)-\PJ_{BA}(B{=}1)\bigr]_{c=0}=-\kappa$, and for the
constant weight $\mathbf 1$ the coefficient is $0$. With $Z\neq0$,
$\kappa'=-K$ for $K=q_0(1-q_0)(r_0-\beta)/Z$ of Proposition DRF.""",
        "note": r"The marginal rows of ORD; no statement mentions $\PB$ or $\lambda$.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_ORD.py, steps 5 and 8 (49/49)",
        "text": r"""Step 8: $\det\,\partial(\kappa,\kappa')/\partial(\alpha,\beta)=q_0q_1r_0r_1J_p/Z^3$ with
$J_p$ irreducible, so $\kappa$, $\kappa'$ vary independently exactly off the hypersurface
$J_p=0$; requiring $\kappa\langle G,R_1\rangle-\kappa'\langle G,R_2\rangle=0$ across priors,
symbolically in $(q_0,r_0)$, gives a system with a $2\times2$ minor $-q_0^2q_1r_0r_1$ that
never vanishes on the cube, so its solutions are exactly $\mathrm{span}\{\mathbf 1,\nabla\assoc\}$,
each annihilating $R_1$ and $R_2$. Step 5: the between-sequence
gap of $\assoc$ is $\Theta(c^2)$, and
$\langle\nabla\assoc(q\otimes r),\,\kappa R_1-\kappa'R_2\rangle=0$.""",
        "note": r"Grounds for the ``if and only if'' clause and for the $\bigO(c^2)$ claim on the association.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_deviation, routeDamped\_mA1\_deviation",
        "text": r"""For any $Q$ with $Q(B{=}1)\neq0$:
$(\mathrm{dampedB}\,Q\,r_0\,\delta)(B{=}1)-(1-r_0)=(1-\delta)\bigl[Q(B{=}1)-(1-r_0)\bigr]$.
For $Q_1=\mathrm{jeffreyA}(\mathrm{prior}(\alpha,\beta,c),q_0)$ with $Q_1(B{=}0)\neq0$,
$Q_1(B{=}1)\neq0$, $Q_1(B{=}0)+Q_1(B{=}1)=1$ and $\mathrm{prior}(A{=}1)\neq0$:
$P^{\delta}_{AB}(A{=}1)-(1-q_0)=\delta\bigl[\PJ_{AB}(A{=}1)-(1-q_0)\bigr]$.
Identities in $c$; $\delta$ is the code's name for $\omega$.""",
        "note": r"The first two lines of Proposition ADJ, exact in $c$.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, orderEffect\_damped\_mA1, orderEffect\_damped\_at\_indep",
        "text": r"""Under the hypotheses above and $Q_B(A{=}1)\neq0$, where
$Q_B=\mathrm{jeffreyB}(\mathrm{prior},r_0)$ is the belief after the $B$-cue alone (not the benchmark $\PB$):
\[
P^{\delta}_{AB}(A{=}1)-P^{\delta}_{BA}(A{=}1)
 =\delta\bigl[\PJ_{AB}(A{=}1)-(1-q_0)\bigr]+(1-\delta)\bigl[(1-q_0)-Q_B(A{=}1)\bigr].
\]
At $c=0$, for $\alpha,1-\alpha,\beta,1-\beta\neq0$:
$P^{\delta}_{AB}(A{=}1)-P^{\delta}_{BA}(A{=}1)=(1-\delta)(\alpha-q_0)$.""",
        "note": r"The third line of ADJ and its value at independence.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_at\_one, dampedB\_at\_zero, routeDamped\_at\_zero\_pins\_A; sympy/check\_zero\_slope\_identification.py, case 3",
        "text": r"""$Q(B{=}1)\neq0\Rightarrow(\mathrm{dampedB}\,Q\,r_0\,1)(B{=}1)=1-r_0$.
$Q(B{=}0)\neq0$, $Q(B{=}1)\neq0$, $Q(B{=}0)+Q(B{=}1)=1\Rightarrow\mathrm{dampedB}\,Q\,r_0\,0=Q$.
Hence, with those hypotheses on $\mathrm{jeffreyA}(\mathrm{prior},q_0)$ and
$\mathrm{prior}(A{=}1)\neq0$: $P^{0}_{AB}(A{=}1)=1-q_0$ for every $c$. Case 3 of the
script: the last-read $c$-slope carries the factor $(1-\delta)$ with a $\delta$-free
quotient, and the first-read slope vanishes only at $\delta=0$.""",
        "note": r"The ``exactly when'' at the endpoints $\omega=1$ and $\omega=0$.",
    },
    {
        "kind": "computation",
        "source": r"sympy/check\_zero\_slope\_identification.py, case 7 (47/47); sympy/verify\_example.py",
        "text": r"""$\delta=\dfrac{P^{A}(B{=}1)-P^{\delta}_{AB}(B{=}1)}{P^{A}(B{=}1)-r_1}$ holds
identically in $c$ (7a); the denominator is $(r_0-\beta)+c(\alpha-q_0)/(\alpha(1-\alpha))$,
zero exactly on that irreducible hypersurface, which at $c=0$ is $r_0=\beta$ (7b). With the numbers of Section 2 and $\omega=\tfrac12$:
$P^{A}(B{=}1)=14/25=.56$, $P^{1/2}_{AB}(B{=}1)=43/100$, $r_1=3/10$, and
$(.56-.43)/(.56-.30)=\tfrac12$.""",
        "note": r"The recovery formula and the closing example sentence.",
    },
    {
        "kind": "quote",
        "source": r"Hogarth and Einhorn 1992, T-p.7 (transcription), Eqs. 3-4",
        "text": r"""``In this case Eq. (1) can be written
$S_k = S_{k-1} + w_k[\,s(x_k) - S_{k-1}\,]$, (3) which, after rearranging terms,
leads to the averaging form $S_k = (1 - w_k)S_{k-1} + w_k\,s(x_k)$. (4)''""",
        "note": r"Eq. 4 is the estimation-mode ($R=S_{k-1}$) form; the source is a LaTeX transcription and pages are its own.",
    },
    {
        "kind": "quote",
        "source": r"Hogarth and Einhorn 1992, T-p.12 (transcription), Eq. 8",
        "text": r"""``Thus, to illustrate by denoting the first piece of evidence as the anchor,
Eq. (5) can be rewritten as $S_k = s(x_1) + w_k[\,s(x_2, \ldots, x_k) - R\,]$, (8)
where for a wide range of functions $s(x_2, \ldots, x_k)$, it follows that the
effective weight accorded to $s(x_1)$ must be greater than the weight attached to
any of the other pieces of evidence.''""",
        "note": r"Their End-of-Sequence form with the first item as anchor.",
    },
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, oneSided\_eq\_eq8, eos2\_est\_orderEffect",
        "text": r"""For all $w$, $S_0$, $a$, $b$:
$\mathrm{oneSided}(w,S_0,a,b):=\mathrm{estStep}_w(\mathrm{estStep}_1(S_0,a),b)
 =(1-w)\,a+w\,b=a+w[\,b-a\,]=\mathrm{eq8}(w,R{=}a,s_1{=}a;\,b)$,
Eq. 8 with $k=2$ and $R=s(x_1)$, where $\mathrm{estStep}_w(S,s)=(1-w)S+ws$ is Eq. 4.
The two-item sequence effect is $(2w-1)(b-a)$.""",
        "note": r"The one-sided rule is their Eq. 8 for two items with a constant weight; what is new is the two-attribute Jeffrey embedding.",
    },
    {
        "kind": "theorem",
        "source": r"Epstein.lean, compromise\_eq\_damped, omegaEff\_eq",
        "text": r"""For $\mathrm{marg}_1p(s_1)\neq0$, $\sum p=1$ and $\alpha(s_1)\ge0$, the
conditional of Epstein's compromise prior (12) is
$(1-\omega)\,p_2(s_2)+\omega\,p(s_2\mid s_1)$ with
$\omega=1-\dfrac{\alpha(s_1)\lambda(s_1)}{1+\alpha(s_1)}
 =\dfrac{1+\alpha(s_1)\bigl(1-\lambda(s_1)\bigr)}{1+\alpha(s_1)}$.""",
        "note": r"A published axiomatic model whose choice posterior is the damped target, with the Bayesian conditional as delivered credence.",
    },
]

# ---------------------------------------------------------------------------
# E.11  Scope, rival mechanisms
# ---------------------------------------------------------------------------
GROUNDS["E.11"] = [
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, appB\_recency, appB\_recency\_pos",
        "text": r"""For all $S_0$, $a$, $b$, $w_a$, $w_b$:
$\mathrm{estStep}_{w_b}(\mathrm{estStep}_{w_a}(S_0,a),b)
 -\mathrm{estStep}_{w_a}(\mathrm{estStep}_{w_b}(S_0,b),a)=w_a w_b\,(b-a)$,
and it is $>0$ for $w_a,w_b>0$, $a<b$ (their B.5). Here
$\mathrm{estStep}_w(S,s)=(1-w)S+ws$ is Eq. 4 applied from a prior anchor $S_0$ to
every item.""",
        "note": r"Every cue damped from a prior anchor gives recency, so that case is the rival stated in the entry.",
    },
    {
        "kind": "quote",
        "source": r"Hogarth and Einhorn 1992, T-p.35 (Appendix B) and T-p.11 (transcription)",
        "text": r"""``Anderson and Hovland (1957) assume that $w_a = w_{ba}$ and $w_b = w_{ab}$ in
which case Eq. (B.3) can be reexpressed as \ldots $D = w_a w_b[s(x_b) - s(x_a)]$. (B.5)
Because $s(x_b) > s(x_a)$, $D > 0$ and recency always obtains.'' And T-p.11:
``when $R = S_{k-1}$, the SbS process always predicts recency (for
$\alpha, \beta \ne 0$)\ldots''""",
        "note": r"Audit P10 and H15: partial adjustment of every item protects the last impression, not the first.",
    },
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, eq8\_estimation\_first\_dominates, oneSided\_primacy\_iff, twoAttr\_oneSided\_orderEffect",
        "text": r"""For $0\le w<\tfrac12$, $c_j\ge0$, $\sum_j c_j=1$: $w\,c_j<1-w$ for every $j$,
the first item's effective weight in Eq. 8 (estimation mode) exceeds every other's.
For $a<b$: $\mathrm{oneSided}(w,S_0,a,b)-\mathrm{oneSided}(w,S_0,b,a)<0\iff w<\tfrac12$.
With two attributes each moved only by its own cue:
$\mathrm{estStep}_1(S_A,s_A)-\mathrm{estStep}_w(S_A,s_A)=(1-w)(s_A-S_A)$.""",
        "note": r"With the first cue as anchor the first impression is protected, and never moved at $w=0$.",
    },
    {
        "kind": "quote",
        "source": r"Hogarth and Einhorn 1992, T-p.12 (transcription)",
        "text": r"""``In these circumstances, the structure of the EoS strategy contains within it a
force toward primacy. Moreover, this result holds whether the evidence is all
positive, all negative, or mixed. This occurs because, in the absence of an explicit
starting point, we assume that the first piece of evidence, or an amalgamation of the
first few pieces, serves as the anchor.''""",
        "note": r"The first account in the entry: the first cue adopted in full as anchor, later cues in part.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_at\_one, routeDamped\_at\_zero\_pins\_A, orderEffect\_damped\_at\_indep; PropIMM.lean, propIMM\_indep",
        "text": r"""$Q(B{=}1)\neq0\Rightarrow(\mathrm{dampedB}\,Q\,r_0\,1)(B{=}1)=1-r_0$: at $\omega=1$ the
last-read marginal is its delivered credence for every $c$. With
$Q_1=\mathrm{jeffreyA}(\mathrm{prior},q_0)$, $Q_1(B{=}0),Q_1(B{=}1)\neq0$,
$Q_1(B{=}0)+Q_1(B{=}1)=1$, $\mathrm{prior}(A{=}1)\neq0$: $P^{0}_{AB}(A{=}1)=1-q_0$ for
every $c$, the first-read one at $\omega=0$. For $\alpha,1-\alpha,\beta,1-\beta\neq0$:
$P^{\delta}_{AB}(A{=}1)-P^{\delta}_{BA}(A{=}1)\big|_{c=0}=(1-\delta)(\alpha-q_0)$, while
$\PJ_{AB}(0)=\PJ_{BA}(0)=\PB(0)$.""",
        "note": r"Which marginal ignores the association at each endpoint, and the sequence effect at $c=0$ that separates an interior weight from the benchmark.",
    },
    {
        "kind": "theorem",
        "source": r"LemmaSEP.lean, isSeparable\_applySteps; PropDEC.lean, oddsRatio\_gap\_eq\_zero, oddsRatio\_PJba\_eq\_prior",
        "text": r"""For any $N$, any $P$ on $\{0,1\}^N$ and any finite list $l$ of attribute-local
steps, $\mathrm{applySteps}\,P\,l=P\cdot\prod_a g_a(x_a)$ for some $g_a$. With
$\alpha,1-\alpha,\beta,1-\beta,q_0,1-q_0,r_0,1-r_0\neq0$, the first-step $B$-marginals,
the prior's off-diagonal cells, and the reweighted table's total and off-diagonal
cells nonzero: $\mathrm{OR}(\PJ_{AB})-\mathrm{OR}(\PB)=0$; likewise
$\mathrm{OR}(\PJ_{BA})=\mathrm{OR}(\mathrm{prior})$ with the first-step $A$-marginals
nonzero.""",
        "note": r"Every rule here is a separable reweighting carrying the prior's odds ratio, so the odds ratio separates none of them; the damped routes are checked in verify\_interior\_omega.py, row 2.",
    },
    {
        "kind": "quote",
        "source": r"Asch 1946, pp. 271-272 and p. 272",
        "text": r"""``the first terms set up in most subjects a direction which then exerts a
continuous effect on the latter terms. When the subject hears the first term, a
broad, uncrystallized but directed impression is born. The next characteristic comes
not as a separate item, but is related to the established direction.'' And p. 272:
``It is not the sheer temporal position of the item which is important as much as
the functional relation of its content to the content of the items following it.''""",
        "note": r"His account is a direction set by early terms for the reading of later ones, not a weight on position (audit A18, A19, A26).",
    },
    {
        "kind": "theorem",
        "source": r"Asch.lean, seriesA\_length, seriesB\_eq\_reverse, checkListI\_length, stimulus\_disjoint\_checklist",
        "text": r"""$|\mathrm{seriesA}|=6$ (Experiment VI, p. 270), $\mathrm{seriesB}=\mathrm{seriesA}$
reversed, $|\mathrm{checkListI}|=18$ (Table 1, p. 262), and no stimulus term occurs in
either member of any check-list pair.""",
        "note": r"Six stimulus terms read in two sequences, eighteen response traits (audit P14).",
    },
    {
        "kind": "quote",
        "source": r"Hawthorne 2004, pp. 98-99 and p. 98",
        "text": r"""``The resulting Amnestic Model raises two related concerns. First, it seems
implausible that the most recent experience or non-propositional state should
completely dictate belief strengths for basis sentences, with no regard for the
import of previous experiences or states.'' And p. 98, in the two-basis example:
``When the x-ray report comes in the physician adopts the radiologist's degree of
confidence that an image of a mass is present, $Q_e[E] = Q_{fe}[E] = .90$.''""",
        "note": r"The sentence begins on p. 98 and ends on p. 99 (audit H7); the objection follows his two-basis medical example (audit H33, P9).",
    },
    {
        "kind": "theorem",
        "source": r"Hawthorne.lean, med\_overwrite, med\_crossBasis",
        "text": r"""With his prior (likelihoods $19/20$, $Q[C]=\tfrac12$): $Q_{fe}[E]=\tfrac{9}{10}$
and $Q_e[E]=\tfrac{9}{10}$, so the $E$-marginal after $f$ then $e$ equals the one
after $e$ alone; the $F$-update alone had moved $Q[E]$ from $\tfrac12$ to
$\tfrac{22}{125}$, which the $e$-update erases.""",
        "note": r"The overwriting on two distinct bases, explicit in his own example.",
    },
]

# ---------------------------------------------------------------------------
# E.12  Scope, the two channels and the rubric prediction
# ---------------------------------------------------------------------------
GROUNDS["E.12"] = [
    {
        "kind": "computation",
        "source": r"sympy/verify\_interior\_omega.py, row 1 (39/39)",
        "text": r"""At $c=0$, symbolic in $\alpha,\beta,q_0,r_0,\delta$:
$P^{\delta}_{AB}(A{=}1)-P^{\delta}_{BA}(A{=}1)=(1-\delta)(\alpha-q_0)$ and
$P^{\delta}_{AB}(B{=}1)-P^{\delta}_{BA}(B{=}1)=-(1-\delta)(\beta-r_0)$.""",
        "note": r"The position channel on both marginals at independence; $\delta$ is the script's name for $\omega$.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_interior\_omega.py, rows 2-3",
        "text": r"""Row 2: each damped step multiplies the table it meets by a column (row) factor,
so both routes are separable reweightings of the prior; their odds ratios equal the
prior's, symbolically in the prior, the cues, $\delta$ and $c$. Row 3: the
between-sequence effect on $\assoc$ is $0$ at $c=0$ for every $\delta$; its $c^1$
coefficient is $(1-\delta)\,G$, $G=H/Z$ with
$H=q_0q_1[\beta(1-\beta)+\delta(\beta-r_0)^2]-r_0r_1[\alpha(1-\alpha)+\delta(\alpha-q_0)^2]$
irreducible over $\mathbb Q$; it vanishes exactly at $\delta=1$ or on the hypersurface
$H=0$, which each prior and cue meets at no more than one weight
$\delta=-H_0/H_1$ unless $H_0=H_1=0$ (at the generic prior, $\delta^*=31/4483$).""",
        "note": r"Odds ratio identical across sequences; association zero between sequences at $c=0$ and first order in $c$.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_interior\_omega.py, row 4",
        "text": r"""At $c=0$ and interior $\lambda$, the mean belief
$\lambda P^{\delta}_{AB}+(1-\lambda)P^{\delta}_{BA}$ differs from $\PB$ by
$(1-\lambda)(1-\delta)(q_0-\alpha)$ on the $A$-marginal and by
$-\lambda(1-\lambda)(1-\delta)^2(\alpha-q_0)(\beta-r_0)$ on $\assoc$; for $\lambda\in(0,1)$
the latter vanishes exactly at $\delta=1$, $q_0=\alpha$ or $r_0=\beta$.""",
        "note": r"The pooled cross-product association that no member holds.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_interior\_omega.py, rows 5-6",
        "text": r"""Scale the second cue's Bayes factor by $\delta$:
$W_{ij}=P_{ij}\,\ell^A_i\,(\ell^B_j)^{\delta}$ when $A$ is read first and
$W_{ij}=P_{ij}\,(\ell^A_i)^{\delta}\,\ell^B_j$ when $B$ is, then normalise. At $c=0$
the $A$-marginal gap is $\alpha(1-\alpha)(a_1a_0^{\delta}-a_0a_1^{\delta})/(\alpha a_0^{\delta}+(1-\alpha)a_1^{\delta})$,
$a_i=q_i/P(A{=}i)$, zero exactly at $\delta=1$ or $q_0=\alpha$. At $\delta=0$ both readings equal the single Jeffrey step on the
first cue, for every $c$.""",
        "note": r"The position channel is not a feature of updating on delivered credences.",
    },
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, orderEffect\_damped\_at\_indep",
        "text": r"""For $\alpha\neq0$, $1-\alpha\neq0$, $\beta\neq0$, $1-\beta\neq0$:
$P^{\delta}_{AB}(A{=}1)-P^{\delta}_{BA}(A{=}1)\big|_{c=0}=(1-\delta)(\alpha-q_0)$; the
amnestic term vanishes there by Proposition IMM.""",
        "note": r"Row 1's first identity, machine-checked.",
    },
    {
        "kind": "quote",
        "source": r"Wagner 2002, preprint p. 4, Theorem 3.1",
        "text": r"""``Theorem 3.1. Given the probability revision schema (3.1), if the Bayes factor
identities (3.2) $\beta_{r',q'}(E_{i_1}:E_{i_2})=\beta_{q,p}(E_{i_1}:E_{i_2})$, for all
$i_1,i_2$, and (3.3) $\beta_{q',p}(F_{j_1}:F_{j_2})=\beta_{r,q}(F_{j_1}:F_{j_2})$, for
all $j_1,j_2$, hold, then $r'=r$.'' Same page: ``where the sequence $(r'(E_i))$ may
differ from $(q(E_i))$, and the sequence $(q'(F_j))$ from $(r(F_j))$''.""",
        "note": r"Commutation requires each cue's Bayes factor to be the same in either position; read on the rendered page, since the text layer drops the primes.",
    },
    {
        "kind": "theorem",
        "source": r"Wagner2002.lean, thm31",
        "text": r"""For $p$ a probability, $q$ from $p$ by kinematics on $E$, $r$ from $q$ on $F$,
$q'$ from $p$ on $F$, $r'$ from $q'$ on $E$, and
$\beta_{r',q'}(E_{i_1}:E_{i_2})=\beta_{q,p}(E_{i_1}:E_{i_2})$,
$\beta_{q',p}(F_{j_1}:F_{j_2})=\beta_{r,q}(F_{j_1}:F_{j_2})$ for all indices:
$r'=r$.""",
        "note": r"Field's theorem as Wagner states it, finite partitions.",
    },
    {
        "kind": "theorem",
        "source": r"BenjaminBodohCreedRabin.lean, example\_marginal\_gap, margOddsA\_orders\_ne",
        "text": r"""Uniform prior on $\{0,1\}^2$, $\alpha=\tfrac12$, likelihood vectors
$a=b=(49/50,\,1/50)$: the believed $P(A{=}0)$ is $7/8$ when $A$ is read first and
$49/50$ when read last (the Bayes value is $49/50$). In general, at independence with
$\alpha\neq1$ and $a_0\neq a_1$, the $A$-marginal odds differ between the two
sequences.""",
        "note": r"A published rule with likelihood inputs and posterior-becomes-prior dynamics shows the position channel at independence.",
    },
    {
        "kind": "theorem",
        "source": r"Aggregate.lean, PJab\_mB1, PJba\_mA1, propDRF\_route\_AB, propDRF\_route\_BA; PropIMM.lean, propIMM\_indep",
        "text": r"""With the first-step marginal nonzero: $\PJ_{AB}(B{=}1)=1-r_0$ and
$\PJ_{BA}(A{=}1)=1-q_0$ for every $c$ (last-read exact). For
$\alpha,1-\alpha,\beta,1-\beta\neq0$:
$\frac{d}{dc}\bigl[\PJ_{AB}(A{=}1)-\PB(A{=}1)\bigr]_{0}=0$ and, with $Z\neq0$,
$\frac{d}{dc}\bigl[\PJ_{BA}(A{=}1)-\PB(A{=}1)\bigr]_{0}=K$, $K=q_0(1-q_0)(r_0-\beta)/Z$;
hence $\PJ_{AB}(A{=}1)-(1-q_0)=-cK+\bigO(c^2)$, the first-read marginal at $cK$ from
its delivered credence. At $c=0$: $\PJ_{AB}=\PJ_{BA}=\PB$.""",
        "note": r"The rubric prediction under full adoption: the sequence effect lives in the first-read belief and vanishes at independence.",
    },
]

# ---------------------------------------------------------------------------
# E.13  Back matter, AI declaration
# ---------------------------------------------------------------------------
GROUNDS["E.13"] = [
    {
        "kind": "quote",
        "source": r"PAPER\_B\_MANUSCRIPT.tex, lines 1220-1228 (author's draft), Declaration of Generative AI",
        "text": r"""``During the preparation of this work the author used Anthropic's Claude (large
language model) in order to draft and refine manuscript prose; propose, prove, and
cross-verify Proposition~\ref{prop:PRO} (uniqueness of the protected statistic), which
was then independently re-derived and machine-verified by the author; propose
bibliographic sources for positioning against neighbouring literatures; and structure
the manuscript to conform to the target journal's house style.''""",
        "note": r"Propositions ORD and ADJ were proposed, proved and cross-verified the same way (PropORD.lean, Anchoring.lean, verify\_ORD.py, check\_zero\_slope\_identification.py), so the declaration names them with PRO once E.10 is applied.",
    },
]
