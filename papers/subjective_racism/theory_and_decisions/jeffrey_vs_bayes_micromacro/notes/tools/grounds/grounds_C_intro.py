#!/usr/bin/env python3
"""Grounds for the introduction entries C.1, C.2, C.3, C.4, C.5, C.8.

Every quote was checked against the local copy of the paper on 2026-09-30
(text layer by grep; Jeffrey 1983 on the rendered scan, pdf pages 193-194 =
printed pp. 182-183).  Pages are as printed on the page.  Theorem statements
follow the Lean hypotheses; computations are the checks of
sympy/verify_interior_omega.py (23/23 pass), whose `delta` is the
manuscript's adoption weight omega (delta = 1: the later cue adopted in full;
delta = 0: the first impression never moved).
"""

GROUNDS = {}

# ---------------------------------------------------------------------------
GROUNDS["C.1"] = [
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO_protection",
         text=r"""For all real $x,y$ and all $(\alpha,\beta,\lambda,q_0,r_0)$,
$\langle xJ + y\,\nabla\mathrm{assoc}(q\otimes r),\; M_\lambda\rangle = 0$, where
$M_\lambda = \lambda\kappa R_1 + (1-\lambda)\kappa' R_2$, $R_1 = q\otimes(1,-1)$,
$R_2 = (1,-1)\otimes r$, $\kappa = (\alpha-q_0)r_0(1-r_0)/Z$,
$\kappa' = (\beta-r_0)q_0(1-q_0)/Z$ and $Z = \alpha\beta(1-\alpha)(1-\beta)$.""",
         note=r"""Proposition PRO (i): a statistic whose gradient lies in the plane
$\mathrm{span}\{J,\nabla\mathrm{assoc}\}$ has zero first-order coefficient,
identically in the prior and in the mix."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO_uniqueness and annihilator_eq_span",
         text=r"""Let $\lambda\in(0,1)$, $q_0,r_0\in(0,1)$, $\beta\neq r_0$ and
$x = \langle\nabla F,R_1\rangle$, $y = \langle\nabla F,R_2\rangle$. If
$\lambda(\alpha-q_0)r_0(1-r_0)\,x + (1-\lambda)(\beta-r_0)q_0(1-q_0)\,y = 0$ at two
priors $\alpha_1\neq\alpha_2$, then $x = y = 0$; and any $V$ with
$\langle V,R_1\rangle = \langle V,R_2\rangle = 0$ equals
$(V_{01} + r_0(V_{00}-V_{01}))J + \frac{V_{00}-V_{01}}{1-q_0}\nabla\mathrm{assoc}$.""",
         note=r"""Proposition PRO (ii): protection on an open set of priors forces
$\nabla F$ into the plane, so the classification is an ``if and only if''."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, margA_not_in_span; Aggregate.lean, propDRF",
         text=r"""For $0<r_0<1$ the $A$-marginal read-out $(0,0,1,1)$ is not of the
form $xJ + y\,\nabla\mathrm{assoc}(q_0,r_0)$. For $\alpha,\beta\notin\{0,1\}$ and
$Z\neq0$,
$\frac{d}{dc}\big[\lambda(P^J_{AB}-P^B)(A{=}1) + (1-\lambda)(P^J_{BA}-P^B)(A{=}1)\big]_{c=0}
= (1-\lambda)\,q_0(1-q_0)(r_0-\beta)/Z$.""",
         note=r"""Proposition DRF: a marginal departs at first order for every
$\lambda<1$, so ``an arbitrary statistic'' is not an order closer; only the
protected ones are."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, lemmaASC_first_order_vanishes (and _BA)",
         text=r"""For all $q_0,r_0,t,\kappa$:
$\mathrm{assoc}\big(q\otimes r + t\kappa R_1\big) = \mathrm{assoc}(q\otimes r)
+ t^2\,\mathrm{assoc}(\kappa R_1)$, and likewise along $R_2$: the first-order
term $t\,\langle\nabla\mathrm{assoc},\kappa R_\sigma\rangle$ is identically zero.""",
         note=r"""Proposition DEC: the believed association is in the protected
class, second order along either route."""),
    dict(kind="theorem",
         source=r"Decision.lean, volume_flipSet and lintegral_stake_le_volume",
         text=r"""With $u$ the benchmark surplus, $\delta$ the first-order score
coefficient and $\mathrm{Flips}(u,c,\delta) := \neg\big(0\le u \iff 0\le u + c\delta\big)$:
$\mathrm{Leb}\{u : \mathrm{Flips}(u,c,\delta)\} = |c\delta|$ and
$\int_{\{\mathrm{Flips}\}} |u|\,du \le |c\delta|^2$, for all real $c,\delta$.""",
         note=r"""Proposition SHR and Theorem LOS: the affected share is first order,
the surplus-weighted loss second order."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.2"] = [
    dict(kind="quote",
         source=r"Zhao and Osherson 2010, p. 290 (opening part, before Experiment 1)",
         text=r"""``The normative status of Jeffrey's rule has nonetheless been
questioned because successive uses produce distinct distributions depending on
the order in which events are considered (D\"{o}ring, 1999). In our view, such
doubts disappear on closer inspection of the evidential weight of probability
judgements (Osherson, 2002; Wagner, 2002).''""",
         note=r"""They report the doubt as settled, not the mechanism as ``unclear''."""),
    dict(kind="quote",
         source=r"Jeffrey 1983, pp. 182-183 (Section 11.11)",
         text=r"""``It is straightforward to verify that the present kinematical
scheme is not generally commutative \ldots the resulting probability measure
may be one thing or another depending on the order of the two applications.
That is as it should be, for just after the $A$-application, one's degrees of
belief in the $A_i$ \emph{are} $a_i$, no matter what they were before \ldots
The notion that the kinematical scheme \emph{ought} to be generally commutative
(Domotor, \emph{Philosophy of Science} 47 [1980]: 395) stems from a conflation
of two attitudes toward The Given''""",
         note=r"""Jeffrey holds non-commutativity to be correct and the demand for
commutativity to rest on a conflation."""),
    dict(kind="quote",
         source=r"Diaconis and Zabell 1982, p. 827, Section 4.2, Remark 2",
         text=r"""``Thus noncommutativity is not a real problem for successive
Jeffrey updating.''""",
         note=r"""D-Z are on the side of the sequence effect, not of the doubt."""),
    dict(kind="quote",
         source=r"Hawthorne 2004, p. 99 (Section 5)",
         text=r"""``The second concern is that, as a result of this amnesia, the
order in which states or experiences are acquired will almost always have a
very significant influence on an agent's belief strengths. This order effect
is very troubling.''""",
         note=r"""The one cited author who treats the order effect as a defect; the
three sources disagree, which is a debate."""),
    dict(kind="theorem",
         source=r"Decision.lean, lintegral_stake_le_volume with volume_flipSet",
         text=r"""For all real $c,\delta$:
$\int_{\{u:\mathrm{Flips}(u,c,\delta)\}} |u|\,du \le |c\delta|\cdot|c\delta|$,
whereas $\mathrm{Leb}\{u:\mathrm{Flips}(u,c,\delta)\} = |c\delta|$.""",
         note=r"""Theorem LOS: the loss carries the sequence effect at second order;
it is not free of it, and the share (Proposition SHR) is first order."""),
    dict(kind="theorem",
         source=r"Hawthorne.lean, factorUpdate\_comm; Cripps.lean, order\_invariance",
         text=r"""With $\mathrm{factorUpdate}(p,u,w)(x)=w(u(x))\,p(x)/\sum_y w(u(y))\,p(y)$, a
belief multiplied by fixed factors $w$ on one partition and $z$ on another, both
normalisers nonzero:
$\mathrm{factorUpdate}(\mathrm{factorUpdate}(p,u,w),v,z)
=\mathrm{factorUpdate}(\mathrm{factorUpdate}(p,v,z),u,w)$. Likewise, under Cripps's
Symmetry and Divisibility, two signals with fixed likelihoods give the same posterior
in either sequence.""",
         note=r"""(sentence 1) The first half of the modus tollens. Read as Bayes factors,
fixed before either cue arrives, the cues cannot leave a trace of the sequence."""),
    dict(kind="computation",
         source=r"sympy/verify\_soft\_vs\_hard.py, symbolic checks (15/15)",
         text=r"""Symbolic in $\alpha,\beta,c,q_0,r_0$: a credential read first moves the
$B$-marginal the letter meets to $\beta+c(q_0-\alpha)/(\alpha(1-\alpha))$, so the
factor the letter implies changes unless $c=0$ or $q_0=\alpha$, and symmetrically for
the credential. Every cell of $\PJ_{AB}-\PJ_{BA}$ carries the factor $c$, and with
$c\neq0$ the difference vanishes if and only if $q_0=\alpha$ and $r_0=\beta$.""",
         note=r"""(sentence 1) Under the delivered-credence reading the sequence leaves a
trace exactly when an earlier cue changes the factor a later one implies, which is
the lived-experience point in the model's terms. The sentence itself claims only the
first half; this row shows that the delivered-credence reading, one of the readings it
leaves open, is one in which what came earlier changes how later cues are read."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.3"] = [
    dict(kind="theorem",
         source=r"Asch.lean, seriesA_length, seriesB_eq_reverse, checkListI_length, allTraits_length, table7_rows_are_checklist_pairs, d_ne_zero, restrained_64_9",
         text=r"""$|\mathrm{Series\,A}| = 6$ and $\mathrm{Series\,B} = \mathrm{reverse}(\mathrm{Series\,A})$;
$|\mathrm{Check\,List\,I}| = 18$ and Table 7 has $18$ rows, row $i$ naming a member
of pair $i$; for every one of the $18$ traits
$\mathrm{ie}(t) - \mathrm{ei}(t) \neq 0$; e.g.\ restrained $64$ vs $9$,
good-looking $74$ vs $35$, humorous $52$ vs $21$, good-natured $18$ vs $0$.""",
         note=r"""Experiment VI reports eighteen per-trait percentages under two
orders, i.e.\ eighteen marginals, not a single score."""),
    dict(kind="quote",
         source=r"Asch 1946, pp. 270-271, Experiment VI and Table 7",
         text=r"""p.~270: ``The two series are identical with regard to their
members, differing only in the order of succession of the latter.''
p.~271: ``The check-list data appearing in Table 7 furnish quantitative
support for the conclusions drawn from the written sketches.''""",
         note=r"""The check list is Asch's own quantitative instrument for the
order comparison."""),
    dict(kind="quote",
         source=r"Diaconis and Zabell 1982, p. 827, Section 4.2, Remarks 1-2",
         text=r"""``Remark 1. There is no reason to require
$P_{\mathcal{E}\mathcal{F}} = P_{\mathcal{F}\mathcal{E}}$ for successive
updating to be useful and valid. \ldots Remark 2. \ldots Thus noncommutativity
is not a real problem for successive Jeffrey updating.''""",
         note=r"""D-Z do not hold the defect view the manuscript groups them with."""),
    dict(kind="quote",
         source=r"Hawthorne 2004, pp. 96 and 108 (Sections 4 and 8)",
         text=r"""p.~96: ``Indeed, it may turn out that there is no one true theory of
uncertain updating -- that each theory has its uses, its domain of
applicability.'' p.~108: ``Let us call this the Basis-Commuting Version of the
Likelihood-Ratio Update Model. Sequential updating is completely
order-independent on this model.''""",
         note=r"""He offers alternatives rather than a repair, and full order-freedom
is claimed only for the Basis-Commuting Version."""),
    dict(kind="theorem",
         source=r"Hawthorne.lean, factorUpdate_comm and extUpdate_basisCommuting",
         text=r"""For bases $u:\Omega\to\iota$, $v:\Omega\to\kappa$ and factor vectors
$w,z$ with $\sum_y w(u(y))p(y)\neq0$ and $\sum_y z(v(y))p(y)\neq0$:
$\mathrm{fU}(\mathrm{fU}(p,u,w),v,z) = \mathrm{fU}(\mathrm{fU}(p,v,z),u,w)$. If the
factor $\Lambda_k(\gamma)$ of every basis-homogeneous subsequence is invariant
under permutation of $\gamma$, then $\mathrm{extUpdate}(p,\delta) =
\mathrm{extUpdate}(p,\delta')$ for every permutation $\delta'$ of $\delta$.""",
         note=r"""Factor updates commute across distinct bases; commutation within a
basis, hence full order-freedom, needs the extra Basis-Commuting hypothesis."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.4"] = [
    dict(kind="theorem",
         source=r"HogarthEinhorn.lean, appB_recency, appB_recency_pos, oneSided_eq_eq8, oneSided_primacy_iff",
         text=r"""With $\mathrm{est}(w,S,s) = S + w(s-S)$ (HE Eq.~3/4): for all reals,
$\mathrm{est}(w_b,\mathrm{est}(w_a,S_0,a),b) - \mathrm{est}(w_a,\mathrm{est}(w_b,S_0,b),a)
= w_a w_b (b-a)$, which is $>0$ when $w_a,w_b>0$ and $a<b$ (recency). The
one-sided rule $\mathrm{est}(w,\mathrm{est}(1,S_0,a),b)$ equals HE's Eq.~8 with
$k=2$, and for $a<b$ it gives $S_{ab}-S_{ba}<0$ iff $w<1/2$.""",
         note=r"""Damping every cue gives recency; primacy comes from adopting the
first cue in full (Eq.~8), and then only for $w<1/2$."""),
    dict(kind="quote",
         source=r"Hogarth and Einhorn 1992, transcription T-p. 35 (Appendix B, Eq. B.5) and T-p. 12 (Eq. 8)",
         text=r"""T-p.~35: ``$D = w_a w_b[s(x_b) - s(x_a)]$. Because $s(x_b) > s(x_a)$,
$D > 0$ and recency always obtains.'' T-p.~12: ``$S_k = s(x_1) + w_k[s(x_2,\ldots,x_k) - R]$
\ldots the effective weight accorded to $s(x_1)$ must be greater than the weight
attached to any of the other pieces of evidence.''""",
         note=r"""Their step-by-step partial adjustment predicts recency; their
primacy is the End-of-Sequence anchoring on the first item."""),
    dict(kind="quote",
         source=r"Asch 1946, p. 272",
         text=r"""``It is not the sheer temporal position of the item which is
important as much as the functional relation of its content to the content of
the items following it.''""",
         note=r"""Asch denies a position channel, so he cannot be cited for ``weights
the later cue less''."""),
    dict(kind="theorem",
         source=r"BenjaminBodohCreedRabin.lean, margOddsA_orders_ne and example_marginal_gap",
         text=r"""On the $2\times2$ joint with prior $u\otimes v$ (independent, $c=0$),
$\alpha\neq1$, positive cue likelihoods $a$ on $A$ and $b$ on $B$, the $A$
cue informative ($a_0\neq a_1$): the odds $P(A{=}0)/P(A{=}1)$ after base-rate-neglect updating
differ between the orders $AB$ and $BA$. Instance: uniform prior,
$\alpha=1/2$, $a=b=(49/50,1/50)$ gives $P(A{=}0)=7/8$ reading $A$ first and
$49/50$ reading $A$ last.""",
         note=r"""A position channel operates with likelihood inputs adopted in full,
so ``which channels operate is fixed by adoption'' holds for this model only."""),
    dict(kind="quote",
         source=r"Diaconis and Zabell 1982, p. 825 (Section 3.1) and p. 827 (Section 4.2)",
         text=r"""p.~825: ``To use Jeffrey's rule at the second stage we must, of
course, accept the J-condition \ldots Clearly, the order of updating matters,
since the second opinion dominates.'' p.~827: ``2. When is successive updating
reasonable?''""",
         note=r"""Successive updating with each impression adopted in full is D-Z's
setting, examined, not a premise they lay down."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.5"] = [
    dict(kind="computation",
         source=r"sympy/verify_interior_omega.py, check (3)",
         text=r"""Between-sequence effect on $\mathrm{assoc}$ with the second cue
adopted with weight $\delta$ (the manuscript's $\omega$): the $c^0$ term is $0$
for every $\delta$; the $c^1$ coefficient is $(1-\delta)\,G$ with $G$ affine in
$\delta$, $G\neq0$ at the generic prior for $\delta=1/2$ and for $\delta=0$; at
$\delta=1$ the factor $(1-\delta)$ makes the effect second order in $c$.""",
         note=r"""Under partial adoption the cross-product association carries the
sequence at first order in $c$."""),
    dict(kind="computation",
         source=r"sympy/verify_interior_omega.py, check (4)",
         text=r"""Pooled belief $\lambda P_{AB} + (1-\lambda)P_{BA}$ at $c=0$:
$\mathrm{assoc}(\bar P) - \mathrm{assoc}(P^B)
= -\lambda(1-\lambda)(1-\delta)^2(\alpha-q_0)(\beta-r_0)$, nonzero for interior
$\lambda,\delta$ whenever both cues differ from the prior marginals, and zero
at $\delta=1$.""",
         note=r"""The pooled association is distorted already at $c=0$ unless the
later cue is adopted in full."""),
    dict(kind="computation",
         source=r"sympy/verify_interior_omega.py, check (2)",
         text=r"""Each damped step multiplies the table it meets by a factor constant
along rows (resp.\ columns), so both routes are separable reweightings of the
prior and $\mathrm{OR}(P_{AB}) = \mathrm{OR}(P_{BA}) = \mathrm{OR}(P)$
for every $\delta$ (witnessed at $\delta=1/3$, $c=1/40$, generic prior).""",
         note=r"""Only the odds ratio is identical across sequences for every
adoption weight."""),
    dict(kind="theorem",
         source=r"LemmaSEP.lean, isSeparable_applySteps and sep_rescales_association",
         text=r"""For $N$ binary attributes, any finite sequence of attribute-local
updates gives $P'(x) = P(x)\prod_a g_a(x_a)$; and for $i\neq j$ such an $F$
satisfies $F_{11}F_{00} - F_{10}F_{01} = g_i(1)g_i(0)g_j(1)g_j(0)\,
\big(\prod_{a\neq i,j} g_a(x_a)\big)^2\,(P_{11}P_{00} - P_{10}P_{01})$.""",
         note=r"""Lemma SEP: a route rescales the association and cannot shift it,
whence the odds ratio is invariant; the cross-product itself is not."""),
    dict(kind="theorem",
         source=r"PropDEC.lean, oddsRatio_gap_eq_zero",
         text=r"""For $\alpha,\beta,q_0,r_0\notin\{0,1\}$, nonzero intermediate
$B$-marginals, nonzero off-diagonal prior cells and nonzero benchmark
normaliser and off-diagonal cells:
$\mathrm{OR}(P^J_{AB}) - \mathrm{OR}(P^B) = 0$, identically in $c$.""",
         note=r"""The odds-ratio form of the panel sentence is exact; the
cross-product form is protected only at $\omega=1$ (Proposition DEC)."""),
]

# ---------------------------------------------------------------------------
GROUNDS["C.8"] = [
    dict(kind="theorem",
         source=r"PropDEC.lean, assoc_PJab; PropPRO.lean, lemmaASC_first_order_vanishes",
         text=r"""$\mathrm{assoc}(P^J_{AB}) = \frac{q_0}{\alpha}\frac{1-q_0}{1-\alpha}
\frac{r_0}{m_0}\frac{1-r_0}{m_1}\,c$ with $m_j$ the $B$-marginal after the
$A$-step; and along either route direction
$\mathrm{assoc}(q\otimes r + t\kappa R_\sigma) = \mathrm{assoc}(q\otimes r)
+ t^2\,\mathrm{assoc}(\kappa R_\sigma)$.""",
         note=r"""Proposition DEC: the believed association departs from the
benchmark only at second order in $c$."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO_protection and margA_not_in_span",
         text=r"""$\langle xJ + y\,\nabla\mathrm{assoc},\,\lambda\kappa R_1 + (1-\lambda)\kappa' R_2\rangle = 0$
for all $x,y,\alpha,\beta,\lambda$; whereas for $0<r_0<1$ the marginal read-out
$(0,0,1,1)$ is not in $\mathrm{span}\{J,\nabla\mathrm{assoc}\}$.""",
         note=r"""Proposition PRO: which statistic is aggregated decides whether the
sequence effect is first or second order."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO_uniqueness",
         text=r"""For $\lambda\in(0,1)$, $q_0,r_0\in(0,1)$, $\beta\neq r_0$: if the
aggregate first-order coefficient
$\lambda(\alpha-q_0)r_0(1-r_0)x + (1-\lambda)(\beta-r_0)q_0(1-q_0)y$ vanishes at
two priors $\alpha_1\neq\alpha_2$, then $x=y=0$.""",
         note=r"""The coefficient is a property of the population's belief, not of a
sample; averaging over more evaluators does not change it."""),
]
