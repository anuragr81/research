"""Grounds for E.1, E.2, E.3, E.4, E.8 and E.9 (notes/tools/plan_entries_E.py).

Quotes are verbatim from the papers under the scratchpad papers/ folder and
were checked against the rendered page where the file is a scan (Phelps p. 659,
Arrow p. 26, Jeffrey 2002 draft p. 59) or against the audit's rendered-page
reading (Asch pp. 271-272).  Hogarth-Einhorn pages are T-pages of the
transcription; Bohren-Imas-Rosenberg pages are the January 2019 working
paper's printed pages (printed page = PDF page - 1).  Theorem statements copy
the Lean hypotheses.  Numbers are the exact rationals the sympy scripts assert.
"""

GROUNDS = {}

# ---------------------------------------------------------------------------
GROUNDS["E.1"] = [
    dict(kind="computation",
         source=r"sympy/verify\_ORD.py, step (9), with steps (1), (3)--(5)",
         text=r"""With $\Pbar_\lambda=\lambda\,\PJ_{AB}+(1-\lambda)\,\PJ_{BA}$, the identity
$\Pbar_{\lambda}-\Pbar_{\lambda'}=(\lambda-\lambda')\,(\PJ_{AB}-\PJ_{BA})$ holds entrywise
and exactly, at every order in $c$ (symbolic in $\alpha,\beta,c,q_0,r_0,\lambda,\lambda'$).
Since $\PJ_{AB}-\PJ_{BA}=c\,(\kappa R_1-\kappa' R_2)+\bigO(c^2)$, the group gap on the
$A$-marginal is $(\lambda-\lambda')\,\kappa'\,c+\bigO(c^2)$ with
$\kappa'=(\beta-r_0)q_0(1-q_0)/Z$, while on the believed association it is $\bigO(c^2)$
(checked at $\lambda=2/5$ against $\lambda'=3/4$).""",
         note=r"""Two groups with the same priors, preferences and evidence differ by the
sequence effect multiplied by the difference in their shares, and the gap inherits
Proposition ORD's classification (marginals first order, association second order)."""),
    dict(kind="quote",
         source=r"Bohren, Imas and Rosenberg, January 2019 working paper, pp.~2, 13--14, 19--20",
         text=r"""``we allow for three potential sources: (i) belief-based with correct beliefs,
(ii) belief-based with incorrect, biased beliefs, and (iii) preference-based'' (p.~2).
Of the first, ``discrimination is due to imperfect information (Phelps 1972)'' or ``is an
equilibrium effect (Arrow 1973)'' (pp.~13--14), and ``evaluators hold prior beliefs about
workers' abilities that differ by group identity'' (p.~14). The impartial type ``has no
belief-based partiality'' (p.~19), yet ``this type discriminates against males in the
second period'' (p.~20).""",
         note=r"""The three sources are named rather than treated as exhaustive, since BIR's own
impartial type discriminates with correct beliefs and no animus (audit D13)."""),
    dict(kind="quote",
         source=r"Phelps 1972, p.~659; Arrow 1973, p.~26 (working paper)",
         text=r"""Phelps: ``the employer who seeks to maximize expected profit will discriminate
against blacks or women if he believes them to be less qualified, reliable, long-term,
etc. on the average than whites and men, respectively, and if the cost of gaining
information about the individual applicants is excessive.'' Arrow: ``The employer cannot
know of any given worker whether or not he is qualified; however, he does believe that the
probability that a random W worker is qualified is $p_W$ and that a random B worker is
qualified is $p_B$.''""",
         note=r"""Both rest on prior beliefs about the groups, the wording the entry uses (audit P4);
Phelps's Case 2 and Further Case rest on variance and test reliability rather than on
mean, so ``different beliefs'' alone would be too broad."""),
]

# ---------------------------------------------------------------------------
GROUNDS["E.2"] = [
    dict(kind="theorem",
         source=r"DiaconisZabell.lean, marg\_jeffrey and jeffrey\_jcond",
         text=r"""On a finite $\Omega$ with a partition $\{E_i\}$ given by a surjective labelling
$e$ and a prior $P>0$, Jeffrey's rule $P^*(\omega)=p_{e(\omega)}\,P(\omega)/P(E_{e(\omega)})$
satisfies $P^*(E_i)=p_i$ for every cell (marg\_jeffrey) and, provided
$p_{e(\omega)}\neq0$ for all $\omega$, $P^*(A\mid E_i)=P(A\mid E_i)$ for every event $A$
and every cell $i$ (jeffrey\_jcond).""",
         note=r"""An impression is a probability distribution on one attribute's partition; the step
installs it as the new marginal and leaves every conditional on that partition unchanged."""),
    dict(kind="theorem",
         source=r"Aggregate.lean, jeffreyA\_pins\_mA0, jeffreyA\_pins\_mA1, jeffreyB\_pins\_mB1",
         text=r"""For the $2\times2$ table $Q$,
$J_A(Q,q_0)=\bigl(\begin{smallmatrix} q_0Q_{00}/Q(A{=}0) & q_0Q_{01}/Q(A{=}0)\\
(1-q_0)Q_{10}/Q(A{=}1) & (1-q_0)Q_{11}/Q(A{=}1)\end{smallmatrix}\bigr)$; if $Q(A{=}0)\neq0$
then $J_A(Q,q_0)(A{=}0)=q_0$, and if $Q(A{=}1)\neq0$ then $J_A(Q,q_0)(A{=}1)=1-q_0$,
identically in $c$; likewise $J_B(Q,r_0)(B{=}1)=1-r_0$ when $Q(B{=}1)\neq0$.""",
         note=r"""The impression $(q_0,1-q_0)$ is on the same scale as the prior marginal
$(\alpha,1-\alpha)$ it replaces, and $q_1=1-q_0$ is built into the step."""),
]

# ---------------------------------------------------------------------------
GROUNDS["E.3"] = [
    dict(kind="computation",
         source=r"sympy/verify\_example.py (18/18 checks)",
         text=r"""At $\alpha=\beta=\tfrac12$, $c=\tfrac1{20}$, $q_0=\tfrac15$, $r_0=\tfrac7{10}$:
prior $\bigl(\begin{smallmatrix}3/10&1/5\\1/5&3/10\end{smallmatrix}\bigr)$; after the
credential $\bigl(\begin{smallmatrix}3/25&2/25\\8/25&12/25\end{smallmatrix}\bigr)$ with
$B$-marginal $11/25$;
$\PJ_{AB}=\bigl(\begin{smallmatrix}21/110&3/70\\28/55&9/35\end{smallmatrix}\bigr)$,
$\PJ_{BA}=\bigl(\begin{smallmatrix}7/45&2/45\\56/115&36/115\end{smallmatrix}\bigr)$,
$\PB=\bigl(\begin{smallmatrix}3/17&6/119\\8/17&36/119\end{smallmatrix}\bigr)$. Hence
$\PJ_{AB}(A{=}0)=18/77\approx.234$, $\PJ_{AB}(B{=}0)=7/10$, $\PJ_{BA}(A{=}0)=1/5$,
$\PJ_{BA}(B{=}0)=133/207\approx.643$, $\PB(A{=}0)=27/119\approx.227$,
$\PB(B{=}0)=11/17\approx.647$; $\assoc(\PJ_{AB})=3/110\approx.0273$,
$\assoc(\PJ_{BA})=28/1035\approx.0271$, $\assoc(\PB)=60/2023\approx.0297$; sequence
effects $13/385\approx.034$, $119/2070\approx.057$, $1/4554\approx.0002$ (ratio
$5382/35>150$); gaps of $AB$ from $\PB$: $9/170\approx.053$, $9/1309\approx.007$,
$531/222530\approx.002$; odds ratio $9/4$ for all four tables.""",
         note=r"""Every number printed in the worked example, in exact rationals."""),
    dict(kind="computation",
         source=r"sympy/verify\_soft\_vs\_hard.py (15/15 checks)",
         text=r"""The letter's implied factor, the odds it delivers over the odds it meets, is
$\frac{7/3}{1}=7/3$ when read first (against $\beta=\tfrac12$) and
$\frac{7/3}{11/14}=98/33$ when read after the credential (against $11/25$); at $c=0$
both equal $7/3$. Hard cues with fixed likelihood ratios $1/4$ (credential) and $7/3$
(letter) commute cell by cell, $E$ then $F$ equals $F$ then $E$, and reproduce $\PB$,
with $A$-marginal $27/119\approx.227$ in either sequence. On one cue the hard and soft
readings coincide, $\mathrm{cond}(P,\ell_A)=J_A(P,q)$ (Proposition IMM).""",
         note=r"""Every number printed in the soft-versus-hard paragraph."""),
    dict(kind="theorem",
         source=r"DiaconisZabell.lean, thm21 and thm21\_finite (D-Z Theorem 2.1, p.~824)",
         text=r"""For finite $\Omega$ with $\sum_\omega P(\omega)=1$, $P^*\ge0$ and
$\sum_\omega P^*(\omega)=1$: $P^*$ can be obtained from $P$ by conditioning on an event
of a finite enlarged space iff there is $B\ge1$ with $P^*(\omega)\le B\,P(\omega)$ for
all $\omega$; when $P>0$ this always holds.""",
         note=r"""Any soft posterior, the Jeffrey step included, is a conditioning on some richer
space; what a soft cue lacks is a proposition with a likelihood fixed before the belief
is formed, not conditionability."""),
    dict(kind="theorem",
         source=r"Cripps.lean, composite\_AB\_eq\_bayes (with jeffreyA\_eq\_bayes\_matched)",
         text=r"""For a full-support belief $\mu$ on $A\times B$ and credences $q>0$, $r$ with
$\sum_a q_a=\sum_b r_b=1$:
$J_B(J_A(\mu,q),r)=\mathrm{bayes}(\mu,\ell)$ with
$\ell(a,b)=\dfrac{q_a}{\mu(A{=}a)}\cdot\dfrac{r_b}{J_A(\mu,q)(B{=}b)}$, a single Bayes
update whose $B$-likelihood is matched to the intermediate belief $J_A(\mu,q)$, not to
$\mu$.""",
         note=r"""The factor a soft cue implies is matched to the marginal in force when it arrives;
the two sequences feed different likelihoods into the same commuting rule, which is the
whole of the sequence effect."""),
    dict(kind="quote",
         source=r"Jeffrey, \emph{Subjective Probability} (2002 draft), p.~59, Example 4",
         text=r"""``Certainly her $\mathit{new}'(D_i)$'s will have arisen through an interaction of
features of her prior mental state with her new experiences at the microscope. But the
results $\mathit{new}'(D_i)$ of that interaction are \emph{data} for the kinematical
formula for updating $\mathit{old}'(H)$; the formula itself does not compute those
results.''""",
         note=r"""One's own experience yields credences that are data for kinematics, while another's
report is what Jeffrey converts to factors (Section 3.3, p.~59), so a panelist's own
impression is modelled as a credence and the counterfactual-likelihood argument is
dropped (audit J2)."""),
]

# ---------------------------------------------------------------------------
GROUNDS["E.4"] = [
    dict(kind="theorem",
         source=r"PropDIV.lean, kappa\_eq\_zero\_iff and kappa'\_eq\_zero\_iff",
         text=r"""With $Z=\alpha\beta(1-\alpha)(1-\beta)\neq0$,
$\kappa=(\alpha-q_0)\,r_0(1-r_0)/Z$ and $\kappa'=(\beta-r_0)\,q_0(1-q_0)/Z$:
$\kappa=0 \iff \alpha=q_0 \ \text{or}\ r_0=0 \ \text{or}\ r_0=1$, and
$\kappa'=0 \iff \beta=r_0 \ \text{or}\ q_0=0 \ \text{or}\ q_0=1$.""",
         note=r"""A cue with $r_0\in\{0,1\}$ is decisive and kills the leading gap, so ``soft'' means
$0<q_0<1$ and $0<r_0<1$, the interior credences Assumption 2 now states."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO\_uniqueness (hypotheses)",
         text=r"""Proposition PRO (ii) is proved under $0<\lambda<1$, $0<q_0<1$, $0<r_0<1$,
$\beta\neq r_0$ and two priors $\alpha_1\neq\alpha_2$; the interior bounds on $q_0,r_0$
are what make the factors $r_0(1-r_0)$ and $q_0(1-q_0)$ nonzero in the leading
coefficient $\lambda(\alpha-q_0)r_0(1-r_0)\,x+(1-\lambda)(\beta-r_0)q_0(1-q_0)\,y$.""",
         note=r"""The same interior condition on the credences is a hypothesis of the paper's
central characterisation."""),
]

# ---------------------------------------------------------------------------
GROUNDS["E.8"] = [
    dict(kind="theorem",
         source=r"PropDIV.lean, propDIV\_gap\_AB\_a00..a11, propDIV\_gap\_BA\_a00..a11, propDIV\_seq\_a00..a11",
         text=r"""For $\alpha,\beta\notin\{0,1\}$ and each cell $(i,j)$, as derivatives at $c=0$:
$\frac{d}{dc}\bigl[\PJ_{AB}-\PB\bigr]_{ij}=\kappa\,(R_1)_{ij}$,
$\frac{d}{dc}\bigl[\PJ_{BA}-\PB\bigr]_{ij}=\kappa'\,(R_2)_{ij}$,
$\frac{d}{dc}\bigl[\PJ_{AB}-\PJ_{BA}\bigr]_{ij}=\kappa(R_1)_{ij}-\kappa'(R_2)_{ij}$, with
$R_1=q\otimes(1,-1)$, $R_2=(1,-1)\otimes r$, $\kappa=(\alpha-q_0)r_0(1-r_0)/Z$,
$\kappa'=(\beta-r_0)q_0(1-q_0)/Z$. Every posterior entry is a ratio of functions affine
in $c$, so this is the exact first-order term.""",
         note=r"""Proposition DIV: the gap from the benchmark is first order in $c$ on either
sequence, with nonzero coefficient off the locus of kappa\_eq\_zero\_iff."""),
    dict(kind="theorem",
         source=r"Aggregate.lean, lemmaSCR\_gap\_AB, lemmaSCR\_gap\_BA, lemmaSCR\_protected\_weight",
         text=r"""For any weight $v\in\mathbb{R}^{2\times2}$ and $\alpha,\beta\notin\{0,1\}$:
$\frac{d}{dc}\langle v,\PJ_{AB}-\PB\rangle\big|_{c=0}=\kappa\,\langle v,R_1\rangle$ and
$\frac{d}{dc}\langle v,\PJ_{BA}-\PB\rangle\big|_{c=0}=\kappa'\,\langle v,R_2\rangle$;
for $v=xJ+y\,\nabla\!\assoc(q_0,r_0)$ both coefficients are $0$.""",
         note=r"""Lemma SCR: the score-gap $s(P)=\langle\vv,P\rangle$ is first order for a generic
desirability vector and second order only for weights in the protected span."""),
    dict(kind="theorem",
         source=r"PropORD.lean, lemmaORD\_gap, propORD\_Amarg, propORD\_Bmarg, propORD\_const",
         text=r"""For any $v$ and $\alpha,\beta\notin\{0,1\}$:
$\frac{d}{dc}\langle v,\PJ_{AB}-\PJ_{BA}\rangle\big|_{c=0}=\kappa\langle v,R_1\rangle-\kappa'\langle v,R_2\rangle$;
in particular $\frac{d}{dc}\bigl[\PJ_{AB}(A{=}1)-\PJ_{BA}(A{=}1)\bigr]_{c=0}=\kappa'$,
$\frac{d}{dc}\bigl[\PJ_{AB}(B{=}1)-\PJ_{BA}(B{=}1)\bigr]_{c=0}=-\kappa$, and the constant
weight gives $0$. The right-hand side mentions neither $\PB$ nor $\lambda$.""",
         note=r"""Proposition ORD: whether a statistic separates the two sequences at first order is
fixed by its differential against $R_1$ and $R_2$, so the classification is settled for
one evaluator before any averaging."""),
    dict(kind="computation",
         source=r"sympy/verify\_ORD.py, steps (5) and (8) (49/49)",
         text=r"""$\assoc(\PJ_{AB})-\assoc(\PJ_{BA})$ has $c^0$ and $c^1$ coefficients zero identically and
$c^2$ coefficient $q_0q_1r_0r_1H_{\mathrm{ord}}/Z^2$,
$H_{\mathrm{ord}}=(q_0-\alpha)(2r_0-1)-(r_0-\beta)(2q_0-1)$ irreducible, so it is second order off
the hypersurface $H_{\mathrm{ord}}=0$; and $\langle\nabla\!\assoc,\ \kappa R_1-\kappa'R_2\rangle=0$.
Requiring $\kappa\langle G,R_1\rangle-\kappa'\langle G,R_2\rangle=0$ across priors, symbolically in
$(q_0,r_0)$, leaves exactly $\mathrm{span}\{J,\nabla\!\assoc\}$ (a $2\times2$ minor $-q_0^2q_1r_0r_1$
never vanishes on the cube), each member annihilating $R_1$ and $R_2$;
$\det\,\partial(\kappa,\kappa')/\partial(\alpha,\beta)=q_0q_1r_0r_1J_p/Z^3$, nonzero off the hypersurface
$J_p=0$.""",
         note=r"""Marginals and generic scores are first order, the association second order, and the
dividing line is annihilation of $\mathrm{span}\{R_1,R_2\}$, the same line Proposition PRO
draws for the aggregate."""),
]

# ---------------------------------------------------------------------------
GROUNDS["E.9"] = [
    dict(kind="theorem",
         source=r"PropPRO.lean, annihilator\_eq\_span (with propPRO\_protection\_each, R1\_R2\_indep)",
         text=r"""If $1-q_0\neq0$ and $\langle V,R_1\rangle=\langle V,R_2\rangle=0$ then
$V=\bigl(V_{01}+r_0(V_{00}-V_{01})\bigr)\,J+\dfrac{V_{00}-V_{01}}{1-q_0}\,\nabla\!\assoc(q_0,r_0)$,
where $J$ is the all-ones matrix and
$\nabla\!\assoc(q_0,r_0)=\bigl(\begin{smallmatrix}(1-q_0)(1-r_0)&-(1-q_0)r_0\\-q_0(1-r_0)&q_0r_0\end{smallmatrix}\bigr)$;
conversely every $xJ+y\,\nabla\!\assoc$ annihilates $R_1$ and $R_2$. For $q_0\neq0$,
$R_1$ and $R_2$ are linearly independent.""",
         note=r"""The annihilator of the plane spanned by the two leading directions is exactly
$\mathrm{span}\{J,\nabla\assoc\}$."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO\_protection",
         text=r"""With $M_\lambda=\lambda\kappa R_1+(1-\lambda)\kappa' R_2$ the leading direction of
$\Pbar_\lambda-\PB$: for all $x,y$ and all $\alpha,\beta,\lambda$,
$\langle xJ+y\,\nabla\!\assoc(q_0,r_0),\ M_\lambda\rangle=0$.""",
         note=r"""Protection is annihilation of each route direction, identically in the mixture,
not a cancellation that depends on $\lambda$."""),
    dict(kind="theorem",
         source=r"PropPRO.lean, propPRO\_uniqueness and coeff\_times\_Z (with margA\_not\_in\_span)",
         text=r"""Let $x=\langle\nabla F,R_1\rangle$, $y=\langle\nabla F,R_2\rangle$; the aggregate
first-order coefficient times $Z$ is
$\lambda(\alpha-q_0)r_0(1-r_0)\,x+(1-\lambda)(\beta-r_0)q_0(1-q_0)\,y$. If $0<\lambda<1$,
$0<q_0<1$, $0<r_0<1$, $\beta\neq r_0$ and this vanishes at two priors
$\alpha_1\neq\alpha_2$, then $x=y=0$. At $\lambda=1$ the $A$-marginal read-out
annihilates $R_1$ but not $R_2$, so an interior $\lambda$ is needed.""",
         note=r"""Across an open set of priors a statistic is second order for some interior mixture
iff its differential annihilates $\mathrm{span}\{R_1,R_2\}$, whatever that mixture is."""),
    dict(kind="theorem",
         source=r"PropORD.lean, lemmaORD\_gap",
         text=r"""For any $v$ and $\alpha,\beta\notin\{0,1\}$:
$\frac{d}{dc}\langle v,\PJ_{AB}-\PJ_{BA}\rangle\big|_{c=0}=\kappa\langle v,R_1\rangle-\kappa'\langle v,R_2\rangle$,
so the contrast between the two sequences is read from the same two directions as the
gap from $\PB$, with no reference to $\PB$ or to $\lambda$.""",
         note=r"""Combined with Proposition DIV, $\Pbar_\lambda-\PB=c\,M_\lambda+\bigO(c^2)$ for every
$\lambda$, which is the plane through $\PB$ the entry names."""),
]
