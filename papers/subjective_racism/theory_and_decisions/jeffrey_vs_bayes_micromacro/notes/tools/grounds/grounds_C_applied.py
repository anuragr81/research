"""Grounds for entries C.16 (5.3), C.18 (6.4) and C.17 (7.1) of
notes/tools/corrections_entries.py, added 2026-10-03 after the entries were
applied without them.

Items already verified under other entries are loaded from their files, not
retyped, so a quotation or a theorem statement has one source; each gets a
note saying what it establishes for the new entry.  New items are the
Ladder.lean statements (Proposition LAD and the factor-input route) and the
pooling computation.  Conventions as in grounds_E_scope.py: alpha = P(A=0),
beta = P(B=0), q0 and r0 the delivered credences on A=0 and B=0, omega the
adoption weight (delta in the code), Z = alpha beta (1-alpha)(1-beta), R1 and
R2 the route directions.
"""
import importlib.util
import os

_HERE = os.path.dirname(os.path.abspath(__file__))
_LOADED = {}


def _pick(fname, entry, prefix, note):
    """The one item of `entry` in grounds file `fname` whose source starts with `prefix`, with a new note."""
    if fname not in _LOADED:
        spec = importlib.util.spec_from_file_location(fname[:-3], os.path.join(_HERE, fname))
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        _LOADED[fname] = mod.GROUNDS
    hits = [it for it in _LOADED[fname].get(entry, []) if it["source"].startswith(prefix)]
    assert len(hits) == 1, (fname, entry, prefix, len(hits))
    item = dict(hits[0])
    item["note"] = note
    return item


GROUNDS = {}

# ---------------------------------------------------------------------------
# C.16 (5.3)  Section 5, the pooling sentence
# ---------------------------------------------------------------------------
GROUNDS["C.16"] = [
    {
        "kind": "theorem",
        "source": r"PropIMM.lean, propIMM\_indep (with PJab\_at\_zero)",
        "text": r"""For $\alpha\neq0$, $1-\alpha\neq0$, $\beta\neq0$, $1-\beta\neq0$: at $c=0$,
$\PJ_{AB}=\PJ_{BA}=\PB$, and $\PJ_{AB}=q\otimes r$.""",
        "note": r"``The two posteriors coincide at independence.''",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_pooling.py (72/72), symbolic in $\alpha,\beta,q_0,r_0,\lambda$",
        "text": r"""With $L=\lambda\PJ_{AB}+(1-\lambda)\PJ_{BA}$ and $G$ the cell-wise geometric pool
$(\PJ_{AB})^{\lambda}(\PJ_{BA})^{1-\lambda}$ renormalised: the $c^0$ and $c^1$ coefficients of
$L-G$ vanish in every cell identically; with $m=q\otimes r$ and $D=\kappa R_1-\kappa'R_2$ the
first-order sequence gap, $[c^2](L-G)_{ij}=\tfrac{\lambda(1-\lambda)}{2}\bigl[D_{ij}^2/m_{ij}-m_{ij}\chi^2\bigr]$,
$\chi^2=\kappa^2/(r_0r_1)+\kappa'^2/(q_0q_1)$, and the whole $c^2$ table vanishes on the cube
only on $\{q_0=\alpha,r_0=\beta\}\cup\{r_0=\beta=\tfrac12\}\cup\{q_0=\alpha=\tfrac12\}$;
$\assoc(L)-\assoc(G)$ has zero $c^0$ and $c^1$ coefficients and $c^2$ coefficient
$-\lambda(1-\lambda)\kappa\kappa'$.""",
        "note": r"``A geometric pool of them differs from the linear one only at second order'', and the association of the two pools agrees to first order, so no first-order result depends on pooling linearly.",
    },
]

# ---------------------------------------------------------------------------
# C.18 (6.4)  Section 6.2 compacted
# ---------------------------------------------------------------------------
GROUNDS["C.18"] = [
    _pick("grounds_E_scope.py", "E.10", r"Anchoring.lean, dampedB\_deviation",
          r"Part 1 and part 6: Proposition ADJ, the adoption weight read from three marginals of one reading group, $1-\omega=[P^{\omega}_{AB}(B{=}1)-r_1]/[P^{A}(B{=}1)-r_1]$; and the rubric sentence's $(1-\omega)[P^{A}(B{=}1)-r_1]$."),
    {
        "kind": "theorem",
        "source": r"Ladder.lean, ladder\_gap and ladder\_gap\_mA1",
        "text": r"""For $\alpha,1-\alpha,\beta,1-\beta\neq0$, with $P^{\omega}_{\sigma}(c)$ route $\sigma$ when the
second cue is adopted with weight $\omega$:
$P^{\omega}_{AB}(0)-P^{\omega}_{BA}(0)=(1-\omega)(\beta-r_0)\,R_1+(1-\omega)(q_0-\alpha)\,R_2$, and
the $A$-marginal gap at $c=0$ is $(1-\omega)(\alpha-q_0)$.""",
        "note": r"Part 1 and part 5: under partial adoption the marginals differ at order zero, along the route directions only (Proposition LAD).",
    },
    {
        "kind": "theorem",
        "source": r"Ladder.lean, assoc\_routeDamped, assoc\_routeDampedBA, ladder\_assoc\_coeff, ladder\_assoc\_coeff\_eq\_zero\_iff, ladder\_assoc\_coeff\_eq\_zero\_iff\_variance, Hcof\_slice",
        "text": r"""$\assoc(P^{\omega}_{AB})=c\,k_{AB}$ and $\assoc(P^{\omega}_{BA})=c\,k_{BA}$ exactly, and at
$c=0$, $k_{AB}-k_{BA}=(1-\omega)H/Z$ with
$H=q_0(1-q_0)\bigl[\beta(1-\beta)+\omega(\beta-r_0)^2\bigr]-r_0(1-r_0)\bigl[\alpha(1-\alpha)+\omega(\alpha-q_0)^2\bigr]$.
For $\alpha,1-\alpha,\beta,1-\beta\neq0$ it is zero exactly when $\omega=1$ or $H=0$,
equivalently when the two sequences' end beliefs at independence have equal products of
marginal variances, $q_0q_1t_0t_1=s_0s_1r_0r_1$. On the slice $q_0=\alpha$,
$H=\alpha(1-\alpha)(\beta-r_0)(t_1-r_0)$.""",
        "note": r"Part 5: the believed association differs at first order under partial adoption and at second order under full adoption.",
    },
    {
        "kind": "theorem",
        "source": r"Ladder.lean, ladder\_oddsShadow\_seqEffect",
        "text": r"""Under the nondegeneracy conditions $q_0(1-q_0)$, $r_0(1-r_0)$, $t_0t_1$, $s_0s_1\neq0$
($t_0=(1-\omega)\beta+\omega r_0$, $s_0=(1-\omega)\alpha+\omega q_0$): the sequence effect of
$\assoc/(m_{A0}m_{A1}m_{B0}m_{B1})$ is, for every $c$, $c$ times a bracket that vanishes at $c=0$,
for every $\omega$.""",
        "note": r"Part 5, the row ``statistics agreeing with the log odds ratio to first order'': second order at every weight.",
    },
    {
        "kind": "theorem",
        "source": r"Ladder.lean, jeffreyA\_eq\_rescale, jeffreyB\_eq\_rescale, oddsRatio\_rescale",
        "text": r"""Every Jeffrey step on $A$, whatever its target, multiplies the rows of the table by
constants, and every step on $B$ the columns; a rescaling with nonzero factors leaves the
odds ratio unchanged.""",
        "note": r"Part 5, the odds-ratio row: no sequence effect at any $c$ or weight, since a damped step is a Jeffrey step to the damped target.",
    },
    _pick("grounds_E_scope.py", "E.12", r"Wagner 2002, preprint p. 4",
          r"Part 2, the sufficiency direction: with each cue's Bayes factor the same in either position the two routes commute."),
    _pick("grounds_E_scope.py", "E.12", r"Wagner2002.lean, thm31",
          r"Part 2: Theorem 3.1's Lean form."),
    _pick("grounds_E_scope.py", "E.12", r"Wagner2002.lean, thm41",
          r"Part 2, ``commutation requires the same factor in either position'': Wagner's Theorem 4.1, the necessity direction. Raising the second cue's factor to the power $\omega$ changes it unless $\omega=1$ or the cue delivers the prior marginal (LadderFactorPow.lean, pow\_factor\_ratio\_iff), so the two sequences then disagree."),
    _pick("grounds_E_scope.py", "E.12", r"Wagner2002.lean, grid\_43\_44",
          r"Part 2: Theorem 4.1's conditions hold for the paper's prior whenever all four cells are positive."),
    {
        "kind": "theorem",
        "source": r"Ladder.lean, rescale\_rescale, factor\_mA1\_gap\_iff, assoc\_factorRoute, oddsRatio\_factorRoute",
        "text": r"""Factor updates on distinct attributes applied in full commute. With the second factor
replaced by $(a_0',a_1')$ and $\alpha,1-\alpha\neq0$, the two sequences' $A$-marginals at $c=0$
agree iff $a_1a_0'=a_0a_1'$. A factor route's association is
$c\,(a_0a_1b_0b_1)/S^2$ exactly, $S$ the normalising sum, and with nonzero factors and
normaliser its odds ratio is the prior's.""",
        "note": r"Part 2: under the Bayes-factor reading with the second factor weighted, the marginals differ at order zero unless the weighted factor keeps the full factor's ratio, the association is zero at $c=0$ and moves at first order, and the odds ratio is unchanged.",
    },
    {
        "kind": "computation",
        "source": r"sympy/verify\_ladder.py, section (F) (99/99)",
        "text": r"""Both factors in full: one table for every $c$, the benchmark $\PB$. With the second
factor weighted, $a'$: the association is $cK/S^2$ exactly, zero at $c=0$; the $A$-marginal gap
at $c=0$ is $\alpha(1-\alpha)(a_1a_0'-a_0a_1')/(\alpha a_0'+(1-\alpha)a_1')$, and at $a'=a^{\omega}$ it is
zero exactly when $\omega=1$ or $q_0=\alpha$; the difference of the two routes' first-order
association factors is $a_0'a_1'b_0'b_1'(M_a^2-M_b^2)/(S_{a'}^2S_{b'}^2)$, zero exactly where
$M_a=M_b$, which holds on all of $\omega=1$, reduces at $\omega=0$ to $H_0=0$, and for each
$\omega<1$ is a proper analytic hypersurface; $\assoc/(m_{A0}m_{A1}m_{B0}m_{B1})$ has first-order
factor $1/Z$ on both routes; the odds ratio equals the prior's for every $c$.""",
        "note": r"Part 2: the orders the amended Wagner sentence states, for the instantiation $a'=a^{\omega}$.",
    },
    {
        "kind": "theorem",
        "source": r"LadderFactorPow.lean, pow\_factor\_ratio\_iff and factor\_pow\_gap\_iff",
        "text": r"""For $a_0,a_1>0$: $a_1a_0^{\omega}=a_0a_1^{\omega}$ iff $\omega=1$ or $a_0=a_1$. For
$0<\alpha<1$, $0<q_0<1$, $a_0=q_0/\alpha$, $a_1=(1-q_0)/(1-\alpha)$: the two sequences'
$A$-marginals at $c=0$ agree iff $\omega=1$ or $q_0=\alpha$.""",
        "note": r"Part 2: under the Bayes-factor reading with the second factor weighted as $a^{\omega}$, the marginals differ at order zero except at full adoption or when the cue delivers the prior marginal.",
    },
    _pick("grounds_E_scope.py", "E.12", r"Aggregate.lean, PJab",
          r"Part 6: under full adoption the sequence effect sits in the belief about the attribute read first ($cK$ from its delivered credence) while the attribute read last is pinned to its score, and it vanishes at independence (Proposition DRF)."),
    _pick("grounds_E_scope.py", "E.12", r"Anchoring.lean, orderEffect",
          r"Part 6: under partial adoption the two sequences differ even when the attributes are believed unrelated."),
]

# ---------------------------------------------------------------------------
# C.17 (7.1)  Concluding remarks, closing paragraph on the debate of Section 3
# ---------------------------------------------------------------------------
GROUNDS["C.17"] = [
    _pick("grounds_E_literature.py", "E.5", r"Doring 1999, S379 (Introduction)",
          r"Doring's objection is normative, so ``the normative question is left where Doring leaves it'' is accurate."),
    _pick("grounds_E_literature.py", "E.5", r"Hawthorne 2004, pp. 115-116",
          r"Hawthorne declines the psychological question and turns to the normative one; the paragraph leaves both where he leaves them."),
    _pick("grounds_E_scope.py", "E.10", r"PropORD.lean, propORD\_Amarg",
          r"``In the levels'': under full adoption the two sequences' marginals differ at first order (Proposition ORD)."),
    {
        "kind": "theorem",
        "source": r"Ladder.lean, ladder\_assoc\_coeff\_at\_one; PropDEC.lean, oddsRatio\_gap\_eq\_zero, oddsRatio\_PJba\_eq\_prior",
        "text": r"""At $\omega=1$ the two routes' first-order association factors agree,
$k_{AB}-k_{BA}=0$ at $c=0$; and $\mathrm{OR}(\PJ_{AB})=\mathrm{OR}(\PB)=\mathrm{OR}(\PJ_{BA})$
under the nondegeneracy conditions of Proposition DEC.""",
        "note": r"``Rather than in the believed link between the traits'': under full adoption the believed association differs between the sequences only at second order and the odds ratio not at all.",
    },
    _pick("grounds_C_intro.py", "C.1", r"Decision.lean, volume_flipSet",
          r"``In the number of decisions changed rather than in their cost'': the share of changed decisions is first order, the surplus-weighted loss second order (Proposition SHR, Theorem LOS)."),
    {
        "kind": "theorem",
        "source": r"Ladder.lean, ladder\_assoc\_coeff and ladder\_assoc\_coeff\_eq\_zero\_iff; sympy/verify\_interior\_omega.py, row 3 (39/39)",
        "text": r"""At $c=0$, $k_{AB}-k_{BA}=(1-\omega)H/Z$, zero exactly when $\omega=1$ or $H=0$. $H$ is
irreducible and affine in $\omega$, so for each prior and cue at most one weight
$\omega=-H_0/H_1$ makes it vanish unless $H_0=H_1=0$ (at the generic prior, $\omega=31/4483$).""",
        "note": r"``Under partial adoption the believed link moves at first order as well'' (Proposition LAD).",
    },
]
