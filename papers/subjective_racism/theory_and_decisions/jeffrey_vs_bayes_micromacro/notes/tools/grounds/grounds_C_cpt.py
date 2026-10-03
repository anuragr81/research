"""Grounds for entry C.20 (3.6) of notes/tools/corrections_entries.py: the
Section 3 sentence setting cumulative prospect theory's rank dependence against
the paper's sequence dependence.

Tversky and Kahneman pages are journal pages of J. Risk Uncertainty 5 (1992),
read in full on 2026-10-03.  Every quotation was read on the page.
"""
import importlib.util
import os

_HERE = os.path.dirname(os.path.abspath(__file__))


def _pick(fname, entry, prefix, note):
    spec = importlib.util.spec_from_file_location(fname[:-3], os.path.join(_HERE, fname))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    hits = [it for it in mod.GROUNDS.get(entry, []) if it["source"].startswith(prefix)]
    assert len(hits) == 1, (fname, entry, prefix, len(hits))
    item = dict(hits[0])
    item["note"] = note
    return item


GROUNDS = {}

GROUNDS["C.20"] = [
    {
        "kind": "quote",
        "source": r"Tversky and Kahneman 1992, Section 1, p.~299",
        "text": r"""``Prospect theory distinguishes two phases in the choice process: framing and
valuation. \ldots The valuation process discussed in subsequent sections is applied to
framed prospects.''""",
        "note": r"The prospect is given to the valuation; the theory, read in full, contains no rule for revising its probabilities.",
    },
    {
        "kind": "quote",
        "source": r"Tversky and Kahneman 1992, Section 1.1, pp.~300--301",
        "text": r"""``To define the cumulative functional, we arrange the outcomes of each prospect in
increasing order.'' \ldots ``The decision weight $\pi_i^+$, associated with a positive outcome,
is the difference between the capacities of the events `the outcome is at least as good as
$x_i$' and `the outcome is strictly better than $x_i$.'\,''""",
        "note": r"The decision weight of an outcome depends on its rank among the outcomes of the prospect.",
    },
    {
        "kind": "theorem",
        "source": r"TverskyKahneman.lean, sum\_piPlus, piPlus\_id, rank\_dependence",
        "text": r"""With $\pi_i^+=w^+(p_i+\dots+p_n)-w^+(p_{i+1}+\dots+p_n)$: for $w^+(0)=0$, $w^+(1)=1$ and
probabilities summing to one, $\sum_i\pi_i^+=1$; for $w^+$ the identity, $\pi_i^+=p_i$; and with
$w^+(p)=p^2$ and three equiprobable gains the top gain is weighted $1/9$ and the middle one $1/3$.""",
        "note": r"Rank dependence, stated and checked: the same probability gets a different weight at a different rank.",
    },
    {
        "kind": "theorem",
        "source": r"PropIMM.lean, propIMM\_indep and propIMM\_no\_sequence\_effect",
        "text": r"""For $\alpha,1-\alpha,\beta,1-\beta\neq0$: at $c=0$, $\PJ_{AB}=\PJ_{BA}=\PB$, so the two
sequences give one belief.""",
        "note": r"``Not at all when the traits are believed unrelated.''",
    },
    _pick("grounds_E_scope.py", "E.10", r"PropORD.lean, propORD\_Amarg",
          r"``Belief depends on the sequence ... through the believed link'': at $c\neq0$ the two sequences' marginals differ at first order in $c$, with coefficients $\kappa'$ and $-\kappa$."),
]
