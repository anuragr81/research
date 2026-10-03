"""Grounds for entry C.24 (6.7) of notes/tools/corrections_entries.py: Proposition
ADJ dropped from Section 6, its first identity kept as a sentence, the
proposition kept in PAPER_B_ADDENDUM.tex.
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

GROUNDS["C.24"] = [
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedB\_mB1 and dampedB\_deviation (dampedTarget $=(1-\delta)\,Q(B{=}1)+\delta r_1$)",
        "text": r"""For $Q(B{=}1)\neq0$: the damped step attains its target,
$P(B{=}1)=(1-\delta)\,Q(B{=}1)+\delta r_1$, so that
$P(B{=}1)-r_1=(1-\delta)\,[Q(B{=}1)-r_1]$, an identity in $Q$, $r_0$ and $\delta$.""",
        "note": r"Part 1: with $Q=P^{A}$, the rating of the attribute read second is $(1-\omega)P^{A}(B{=}1)+\omega r_1$ for every $c$; it equals $r_1$ exactly when $\omega=1$ or $P^{A}(B{=}1)=r_1$, and otherwise $\omega$ solves it. Part 4's distance from the score is the second form.",
    },
    _pick("grounds_E_scope.py", "E.10", r"sympy/check\_zero\_slope\_identification.py, case 7",
          r"Part 1, the worked example kept from the old text: $(.56-.43)/(.56-.30)=\tfrac12$."),
    _pick("grounds_E_scope.py", "E.12", r"Anchoring.lean, orderEffect\_damped\_at\_indep",
          r"Part 4: under partial adoption the two sequences differ at $c=0$, which Proposition LAD(i) states; the reference moves there from ADJ."),
]
