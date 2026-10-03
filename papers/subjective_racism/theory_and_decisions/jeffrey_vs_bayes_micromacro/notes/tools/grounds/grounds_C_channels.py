"""Grounds for entry C.23 (3.7) of notes/tools/corrections_entries.py: the two
channels named by what each needs (Section 3, two sentences; Section 6, two
sentences).  Every item is reused from an earlier entry's grounds, except the
closed form of the first cue's effect on the other marginal.
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

GROUNDS["C.23"] = [
    _pick("grounds_C_applied.py", "C.16", r"PropIMM.lean, propIMM\_indep",
          r"Part 2, ``neither the evidence \ldots alone'': credences adopted in full give one belief from the two sequences when the evaluator believes the traits unrelated."),
    _pick("grounds_E_scope.py", "E.12", r"Wagner 2002, preprint p. 4",
          r"Part 2, ``nor the evaluator alone'': with each cue's Bayes factor the same in either position the two sequences end at one belief, whatever association the evaluator's prior holds."),
    _pick("grounds_E_scope.py", "E.12", r"Wagner2002.lean, thm31",
          r"Part 2, the same statement machine-checked."),
    {
        "kind": "theorem",
        "source": r"PropDIV.lean, jeffreyA\_prior\_mB0",
        "text": r"""For $\alpha\neq0$, $1-\alpha\neq0$: after the Jeffrey step on $A$ to $(q_0,q_1)$,
$Q(B{=}0)=[\alpha\beta(1-\alpha)+c\,(q_0-\alpha)]/[\alpha(1-\alpha)]$, that is
$\beta+c\,(q_0-\alpha)/(\alpha(1-\alpha))$.""",
        "note": r"Part 3: the cue on one attribute moves the belief about the other by a multiple of $c$, the covariance of the evaluator's prior, and not at all when $c=0$ or when the cue delivers the prior marginal.",
    },
    _pick("grounds_E_scope.py", "E.10", r"PropORD.lean, propORD\_Amarg",
          r"Parts 1 and 2, ``derived'': with each cue adopted in full in either position, the $A$-marginals of the two sequences differ by $\kappa'c+\bigO(c^{2})$, and $\kappa'=-q_0(1-q_0)(r_0-\beta)/Z$ is zero only when $q_0\in\{0,1\}$ or the letter delivers the prior marginal, $r_0=\beta$."),
    _pick("grounds_E_scope.py", "E.12", r"Anchoring.lean, orderEffect\_damped\_at\_indep",
          r"Part 4: a weight $\omega<1$ on the cue read second gives a sequence effect $(1-\omega)(\alpha-q_0)$ at $c=0$, so the position channel is present whether or not the attributes are believed related."),
]
