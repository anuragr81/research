"""Grounds for entries C.25 (2.7) and C.26 (6.8) of notes/tools/corrections_entries.py:
the Section 2.2 sentence placing the results at full adoption, and the Section 6
repairs after the author's cuts of 7e8d546e, with the rewritten Hawthorne paragraph.
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

GROUNDS["C.25"] = [
    _pick("grounds_E_scope.py", "E.10", r"Anchoring.lean, dampedB\_at\_one, dampedB\_at\_zero",
          r"At $\omega=1$ the damped step is the Jeffrey step of Section 2.2, resetting the marginal to the delivered credence; at $\omega=0$ it leaves the belief unchanged, so the second cue is ignored."),
]

GROUNDS["C.26"] = [
    _pick("grounds_C_channels.py", "C.23", r"PropDIV.lean, jeffreyA\_prior\_mB0",
          r"Part 1: the first cue moves the belief about the other attribute by a multiple of $c$, the association in the evaluator's prior; the cut had left ``in the evaluator's prior'' without ``through the association''."),
    _pick("grounds_E_scope.py", "E.11", r"Anchoring.lean, dampedB\_at\_one, routeDamped",
          r"Part 7, ``unrelated traits lose their immunity'': under partial adoption the two sequences differ at $c=0$ by $(1-\omega)(\alpha-q_0)$, where Proposition IMM gives none under full adoption."),
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, ladder\_gap and",
          r"Part 7, ``the gap is then of order zero'': at $c=0$ sequence $AB$ ends at $q\otimes t$ while the benchmark is $q\otimes r$, so the score departs from the benchmark's at order zero whenever $t\neq r$."),
    _pick("grounds_C_intro.py", "C.1", "Decision.lean, volume_flipSet",
          r"Part 7, ``Theorem LOS, which needs a first-order score-gap'': the flip band has width $|c\delta|$, first order in $c$, which is what makes the loss second order; with a departure of order zero the band is of order zero."),
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, assoc\_routeDamped",
          r"Part 7, the ranking under partial adoption: the marginals differ at order zero (LAD(i)) and the believed association at first order, with coefficient $(1-\omega)H/Z$."),
    _pick("grounds_E_scope.py", "E.10", r"PropORD.lean, propORD\_Amarg",
          r"Part 7, the ranking under full adoption: the marginals differ at first order in $c$."),
    _pick("grounds_C_applied.py", "C.17", r"Ladder.lean, ladder\_assoc\_coeff\_at\_one",
          r"Part 7, the ranking under full adoption: the believed association's first-order coefficient vanishes at $\omega=1$, so it differs at second order, one order after the marginals as under partial adoption."),
]
