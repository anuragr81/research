"""Grounds for entry C.32 (6.12): the logic fixes in the author's rewrite of Section 6."""
import importlib.util, os
_HERE = os.path.dirname(os.path.abspath(__file__))
def _pick(fname, entry, prefix, note):
    spec = importlib.util.spec_from_file_location(fname[:-3], os.path.join(_HERE, fname))
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    hits = [it for it in mod.GROUNDS.get(entry, []) if it["source"].startswith(prefix)]
    assert len(hits) == 1, (fname, entry, prefix, len(hits))
    item = dict(hits[0]); item["note"] = note; return item
GROUNDS = {}
GROUNDS["C.32"] = [
    _pick("grounds_E_scope.py", "E.12", r"Anchoring.lean, orderEffect\_damped\_at\_indep",
          r"Part 1, ``present only if Hawthorne's objection to full adoption is right'': the position channel needs $\omega<1$; at $\omega=1$ the sequences agree at $c=0$."),
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, ladder\_gap and",
          r"Part 2, ``it follows from the above proposition'': LAD(i) gives the beliefs at $c=0$, from which the score departure follows."),
    _pick("grounds_C_intro.py", "C.1", "Decision.lean, volume_flipSet",
          r"Part 2: the decision consequences follow from the score departure through the band bounds, for any departure."),
]
