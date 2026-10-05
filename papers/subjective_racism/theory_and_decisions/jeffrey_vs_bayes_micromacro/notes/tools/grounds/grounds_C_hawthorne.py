"""Grounds for entry C.29 (6.10): the corrected Hawthorne paragraph."""
import importlib.util, os
_HERE = os.path.dirname(os.path.abspath(__file__))
def _pick(fname, entry, prefix, note):
    spec = importlib.util.spec_from_file_location(fname[:-3], os.path.join(_HERE, fname))
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    hits = [it for it in mod.GROUNDS.get(entry, []) if it["source"].startswith(prefix)]
    assert len(hits) == 1, (fname, entry, prefix, len(hits))
    item = dict(hits[0]); item["note"] = note; return item
GROUNDS = {}
GROUNDS["C.29"] = [
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, ladder\_gap and",
          r"At $c=0$ sequence $AB$ ends at $q\otimes t$ with $t-r=(1-\omega)(\beta-r_0)(1,-1)$, and the benchmark is $q\otimes r$ (PropIMM.lean, PB\_at\_zero), so the score departs from the benchmark's by $(1-\omega)(\beta-r_0)\sum_i q_i(v_{i0}-v_{i1})$, proportional to $1-\omega$."),
    _pick("grounds_C_intro.py", "C.1", "Decision.lean, volume_flipSet",
          r"For any score departure $x$, not only $x=c\delta$: the band of changed decisions has width $|x|$, so the share does not vanish with $c$ when $x$ does not; and the surplus lost over the band is at most $|x|$ times the band's mass (lintegral\_stake\_le, any population measure)."),
]
