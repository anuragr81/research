"""Grounds for entry C.31 (6.11): both channels inside the evaluator; the rule in Proposition LAD."""
import importlib.util, os
_HERE = os.path.dirname(os.path.abspath(__file__))
def _pick(fname, entry, prefix, note):
    spec = importlib.util.spec_from_file_location(fname[:-3], os.path.join(_HERE, fname))
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    hits = [it for it in mod.GROUNDS.get(entry, []) if it["source"].startswith(prefix)]
    assert len(hits) == 1, (fname, entry, prefix, len(hits))
    item = dict(hits[0]); item["note"] = note; return item
GROUNDS = {}
GROUNDS["C.31"] = [
    _pick("grounds_C_belief.py", "C.28", r"Anchoring.lean, dampedTarget and dampedB",
          r"The rule the proposition is about, a Jeffrey step on the second cue's partition to $(1-\omega)m+\omega x$."),
]
