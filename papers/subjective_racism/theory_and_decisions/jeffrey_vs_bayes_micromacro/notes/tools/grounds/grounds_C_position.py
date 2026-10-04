"""Grounds for the 2026-10-04 amendment of entry C.4 (1.3, the introduction's channel
sentences) and for entry C.27 (6.9, the varieties of the position channel in Section 6).
Items for C.4 are appended to those in grounds_C_intro.py (load_grounds merges by key).

Hogarth and Einhorn pages are the transcription's (T-p.N); see
literature/hogarth_einhorn1992/README.md for the caveat on that source.
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

GROUNDS["C.4"] = [
    _pick("grounds_C_applied.py", "C.16", r"PropIMM.lean, propIMM\_indep",
          r"Amendment, ``the channel exists only when $c\neq0$'': at $c=0$ the two sequences give one belief under full adoption."),
    _pick("grounds_E_scope.py", "E.10", r"Hogarth and Einhorn 1992, T-p.7 (transcription), Eqs. 3-4",
          r"Amendment, ``a general form, a weight on each cue'': the averaging form gives each item its own weight $w_k$."),
    _pick("grounds_E_scope.py", "E.10", r"Anchoring.lean, dampedB\_at\_one, dampedB\_at\_zero",
          r"Amendment, ``when each impression is adopted in full, $\omega=1$ \ldots\ the position channel is absent'': at $\omega=1$ the damped step is the Jeffrey step, so no cue is discounted."),
]

GROUNDS["C.27"] = [
    _pick("grounds_E_scope.py", "E.10", r"Hogarth and Einhorn 1992, T-p.12 (transcription), Eq. 8",
          r"``The rule discounts the later cue alone, and by a constant'': their end-of-sequence form with the first item as anchor (Eq. 8)."),
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, twoSided\_orderEffect and twoSided\_recency",
        "text": r"""With the same weight $w$ on both cues, step-by-step from a prior $S_0$ in estimation
mode ($R=S_{k-1}$): $S(a\text{ then }b)-S(b\text{ then }a)=w^{2}(s_b-s_a)$, positive for every
$w>0$ when $s_a<s_b$, so the item read last weighs more.""",
        "note": r"``Each cue may be discounted, from a prior rather than from the first cue, and with the same weight on both cues the belief-adjustment model then predicts that the cue read last weighs more'', in their single-score model; their Appendix B proves it (Eq. B.5) for weights that stay with each item wherever it stands.",
    },
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, contrastWeight (Eqs. 6a/6b, T-p.10) and contrastWeight\_mem\_Icc",
        "text": r"""$w_k=\alpha S_{k-1}$ for evidence at or below the reference point $R$ and
$w_k=\beta(1-S_{k-1})$ above it, with $0\le\alpha,\beta\le1$; for $0\le S_{k-1}\le1$ these weights
lie in $[0,1]$, as their model requires (T-p.6).""",
        "note": r"``The weight may depend on what the cue says rather than on its position alone, as under their contrast assumption, which gives evidence above the current impression one weight and evidence below it another''; in estimation mode the reference point is the current impression, $R=S_{k-1}$.",
    },
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, jeffreyA\_eq\_rescale",
          r"``Each step remains a Jeffrey step on one attribute's partition and so rescales rows or columns of the belief, so the odds ratio is the same in both sequences under every variety'': a step on one attribute rescales rows or columns whatever its target."),
]
