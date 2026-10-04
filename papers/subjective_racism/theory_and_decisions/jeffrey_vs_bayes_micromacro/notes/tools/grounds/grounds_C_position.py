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
          r"``The rule adopts the first cue in full and gives the second the weight $\omega$'': their end-of-sequence form with the first item as anchor (Eq. 8), applied with one $\omega$ whichever cue is second."),
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, twoSided\_orderEffect and twoSided\_recency",
        "text": r"""With the same weight $w$ on both cues, step-by-step from a prior $S_0$ in estimation
mode ($R=S_{k-1}$): $S(a\text{ then }b)-S(b\text{ then }a)=w^{2}(s_b-s_a)$, positive for every
$w>0$ when $s_a<s_b$, so the item read last weighs more.""",
        "note": r"``In the belief-adjustment model, where both cues bear on one judgment, equal weights on the two cues make the cue read last weigh more'': their step-by-step form from a prior, each step a fraction $w$ of the way; Appendix B proves it (Eq. B.5) for weights that stay with each item wherever it stands.",
    },
    {
        "kind": "theorem",
        "source": r"HogarthEinhorn.lean, contrastWeight (Eqs. 6a/6b, T-p.10) and contrastWeight\_mem\_Icc",
        "text": r"""$w_k=\alpha S_{k-1}$ for evidence at or below the reference point $R$ and
$w_k=\beta(1-S_{k-1})$ above it, with $0\le\alpha,\beta\le1$; for $0\le S_{k-1}\le1$ these weights
lie in $[0,1]$, as their model requires (T-p.6).""",
        "note": r"``A cue delivering $r_1<m$ gets a weight proportional to $m$ and one delivering $r_1>m$ a weight proportional to $1-m$'': their $\alpha S_{k-1}$ and $\beta(1-S_{k-1})$ with the current impression $S_{k-1}$ read as the marginal $m$ the cue meets and the evidence $s(x_k)$ as the delivered credence $r_1$; in estimation mode their reference point is $R=S_{k-1}$.",
    },
    _pick("grounds_C_applied.py", "C.18", r"Ladder.lean, jeffreyA\_eq\_rescale",
          r"``Each still answers a cue by a Jeffrey step on its own partition, only to a different target, so it rescales rows or columns of the belief, and the odds ratio is the same in both sequences under every variant'': a step on one attribute rescales rows or columns whatever its target."),
]
