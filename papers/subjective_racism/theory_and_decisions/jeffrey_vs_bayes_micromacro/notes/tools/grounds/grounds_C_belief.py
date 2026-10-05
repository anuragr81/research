"""Grounds for entry C.28 (2.8) of notes/tools/corrections_entries.py: the Setup 2.1
sentence saying the adoption weight is not part of the belief."""

GROUNDS = {}

GROUNDS["C.28"] = [
    {
        "kind": "theorem",
        "source": r"Anchoring.lean, dampedTarget and dampedB",
        "text": r"""$\mathrm{dampedTarget}(Q,r_0,\delta)=(1-\delta)\,Q(B{=}1)+\delta(1-r_0)$, and the damped step
is the Jeffrey step on $B$ to that target. The belief $Q$, the delivered credence $r_0$ and the
weight $\delta$ are separate arguments of the step.""",
        "note": r"``The weight is not part of what the evaluator believes \ldots\ but describes how the evaluator revises $P$ on a cue'': in the model the weight is an argument of the revision, not a coordinate of the belief.",
    },
    {
        "kind": "quote",
        "source": r"Wagner 2002, note 9 (preprint p.~13)",
        "text": r"""``\emph{considered} experience (in light of ambient memory and prior probabilistic
commitment)''""",
        "note": r"``What the evaluator believes about a document \ldots\ enters instead the credence the document delivers'': on this reading the delivered credence is the evaluator's considered response to the document, so a judgment of its reliability is already in it. Read on the page for literature/wagner2002 (README, ``Note 9'').",
    },
]
