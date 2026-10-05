"""Grounds for entry C.33 (5.4): when a decision flips (Table 1)."""
GROUNDS = {}
GROUNDS["C.33"] = [
    {
        "kind": "theorem",
        "source": r"Decision.lean, flips\_iff and not\_flips\_of\_pointing\_away",
        "text": r"""With $\mathrm{Flips}(u,c,\delta)$ the event that the action at $u+c\delta$ differs from the
action at $u$: it holds iff $u<0\le u+c\delta$ or $u+c\delta<0\le u$; and for $u\ge0$ with
$c\delta\ge0$ (or $u<0$ with $c\delta\le0$) it never holds, however large $|c\delta|$.""",
        "note": r"A decision flips exactly when the shift carries the surplus across zero; $|c\delta_\sigma|\ge|u|$ is not enough.",
    },
]
