# Notes — Morgan, Orzen & Sefton (2012), pass 1

Run: `python3 verify_mos.py` — **6 checks, 0 failures**.

**Scope.** Our reading is arithmetically consistent. One survey figure is
correct but under-qualified, and one substantive asymmetry is omitted. Nothing
here reproves any result of the paper, and **none of the experimental
statistics were re-analysed**.

---

## The theory reads cleanly and is fully reproducible

MOS-1 derives both `x_n* = (n−1)P/n²` and `π_n* = w + P/n²` from the stated
payoff `π_i = w + x_i P/X − x_i` rather than taking them on trust, and MOS-2
gets `√(P/F)` from *"the largest integer such that `P/n*² > F`"*.

**MOS-3 is the strongest check here.** All twelve entries of their Table 1
investment column reproduce from `(n−1)P/n²`:

| | n=1 | n=2 | n=3 | n=4 | n=5 | n=6 |
|---|---|---|---|---|---|---|
| `P=50` | 0.00 | 12.50 | 11.11 | 9.38 | 8.00 | 6.94 |
| `P=200` | 0.00 | 50.00 | 44.44 | 37.50 | 32.00 | 27.78 |

matching the printed table throughout, and MOS-4 gives `⌊√(50/10)⌋ = 2`,
`⌊√(200/10)⌋ = 4` as printed. Recovering a published design table from the
model is much better evidence the section was read correctly than restating it.

---

## Imprecision: "observed was 2.5 and 3.7"

The survey reports these without qualification. They are the **second-half**
figures (rounds 26–50). The full picture from Table 5:

| treatment | rounds 1–25 | rounds 26–50 | prediction |
|---|---|---|---|
| small prize | 2.7 | **2.5** | 2 |
| large prize | 3.6 | **3.7** | 4 |

The numbers quoted are right; they are post-learning averages, and should be
labelled as such if cited.

---

## Omission: the two treatments deviate in OPPOSITE directions

This is the more substantive point, and the survey's phrasing hides it.

- Small prize: **2.5 against a prediction of 2** — *excess* entry, significant
  in both halves (p = 0.004, 0.012).
- Large prize: **3.7 against a prediction of 4** — a *shortfall*, differing from
  Nash only at the 6% and 12.5% levels.

MOS-5 records the sign difference. The paper makes a great deal of it, because
both deviations run *against* what within-contest behaviour predicts: with
negative returns to two-person competition one would expect *fewer* than two
entrants, and with under-investment one would expect *more* than four. Their
words: *"we observe precisely the opposite"* — in both cases.

Anyone citing this as "observed entry exceeds prediction" would be wrong for
half the experiment. If the survey's sentence is carried into `PROOFS.tex` or
the welfare section, it needs the directions attached.

---

## MOS-G confirmed verbatim

The risk-attitude rejection is exactly as described (§5.2.1):

> "in the small prize three-player contest subjects over-invest and earn less
> than the outside option, whereas subjects in the large prize three-player
> contest under-invest and earn more. A two-sided two-sample randomization test
> applied at the group level shows the differences in earnings to be
> significant at the 1% level."

The paper then applies the same controlled comparison to a "contest premium"
explanation, which also fails it. Worth knowing that two behavioural
explanations, not one, are ruled out this way.

---

## Third confirmation of the identity problem — and P5's place

This is the third independent source for the identity problem, after
Levin–Smith (fn. 2, mixed entry) and Fu–Lu (fn. 6, sequential entry). MOS use
private random delays (Proposition 1), so that arrival order breaks the tie.

**All three devices are properties of the protocol, not of the agents.** P5 is
the fourth and the only one where entrant identity is a property of the agents
themselves. MOS-6 makes the contrast arithmetic: with `N = 6` and `n* ∈ {2,4}`
there are `C(6,2) = 15` and `C(6,4) = 15` distinct equilibrium entrant sets,
against exactly **one** under P5 given the wealth profile.

---

## A point worth adding to the positioning

**The experiment deliberately switches off the variable our model is about.**
Table 1 holds `w = 100` for every subject by design, so there is no wealth
heterogeneity and hence no `κ(w)` channel. The survey notes the design fact
(MOS-D) but does not draw the conclusion: the one experimental paper in this
literature cannot speak to P5's mechanism, in either direction. That is worth a
sentence, because it forecloses "has this been tested?" cleanly rather than
leaving it open.

---

## Results

| ID | Result |
|---|---|
| MOS-1 | CONSISTENT. `x_n*` and `π_n*` both derived from the payoff function. |
| MOS-2 | CONSISTENT. The threshold solves to `√(P/F)`. |
| MOS-3 | CONSISTENT. All twelve Table 1 entries reproduce. |
| MOS-4 | CONSISTENT. `n* = 2` and `4` as printed. |
| MOS-5 | CONSISTENT, and it corrects an omission — the gaps are `+0.5` and `−0.3`. |
| MOS-6 | CONSISTENT. 15 equilibrium entrant sets per treatment; P5 has one. |

## Not attempted

- Proposition 1's full PBE argument.
- **Every regression, randomization test and p-value** — none re-run. All
  statistical claims here rest on the paper's own reporting.
- §2's survey of previous experiments; the investment-dynamics sections; §5.2.1
  beyond the three-player comparison.

Anything leaning on these is **[A]-grade** despite the [F] tag.
