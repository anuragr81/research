# Notes — Hopkins & Kornienko, pass 1

Run: `python3 verify_hk.py` — **7 checks, 0 failures**, including a Lean
compile and axiom audit.

**Scope.** These checks establish that our reading of the sources is
arithmetically consistent, and they machine-check the dispersive-order bridge
that `LITERATURE.tex` asked to have checked. They do not reprove any result of
the three papers; the separating-equilibrium and welfare arguments are far out
of reach.

## Source confirmation

Read directly from the PDFs, not from the survey.

**Definition 1** (HK2010, p.128, quoting Shaked & Shanthikumar 2007, p.148):

> A variable with distribution `F` is said to be smaller in the dispersive
> order (or less dispersed) than a variable with a distribution `G` (denoted
> as `F <d G`) whenever `G^{-1}(r) - F^{-1}(r)` is (weakly) increasing for
> `r ∈ (0,1)`.

followed by eq. (18): `G >d F if and only if f(F^{-1}(r)) >= g(G^{-1}(r))`,
glossed "for a fixed rank, the more dispersed distribution is less dense than
the less dispersed one." `LITERATURE.tex` reproduces both correctly.

**Proposition 4** (HK2010): conclusion confirmed verbatim — "performance is
higher ex post for the bottom and middle: `x_p(r) > x_a(r)` on `[0, r̂]`, where
`r̂` is the only point of crossing of `Z_a(r)` and `Z_p(r)`. Second, utility
rises at the bottom, `U_p(0) > U_a(0)`, but utility is lower ex post for the
middle and top." The survey's careful two cautions — that this is an interval
result, not a uniform-sign aggregate claim, and that their crossing is in the
endowment functions with a sign change in *utility* rather than in the
direction of the expenditure response — are both faithful to the source.

**SOSD remark** confirmed at p.130: "there is no clear relation between the
dispersive order and second order stochastic dominance."

## Results

| ID | Result |
|---|---|
| HK-1 | CONSISTENT. `S(r) = H^{-1}(r) ⟹ S'(r) = 1/h(S(r))` exactly, on three families. This identity is what makes Definition 1 and eq. (18) equivalent. |
| HK-2 | CONSISTENT, and exact rather than coincidental: `d/dr[G^{-1}-F^{-1}]` equals `1/g(G^{-1}) - 1/f(F^{-1})` identically on all four pairs, so the two formulations are the same condition. |
| HK-3 | CONSISTENT. A pure translation has displacement slope exactly `0`, so shifts are dispersively comparable in both directions — the order is location-free. |
| HK-4 | CONSISTENT. A shift by `c` leaves dispersion equal while moving the mean by `c`, witnessing that the two orders cut differently. |
| HK-5 | CONSISTENT. Dispersive ordering is *decided* (min of the slope over `(0,1)`), not assumed; 3 ordered pairs all satisfy `Var F <= Var G`, and an unordered control is correctly rejected. |
| HK-F | **MACHINE-CHECKED.** See below. |

## HK-F — the bridge, now formal

`Dispersive.lean` compiles under Lean 4 (core only, no Mathlib), 7 theorems,
0 `sorry`, all 7 axiom-audited, dependencies limited to `propext` and
`Quot.sound` via `omega` on `Int`.

Transporting Definition 1 through `T = G^{-1} ∘ F` gives
`Dispersive T := ∀ a b, a ≤ b → T a - a ≤ T b - b`. Then:

- **HK-L1** `dispersive_imp_monotone` — the dispersive order *already forces*
  `T` to be monotone. This is more than the survey claimed: P9-gen carries
  `MonotoneT` as a separate hypothesis, and under the dispersive order it is
  not an extra assumption but a consequence.
- **HK-L2** `dispersive_crossing_imp_pivot` — dispersive plus a zero of the
  displacement gives `PivotSpread` about that zero, in exactly the shape
  `EntryContest.lean` defines it.
- **HK-L3/L3'** `dispersive_no_return`, `dispersive_no_return_below` — the
  displacement cannot cross back, which is the content of "if it changes sign
  it does so once".
- **HK-L5** `dispersive_crossing_gives_P9gen_hypotheses` — both hypotheses
  P9-gen needs, delivered together.

**So the survey's claim is confirmed and can be strengthened.** The pivot-spread
class is not ad hoc: under the dispersive order plus a crossing, `MonotoneT`
comes free rather than being assumed alongside.

## HK-L4 — the crossing condition is load-bearing

`shiftUp_not_pivotSpread` proves that `T w = w + 1` is dispersive and monotone
yet is a pivot-spread about **no** point at all. So the dispersive order alone
does *not* deliver the P9-gen hypothesis, and `LITERATURE.tex`'s qualifier
"if in addition `T(w) - w` changes sign" is necessary, not decorative. Anyone
tempted to state the bridge without the crossing condition should read this
theorem first.

## A vacuity bug found and fixed during this pass

HK-5 initially used `sp.ask(sp.Q.nonnegative(...))` to decide dispersive
ordering. SymPy returned `None` on every pair, and `(not None) or ...`
evaluates to `True`, so **the check passed unconditionally** — it would have
passed had the variance implication been false. It also silently tested three
pairs that are not dispersively ordered at all (`U[0,1]` vs `z^2` has slope
`1/(2√r) - 1`, negative above `r = 1/4`).

Rewritten to decide ordering by `sp.minimum` of the slope over `(0,1)`, with
an explicit count of how many pairs were actually ordered and an unordered
control that must be rejected. Recorded here as a live instance of
`lit/README.md` rule 3, and of the same failure mode as the P7 end-to-end
episode in `HANDOVER_20260901.md`.

## Discrepancy found outside this paper

The axiom-audit `sed` in the main `verify.sh` used `[a-zA-Z_]*`, which
truncates any theorem name containing a digit
(`dispersive_crossing_gives_P9gen_hypotheses` → `..._gives_P`). No current
`EntryContest.lean` name contains a digit, so nothing was mis-audited, and the
`NAUD -ne NTHM` guard would have failed the run loudly rather than silently.
Regex widened to `[A-Za-z0-9_']*` in both places.

## Not attempted

- HK2004 Proposition 4's **two-threshold ULR result**. This is the source of
  the open question in `LITERATURE.tex` about whether a two-pivot
  generalisation of P9-gen exists. A two-pivot Lean development is the
  natural next artifact and is not started.
- HK2009 Propositions 4 and 5 (welfare comparisons under rank indexing).
- HK-A, HK-B, HK-C, HK-J: structural/textual readings, not arithmetic.
