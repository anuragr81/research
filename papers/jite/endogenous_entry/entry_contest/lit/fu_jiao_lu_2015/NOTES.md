# Notes — Fu, Jiao & Lu (2015), pass 1

Run: `python3 verify_fjl.py` — **6 checks, 0 failures**, including a Lean
compile and axiom audit.

**Scope.** Our reading of the source is arithmetically consistent, and the
"same accounting logic as P7" claim is now machine-checked rather than
asserted. Nothing here reproves any FJL result.

## Source confirmation

Equation (2), p.397, read directly from the PDF:

> the rent to be dissipated in the contest cannot exceed `[1 − (1−q)^M]V`.
> Hence, bidders receive an expected overall rent less than `[1 − (1−q)^M]V`,
> while they incur, on average, entry cost `MqΔ`. The following fundamental
> equality must hold in a symmetric equilibrium, as the expected total payoff
> of players must be positive:
>
> `[1 − (1−q)^M]V ≥ Mq(Δ + E(x^α))`   (2)

`LITERATURE.tex` reproduces this correctly. The shortlist cutoff quoted from
Lemma 6 / Theorem 9, `M̄ = min{N : V/N < αΔ/(α−1)}`, is confirmed at p.412.

## The attribution question, answered precisely

`LITERATURE.tex` asserts eq. (2) "implies that the expected number of entrants
`Mq` cannot exceed `V/Δ`". **Fu, Jiao and Lu never display that inequality.**
What they derive from (2) is a chain about *bids*: (3), then (4) by convexity,
then (5), then the bid bound `x̄_T(q)` at (6), then Theorem 2's
single-peakedness.

That could have made the survey's verdict on P7 too harsh — attributing to FJL
a count bound they never state. It does not, and the reason is **Definition 2**
(p.398), which defines

> `q̄ = argmax { q ∈ [0,1] : (1−(1−q)^M)V ≥ MqΔ }`

Every feasible `q` in that set satisfies `MqΔ ≤ V`, because the bracket is at
most 1. So the count bound **is** in the paper — as a feasibility cutoff on the
entry probability rather than as a displayed bound on `Mq`. The survey's
inference is faithful; it is a restatement of Definition 2, not an addition.

**Consequence for the write-up.** The survey's instruction stands, but the
citation should point at Definition 2 alongside eq. (2), not at eq. (2) alone.
Citing (2) by itself invites a referee to check the neighbouring derivation,
find only the bid chain, and conclude the attribution is loose.

## Results

| ID | Result |
|---|---|
| FJL-1 | CONSISTENT. `1−(1−q)^M` has min 0 and max 1 on `[0,1]` for `M ∈ {1,2,3,5,8}`, so `[1−(1−q)^M]V ≤ V`. |
| FJL-2 | CONSISTENT. `Mq(Δ+E) − MqΔ = Mq·E ≥ 0` exactly, so dropping the effort term is the only step and it is signed. |
| FJL-3 | CONSISTENT and non-vacuous. On a grid at `V=10, Δ=1`: 35 feasible points, all satisfying the bound, and **5 points excluded** by the constraint — so the feasibility set is not everything. |
| FJL-4 | CONSISTENT. `d/dN(V/N) = −V/N²< 0` so the min is well defined; `αΔ/(α−1) − Δ = Δ/(α−1) > 0`; the threshold tends to `Δ` as `α → ∞`. |
| FJL-D | **MACHINE-CHECKED.** See below. |

## "The same accounting logic", made formal

`Accounting.lean` compiles under Lean 4 core, 4 theorems, 0 `sorry`, 4/4
axiom-audited.

- **FJL-L1** `accounting_bound` — the shared lemma. Count times unit cost,
  plus a non-negative residual, covered by dissipated rent that is capped by
  the budget, gives `count × unitCost ≤ budget`. This *is* the accounting
  logic; everything else in either paper is interpretation of its three
  hypotheses.
- **FJL-L2** `fjl_expected_count_bound` — eq. (2) as an instance, residual =
  `E(x^α)`.
- **FJL-L3** `p7_entry_count_bound` — P7's cap as an instance, residual = `0`
  (the entry_contest model has no bidding stage, so there is no effort cost
  to carry). **The residual being zero is the formal statement of what P7
  drops.**
- **FJL-L4** `p7_count_is_integral` — what P7 has that the shared lemma does
  not: its count is `m+1`, a natural number from a pure-strategy profile, so
  the cap yields a genuine finite index bound. FJL's `Mq` is an expected count
  and need not be an integer. This is the formal residue of the survey's
  "deterministic ... version of a known one".

So the survey is right that the inequality is not new, and the Lean file now
says exactly *how much* is shared and exactly what is left: integrality of the
count, utility units, and no designer.

## Non-vacuity controls

- FJL-3's grid excludes 5 of 40 points; a check where the constraint bound
  nothing would have been empty.
- FJL-2's slack is verified to be exactly `Mq·E`, not merely non-negative.
- Lean audit covers 4 of 4 theorems; `p7_count_is_integral` requires
  `0 < kappa` and fails without it.

## Not attempted

- Theorem 1 (existence via Dasgupta–Maskin), Theorem 2's single-peakedness,
  Theorem 7, Theorem 9, Theorem 11. All are genuine equilibrium/design
  results well outside SymPy and Mathlib-free Lean.
- FJL-G's claim that Theorem 11 is "the design-side cousin of P8" is a
  structural reading and is unchecked.
