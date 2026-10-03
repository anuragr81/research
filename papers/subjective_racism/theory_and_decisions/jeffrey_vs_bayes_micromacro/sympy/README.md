# SymPy verification of PAPER_B_MANUSCRIPT.tex

Machine verification, by exact symbolic computation, of every proposition,
lemma and theorem in *"Order-Sensitivity of Belief and Decision Statistics under
Jeffrey Conditioning"* — including every displayed closed form in the deferred
proofs of Appendix A.

Requires Python 3 with SymPy (tested on SymPy 1.14, Python 3.13).

```
python3 run_all.py          # everything, with a summary
python3 run_all.py C3       # just the scripts matching "C3"
python3 verify_DIV.py   # or run one directly
```

## What is checked where

| script | result | what it establishes |
|---|---|---|
| `verify_IMM.py` | Prop. IMM | one cue: Jeffrey = matched Bayes *identically*; two cues at `c=0`: both routes and the benchmark equal `q⊗r` |
| `verify_DIV.py` | Prop. DIV, App. A.1 | the leading gap `cκR₁`, the mirror `cκ'R₂`, the sequence effect, and every explicit entry of `D` and `Δ_seq` printed in the appendix, including the symmetry-locus formula |
| `verify_ASC.py` | Lemma ASC | `∇assoc` at `q⊗r`, the two vanishing inner products, and that the association gap is exactly `Θ(c²)` |
| `verify_SEP.py` | Lemma SEP, App. A.2 | separability of attribute-local composites at `N = 2…6`, every interaction contrast, the converse (additivity), and a scope check that a joint-event cue breaks it |
| `verify_SCR.py` | Lemma SCR | the first-order score gap `δ_σ`, its vanishing on constant and on protected weights, and `Θ(c)` on an unprotected weight |
| `verify_DEC.py` | Prop. DEC | the separable-reweighting multiplier identity, the common leading factor `K`, the aggregate `O(c²)`, and the **exact** odds-ratio / Yule's-Q invariance |
| `verify_DRF.py` | Prop. DRF | `Pᴶ_BA(A=1) = q₁` exactly, the drift coefficient `K = q₀(1-q₀)(r₀-β)/Z`, its sign, and the `λ=1` case |
| `verify_LOS.py` | Thm. LOS, §4.4, App. A.3 | the flip characterisation (exhaustive over 3800 exact rational cases), the flat interval `(-c*, c*)`, the band bound, the named constant `f(0)/2·E[δ²]·c²` on four densities, and `o(c)` for an unbounded density and for a law with an atom |
| `verify_SHR.py` | Prop. SHR | the band mass `f(0)E|δ|c`, incidence `Θ(c)` versus intensity `O(c²)`, and `L(c)/share(c) → 0` |
| `verify_PRO.py` | Prop. PRO, App. A.4 | all four proof steps, and the classification exercised on nine statistics — `assoc`, odds ratio, log OR, Yule's Q and the conditional *difference* (protected) against the marginals, the conditional probability and a cell probability (unprotected) — each cross-checked against the actual expansion in `c` |
| `verify_tables.py` | Tables 1, 2 | every order claim in the paper's two summary tables, recomputed from the model |

`jeffrey_core.py` holds the model (prior, Jeffrey steps, benchmark, the route
directions `R₁`, `R₂`, `κ`, `κ'`, `∇assoc`) and the test harness;
`decision_core.py` holds the flip/band machinery for the decision statistics.

## Method notes

* Every quantity in the belief model is a **rational function** of
  `(α, β, c, q₀, r₀)`, so `cancel(together(·))` decides equality exactly.  No
  floating point is used anywhere.
* `exactly` in the paper means a coefficient vanishing identically in the
  parameters — checked by symbolic cancellation.  `generically` means vanishing
  only on a lower-dimensional set — checked (since 2026-10-03) by computing the
  coefficient symbolically in every parameter, asserting its factored closed
  form, and certifying its exact zero set on the open cube: every irreducible
  factor of the numerator over Q is either sign-definite there or listed, each
  listed factor is shown to vanish inside the cube (a rational point, or a sign
  change, which for an irreducible factor means a codimension-one set), and the
  denominator is sign-definite.  The exceptional sets are stated in each
  script's docstring.  A one-point evaluation at the rational point `GENERIC`
  survives only as a row labelled "(sanity, one point)"; no claim rests on it.
* Lemma SEP is a statement about **arbitrary** `N`.  The scripts check it at
  `N = 2…6` (fully symbolically for `N = 2`, and for larger `N` with the
  step normalisers carried as opaque symbols, which is exactly the hypothesis
  the appendix induction uses).  The induction itself is formalised in Lean:
  see `../lean/JeffreyOrder/LemmaSEP.lean`.
* Theorem LOS's `o(c)` claim holds for *any* integrable density; the script
  confirms it on an unbounded one (`f = 1/(4√|u|)`, where the rate is `c^{3/2}`,
  so the `O(c²)` rate genuinely needs the continuity hypothesis of Step 5) and
  on a law with an atom at the threshold.

## Name map (for readers holding an earlier draft)

The results were previously labelled by letter.  The correspondence is:

| was | now | result |
|---|---|---|
| Prop. A1 | **IMM** | exact immunity |
| Prop. A2 | **DIV** | micro divergence |
| Lemma A3 | **ASC** | individual-level association immunity |
| Lemma SEP | **SEP** | separability of attribute-local composites (unchanged) |
| Lemma B1 | **SCR** | first-order score gap |
| Thm. B2 | **LOS** | surplus-weighted loss |
| Prop. C1 | **DEC** | belief-level decoupling |
| Prop. C1′ | **DRF** | marginal-probability drift |
| Prop. C2 | **SHR** | sequence-affected share |
| Prop. C3 | **PRO** | uniqueness of the protected statistic |

Appendix *section* numbers (A.1–A.4) are unchanged; only the named results were
relabelled.
