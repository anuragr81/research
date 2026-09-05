# Lean 4 formalisation of PAPER_B_MANUSCRIPT.tex

A Lean 4 + Mathlib development formalising the results of *"Order-Sensitivity
of Belief and Decision Statistics under Jeffrey Conditioning"*.

Every theorem below is proved with no `sorry` and depends only on Lean's three
standard axioms (`propext`, `Classical.choice`, `Quot.sound`).

```
lake build          # builds everything (~10s once Mathlib is available)
```

## Module map

| module | contents |
|---|---|
| `JeffreyOrder/Basic.lean` | the model of Section 2: `Mat` (a 2×2 joint law), `assoc`, `oddsRatio`, the marginals, `prior α β c`, `jeffreyA`, `jeffreyB`, the Bayes-factor benchmark, and the two routes `PJab`, `PJba` |
| `JeffreyOrder/PropIMM.lean` | **Proposition IMM**: `propIMM_single_cue` (Jeffrey = matched Bayes on one cue, identically in all parameters) and `propIMM_indep` (both routes and the benchmark coincide at `c = 0`) |
| `JeffreyOrder/PropDIV.lean` | **Proposition DIV** and Appendix A.1: closed forms showing every entry of every posterior is (affine in `c`)/(affine in `c`), then `propDIV_gap_AB_*`, `propDIV_gap_BA_*`, `propDIV_seq_*` as genuine `HasDerivAt` statements with values `κR₁`, `κ'R₂`, `κR₁ - κ'R₂`; plus `kappa_eq_zero_iff` (Appendix Step 4) |
| `JeffreyOrder/PropDEC.lean` | **Proposition DEC**'s mechanism, exactly: `sep_assoc` (a separable reweighting rescales the association and can never shift it), `sep_oddsRatio`, and `PJab_sep`/`PJba_sep`/`PB_sep` showing each route *is* such a reweighting — hence `oddsRatio_gap_eq_zero`, the exact form of the manuscript's remark that the odds-ratio departure is zero rather than `O(c²)` |
| `JeffreyOrder/LemmaSEP.lean` | **Lemma SEP** at arbitrary `N`: `isSeparable_applySteps` (any finite sequence of attribute-local updates is a separable reweighting, by induction on the sequence) and `sep_no_pair_interaction` (the equivalent no-interaction formulation, multiplicatively) |
| `JeffreyOrder/PropPRO.lean` | **Lemma ASC** (`inner_gradAssoc_R1`, `inner_gradAssoc_R2`) and **Proposition PRO**: `R1_R2_indep`, `Jmat_gradAssoc_indep`, `propPRO_protection` (part i), `annihilator_eq_span` (the annihilator of the route plane *is* `span{J, ∇assoc}`), `propPRO_uniqueness` (part ii), and `margA_not_in_span` showing why the interior-`λ` hypothesis is essential |
| `JeffreyOrder/Aggregate.lean` | **Lemma SCR** (`lemmaSCR_gap_AB`, `lemmaSCR_gap_BA`: for any weight `v`, `d/dc⟨v, Pᴶ_σ - Pᴮ⟩|₀ = κ_σ⟨v, R_σ⟩`) and **Proposition DRF** (`jeffreyA_pins_mA1` — a Jeffrey step pins its own marginal *exactly in `c`* — and the route-level and aggregate marginal drifts, with signs) |
| `JeffreyOrder/Decision.lean` | **Section 4.4, Theorem LOS, Proposition SHR**: `flips_iff`, `abs_le_of_flips` (the band), `not_flips_of_lt_cstar` (nothing flips while `|c| < c* = |u/δ|`, so every Taylor coefficient of the indicator at `c=0` vanishes), `volume_flipSet` (the band has Lebesgue measure exactly `|cδ|`), and `lintegral_stake_le` (the surplus-weighted loss is at most `|cδ| · μ(band)` — the one extra factor of `c` that separates intensity from incidence) |
| `AssocLocality.lean` | A standalone, self-contained corollary in the spirit of **ASC**: for a *general* prior `(a,b)` and *arbitrary* target marginals `(tA,tB)`, the association order-gap `assoc(Pᴶ_AB) − assoc(Pᴶ_BA)`, cleared of its Jeffrey normalisers, is a polynomial `gapNum` proved divisible by `c²` (`gapNum = c²·gapQuot`, closed by `ring`). This is the local half of "locality is the switch": with attribute-local cues the sequence effect on the association is second order for any prior and any impressions. It uses the `(a,b,c,tA,tB)` parametrisation directly and needs raised `maxHeartbeats`/`maxRecDepth` because each polynomial has ~84 terms. |
| `AssocLocalityStructural.lean` | The **structural** companion to `AssocLocality.lean`: instead of taking the gap polynomial `gapNum` from computer algebra, it *defines* the two Jeffrey routes (`routeAB`, `routeBA`) as staged rescalings of the prior cells and has Lean *derive* the association order-gap. Over a field, with the four Jeffrey normalisers nonzero, `assoc(routeAB) - assoc(routeBA) = c^2 * gapQuot / (D1 D2 D3 D4)` — proved by rewriting each compound mass to its `Dᵢ` closed form, then `field_simp`/`ring`. This closes the seam in the literal-polynomial version, where only `gapNum`'s divisibility by `c²` was checked, not that `gapNum` *is* the model's gap. |

## What is and is not formalised here

The Lean development covers the algebraic and measure-theoretic core: every
exact identity, every leading-coefficient computation (as `HasDerivAt`, so the
`O(c²)` claims are genuine derivative statements rather than asymptotic
assertions), the general-`N` separability induction, the full linear algebra of
the protected class, and the deterministic band structure behind Theorem LOS and
Proposition SHR.

The *statistical* half of Theorem LOS — that `L(c) = f(0)/2 · E[δ²|u=0] · c² +
O(c³)` under a density continuous at the threshold, and that `L(c) = o(c)` for
any integrable law — is verified symbolically in `../sympy/verify_LOS.py` on
four densities plus an unbounded one and a law with an atom, rather than
formalised here; `lintegral_stake_le` gives the `O(c²)` bound that drives it.

## Build setup

`lean-toolchain` pins `leanprover/lean4:v4.32.0-rc1` and `lakefile.toml`
requires Mathlib at the matching tag.  To save a full Mathlib build, this
project's `.lake/packages` is a symlink to the sibling
`../../lean_project/.lake/packages`.  If that project is moved or removed,
replace the symlink with a real directory and run `lake exe cache get` to fetch
Mathlib's prebuilt artefacts.

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
