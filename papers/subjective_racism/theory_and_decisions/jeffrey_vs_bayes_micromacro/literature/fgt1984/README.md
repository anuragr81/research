# Foster, J., Greer, J. & Thorbecke, E. (1984), "A Class of Decomposable Poverty Measures"

*Econometrica* 52(3), 761-766 (May 1984). I read all nine PDF pages of the JSTOR
copy: the cover, journal pp. 761-766, and two pages of linked citations. The
equations were read from the page images, because the text layer drops them.

## Claims formalized

Setup (pp. 761-762):

* incomes `y_1 ≤ … ≤ y_n` and a poverty line `z > 0`;
* shortfalls `g_i = z - y_i`;
* `q` = the number of poor, "having income no greater than `z`";
* the headcount ratio `H = q/n`;
* the income-gap ratio `I = Σ_{i≤q} g_i/(qz)`.

The claims:

* **(3), p. 763.** `P_α(y; z) = (1/n) Σ_{i=1}^{q} (g_i/z)^α` for `α ≥ 0`. "The
  measure `P_0` is simply the headcount ratio `H`, while `P_1` is `H · I`, a
  renormalization of the income-gap measure. The measure `P` is obtained by
  setting `α = 2`."
* **(2), p. 762.** `P = P_2 = H[I² + (1-I)² C_p²]`.
* **Proposition 1, p. 763.** `P_α` satisfies:
  * the Monotonicity Axiom for `α > 0`;
  * the Transfer Axiom for `α > 1`;
  * the Transfer Sensitivity Axiom for `α > 2`.
* **Proposition 2 and (4), p. 764.** `P_α(y; z) = Σ_j (n_j/n) P_α(y^(j); z)`. This
  is additive decomposability with population-share weights, and it implies the
  Subgroup Monotonicity Axiom (p. 763).
* **Footnote 6, p. 763.** Cowell's example, in which the Sen measure violates
  subgroup monotonicity.
* **Section 4 and Table I, pp. 764-765.** The Nairobi decomposition of `P_2`.

## Result

`lean/FGT.lean` is a symlink to `lean/Literature/FGT.lean`. It compiles with
`lake env lean`, has no `sorry`, and its headline theorems use only the axioms
`[propext, Classical.choice, Quot.sound]`.

The population is a `Finset` of households. Each poor household (`y_i ≤ z`)
contributes `((z-y_i)/z)^α`, a real power, and each non-poor household
contributes 0.

**Identities for `P_0`, `P_1` and `P_2`:**

* `P0_eq_H`: `P_0 = H`. Because the paper uses `≤`, a household exactly at the
  line counts in `P_0` (since `0^0 = 1`) and contributes 0 to `P_α` for `α > 0`.
* `P1_eq_H_mul_I`: `P_1 = H · I`.
* `P1_eq_popMean_normGap`: `P_1 = (1/n) Σ_i max(z - y_i, 0)/z`. This is the mean
  of the normalised shortfall **over the whole population**, with the non-poor
  counting as 0.
* `I_eq_meanGapPoor_div_z`, `P1_eq_H_mul_meanGapPoor`:
  * The mean shortfall among the poor, `(1/q)Σ g_i`, equals `z·I`.
  * `P_1 = H · (mean shortfall among the poor)/z`.
* `P2_formula`: equation (2), for general finite populations.

**Proposition 2 (decomposability):**

* `P_decomp`, `P_decomp_shares`: **Proposition 2** for any finite partition, in
  both the `n·P = Σ n_j P_j` form and the population-share form.
* `subgroup_monotone`: the Subgroup Monotonicity Axiom, for a two-way split.

**Proposition 1 (monotonicity and transfers):**

* `P_monotone`: **Proposition 1**, the Monotonicity Axiom for `α > 0`.
* `P0_not_monotone`: the axiom fails at `α = 0`, because the headcount ignores
  depth.
* `P_transfer`: **Proposition 1**, the Transfer Axiom for `α > 1`, in case (i) of
  the paper's proof (the recipient is richer and stays poor). It uses the paper's
  own argument, strict convexity of `x ↦ x^α` (`strictConvexOn_rpow`), through
  the general lemma `strictConvex_spread`.
* `P_transfer_to_nonpoor`: case (ii) of the paper's proof (the recipient is at or
  above the line), which holds for every `α > 0`.
* `P1_transfer_neutral`: the Transfer Axiom fails at `α = 1`. A case-(i) transfer
  leaves `P_1 = 5/8` unchanged, while `P_2` rises from 13/32 to 29/64.

**Decomposability separates `P_1` from `I`:**

* `I_not_decomposable`, `P1_decomposes_on_example`: on the same three households
  (`z = 1`, incomes `(0, 1/2, 2)`, split into `{0}` and `{1/2, 2}`):
  * the overall `I` is 3/4, but the share-weighted `I` is 2/3;
  * `P_1` decomposes exactly, with 1/2 on both sides.

**`sympy/check_fgt.py`** runs 26/26 checks in exact rationals and exits 0. It
covers:

* the identities above;
* (2), checked symbolically;
* Proposition 2, checked symbolically in `α`;
* the boundary cases of Proposition 1;
* footnote 6 (see below);
* Table I (see below).

## What formalizing revealed

**1. `P_1` is not "the average of the poor's distances from the line".** FGT's
`P_1` is that average, divided by `z`, times `H`. Equivalently, it is the
whole-population mean of the normalised shortfall, with the non-poor counting as
zero.

The mean shortfall among the poor, `z·I`, is FGT's `I` rescaled. It is not a
member of the `P_α` family. It is also **not** additively decomposable with
population-share weights (`I_not_decomposable`), and decomposability is the
paper's whole point.

**2. `P_0` and `P_1` are the boundary members for the two axioms.** `P_0` fails
monotonicity, since the headcount cannot see depth (`P0_not_monotone`). `P_1`
satisfies monotonicity but fails the transfer axiom, since it is linear in the
gaps (`P1_transfer_neutral`). So Proposition 1's thresholds `α > 0` and `α > 1`
are sharp at the integers 0 and 1. The paper states the thresholds but does not
exhibit the failures.

**3. Footnote 6 reproduces.** For `z = 13, 14, 20, 100`:

* the Sen (1976) measure ranks `y^(1) = (1,6,12)` as poorer than
  `ŷ^(1) = (3,3,13)`;
* yet it ranks the whole population `ŷ` as poorer than `y`.

Every `P_α` (α = 0, …, 3) ranks the subgroup and the whole population in the same
direction.

**4. Table I (p. 764) contains a small internal inconsistency.** The row totals
check out:

* the group sizes sum to 3987;
* the printed percentages sum to 99.9, as the table notes;
* the headcount decomposes to 0.1348, consistent with the "13 per cent" on
  p. 765;
* every row satisfies `P_2 ≥ H·I²`, which follows from (2) with
  `I = 1 - ȳ_p/z`, `z = 515`.

But the row for heads resident 6-10 years does not match its own entries. With
`n_j = 793` and `P_2 = .0343`, its percentage contribution must lie in
[12.197, 12.255]%, even allowing for rounding in all printed figures. The table
prints 12.1%.

The recomputed total `Σ(n_j/n)P_2^(j) = 0.05574` also rounds to 0.0557, not the
printed 0.0558. It is within 1e-4 of it and consistent with the text's "0.056".

These are printing or rounding slips. They do not affect the mathematics, and the
script asserts them as documented findings.

## Bearing on Paper B

Paper B pairs the **share** of evaluators whose decision is flipped by the reading
sequence with the **surplus-weighted loss**
`L(c) = E[|u| · 1{flip}]` (MS:925; Theorem LOS).

* The share is the analogue of `H = P_0`, the population share past a threshold.
* `L(c)` is the analogue of `P_1`: a whole-population mean in which each affected
  individual contributes its distance `|u|` from the decision threshold `u = 0`,
  and the unaffected contribute zero. There is no normalisation by a line `z`, so
  it is `P_1` "up to normalisation".
* The identity `P_1 = H · (mean gap among the poor)/z`
  (`P1_eq_H_mul_meanGapPoor`) has the exact analogue
  `L(c) = Pr(flip) · E[|u| | flip]`. This is the arithmetic of Theorem LOS: the
  share is `O(c)`, and the flipped sit within `O(c)` of the threshold, so the loss
  is `O(c²)`.

The FGT framing therefore fits the manuscript better than its current gloss
suggests. Decomposability (Proposition 2) is also the property that lets both
statistics be aggregated from subpopulations to the population by population
shares. The "mean among the affected" version would not decompose.

## Not formalized

* The Transfer Sensitivity Axiom for `α > 2`. FGT cite Kolm [11, p. 88] and give
  no proof.
* The "Rawlsian" limit as `α → ∞`.
* The statement that `C²` is the inequality measure "corresponding" to `P`.
* The general-`m` Subgroup Monotonicity Axiom. It follows from `P_decomp` in the
  same way as the two-group `subgroup_monotone`.

## Audit findings (2026-09-29)

**How the manuscript uses the paper.** MS:297:

> "This is the standard incidence-versus-intensity pairing of the measurement
> literature, where a headcount (a count of those past a threshold) and a mean
> shortfall (average of their distances from it) are first two members of a
> single parametrised family of indices \citep{FosterGreerThorbecke1984}."

The bibliography entry is `bibliography.bib:219-226`.

**What the audit found** (`verify_discrimination_econ.md` §6, F1-F6), with what
the formalization adds:

* **F1: "a single parametrised family"** (VERIFIED). This is (3), p. 763.
* **F2: "a headcount (a count of those past a threshold)"**
  (VERIFIED-WITH-CAVEAT).
  * `P0_eq_H` shows that `P_0` is the headcount **ratio** `q/n`, a share, not a
    count.
  * The paper's "poor" includes those exactly at the line (`y ≤ z`).
  * **Confirmed.**
* **F3: "a mean shortfall (average of their distances from it)"** (MISQUOTED).
  * `P1_eq_H_mul_I` and `P1_eq_popMean_normGap` show that `P_1` is the average
    over **the whole population** of the shortfall **normalised by `z`**, with
    the non-poor contributing 0.
  * "The average of their distances" is `(1/q)Σ g_i = z·I`
    (`I_eq_meanGapPoor_div_z`). That is not in the family, and it is not
    population-share decomposable (`I_not_decomposable`).
  * **Confirmed.** The audit's observation that the manuscript's own `L(c)` is of
    the correct `P_1` kind is also confirmed (see Bearing on Paper B).
* **F4: "first two members"** (VERIFIED-WITH-CAVEAT). `α` ranges over the reals
  `≥ 0`, so 0 and 1 are the first two *integer* members, and FGT's headline
  measure is `α = 2`.
  * **Confirmed, with one addition:** 0 and 1 are also exactly the thresholds at
    which the monotonicity and transfer axioms switch on (`P0_not_monotone`,
    `P1_transfer_neutral`).
  * That gives a principled reason to single them out: `P_0` registers only *how
    many*, and `P_1` is the least sensitive member that registers *how much*.
* **F5: "incidence-versus-intensity pairing"** (VERIFIED-WITH-CAVEAT). FGT do not
  use these words; the pairing is later usage. **Confirmed.**
* **F6: bibliographic data** (VERIFIED). *Econometrica* 52(3), May 1984,
  pp. 761-766.
* **New (not in the audit):** Table I has a small internal inconsistency (row
  "6-10": 12.1% printed, 12.2% implied). It does not affect any manuscript claim.

**Proposed corrected wording for MS:297** (not applied):

> This is the standard incidence-versus-intensity pairing of the
> poverty-measurement literature: the headcount ratio (the share of the
> population at or past a threshold) and the average normalised shortfall (each
> individual's distance past the threshold as a fraction of it, averaged over the
> whole population, with those not past it contributing zero) are the $\alpha=0$
> and $\alpha=1$ members of the parametrised family $P_\alpha$ of
> \citet{FosterGreerThorbecke1984}, both additively decomposable across
> subpopulations with population-share weights. Our share of flipped decisions
> is the analogue of the headcount ratio and $L(c)$, up to normalisation, of the
> average shortfall; as there, $L(c)$ factors as the share times the mean
> shortfall among those affected.

If the sentence must stay short: replace "a headcount (a count of those past a
threshold)" with "the headcount ratio (the share past a threshold)". Replace "a
mean shortfall (average of their distances from it)" with "the average
normalised shortfall (distances past it, averaged over the whole population)".
Replace "are first two members" with "are the $\alpha = 0$ and $\alpha = 1$
members".
