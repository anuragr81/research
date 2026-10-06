# Starter: weakening the utility hypothesis, and the reference-point addendum

Written 2 September 2026. Read this before doing any work on items U1, U2 or
N5 in `TODO.md`. It supersedes the U2 entry, which asked a question this
document argues is the wrong one.

Read first, in this order: `PROOFS.tex` §Primitives and §Terminology,
`TODO.md` (Now section and the novelty ledger), `SOUNDNESS_20260902.md`
Findings 1-3. Everything below assumes that context.

---

## 0. Status of every claim in this document

Nothing here has entered `PROOFS.tex` and nothing here is proved. The
epistemic state varies by item and is flagged per section. Summary:

| Claim | Status |
|---|---|
| Concavity is used only to get burden-monotonicity | **Verified by inspection** of `PROOFS.tex` and `lean/EntryContest.lean` |
| Burden-monotonicity is strictly weaker than concavity | **PROVED, tier S** (2 Sep). `PROOFS.tex` Claim 1, index row BM; `checks/verify_sympy.py` S22a–S22d, control S23. Stronger than this document originally claimed — see §3. |
| The weakening is thin (knife-edge) | **Exactly characterised for this family** (amplitude `2*eps*sin(k*c/2)` vanishes iff `p` divides `c`); **not characterised in general** — that is §4 |
| Kinked loss aversion flattens the burden below the reference | **Hand algebra only**, not symbolically verified |
| The fall branch collapses under a reference-pivoted spread | **Conjecture** following from the above, unverified |
| Participation-rate corollary | **Conjecture**, no check attempted |
| Lognormal parametrisation | Not started |

The standing rule applies: none of this is claimable until the layer that
would fail if it were false has been named and run. Do not let the
plausibility of the algebra below substitute for that.

---

## 1. The observation this all rests on

`PROOFS.tex` assumes `u` increasing and strictly concave (§Primitives). Grep
every use of concavity in the document and it does exactly one job: it makes
the utility entry cost `kappa(w) = u(w) - u(w-c)` strictly decreasing in `w`.
The only other appearance of `u` in any proof is `u'(w-c) > 0` in P8, which
is monotonicity, not curvature.

The Lean development never assumed concavity in the first place. Every
theorem that needs the cost side takes

    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))

which is burden-monotonicity as a hypothesis on `kap` directly. `u` does not
appear in `EntryContest.lean` at all. So the formal layer is already at the
weaker generality and only the prose is not — the same gap F3 closed for the
tail condition, in the same direction (prose claiming *less* than was
proved).

**Consequence.** The chassis — single crossing, prefix entrant set, count
invariance, the assortative selection, the tail condition, both branches of
the rule — needs exactly one property of `u`:

> `u` has decreasing increments over spans of the fixed length `c`:
> for `w' > w`, `u(w') - u(w'-c) < u(w) - u(w-c)`.

Call this **burden-monotonicity**. It is a condition at the scale of the
entry cost, not a pointwise curvature condition. Concavity implies it.

---

## 2. Item 1 — DONE 2 Sep (main text, not addendum)

> **Done.** All five bullets below are complete, plus an index row (BM) and a
> paragraph in §What the three evidence tiers mean justifying why a claim
> about the primitives is in a table that otherwise records results derived
> within the model. `PROOFS.tex` compiles clean; `verify_sympy.py` is at 29
> checks, 0 failures. §4 remains the open question about whether this
> weakening is substantial, and the warning at the end of this section still
> stands.


Restate the primitive on burden-monotonicity, with concavity demoted to a
sufficient condition. Strictly weakens the paper's assumption at
approximately zero cost, because no proof changes — they all already run on
`kappa`.

Work required:

- `PROOFS.tex` §Primitives: replace "continuous and strictly decreasing in
  `w` by strict concavity of `u`" with the condition itself, then note
  concavity as sufficient. Keep `kappa(w) -> ∞` as `w ↓ c`, which is a
  separate primitive and is used for existence.
- §Terminology: the `entry cost (utility)` row currently derives
  monotonicity from concavity. Change it to state burden-monotonicity as the
  assumption. Add a row for it if it carries load in a stated result — by the
  definitions-table discipline it does, since it becomes a named hypothesis.
- §Contribution: the sentence "under strict concavity of `u` alone" is now
  understating the result. It should say burden-monotonicity, with concavity
  parenthetical.
- Lean: nothing to do. Say so explicitly in the text — that the formal layer
  was always at this generality is worth one sentence, and it is the same
  point F3 made.
- Check: add a SymPy or numeric block confirming concavity ⇒
  burden-monotonicity, and exhibiting the separating example from §3 so the
  implication is shown to be strict.

Do **not** claim the weakening is substantial until §4 is done. It may not
be.

---

## 3. The separating example (EXACT, tier S — upgraded 2 Sep)

> **Update.** This section originally recorded the example as numerically
> witnessed. It is exact, and the exactness was available without extra work.
> The oscillation identity below holds *abstractly* in `k`, `c` and `w`, so no
> family need be assumed and no period sampled: `checks/verify_sympy.py` S22a
> verifies it symbolically, S22b evaluates the amplitude to exactly zero at
> `p ∈ {1, 1/2, 1/3}`, and the burden therefore reduces to
> `sqrt(w) - sqrt(w-1)` *identically*, not approximately. S22d exhibits the
> points where `u'' > 0`. S23 is the non-vacuity control. The claim is now
> `PROOFS.tex` Claim 1 with index row BM.
>
> One methodological note worth carrying forward: the first version of the
> check tested `expand_trig(kappa - concave_part) == 0` per period, and it
> **failed at `p = 1/3`** — not because the mathematics is wrong (the
> expression is identically zero) but because SymPy's `expand_trig` does not
> reduce it. Restructuring the check around the abstract identity plus a
> single amplitude evaluation removed the dependence on simplification
> heuristics. A check whose outcome turns on how hard the CAS tries is not
> a check.


    u(x) = sqrt(x) + eps * sin(2*pi*x/period),   c = 1

With `eps = 0.06` and `period ∈ {1, 1/2, 1/3}` — any period dividing `c` —
`u` is non-concave everywhere (its second derivative changes sign
repeatedly) and the entry burden is strictly decreasing throughout
`w ∈ [1.2, 12]`. At `period = 0.7`, which does not divide `c`,
burden-monotonicity fails.

The algebra behind it: for `u = v + eps*sin(kx)`,

    kappa(w) = v(w) - v(w-c) + 2*eps*sin(k*c/2)*cos(k*(w - c/2))

so the oscillation in the burden has amplitude proportional to
`sin(k*c/2)`, which vanishes exactly when `k*c` is a multiple of `2*pi`.
That is why the construction is knife-edge, and it should be reported as
knife-edge rather than as evidence that non-concavity is generally harmless.

In `checks/verify_sympy.py` as S21–S23 (2 Sep).

---

## 4. Item 4: the scale condition (do before claiming §2 is a real weakening)

Characterise how much non-concavity burden-monotonicity tolerates. The
conjecture is that it is governed by the width of the convex region relative
to `c`: a convex stretch much narrower than `c` is invisible to a burden that
only reads wealth differences of size `c`, and a stretch wider than `c` is
not.

The example in §3 shows the tolerance is not empty. It does not show it is
generous, and the knife-edge structure suggests it is not. Wanted: a
statement of the form "if `u` is concave outside a set of intervals each of
width less than `delta(c)`, and the convexity is bounded by ..., then
burden-monotonicity holds", plus a counterexample on the other side of the
boundary.

This determines whether §2 is a genuine generalisation or a technicality, and
it determines whether S-shaped preferences (§6) are inside or outside scope.
Do not skip it — writing §2 into the paper without it risks claiming a
weakening that turns out to be vacuous in every economically interesting
direction.

---

## 5. Item 2: the participation-rate corollary (highest expected value)

For a mean-pivoted linear spread the pivot is the mean, so the branch
condition — is the marginal entrant richer or poorer than the pivot —
becomes a condition on where the entry count sits relative to the mean
quantile of the wealth distribution.

Conjecture: entry widens with inequality when

    k* / Q  <  Lambda(mean of Lambda)

and narrows when the inequality reverses; the critical participation rate is
the distribution function evaluated at its own mean. Under symmetry that is
one half; under right skew (which is what income distributions do) it is
strictly above one half.

Why this is worth doing before anything else in the addendum: it converts the
pivot condition, which is stated in terms of an unobservable, into a
statement about the observed participation rate. It is the most quotable
sentence the paper could have and it costs one proposition plus a numerical
sweep across distribution families.

Watch for: this is an asymptotic-in-`Q` statement, and `k*` is an integer, so
the exact finite-`Q` version will have a rounding boundary. State which one is
being claimed. Also the mean-pivot is specific to the *linear* spread family
(P9); for general pivot-spreads the pivot is not the mean and the corollary
does not apply as stated.

---

## 6. Item 3: the kinked collapse (the core of the addendum)

Canonical loss aversion: gains at slope 1, losses at slope `lambda > 1`,
reference `r`, `d = w - r`. Hand algebra gives the burden as

    d >= c        ->  kappa = c
    d <= 0        ->  kappa = lambda*c
    0 < d < c     ->  kappa = lambda*c - (lambda-1)*d

Two consequences, both unverified:

1. **The chassis survives.** The burden is weakly decreasing throughout, so
   burden-monotonicity holds, single crossing holds, the entrant set is still
   a prefix, count invariance still holds, the tail condition still applies.
   So "does the model survive a kink" — the question `TODO.md` U2 asks — has
   answer yes, trivially, and is not worth asking.

2. **The fall branch collapses.** Put the reference at the pivot, which is
   what an income-difference reference point delivers for a mean-pivoted
   spread. Everyone below the reference stays below and their burden stays
   pinned at `lambda*c`; everyone above stays above; only agents within `c`
   of the reference move, and they move cheaper. Entry weakly rises and never
   falls. The fall branch does not weaken — it disappears.

If (2) holds it is a result about the mechanism, not a failed extension: the
two-branch rule is a **curvature** result, not a loss-aversion result, and
the fall branch specifically depends on the primitive `kappa -> ∞` as
`w ↓ c`, which piecewise-linear loss aversion destroys by bounding the burden
above at `lambda*c`. Restore curvature in the loss domain and the branch
should return — check this, it is the control that makes the finding
interpretable.

Secondary consequence worth stating if (2) holds: the affordability channel
becomes **localised**. Under concave `u` the burden varies at every wealth
level; here it is flat at `lambda*c`, flat at `c`, and varies only in a band
of width exactly `c` around the reference. Inequality can then only act
through agents near the reference income — a sharper and more testable
statement than anything currently in the paper.

Verification wanted before any of this is claimed: the piecewise algebra in
SymPy; a numeric enumeration in the style of `verify_p9gen.py` R1 showing the
fall branch is empty under a reference-pivoted spread; and the curvature
control.

---

## 7. Item 5: distributional addendum (only if 5 and 6 both land)

Use **lognormal, not normal.** Three reasons, all of which should be stated
in the text rather than left as a choice:

- income is right-skewed, and the skew is exactly what moves the critical
  participation rate off one half, so it is the parameter that earns its
  place;
- the normal has support below `c`, violating the primitive that the burden
  diverges as wealth approaches `c`;
- a normal addendum would give a weaker result and a support problem, and
  would spend the paper's distribution-free claim for nothing.

Keep it as an addendum with the distribution-free results stated first. The
parametrisation illustrates; it must not become a hypothesis of anything.

---

## 8. Framing, and the risk

Motivate the addendum as "what does the model actually need from utility",
not as "let us try prospect theory". The route is: identify the true
hypothesis (§2), show it is weaker than concavity and by how much (§3, §4),
then show which reference-dependent forms sit inside it and what breaks when
they sit outside (§6). The income-difference reference point then arrives as
the economically motivated member of that family rather than as a change of
topic.

Scope this as expected-utility throughout, with real-world probabilities.
Reference-dependence enters through the utility index only. Say so
explicitly: no probability weighting, no subjective probabilities, so the
descriptive/CPT apparatus is deliberately not invoked and the departure from
the existing model is one-dimensional.

Name the reference as **social-comparison based**, not expectations-based, in
§Terminology as well as in prose. "Endogenous reference point" reads as
Koszegi-Rabin to most readers, which would put a fixed point between the
reference and the equilibrium being solved for. This model avoids that, and
the avoidance is worth one explicit sentence.

**The risk to state up front.** The loss-aversion paper
(`/areas/loss-aversion-boom-bust-paper`) ended with theory fully verified and
no empirical prediction surviving its own null. This direction has the same
shape, and §6's collapse result *is* a null. It is publishable if framed as
locating the mechanism — showing which assumption the comparative static
actually rests on — and a dead end if framed as an extension that generalises
the model. Decide the framing before writing, not after.

---

## 9. Explicitly out of scope

- **S-shaped preferences with a convex region wider than `c`.** The burden
  stops being monotone, single crossing fails, and the prefix structure and
  count invariance go together. That is a different model, not an extension.
  Revisit only if the resulting multiplicity is itself the object of study.
- **Expectations-based (Koszegi-Rabin) reference points.** Introduces a fixed
  point between the reference and the equilibrium. Different paper.
- **Normal distribution.** See §7.
- **`V` entering the wealth argument of `u`.** This is U1's trap: it moves
  the model into Schroyen-Treich's rent-seeking contest where the wealth
  effects cancel under CARA. `SOUNDNESS_20260902.md` Finding 2 pins the
  model's form as `V` staying a pure win/lose indicator outside `u`, and
  everything in this document assumes that. Reference-dependence must act on
  the cost side only.

---

## 10. Suggested order

1. §2 restate the primitive (main text, cheap, do regardless of the rest)
2. §5 participation-rate corollary (cheap, general, highest value)
3. §6 kinked collapse plus curvature control (the addendum's core)
4. §4 scale condition (needed to know whether §2 was substantial)
5. §7 lognormal (only if 2 and 3 land)

Items 2 and 3 are not headline results individually. Together they might be:
one says where in the distribution inequality bites, the other says why. That
is the only combination in this document worth calling a contribution, and it
should not be described as one until both are verified.
